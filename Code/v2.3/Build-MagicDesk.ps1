[CmdletBinding()]
param(
    [ValidateSet('WIN', 'MAC')]
    [string]$Design = 'WIN'
)

$ErrorActionPreference = 'Stop'
$designName = $Design.ToLowerInvariant()
$buildPath = Join-Path $PSScriptRoot 'build'

function Write-BigEndian16 {
    param([System.IO.BinaryWriter]$Writer, [int]$Value)
    $Writer.Write([byte](($Value -shr 8) -band 255))
    $Writer.Write([byte]($Value -band 255))
}

function Write-BigEndian32 {
    param([System.IO.BinaryWriter]$Writer, [int]$Value)
    $Writer.Write([byte](($Value -shr 24) -band 255))
    $Writer.Write([byte](($Value -shr 16) -band 255))
    Write-BigEndian16 $Writer ($Value -band 65535)
}

Push-Location -LiteralPath $PSScriptRoot
try {
    & (Join-Path $PSScriptRoot 'Build-Krill.ps1')
    $acme = (Get-Command acme -CommandType Application -ErrorAction Stop).Source
    New-Item -ItemType Directory -Force -Path $buildPath | Out-Null
    $baseName = "GUI64.$designName"
    $prgPath = Join-Path $buildPath "$baseName.cart.prg"
    $romPath = Join-Path $buildPath "$baseName.magicdesk.bin"
    $crtPath = Join-Path $buildPath "$baseName.crt"
    $defines = @()
    if ($Design -eq 'MAC') { $defines += '-DMAC=1' }

    & $acme @defines -f cbm -r "build/$baseName.cart.report" --vicelabels "build/$baseName.cart.labels" -o $prgPath 'GUI64.asm'
    if ($LASTEXITCODE -ne 0) { throw "GUI64 $Design assembly failed ($LASTEXITCODE)." }

    $prg = [System.IO.File]::ReadAllBytes($prgPath)
    if ($prg.Length -lt 3) { throw 'GUI64 RAM image is missing its header or payload.' }
    $loadAddress = [int]$prg[0] + ([int]$prg[1] -shl 8)
    if ($loadAddress -ne 0x033c) { throw 'Expected the GUI64 RAM image at $033C.' }

    # The intermediate PRG is the cartridge's RAM payload, not a standalone build.
    # Read its entry from assembler labels; no KERNAL autostart vector is emitted.
    $guiLabels = @{}
    foreach ($guiLabelLine in [IO.File]::ReadAllLines((Join-Path $buildPath "$baseName.cart.labels"))) {
        if ($guiLabelLine -match '^al C:([0-9A-Fa-f]+) \.(\S+)$') {
            $guiLabels[$Matches[2]] = [Convert]::ToInt32($Matches[1], 16)
        }
    }
    foreach ($romSymbol in 'autostart','Krill_ROMImage','Krill_ROMStart','Krill_ROMSize','Krill_GameROMImage','Krill_GameROMSize') {
        if (-not $guiLabels.ContainsKey($romSymbol)) { throw "Missing ROM control symbol: $romSymbol" }
    }
    $entry = $guiLabels['autostart']
    $controlOffset = $guiLabels['Krill_ROMImage'] - $loadAddress
    $controlSize = $guiLabels['Krill_ROMSize']
    $controlStart = $guiLabels['Krill_ROMStart']
    $gameOffset = $guiLabels['Krill_GameROMImage'] - $loadAddress
    $gameSize = $guiLabels['Krill_GameROMSize']
    if ($controlOffset -lt 1 -or $controlSize -lt 1 -or
        $controlOffset + $controlSize -ne $gameOffset -or
        $gameSize -lt 1 -or $gameOffset + $gameSize -ne $prg.Length - 2 -or
        $controlStart -lt 0x8000 -or $controlStart + $controlSize -gt 0xa000) {
        throw 'Invalid appended ROM control layout.'
    }
    $control = New-Object byte[] $controlSize
    [Array]::Copy($prg, 2 + $controlOffset, $control, 0, $controlSize)
    [IO.File]::WriteAllBytes((Join-Path $buildPath "$baseName.krill-control.bin"), $control)
    $game = New-Object byte[] $gameSize
    [Array]::Copy($prg, 2 + $gameOffset, $game, 0, $gameSize)
    [IO.File]::WriteAllBytes((Join-Path $buildPath "$baseName.krill-game.bin"), $game)
    $payloadSize = $controlOffset
    if ($payloadSize -lt 1) { throw 'GUI64 PRG contains no code at $033C.' }
    $cartPrg = New-Object byte[] ($payloadSize + 2)
    $cartPrg[0] = [byte]($loadAddress -band 255)
    $cartPrg[1] = [byte]($loadAddress -shr 8)
    [Array]::Copy($prg, 2, $cartPrg, 2, $payloadSize)
    [System.IO.File]::WriteAllBytes($prgPath, $cartPrg)
    $prg = $cartPrg

    & $acme @defines "-DMD_IMAGE_START=$loadAddress" "-DMD_IMAGE_SIZE=$payloadSize" "-DMD_ENTRY=$entry" -f plain -r "build/$baseName.magicdesk.report" --vicelabels "build/$baseName.magicdesk.labels" -o $romPath 'MagicDesk.asm'
    if ($LASTEXITCODE -ne 0) { throw "Magic Desk $Design assembly failed ($LASTEXITCODE)." }

    $rom = [System.IO.File]::ReadAllBytes($romPath)
    if ($rom.Length -ne 65536) { throw "Expected 65536 ROM bytes, got $($rom.Length)." }
    for ($index = 0; $index -lt $payloadSize; $index++) {
        if ($rom[8192 + $index] -ne $prg[2 + $index]) {
            throw "ROM payload differs from GUI64 PRG at byte $index."
        }
    }

    $krillBytes = [IO.File]::ReadAllBytes((Join-Path $buildPath 'krill/install-bank.bin'))
    for ($index = 0; $index -lt $krillBytes.Length; $index++) {
        if ($rom[5 * 8192 + $index] -ne $krillBytes[$index]) {
            throw "ROM Krill bank differs at byte $index."
        }
    }
    if ($controlStart -ne 0x8000 + $krillBytes.Length) { throw 'ROM control must follow the installer.' }
    for ($index = 0; $index -lt $control.Length; $index++) {
        if ($rom[5 * 8192 + $krillBytes.Length + $index] -ne $control[$index]) {
            throw "ROM control differs at byte $index."
        }
    }
    for ($index = 0; $index -lt $game.Length; $index++) {
        if ($rom[6 * 8192 + $index] -ne $game[$index]) { throw "Game ROM differs at byte $index." }
    }
    $guard = [IO.File]::ReadAllBytes((Join-Path $buildPath 'krill/guard.bin'))
    for ($index = 0; $index -lt $guard.Length; $index++) {
        if ($rom[6 * 8192 + 0x200 + $index] -ne $guard[$index]) { throw "Shared guard ROM differs at byte $index." }
    }
    $copyBank = [IO.File]::ReadAllBytes((Join-Path $buildPath 'krill/copy-bank.bin'))
    if ($copyBank.Length -ne $krillBytes.Length) { throw 'Copy installer bank size differs.' }
    for ($index = 0; $index -lt 8192; $index++) {
        $expectedCopyByte = 255
        if ($index -lt $copyBank.Length) { $expectedCopyByte = $copyBank[$index] }
        if ($rom[7 * 8192 + $index] -ne $expectedCopyByte) { throw "Copy ROM differs at byte $index." }
    }
    $stream = New-Object System.IO.MemoryStream
    $writer = New-Object System.IO.BinaryWriter($stream)
    try {
        $writer.Write([System.Text.Encoding]::ASCII.GetBytes('C64 CARTRIDGE   '))
        Write-BigEndian32 $writer 64
        Write-BigEndian16 $writer 0x0100
        Write-BigEndian16 $writer 19  # Magic Desk
        $writer.Write([byte]0)        # EXROM low
        $writer.Write([byte]1)        # GAME high: 8 KiB cartridge
        $writer.Write((New-Object byte[] 6))
        $name = New-Object byte[] 32
        $nameBytes = [System.Text.Encoding]::ASCII.GetBytes("GUI64 $Design Magic Desk")
        [Array]::Copy($nameBytes, $name, $nameBytes.Length)
        $writer.Write($name)
        for ($bank = 0; $bank -lt 8; $bank++) {
            $writer.Write([System.Text.Encoding]::ASCII.GetBytes('CHIP'))
            Write-BigEndian32 $writer 0x2010
            Write-BigEndian16 $writer 0       # ROM
            Write-BigEndian16 $writer $bank
            Write-BigEndian16 $writer 0x8000
            Write-BigEndian16 $writer 0x2000
            $writer.Write($rom, $bank * 8192, 8192)
        }
        $writer.Flush()
        [System.IO.File]::WriteAllBytes($crtPath, $stream.ToArray())
    }
    finally {
        $writer.Dispose()
        $stream.Dispose()
    }
    Write-Host ('Built {0}: 64 KiB ROM, {1} bytes CRT, GUI64 ${2:X4}-${3:X4}, entry ${4:X4}.' -f $crtPath, (Get-Item -LiteralPath $crtPath).Length, $loadAddress, ($loadAddress + $payloadSize - 1), $entry)
}
finally {
    Pop-Location
}
