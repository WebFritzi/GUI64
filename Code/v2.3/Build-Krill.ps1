[CmdletBinding()]
param(
    [string]$AssemblerDirectory = $env:GUI64_CC65_BIN,
    [switch]$Rebuild,
    [switch]$UpdatePrebuilt
)
$ErrorActionPreference = 'Stop'
$krillRoot = $PSScriptRoot
$krillOutput = Join-Path $krillRoot 'build/krill'
$krillPrebuilt = Join-Path $krillRoot 'Krill/prebuilt'
$krillFiles = @('loader.bin', 'install.bin', 'drivecode.bin', 'fallback.bin', 'guard.bin', 'install-bank.bin', 'copy-bank.bin', 'symbols.asm')
New-Item -ItemType Directory -Force -Path $krillOutput | Out-Null

# Fingerprint relative paths and source bytes so cached binaries cannot conceal
# a changed config, linker layout, or upstream source file.
$krillInputs = @(Get-ChildItem -LiteralPath (Join-Path $krillRoot 'ThirdParty/Krill') -Recurse -File |
    Where-Object { $_.Extension -in '.s', '.inc' })
$krillInputs += Get-Item -LiteralPath (Join-Path $krillRoot 'Krill/loaderconfig.inc')
$krillInputs += Get-Item -LiteralPath (Join-Path $krillRoot 'Krill/cartridge.cfg')
$krillInputs += Get-Item -LiteralPath (Join-Path $krillRoot 'Krill/copy-drive.asm')
$krillInputs += Get-Item -LiteralPath $PSCommandPath
$krillInputLines = @($krillInputs | ForEach-Object {
    $krillRelative = $_.FullName.Substring($krillRoot.Length + 1).Replace('\', '/')
    '{0} {1}' -f $krillRelative, (Get-FileHash -LiteralPath $_.FullName -Algorithm SHA256).Hash
})
[Array]::Sort($krillInputLines, [StringComparer]::Ordinal)
$krillSha = [Security.Cryptography.SHA256]::Create()
try {
    $krillSourceHash = [BitConverter]::ToString($krillSha.ComputeHash(
        [Text.Encoding]::UTF8.GetBytes(($krillInputLines -join "`n")))).Replace('-', '')
} finally { $krillSha.Dispose() }

if (-not $Rebuild -and -not $UpdatePrebuilt -and -not $AssemblerDirectory) {
    $krillManifestPath = Join-Path $krillPrebuilt 'manifest.json'
    if (-not (Test-Path -LiteralPath $krillManifestPath)) {
        throw 'Krill prebuilt files missing. Run Build-Krill.ps1 -Rebuild with ca65/ld65 available.'
    }
    $krillManifest = Get-Content -LiteralPath $krillManifestPath -Raw | ConvertFrom-Json
    if ($krillManifest.SourceHash -ne $krillSourceHash) {
        throw 'Krill source/config changed. Rebuild with Build-Krill.ps1 -Rebuild (ca65/ld65 required).'
    }
    foreach ($krillFile in $krillFiles) {
        $krillCached = Join-Path $krillPrebuilt $krillFile
        if ((Get-FileHash -LiteralPath $krillCached -Algorithm SHA256).Hash -ne $krillManifest.Files.$krillFile) {
            throw "Krill prebuilt checksum mismatch: $krillFile"
        }
        Copy-Item -LiteralPath $krillCached -Destination (Join-Path $krillOutput $krillFile) -Force
    }
} else {
    if ($AssemblerDirectory) {
        $krillCa65 = Join-Path $AssemblerDirectory 'ca65.exe'
        $krillLd65 = Join-Path $AssemblerDirectory 'ld65.exe'
    } else {
        $krillCa65 = (Get-Command ca65 -CommandType Application -ErrorAction Stop).Source
        $krillLd65 = (Get-Command ld65 -CommandType Application -ErrorAction Stop).Source
    }
    Push-Location -LiteralPath $krillRoot
    try {
        $krillArguments = @('--cpu', '6502X', '-t', 'c64', '-D', 'PLATFORM=64', '-D', 'EXTCONFIGPATH=1', '-D', 'KERNAL_FALLBACK_SEI_WORKAROUNDS=0',
            '-I', 'Krill', '-I', 'ThirdParty/Krill/loader/include',
            '-I', 'ThirdParty/Krill/shared', '-I', 'ThirdParty/Krill/loader/src')
        foreach ($krillModule in @('resident', 'install')) {
            & $krillCa65 @krillArguments -o "build/krill/$krillModule.o" "ThirdParty/Krill/loader/src/$krillModule.s"
            if ($LASTEXITCODE -ne 0) { throw "Krill $krillModule assembly failed ($LASTEXITCODE)." }
        }
        & $krillLd65 -C 'Krill/cartridge.cfg' -m 'build/krill/loader.map' -Ln 'build/krill/loader.labels' `
            'build/krill/resident.o' 'build/krill/install.o'
        if ($LASTEXITCODE -ne 0) { throw "Krill link failed ($LASTEXITCODE)." }
        $krillSymbols = @{}
        foreach ($krillLine in [IO.File]::ReadAllLines((Join-Path $krillOutput 'loader.labels'))) {
            if ($krillLine -match '^al ([0-9A-Fa-f]+) \.(\S+)$' -and -not $krillSymbols.ContainsKey($Matches[2])) {
                $krillSymbols[$Matches[2]] = [Convert]::ToInt32($Matches[1], 16)
            }
        }
        # Assemble a separate 1541 copy overlay from exported addresses, then
        # apply only its bounded records to a second, otherwise identical bank.
        $copySymbols = @($krillSymbols.Keys | Where-Object { $_ -like 'mdCopy*' } | Sort-Object | ForEach-Object {
            '{0} = ${1:x4}' -f $_,$krillSymbols[$_]
        })
        [IO.File]::WriteAllLines((Join-Path $krillOutput 'copy-drive-symbols.asm'), $copySymbols, [Text.UTF8Encoding]::new($false))
        & acme -f plain -o 'build/krill/copy-patches.bin' 'Krill/copy-drive.asm'
        if ($LASTEXITCODE -ne 0) { throw 'Krill copy overlay assembly failed.' }
        $copyBank = [IO.File]::ReadAllBytes((Join-Path $krillOutput 'install-bank.bin'))
        $copyPatches = [IO.File]::ReadAllBytes((Join-Path $krillOutput 'copy-patches.bin'))
        $copyAt = 0
        while ($copyAt + 4 -le $copyPatches.Length) {
            $copyOffset = [int]$copyPatches[$copyAt] + 256*[int]$copyPatches[$copyAt+1]
            $copyCount = [int]$copyPatches[$copyAt+2] + 256*[int]$copyPatches[$copyAt+3]
            $copyAt += 4
            if ($copyOffset -eq 65535) { break }
            if ($copyCount -lt 1 -or $copyAt+$copyCount -gt $copyPatches.Length -or
                $copyOffset -lt 0 -or $copyOffset+$copyCount -gt $krillSymbols['mdCopyImageEnd']-0x8000) {
                throw 'Invalid Krill copy overlay record.'
            }
            [Array]::Copy($copyPatches,$copyAt,$copyBank,$copyOffset,$copyCount)
            $copyAt += $copyCount
        }
        if ($copyOffset -ne 65535 -or $copyAt -ne $copyPatches.Length) { throw 'Incomplete Krill copy overlay.' }
        [IO.File]::WriteAllBytes((Join-Path $krillOutput 'copy-bank.bin'),$copyBank)
        # Both read-only segments share one bank; extract individual files for inspection.
        $krillBank = [IO.File]::ReadAllBytes((Join-Path $krillOutput 'install-bank.bin'))
        foreach ($krillSegment in @(@('DISKIO_DRIVECODE','drivecode.bin'), @('DISKIO_INSTALL','install.bin'), @('DISKIO_FALLBACK','fallback.bin'))) {
            $krillOffset = $krillSymbols['__'+$krillSegment[0]+'_LOAD__'] - 0x8000
            $krillLength = $krillSymbols['__'+$krillSegment[0]+'_SIZE__']
            $krillBytes = New-Object byte[] $krillLength
            [Array]::Copy($krillBank, $krillOffset, $krillBytes, 0, $krillLength)
            [IO.File]::WriteAllBytes((Join-Path $krillOutput $krillSegment[1]), $krillBytes)
        }
        $krillConstants = @('KRILL_ROM_BANK = 5', 'KRILL_RESIDENT_RAM = $0500', 'KRILL_GAME_BANK = 6', 'KRILL_GAME_STATE = $07d0',
            ('KRILL_INSTALL_STATE_RAM = ${0:x4}' -f $krillSymbols['mdState']),
            ('KRILL_INSTALL_STATE_SIZE = {0}' -f ($krillSymbols['mdStateEnd']-$krillSymbols['mdState'])))
        foreach ($krillSymbol in @('install', 'uninstall', 'loadraw', 'mdWarning', 'endaddrlo', 'endaddrhi',
                'loadaddrlo', 'loadaddrhi', 'BLOCKDESTLO', 'mdBootSource', 'mdFastSource', 'openfile', 'pollblock', 'closefile',
                'mdGuardBody', 'mdAppStart', 'mdAppLimit', 'mdAppMode', 'mdAppError', 'mdGameWritten', 'mdGameWrap', 'mdInstallProgressFirst', 'mdInstallProgressSecond')) {
            if (-not $krillSymbols.ContainsKey($krillSymbol)) { throw "Missing Krill export: $krillSymbol" }
            $krillPrefix = 'KRILL_'
            if ($krillSymbol -in 'install','uninstall','loadraw','openfile','pollblock','closefile') { $krillPrefix = 'KRILL_CORE_' }
            $krillConstants += '{0}{1} = ${2:x4}' -f $krillPrefix,$krillSymbol.ToUpperInvariant(),$krillSymbols[$krillSymbol]
        }
        $krillConstants += 'KRILL_BANK_END = ${0:x4}' -f (0x8000 + $krillBank.Length)
        $krillConstants += 'KRILL_FALLBACK_SIZE = {0}' -f $krillSymbols['__DISKIO_FALLBACK_SIZE__']
        $krillConstants += 'KRILL_INSTALL_SIZE = {0}' -f (Get-Item -LiteralPath (Join-Path $krillOutput 'install.bin')).Length
        $krillConstants += 'KRILL_DRIVECODE_SIZE = {0}' -f (Get-Item -LiteralPath (Join-Path $krillOutput 'drivecode.bin')).Length
        [IO.File]::WriteAllLines((Join-Path $krillOutput 'symbols.asm'), $krillConstants, [Text.UTF8Encoding]::new($false))
    } finally { Pop-Location }
    if ($UpdatePrebuilt) {
        New-Item -ItemType Directory -Force -Path $krillPrebuilt | Out-Null
        $krillHashes = [ordered]@{}
        foreach ($krillFile in $krillFiles) {
            Copy-Item -LiteralPath (Join-Path $krillOutput $krillFile) -Destination (Join-Path $krillPrebuilt $krillFile) -Force
            $krillHashes[$krillFile] = (Get-FileHash -LiteralPath (Join-Path $krillOutput $krillFile) -Algorithm SHA256).Hash
        }
        $krillManifest = [ordered]@{ Revision = 194; SourceHash = $krillSourceHash; Files = $krillHashes }
        [IO.File]::WriteAllText((Join-Path $krillPrebuilt 'manifest.json'),
            ($krillManifest | ConvertTo-Json -Depth 3), [Text.UTF8Encoding]::new($false))
    }
}
$krillInstallSize = (Get-Item -LiteralPath (Join-Path $krillOutput 'install.bin')).Length
$krillResidentSize = (Get-Item -LiteralPath (Join-Path $krillOutput 'loader.bin')).Length
$krillDriveSize = (Get-Item -LiteralPath (Join-Path $krillOutput 'drivecode.bin')).Length
if ($krillInstallSize -lt 1 -or $krillInstallSize -gt 0x2000) { throw 'Krill installer exceeds one ROM bank.' }
if ($krillResidentSize -lt 1 -or $krillResidentSize -gt 0x2d0) { throw 'Krill resident exceeds $0500-$07CF.' }
if ($krillDriveSize -lt 1 -or $krillDriveSize -gt 0x2000) { throw 'Krill drive images exceed ROM bank 5.' }
$krillFallbackSize = (Get-Item -LiteralPath (Join-Path $krillOutput 'fallback.bin')).Length
$krillBankSize = (Get-Item -LiteralPath (Join-Path $krillOutput 'install-bank.bin')).Length
if ($krillBankSize -ne ($krillDriveSize + $krillInstallSize + $krillFallbackSize) -or $krillBankSize -gt 0x2000) { throw 'Krill installer, support and drive images exceed bank 5 or contain a gap.' }
Write-Host ("Krill v194: ROM installer {0} bytes, resident {1} bytes, drive images {2} bytes, ROM support {3} bytes." -f `
    $krillInstallSize, $krillResidentSize, $krillDriveSize, $krillFallbackSize)