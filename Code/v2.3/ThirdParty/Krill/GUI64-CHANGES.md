# Krill v194 in GUI64

Loader by Krill (Gunnar Ruthenberg), November 2022.

Upstream archive: https://csdb.dk/getinternalfile.php/238373/loader-v194.zip
Release: https://csdb.dk/release/?id=226124
Archive SHA-256: C273980BB81EB5B16E6163869CEC147FC76E27BDBA6D03E8A3438B8251AA00F1

The loader/src, loader/include, shared sources, loader/version.inc and the
original loader/README are retained here. The README contains the upstream
copyright and redistribution conditions. Credit: Loader by Krill.

Local source changes:

- loader/src/install.s: separate DISKIO_DRIVECODE and DISKIO_INSTALL as
  read-only segments sharing ROM bank 5 ($8000-$9FFF).
- Replace C64 self-modifying instruction operands with RAM variables and
  indirect zero-page reads. DISKIO_INSTALL_ZP uses $E8-$EB;
  DISKIO_INSTALL_STATE holds 45 bytes at $68D3-$68FF (shared upper FREEMEM tail) for state and command buffers.
- Initialize writable state and copy immutable DOS-command templates from ROM
  on every install call. Slow bootstrap reads preserve X and Y; the fast
  upload retains the original SENDBYTE macro and wire protocol.
- Keep the five 1581/CMD-FD patch values in RAM and substitute them while
  streaming the main image. Assert that all five patch locations precede the
  slow bootstrap. No C64 store targets ROM installer code or drive images.
- Replace the original message routine, which changes screen modes and prints
  at $0400, with a saved RAM warning index. Export state/warning/source-pointer
  locations for GUI64. The wrapper does not expose the transient ROM pointer
  to the original version string after disabling the cartridge.
- Assert C64 and ONLY_1541_AND_COMPATIBLE=0: the local ROM adaptation targets
  the configuration used by GUI64; other upstream build paths are unsupported.
- loader/src/install.s: probe all passive devices before uploading any
  silencing routine. A second bus pass mutes compatible devices. This avoids
  leaving an earlier passive drive muted when a later incompatible/virtual
  device selects KERNAL fallback. Reuse mdUploadOffset as the pre-upload phase
  flag; no extra RAM state is allocated. Drive code and protocol are unchanged.
- loader/include/loader.inc: permit the command-line definition of
  KERNAL_FALLBACK_SEI_WORKAROUNDS. The GUI64 build sets it to 0 for virtual IEC.

- loader/src/resident.s: add an optional GUI64 App block guard before RECEIVE.
  Validate the PRG load address and complete destination block against
  $A000-$CFFF; consume rejected blocks into disposable FREEMEM $6700-$67FF
  and report error 32. Preserve registers and flags; keep timed RECEIVE intact.
- During native GUI mode, reject an unexpected switch to the core KERNAL
  fallback. GUI64 owns its bounded legacy fallback and pauses raster IRQ
  explicitly before mapping KERNAL.
- Export App bounds/mode/error symbols; enable OPENFILE_POLLBLOCK_API in the
  GUI64 configuration. The wrapper polls blocks to update approximate GUI
  progress and reuses installer state after installation, keeping 62 bytes
  total reserved in FREEMEM.

- loader/src/install.s: add two progress callbacks at the completed first
  bus scan and early in the fast upload (about 29%/66% in the VICE 1541 test).
  The callback vector temporarily reuses wrapper status/warning bytes at
  $68C3/$68C4. Reuse mdUploadOffset as a page counter after the bootstrap;
  no additional installer state is allocated. Both call sites discard A/X/
  flags; the GUI64 callback preserves the live upload index in Y and calls
  the existing LoadMotion routine for App and game installations. The timed SENDBYTE code and
  all drive binaries remain unchanged.

- loader/src/resident.s: place raw-load control, KERNAL support and uninstall
  in read-only DISKIO_FALLBACK, after the installer in bank 5. Replace the
  KERNAL filename operand with the existing BLOCKDESTLO pointer and an indirect
  Y-indexed name scan. No additional scratch RAM is needed. The remaining
  self-modifying block/receive kernel is shared by Apps, games and Viewer at $0500 (583 bytes).
- GUI64 control, its bounded legacy reader and progress routines are also
  executed from bank 5. Low-RAM entry gates map/unmap the cartridge and restore
  caller banking/flags. Raw calls use Krill_LoadRaw so the internal ROM support
  remains mapped. GUI64-specific ACME sources are outside this third-party tree.

- Shared native core: enable RAM-under-I/O and link it below $0801. Mode/error
  move to $07FE/$07FF so a game cannot overwrite them. App mode validates
  $A000-$CFFF; game mode validates $0801-$FFFF and rejects address wrap.
- Move the shared destination guard to DISKIO_GUARD in ROM bank 6 at $8200.
  A short RAM gate preserves receive flags/registers and restores bank 5
  for Apps. Both normal and RAM-under-I/O data stores are checked; control
  bytes and empty blocks do not count as written game payload.
- Retain the original getbyteswt/RECEIVE flag flow. Direct GUI pollblock error
  returns skip loadraw's stack/memory-map unwind, because those wrappers do
  not enter through loadraw. The internal raw API retains its original frame.
- The raw KERNAL fallback uses a 13-byte RAM store helper while ROM/I/O are
  mapped out; KERNAL read/control stays in ROM.
- hal/hal-c64-c128.inc: GUI64's ENABLE_IO_SPACE_X restores $01=$37 after each
  RAM-under-I/O store so the cartridge control remains visible on return.
  Instruction lengths and receive timing remain unchanged.

- drivecode1541.s / drivecode1571.s: GUI64_DIRECTORY_COMPAT accepts unused
  final-directory byte counts ($00/$01) on DIRTRACK. Keep the original minimum
  count check on other tracks: accepting zero-filled speculative file sectors
  there could signal EOF prematurely. Track/sector/checksum and host guards
  remain unchanged. The 1541 reuses its LED fade-delay space for the check
  (idle LED/motor now switch off immediately), preserving code addresses,
  timed GCR decoding and next-file prefetch. The 1571 grows by six bytes;
  no additional C64 RAM is used. Test-KrillDirectory.ps1 covers native loads
  through one- and two-sector directories with $00/$00 and $00/$01 end links.

The drive sources and wire protocol are otherwise unchanged. GUI64's
configuration is in ../../Krill/loaderconfig.inc; see ../../Krill.md for build,
RAM/ROM layout, invocation contracts, verification and remaining work.

- resident.s / loaderconfig.inc: mode 3 keeps all Viewer file bytes, including
  the first two bytes normally consumed as a PRG address. Use carry-set block
  address calculation for every Viewer block. Stage each native data block in
  the existing $6700 discard page; record its destination/Y in reused installer
  scratch at $68E3-$68E6. The timed RECEIVE loops and drive protocol are unchanged.
  The shared RAM guard keeps bank 6 selected for Viewer/game control and restores
  bank 5 only for Apps. The mode dispatch costs nine additional resident bytes.
- drivecode1541.s / drivecode1571.s: accept final file-link count 1 as legal
  empty data, while continuing to reject count 0 on file tracks. The C64 Viewer
  resolves the empty/FILE_NOT_FOUND status ambiguity through a DOS existence
  check after uninstall. This changes only the two comparison constants;
  drive-code sizes, addresses and GCR/serial timing remain unchanged.

## Copy-only 1541 variant in cartridge bank 7

The normal resident, guard, installer and drive-image bank remain byte-identical.
Only address exports were added to install.s/drivecode1541.s. Build-Krill.ps1
assembles Krill/copy-drive.asm and applies its bounded overlay records to a
separate copy of the installer bank. The regular image stays in bank 5.

The copy image opens an encoded first track/sector, found beforehand through
DOS, instead of hashing a filename. The former hash/finder and speculation areas
hold pause/resume control and ordered-block selection. Skipped candidates retain
their pending/index information. Source execution does not use the data-buffer
stack for additional context. Actual block counts are separate from transmitted
status values, including modulo-256 wrap. An empty final data block uses the
regular EOF status. The host can abort a stalled disk read by releasing ATN;
the copy variant checks this between sector reads after opening the file.
GCR decode and native serial bit loops are unchanged. No additional C64 receive
code or App/Viewer buffer is required. See Krill.md and Test-KrillCopy.ps1.
