![alt text](https://github.com/WebFritzi/GUI64/blob/main/GUI64.png)

# GUI64 v1.0
GUI64 is a graphical user interface for the C64 that allows you to run programs and games, as well as manage files by cutting, copying, pasting, deleting, and renaming them. Additionally, it enables you to format and rename disks.

**Control:**<br>
Mouse in Port #1<br>
Joystick in Port #2

# WIP Version
Please find the latest WIP version of GUI64 in the branch "Developer". Please be aware that this version might contain bugs, might not run smoothly, and exhibits unfinished features.

# Binaries
There are currently two options: you either download GUI64.D64 and load GUI64 from disk with LOAD"*",8,1 or you use GUI64.PRG. The latter works great with a Kung Fu Flash cartridge.

# Code
GUI64 was developed in 6502 assembler (ACME syntax) with _C64 Studio_ which you can download here:<br>
<p align="center">https://www.georg-rottensteiner.de/files/C64StudioRelease.zip</p>
To build GUI64, download the files in the "Code" folder and open C64 Studio. In C64 Studio, go to "File->Open->Solution or Project", choose GUI64.c64, and in the next file browser click on "Cancel". The main file is GUI64.asm.<br><br>

**Memory map of GUI64 v1.0**

Code and fixed data:<br>
$033c - $5700 : Program<br>
$5700 - $5800 : FREEMEM, used, e.g., for copying files<br>
$5800 - $6000 : Char set 1 (Desktop)<br>
$6000 - $6400 : Char set 2 (Task bar)<br>
$6400 - $6c00 : Sprites<br>
Dynamic:<br>
$6c00 - $7000 : Screen memory<br>
$7000 - $7100 : 16 window structs<br>
$7100 - $7800 : control structs (112 controls max)<br>
$7800 - $7b70 : buffer for desktop data<br>
$7b70 - $7c00 : buffer for taskbar data<br>
$7c00 - $8000 : buffer for color data<br>
$8000 - $9000 : string list for drive 8<br>
$9000 - $a000 : string list for drive 9

# Coming in Version 2.0 (Work in Progress)
* A more compact appearance of the file browser window with an icon indicating the file type, followed by file name and (optional) file size.
* Support of device numbers other than #8 and #9.
* SD2IEC support, browsing between directories and disk images.
* New menu "View" in the disk browser window with menu items "File Sizes" (On/Off), "Lower Case" (On/Off), "Sort by Name", "Sort by Type", "Sort by Size".
* New menu item "New" in the "File" menu lets you create new disk images (d64/d71/d81/dnp).
* New menu item "View" in the "File" menu lets you look into files in text and hex mode.
* Copying files between disks in the same drive via disk swap.
* Maybe even copying files between SD2IEC directories will be possible.

# Future Plans
* Cartridge version of GUI64 v2.0.
* Retrieve time from Ultimate 64 and SD2IECs with RTC.
* Application programming interface: It will be possible to run a single app in GUI64.
