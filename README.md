![alt text](https://github.com/WebFritzi/GUI64/blob/main/GUI64.png)

# GUI64 v1.0
GUI64 is a graphical user interface for the C64 that allows you to run programs and games, as well as manage files by cutting, copying, pasting, deleting, and renaming them. Additionally, it enables you to format and rename disks.

**Control:**<br>
Mouse in Port #1<br>
Joystick in Port #2

# WIP Version
You find the latest WIP version of GUI64 in the branch "Developer". Please be aware that this version might contain bugs, might not run smoothly, and exhibits unfinished features.

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

# Coming in Version 1.1 (Work in Progress)
* A file viewer which lets you look into files either in text mode or in hex mode.
* Support of device numbers other than #8 and #9.
* SD2IEC support
* A new menu item in the disk browser window
* Rearrangement of the memory map. For example, graphics data (char sets and sprites) are now copied to the RAM under the kernal when GUI64 starts. There will be a free area in RAM dedicated to applications.

# Future Plans
The next big leap will be a cartridge version of GUI64 v1.1. For this, I'll have to become familiar with C64 cartridge programming.
