![alt text](https://github.com/WebFritzi/GUI64/blob/main/GUI64.png)

# GUI64 v1.9
GUI64 is a graphical user interface for the Commodore 64 that allows you to run programs and games, as well as to manage files by cutting, copying, pasting, deleting, and renaming them. It moreover enables you to format and rename disks, browse your folders on your SD2IEC device, and create new disk images and folders. For detailed information on GUI64, check out the C64-Wiki page https://www.c64-wiki.com/wiki/GUI64.

**Control:**<br>
Mouse in Port #1 (mouse wheel support with adapter Micromys)<br>
Joystick in Port #2 (right click with Commodore key + fire)

# Binaries
There are currently three options: the first is gui64.crt. This is a cartridge image with which you can enjoy GUI64 right after switching on your computer. The second option is gui64.prg which works great with a Kung Fu Flash cartridge. The third option is gui64.d64. Put in the disk and load GUI64 with 'LOAD"*",8,1'.

# Code
GUI64 was developed in 6502 assembly code (ACME syntax) with _C64 Studio_ which you can download here:<br>
<p align="center">https://www.georg-rottensteiner.de/files/C64StudioRelease.zip</p>
To build GUI64, download the files in the "Code" folder and open C64 Studio. In C64 Studio, go to "File->Open->Solution or Project", choose GUI64.c64, and in the next file browser click on "Cancel". The main file is GUI64.asm.<br><br>

**Memory map of GUI64 v1.9**

| Range of memory        | Contents                               |
| :---------------------- | :-------------------------------------- |
| ``$033c - $6500``      | Program code                           |
| ``$6500 - $6600``      | Path for drive A                       |
| ``$6600 - $6700``      | Path for drive B                       |
| ``$6700 - $6800``      | FREEMEM, used, e.g., for copying files |
| ``$6800 - $9800``      | 12 KB free for one single GUI64 app    |
| ``$9800 - $9900``      | 16 window structs                      |
| ``$9900 - $a000``      | 112 control structs                    |
| ``$a000 - $a370``      | Buffer for desktop data                |
| ``$a370 - $a400``      | Buffer for taskbar data                |
| ``$a400 - $a800``      | Buffer for color data                  |
| ``$a800 - $bc00``      | String list for drive A (255 entries)  |
| ``$bc00 - $d000``      | String list for drive B (255 entries)  |
| **Graphics elements in I/O area**                           |
| ``$d000 - $d800``      | Char set (desktop)                     |
| ``$d800 - $e000``      | Sprites (currently - $dbc0)            |
| ``$e000 - $e400``      | Char set (taskbar)                     |
| ``$e400 - $e800``      | Screen memory                          |
| ``$e800 - $ff00``      | 5.75 KB buffer for file viewer content |
| ``$ff00 - $ffff``      | Jump tables                            |


# Coming in Version 2.0 (Work in Progress)
* File browser: Full implementation of file viewer (menu entry "View" in "File" menu) to examine files in text and hex mode
* File browser: Copy files between SD2IEC directories; maybe even copying files between disks in the same drive via disk swap will be possible as well

# Future Plans
* Retrieve time from Ultimate 64 and SD2IECs with RealTimeClock
* Application programming interface: program your app for GUI64

# Thank You
A huge THANK YOU to all the users on http://www.Forum64.de who have supported me in bringing this project to life. I'm truly grateful for their help.
