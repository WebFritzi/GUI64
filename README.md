![alt text](https://github.com/WebFritzi/GUI64/blob/main/GUI64.png)

# GUI64 v1.95
GUI64 is a graphical user interface (GUI) for the Commodore 64. It comes in two designs: one is Windows-like, the other Mac-like. With GUI64 you can do the following:
* run programs and games with a simple double click
* manage files by cutting, copying, pasting, deleting, and renaming them
* format and rename disks
* browse your folders on your SD2IEC device
* create new disk images and folders
* view files as text and hex

For detailed information on GUI64, check out the C64-Wiki page https://www.c64-wiki.com/wiki/GUI64.

**Control:**<br>
Mouse in Port #1 (mouse wheel support with adapter Micromys)<br>
Joystick in Port #2 (right click with Commodore key + fire)

# Binaries
There are currently three options: the first is gui64.crt. This is a cartridge image with which you can enjoy GUI64 right after switching on your computer. The second option is gui64.prg which works great with a Kung Fu Flash cartridge. The third option is gui64.d64. Put in the disk and load GUI64 with 'LOAD"*",8,1'.

# Code
GUI64 was developed in 6502 assembly code (ACME syntax) with _C64 Studio_ which you can download here:<br>
<p align="center">https://www.georg-rottensteiner.de/files/C64StudioRelease.zip</p>
To build GUI64, download the files in the "Code" folder and open C64 Studio. In C64 Studio, go to "File->Open->Solution or Project", and choose the *.s64 file. C64 Studio then opens the solution. The main file is GUI64.asm.<br><br>

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
| ``$a000 - $a400``      | Buffer for screen data                 |
| ``$a400 - $a800``      | Buffer for color data                  |
| ``$a800 - $bc00``      | String list for drive A (255 entries)  |
| ``$bc00 - $d000``      | String list for drive B (255 entries)  |
| **Graphics data in I/O area**                                   |
| ``$d000 - $d800``      | Char set (desktop)                     |
| ``$d800 - $e000``      | Sprites                                |
| ``$e000 - $e400``      | Win: Char set (taskbar), Mac: Screen memory |
| ``$e400 - $e800``      | Win: Screen memory, Mac: File viewer buffer |
| ``$e800 - $ff00``      | File viewer buffer                     |
| ``$ff00 - $ffff``      | Jump tables                            |


# Coming in Version 2.0 (Work in Progress)
* File browser: Copy files between SD2IEC directories and images; maybe even copying files between disks in the same drive via disk swap will be possible as well

# Future Plans
* Retrieve time from Ultimate 64 and/or SD2IECs with RealTimeClock
* Application programming interface: program your app for GUI64

# Thank You
A huge THANK YOU to all the users on http://www.Forum64.de who have supported me in bringing this project to life. I'm truly grateful for their help.
