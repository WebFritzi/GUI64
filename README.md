![alt text](https://github.com/WebFritzi/GUI64/blob/main/GUI64.png)

# GUI64 v1.97
GUI64 is a graphical user interface (GUI) for the Commodore 64. It comes in two designs: one is Windows-like, the other Mac-like. With GUI64 you can do the following:
* run programs and games with a simple double click
* manage files by cutting, copying, pasting, deleting, and renaming them
* format and rename disks
* browse the folders on your SD2IEC device
* create new disk images and folders
* view files in text and hex mode

For detailed information on GUI64, check out the C64-Wiki page https://www.c64-wiki.com/wiki/GUI64.

**Control:**<br>
Mouse in Port #1 (mouse wheel support with adapter Micromys)<br>
Joystick in Port #2 (right click with Commodore key + fire)

# Ultimate Users
Ultimate users (1541 Ultimate II(+L), Ultimate 64(EI/II), and C64 Ultimate) benefit from automatic saving of their settings (colors, desktop pattern, icon positions) in the Ultimate's Flash folder, along with an accurate time display.

# Binaries
There are currently two options: the first is gui64.crt (recommended). This is a cartridge image with which you can enjoy GUI64 right after switching on your computer. The second option is gui64.d64. Put in the disk and load GUI64 with 'LOAD"*",8,1'.

# Code
GUI64 was developed in 6502 assembly code (close to ACME syntax) with _C64 Studio_ which you can download here:<br>
<p align="center">https://www.georg-rottensteiner.de/files/C64StudioRelease.zip</p>
To build GUI64, download the files in the "Code" folder and open C64 Studio. In C64 Studio, go to "File->Open->Solution or Project", and choose the *.s64 file. C64 Studio then opens the solution. The main file is GUI64.asm.<br><br>

# Coming in Version 2.0 (Work in Progress)
* Ultimate users: Browse Ultimate content (USB stick, SD card) in a window and run your games from within GUI64
* File browser: Copy files between SD2IEC directories and images; maybe even copying files between disks in the same drive via disk swap will be possible as well

# Future Plans
* Application programming interface: program your app for GUI64

# Thank You
A huge THANK YOU to all the users on http://www.Forum64.de who have supported me in bringing this project to life. I'm truly grateful for their help.

# Update Log (since v1.97)
## Version 1.97
* Improved joystick performance
* Ultimate: clock has access
* Ultimate: settings are saved in Flash folder
* Draggable icons
* Intern: joint 2 projects for Win and Mac version to one project
