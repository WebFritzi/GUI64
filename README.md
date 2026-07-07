![alt text](https://github.com/WebFritzi/GUI64/blob/main/GUI64.png)

# GUI64 2.0
GUI64 is a graphical user interface (GUI) for the Commodore 64, ready to use immediately after startup. It comes in two designs: one is Windows-like, the other Mac-like. With GUI64 you can do the following:
* run programs and games with a simple double click and see the loading progress
* manage files by cutting, copying, pasting, deleting, and renaming them
* format and rename disks
* view files in text and hex mode
* SD2IEC: all of the above + browse folders and create disk images and folders
* Ultimate: browse Ultimate content and mount images

For detailed information on GUI64, check out the C64-Wiki page https://www.c64-wiki.com/wiki/GUI64 or my [YouTube playlist](https://www.youtube.com/watch?v=iCAIbygV_Ac&list=PL0MXvn2FofiZXY7OWdk8VMvzWTT8VbzW2) on the development process of GUI64..

**Control:**<br>
Mouse in Port #1 (mouse wheel support with adapter Micromys)<br>
Joystick in Port #2 (right click with Commodore key + fire)

## Time
The current time is displayed either if the host is an Ultimate (with Command Interface enabled) or if there is a working [WiC64](https://www.wic64.net/web/) attached to the user port. Otherwise, the clock is set to "00:00".

## Ultimate Users
Ultimate users [1541 Ultimate II(+L), Ultimate 64(EI/II), and C64 Ultimate, each with the **Ultimate's Command Interface enabled**] can browse content on SD-card or USB devices attached to their Ultimates and mount images right in GUI64. Moreover, they benefit from automatic saving of their settings (colors, desktop pattern, icon positions, device numbers) in the Ultimate's Flash folder, along with an accurate time display.

## Binaries
There are currently two options: the first is gui64.crt (recommended). This is a cartridge image with which you can enjoy GUI64 right after switching on your computer. The second option is gui64.d64. Put in the disk and load GUI64 with 'LOAD"*",8,1'.

## Code
GUI64 was developed in 6502 assembly code (close to ACME syntax) with _C64 Studio_ which you can download here:<br>
<p align="center">https://www.georg-rottensteiner.de/files/C64StudioRelease.zip</p>
To build GUI64, download the files in the "Code" folder and open C64 Studio. In C64 Studio, go to "File->Open->Solution or Project", and choose the *.s64 file. C64 Studio then opens the solution. The main file is GUI64.asm.

## Work in Progress
* File browser: Copy files between SD2IEC directories and images; maybe even copying files between disks in the same drive via disk swap will be possible as well
* Application programming interface: program your app for GUI64

## Thank You
A huge THANK YOU to all the users on http://www.Forum64.de who have supported me in bringing this project to life. I'm truly grateful for their help. I would also like to thank [Bart van Leeuwen](https://github.com/bvl1999) for his support concerning the Ultimate and [rh70](https://www.forum64.de/wcf/index.php?user/20464-rh70/) from Forum64 for providing the code for grabbing the time from the WIC64.

## Update Log (since v1.97)
### Version 2.0
* WIC64: Grabs current time
* Ultimate, SD2IEC, and disks: double click txt files to view them
### Version 1.98
* Ultimate: A new desktop icon has been added
* Ultimate: A browser window has been added
* Ultimate: A new CBM menu entry "Reboot" has been added
### Version 1.97
* Improved joystick performance: joystick acceleration
* Ultimate: clock has access
* Ultimate: settings are saved in Flash folder
* Draggable icons
* Internally: joint 2 projects for Win and Mac version to one single project
