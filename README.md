# ZHModLauncher
Mod launcher/manager for C&amp;C: Generals: Zero Hour

## Why use this mod launcher?
* In red2.net, they organize the mods in a uniform way.
  That is, they extract the .big files and rename it as .zbig files,
  so that mods won't clutter each other, unless "activated".
  -mod command line parameter can be used for mods with just one .big file.
  However, some mods come with multiple .big files so -mod is no longer
  valid for some mods.
* This mod launcher will scan for mods and active them, one at a time
  at game launch time.

## Installation
You don't install this one, you just unzip the zip file in the releases
and execute the .exe file :)

## Mod Installation Instructions
* Locate your mod files. Most mods will come with a couple of .big files
  and no other files.
* Locate your game folder. If you installed ZH with TFD,
  The probable location is something like C:\Program Files (x86)\EA Games\Command & Conquer The First Decade\Command &amp; Conquer(tm) Generals Zero Hour.
  Locate it, wherever it is, it should contain Generals.exe file.
* Create a folder inside that directory and name is as "Mods", without quotes.
* For each mod, create a folder inside the Mods folder you created.
* Inside each folder, place .big file but change the extension as .zbig.
* If you've downloaded mods from red2.net, the mod files should already have .zbig extension.
* Sometimes you might choose to select Zero Hour folder again. If so, go into
  C:\Users\\{user-id}\AppData\Local\mod4 and remove config.json file.

## Launching Mods
* Run the mod launcher program
* The program will scan the folders inside the Mods folder you've created.
* Select one mod of interest from the combo box
* Hit run game button or run game windowed button
* The mod launcher will automatically rename .zbig files to .big files and move
  them to the game folder.
* Upon termination of the game, the launcher will revert the changes:
  .big files back as .zbig files in the mod folder.
* Since the mod launcher is renaming files in the game folder,
  this operation might require Administrator privileges.
  If things don't work, consider launching the program with
  Administrator privileges.

## Where are the settings saved?
* In folder C:\Users\{user-id}\AppData\Local\mod4,
* The settings are saved in plain text, as config.json.

## Compiling
* The project is created and compiled with Lazarus 1.6 (for 32bits Windows).
* Download it and start from mod4.lpr file in src/.

## About .Big file decoding
* The .big content scanning code (for seeing if the mod has AI or not) is from
  Open Source BIG Editor: http://www.ppmsite.com/?go=osbigeditorinfo
* OS BIG Editor by Carlos "Banshee" Muniz
* RefPack decompression by Jonathan Wilson (jonwil)
* See [AboutBig.txt](AboutBig.txt) for details
