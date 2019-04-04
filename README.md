# Beatmeter Generator
##### 0.3.0

A graphical editor to generate so called beatmeters for videos.
The editor lets you edit beats and beat patters visually and then generade an audio track and image sequences for video generation (one image per frame).

Please read through the features, basic workflow and hints to get a better idea of how to use the editor.

## Example Images

![example](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/wikis/example3.png)

![example](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/wikis/example.png)

![example](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/wikis/example2.png)

![example](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/wikis/example.gif)


## Features

- Generation of an audio track with beat sounds
- Generation of video (image sequences) for an animated beatmeter
- Many options to customize beatmeter style (colors, speed, positions, fonts, custom images)
- Multiple custom beat sounds
- Visiual editing of beats
- Multiple beat tracks
- Notification messages displayed in the generated video
- Highlighted beats in the beatmeter to indicate beat changes
- Flexible beat groups
  * Groups with constant BPM
  * Groups repeating a (custom) beat pattern
- Snapping of beats and beat patterns to a base beat.
- Copy and paste/Drag and Drop of beats between tracks or different projects
- BPM detection (does detect the BPM but the first beat has to be aligned manually)
- Undo/Redo
- Zooming

## Basic Workflow

1. I recommend using a mouse, using a laptop touch pad can give you too little control.
1. Select *File->Load Audio* and load a .wav file with the audio of your video. It has to be a 16bit signed int wav audio files, which most .wav files are. The .wav file will be loaded completely into memory, so don't try to load the audio track of a complete 1h video but split it up one song at a time.
1. Select *Edit->New Tracks* to generate three new beat tracks.
1. You can name the tracks using the context menu on the left over the gray areas, e.g. name the tracks "Beats", "Messages", and "Base".
1. Open the context menu on the base track and select *New BPM Pattern*
1. Drag and scale the pattern to a larger part of the song with clearly audible beats. (You can scale patterns at the end of the box when the scale cursor appears). Usually, beat detection is quite robust, so selecting a large part of the song or the complete song usually works fine. Usually, larger is better but takes more time.
1. Open the context menu on the pattern and select "Detect BPM". The bpm of the corresponding part of the song will be detected and set as bpm of the pattern. You can see it afterwards in the context menu.
1. Drag the pattern to the first beat and scale it to cover the complete song.
1. Press the "S" button in front of the base track to select the track for snapping.
1. Press the "D" button in front of the messages and beats tracks to select them to be displayed in the generated video.
1. Press the "P" button in front of the beats track to select if to be played in the generated audio.
1. Open the context menu on the beats track, select "Common Patterns->123-" to insert a simple 123-pattern.
1. Drag and scale it, it will snap to the positions of the snap track.
1. There is a black bar which controls the duration of the repeated pattern.
1. Press *CTRL* can drag it around to scale the pattern. This allows you to adjust the pattern to different speeds.
1. If you want to modify the pattern, you can drag the beats and the duration bar arround and delete and insert beats.
1. You can hold the 1-9 keys down while dragging or scalling to snap to 1 to 9 additional positions.
1. You can hold the 0 key down while dragging or scalling to disable snapping temporarily.
1. Select "Tools->Beatmeter Settings" to adjust the style of the beatmeter.
1. Choose the width and frame rate to match your video, you can leave the other settings as they are for now.
1. Open the context menu on the messages track and choose "New Message". Drag and scale it appropriately. You can change the message from the context menu.
1. Select "Tools->Generate Audio" and "Tools->Generate Video" to generate both.
1. Open a video editing tool and import your video and the generated audio file. Import the beatmeter image sequence at one image per frame (most video editing tools can do that).
1. Line them all up and position the beatmeter video track appropriately.
1. Have fun.


## Hints
- Zoom in and out by pressing *CTRL* and scrolling
- Press shift to scroll up and down
- Most things have context menus to change settings
- You can scale a beat pattern by pressing *CTRL* and tracking the black bar.
- You can scale beat and bpm patterns at the end.
- You can drag beats between tracks and between tracks and beat patterns.
- Press control when dragging to copy
- You can copy and paste from the context menu
- Use the buttons in front of the tracks to select tracks for display, playing, recording of beats and snapping.
- You can press the beat button at the bottom to insert a beat while playing the song.
- You can reduce playback speed using the slider at the bottom.
- You can double click on the wave form at the top to move the playback position or drag the red line.
- You can press 1-9 to get additional snap positions between the beats from the snap track. Press 0 do disable snapping.
- Copy and Dragging works between multiple instances of the editor, so you can start the editor two times and copy beats between them.
- You can chose a custom beat sound from the context menu on the left of the tracks.
- You can select to highlight single beats, the first beats of patterns, or the first repetition of a pattern. These beats will be highlighted in the generated beatmeter.
- When saving, paths refering to external files (e.g. audio files) will be stored relatively if they are in the same directory or a subdirectory. 
- Marking a track with "I" sets it as insert target: you can now insert and paste to that track at the current position using short-cuts

## New features in 0.3.0 and 0.3.1
- Support for Java 11
- Faster and nicer Waveform
- Shortcuts for common functions (move position, insert, copy, ...)
- Mark track with "I" and copy at current playback position
- List of recently accessed files
- File dialogs open at location of save file

## Download
### Windows
There is a windows installer available (64bit only) at:

[beatmeter-generator-x64.msi](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/builds/artifacts/master/raw/target/msiPackage/beatmeter-generator-x64.msi?job=build)

It is larger than the platform independent build below but it contains the Java runtime as well.
It should work on Windows 7 and later.

### Platform independent version
A platform independet .jar file can be downloaded at:
[beatmeter-generator.jar](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/builds/artifacts/master/raw/target/scala-2.12/beatmeter-generator.jar?job=build)

You will need the Java 11 JDK (separate JRE has been droped by Oracle) to run the application. You can download it at:
[Oracle Java 11](https://www.oracle.com/technetwork/java/javase/downloads/jdk11-downloads-5066655.html)

After installing the JRE you should be able to start the jar by double clicking on it.
If you can't try starting it from the command line using `java -jar beatmeter-generator.jar`.

### Other Java Versions
Beatmeter Generator should run on any JRE since Java 8, but it has not been tested on these versions.

## Screenshot
![main](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/wikis/screenshot-0.3.0.png)

## Ideas
- Counters
- Speedup/Slowdown Beat Pattern
