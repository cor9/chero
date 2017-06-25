# Beatmeter Generator

A set of tools to generate so called beat meters for videos, i.e. a visual indicator for beats in the video's music.
The set comprises a visual editor to edit beat sequences matching an audio file, a command line tool to generate a sequence of images at a selected frame rate, suitable to generate the animated beat meter, and a command line tool to generate a audio file playing a beat sound for every beat.
The file format used to store beats is compatible with the Audacity lable format.

## Example Images

![example](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/wikis/example.png)

![example](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/wikis/example2.png)

## Basic usage
### Beat editing

The beat editor only supports (integer-based) wav audio files.
Convert your audio to wav or extract it from your video using eg. Audacity or Avidemux.

Then start the visual beat editor from the command line:

```
java -jar beatmeter-generator.jar editor -i myaudio.wav
```

You can add beats clicking the beat-button. Start playing the audio file (adjust the speed appropriately, by default the editor plays at half speed.) and click the beat button to insert beats.

You can jump directly to positions in the audio file by double clicking in the audio visualisation at the top. You can also select beats from that view.

If you double click on a beat you jump exactly to that beat.

You can copy and paste beats (even multiple times at once). They will get inserted to the left or the right of the current position moving the current position to the last beat of the insertion. To insert a pattern repeatedly, select the pattern and the first beat of its repetition and paste it as many times as you want.

There are also two aligment tools, align equally will spead the selected beats such that their distance is equal. Align pattern requires you to select a number of pattern repetitions plus one additional beat. Then it will distribute the pattern euqaly adjusting the beats within a pattern to their average positions. For example if you have a pattern 123 123 123 123, set repetition to 3 and rest to 2 as you have 3 repetitions of the 123 pattern with two additional beats. (The last 123 is not a full pattern, as there is no beat to mark the end of the break after the last 3-beat).

### BPM detection

The tool can detect the beats per minute of a song. It uses an algorithm based on wavelet transformations that works very well at least for music with a constant (but not necessarily always clearly audible) beat. The implementation is taken from [here](https://github.com/mziccard/scala-audio-file/). To analyze some audio file run:

```
java -jar beatmeter-generator.jar bpm -i myaudio.wav
```

There are command line options to analyze only part of a file and fine-tune the algorithm but just running it on the hole file should usually work. Use the Add-bpm-button in the edior to add beats at the detected bpm rate. The tool outputs intermediate results, the end result can often be accurate even if many intermediate results are not.

### Generating image sequences
Once you are satisfied with your beat pattern you can generate a sequence of images for the beatmeter animation:

```
java -jar beatmeter-generator.jar video -i mybeats.txt -d 60 -w 1280 -o outputDirectory -f 29.97
```

The command has a lot of options that allows you to style your beatmeter. There are reasonable defaults though, you just have to set the duration of your video in seconds with -d, the width of your video in pixels with -w, and usually the framerate in frames per second (unless the default of 25 is appropriate).

If you prefer the "flying bubbles" style instead of the classic waveform, you can do this:

```
java -jar beatmeter-generator.jar video -i mybeats.txt -d 60 -w 1280 -o outputDirectory -f 29.97 flying
```

Run the programm without any options to see all available options, e.g to change beatmeter colors.

### Generating audio
You can also generate an audio file to underline the beats.

```
java -jar beatmeter-generator.jar audio -i mybeats.txt -d 60 -o output.wav
```

You only have to supply the files and the duration. You can choose an alternative beat sound with the option --click or provide your own beat wav-file.

## Shortcuts

  - *b* play/pause
  - *b* insert beat
  - *ctrl+left* move marked beats left
  - *ctrl+right* move marked beats right
  - *left* one second left
  - *right* one second right
  - *alt+left* 0.05 seconds left
  - *alt+right* 0.05 second right
  - *shift+left* 10 seconds left
  - *shift+right* 10 seconds right
  - *n* move left side of selection to current position
  - *shift+n* move right side of selection to current position
  - *c* copy selected beats
  - *v* insert selected beats to the right
  - *shift+v* insert selected beats to the left
  - *e* align selected beats equally
  - *d* deselect all beats
  - *delete* remove selected beats
  - *ctrl+digit* select/deselect ith beat right of current position
  - *ctrl+alt+digit* jump exactly to ith beat right of current position
  - *z* undo last change to beats
  - *shift+z* redo last change to beats
  
## Download
The current binary can be downloaded:
[beatmeter-generator.jar](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/builds/artifacts/master/raw/target/scala-2.12/beatmeter-generator.jar?job=build)

You will need the Java 8 Runtime Environment (JRE) to run the application. You can download it at:
(Oracle Java 8)[http://www.oracle.com/technetwork/java/javase/downloads/index.html]

If you are using Linux, your distribution will usually include OpenJDK 8 which works fine as well.
Note, that under Linux you usually have to install OpenJFX as a separate package.

## Notes
- The tools do not do much error handling yet. If you provide unreasonable options like a duration shorter than the list of beats you might get strange output or internal error messages.
- To see all command line options just run
`java -jar beatmeter-generator.jar`

## Screenshot
![main](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/wikis/screenshot.png)

