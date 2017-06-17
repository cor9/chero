# Beatmeter Generator

A set of tools to generate so called beat meters for videos, i.e. a visual indicator for beats in the video's music.
The set comprises a visual editor to edit beat sequences matching an audio file, a command line tool to generate a sequence of images at a selected frame rate, suitable to generate the animated beat meter, and a command line tool to generate a audio file playing a beat sound for every beat.
The file format used to store beats is compatible with the Audacity lable format.

## Example Image

![example](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/wikis/example.png)

## Basic usage
### Beat editing

The beat editor only supports (integer-based) wav audio files.
Convert your audio to wav or extract it from your video using eg. Audacity or Avidemux.

Then start the visual beat editor from the command line:

``java -jar beatmeter-generator.jar editor -i myaudio.wav

You can add beats clicking the beat-button. Start playing the audio file (adjust the speed appropriately, by default the editor plays at half speed.) and click the beat button to insert beats.

You can jump directly to positions in the audio file by double clicking in the audio visualisation at the top. You can also select beats from that view.

### Generating image sequences
Once you are satisfied with your beat pattern you can generate a sequence of images for the beatmeter animation:

``java -jar beatmeter-generator.jar video -i mybeats.txt -d 60 -w 1280 -o outputDirectory -f 29.97

The command has a lot of options that allows you to style your beatmeter. There are reasonable defaults though, you just have to set the duration of your video in seconds with -d, the width of your video in pixels with -w, and usually the framerate in frames per second (unless the default of 25 is appropriate).

### Generating audio
You can also generate an audio file to underline the beats.

``java -jar beatmeter-generator.jar audio -i mybeats.txt -d 60 -o output.wav

You only have to supply the files and the duration. You can choose an alternative beat sound with the option --click or provide your own beat wav-file.

## Download
The current binary can be downloaded:
[beatmeter-generator.jar](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/wikis/beatmeter-generator.jar)

## Notes
- The tools do not do much error handling yet. If you provide unreasonable options like a duration shorter than the list of beats you might get strange output or internal error messages.
- To see all command line options just run
`java -jar beatmeter-generator.jar`

## Screenshot
![main](https://gitlab.com/SklaveDaniel/BeatmeterGenerator/wikis/screenshot.png)

