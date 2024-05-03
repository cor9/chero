@echo off
setlocal enabledelayedexpansion
set prevToken=none
set resolution=none
set width=none
set height=none
for /f "tokens=*" %%a in ('ffmpeg -i origvid.mp4 2^>^&1 ^| findstr /r /c:"Stream.*Video"') do (
    for %%b in (%%a) do (
        echo %%b | findstr /r /c:"[0-9][0-9]*x[0-9][0-9]*" > nul
        if !errorlevel! equ 0 (
            set resolution=%%b
            for /f "tokens=1,2 delims=x" %%c in ("!resolution!") do (
                set width=%%c
                set height=%%d
            )
        )
        if "%%b"=="fps" (
            set framerate=!prevToken!
            goto :next
        )
        set prevToken=%%b
    )
)

:next

ffmpeg -i origvid.mp4 -framerate %framerate% -i frame-%%010d.png -filter_complex "[0:v][1:v]overlay=0:%height%-125[out]" -map "[out]" -map 0:a -c:v libx264 -c:a copy -crf 18 -preset veryfast output.mp4

