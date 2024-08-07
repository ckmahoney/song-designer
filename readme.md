# Song Designer

Hi, I'm your song designer! Use me to design the layout of your song. 


## Description

This repository contains the code required to develop the UI for Synthony. It is made with Elm, because this language provides long term scalability and stability. JavaScript is used to initialize the application with the logged in member, or anonymous member; and to call WaveSurer for audio playback and waveform rendering. 

At the time of writing, there are three distinct UIs for designing songs. 

The Designer.Chart module is what is visible today on the website. This interface allows you to design a song focusing only on the structure and layout.


The Designer.MiniMaker module was the previous version, and will probably make a re-appearance in the future. This interface allows you to design a song focusing on the insruments that go inside of it.

The Designer.Song module was the previous previous version. It provides the most control over design, allowing granular control over which voices go in which sections of the song, also adding controls for Density, Complexity, and Speed. This interface needs a re-visit before it can be deployed again. Or, more likely, it can be used as a reference for a future redesign of the Song Designer.


## How You Can Help

This project began in November 2021 and was actively developed through May 2022. Now it is under passive maintence while funding for the project is being worked out. Anybody is welcome toreport bugs, request feqtures, suggest changes or submit PRs. I will review them within 2 weeks of submission.

If you are interested in the Elm language, you might enjoy reading and editing the source code.

If you are interested in creating new ways of designing music, you might enjoy creating sketches or mockups of new interfaces that can be implemented.  

If you can do both, then your pull requests are extremely welcome :) 



## How to Install

To  install : 

```
git clone https://github.com/ckmahoney/song-designer.git

cd song-designer;

npm install;

```


Do this once to build the javascript which initializes the Elm application:

```
npm run dev-build-js; # this is to build the JavaScript that initialized the elm application 
```


Do this every time you want to edit the Elm code:

```
npm run dev; # this runs the hot reloader for developing the elm application 
```


### Small Note 

A scrutinous eye may also notice that the commits are made under my alias, Naltroc. To produce the name Naltroc, take the name Cortland. Then reverse it, then remove the fisrt character. My alt GitHub repo (for music distribution) is at https://github.com/naltroc.
