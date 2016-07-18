# Bar_within_circle
Generates translating or looming stimuli constained to a circular field of view

## Description
Use this script for generating image sequences of a moving rectangle (or square). The rectangle is confined within a circle, to optimise projecting the images into a circular, image carrying light guide.

## Work flow
1. Install R using the method specified for your platform. (Additionally, I can recommend using [RStudio](https://www.rstudio.com/))
2. Copy the script to a new folder
3. Install the required package *ggplot*

 ``` {r eval=FALSE} 
 library(ggplot2)
 ```
4. Set *User settings* in the script and run the script
5. The script will generate a new folder called *Image_sequence* where each new sequence will be save in it's own folder.
6. This image sequence can be made into an AVI file by using, for instance, [ImageJ](http://imagej.nih.gov/).
