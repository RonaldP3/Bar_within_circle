# Script to generate image sequences of moving bars, later to be converted into
# movies.

# Author: Ronald Petie (ronald.petie@gmail.com)

# Install ggplot
# install.packages(ggplot2)

# Attache package
library(ggplot2)

#------------------------------------------------------------------------------
# File and folder management

# Get current directory
curr.dir <- getwd()

# Get date/time string, to be used as folder name
date.time <- format(Sys.time(), "%Y%m%d_%H%M")

# Create main directory for sequences, if it does not exist already
dir.create("Image_sequence",
           showWarnings = F)
# Change directory
setwd("Image_sequence")

# Create folder for this sequence
dir.create(date.time)
# Change directory
setwd(date.time)

#------------------------------------------------------------------------------
# Image sequence main settings

### Set frame rate in Hz ###
rate <- 10
### Set duration in seconds ###
duration <- 5
### Set resolution in dpi ###
res <- 300
### Set aspect ratio
asp.ratio <- c(4,3)

### Set movement type ("move", "looming" or "stationary")
move.type <- "stationary"

# Set output file name depending on movement type
if (move.type == "move") {
  file.out <- "Moving_bar"
} else if (move.type == "looming") {
  file.out <- "Looming_bar"
} else {
  file.out <- "Stationary_bar"
}

# Number of frames in sequence
frames <- rate * duration
# width and height in inches of output files
im.width <- 800/res
im.height <- 600/res

# Check if aspect ratio matches resolution specified
if(im.width/asp.ratio[1] != im.height/asp.ratio[2]) {
  stop("Resolution should match aspect ratio!")
}

#------------------------------------------------------------------------------
# Light guide settings

### Set diameter and distance to eye in mm
light.guide <- list(diameter = 4.5,
                    distance = 1.0)

# Light guide diameter in degrees
light.guide$diameter.degrees <- 
  with(light.guide,
       2* 360 * atan(0.5*(diameter/distance)) / (2*pi))

#------------------------------------------------------------------------------
# Stimulus (polygon) shape and behaviour

### Set stimulus height in degrees ###
stim.height <- 20

# Set dimensions of stimulus rectangle relative to screen. Note that the 
# shortest side corresponds to the radius of the light guide.
### Set to full available screen width ###
rect.width <- 1    
# Height relative to light guide diameter
rect.height <- stim.height / light.guide$diameter.degrees

### Set rotation of the coordinate system in degrees from 0 to 360. 
# Default is a horizontal bar starting at the bottom moving up.
coord.rot <- 0
# In radians
coord.rot[2] <- (coord.rot /360) * 2 * pi

### Set looming. Sizes are relative and cannot be larger than 1.
looming <- data.frame(start = 0.1,
                      end = 1)

# Check for wrong entries
if (max(looming) > 1 | min(looming) < 0) {
  stop("Looming values must be between 0 and 1!")
}

# Generate sequence with relative size values based on looming settings
looming.size <- seq(from = looming$start[1],
                    to = looming$end[1],
                    by = ((looming$end[1]-looming$start[1])/frames))

#------------------------------------------------------------------------------
# Create stimulus polygon.

# Create stimulus square. This can be adjusted below to any desired rectangle
rectangle <- data.frame(x=c(-1,1,1,-1),
                        y=c(1,1,-1,-1))

# The movement of the stimulus should be contained within the circle describing
# the diameter of the light guide. The relative width of a square bounded by a
# circle is sqrt(0.5). This means that the limits in both x and y direction are
# about 71% of the light guide radius. This means that the width of the bar
# can't be more than:

# Correct for relative size of WIDTH compared to height. The HEIGHT of the bar
# stays the same.
rect.width <- rect.width * (asp.ratio[2] / asp.ratio[1])
# Fit within bounding circle
rect.width <- rect.width * sqrt(0.5)

# Shape rectangle based on aspect ratio and dimensions chosen above
# Width
rectangle$x <- rectangle$x * (asp.ratio[1] / 2) * rect.width
# Height
rectangle$y <- rectangle$y * (asp.ratio[2] / 2) * rect.height

#------------------------------------------------------------------------------
# Stimulus motion

### Set stimulus motion speed in degrees/second ###
stim.speed <- 10

# Time for one period of wave. (Covers twice light guide radius!)
period <- (2 * light.guide$diameter.degrees) / stim.speed

# Create time axis
t <- seq(0, duration, by = 1/rate)

# Model (y) motion of bar as triangular wave.
# This formula works with a half period
period <- period / 2
# amplitude should be equal to sqrt(0.5) - relative bar width. To prevent
# object from exiting circle.
amplitude <- sqrt(0.5) - rect.height
# Create wave
y <- (amplitude/period) * (period - abs(t %% (2*period) - period) )
# Center on zero
y <- y - amplitude / 2
# Correct for screen dimensions
y <- y * min(asp.ratio)
# Apply safety margin to prevent corners of rectangle from touching the circle
y <- y * 0.98

#------------------------------------------------------------------------------
# Generate sequence

# Variables needed for circle around origin
angle <- seq(-pi, pi, length = 200)
circle <- data.frame(x = sin(angle), y = cos(angle))
# Radius should match shortest side of screen
circle <- circle * min(asp.ratio)/2

# Create progress bar
pb <- txtProgressBar(min = 0, max = frames, style = 3)

# Generate frames
for (a in 1:frames) {
  
  # Update polygon position. Copy first
  new.pos <- rectangle
  
  # Move or keep stationary, depending on settings above
  if (move.type == "move") {
    # Move
    new.pos$y <- new.pos$y + y[a]
  } else if (move.type == "stationary") {
    # Stationary bar at lowest position
    new.pos$y <- new.pos$y + y[1]
  } else if (move.type == "looming") {
    # Looming stimulus is centred on origin
    new.pos$y <- new.pos$y
  } else {
    stop("Select valid movement type")
  }
  
  # Initiate rotated coordinate variable
  rotated <- new.pos
  
  # Rotate coordinate system
  rotated$x <- new.pos$x * cos(coord.rot[2]) +  new.pos$y * sin(coord.rot[2])
  rotated$y <- -new.pos$x * sin(coord.rot[2]) + new.pos$y * cos(coord.rot[2])
  
  # Apply looming if specified above
  if (move.type == "looming") {
    # Apply looming
    rotated <- rotated * looming.size[a]
  }
  
  # Create Plot
  plot <- ggplot(rotated, aes(x=x, y=y)) + 
    # Set 1:1 aspect ratio
    coord_fixed(ratio=1) +
    # Set limits to match aspect ratio of projector
    xlim(-(asp.ratio[1] / 2), (asp.ratio[1] / 2)) +
    ylim(-(asp.ratio[2] / 2), (asp.ratio[2] / 2)) +
    # Draw Circle of radius of shortest plot area
    geom_polygon(data = circle, aes(x, y),inherit.aes = F,
                 fill = "white", colour =  "black") +
    # Draw bar
    geom_polygon(fill = "black") +
    # Set empty theme
    theme_minimal() +
    theme(line=element_blank(),
          text=element_blank())
  
  # Create file name for current frame
  current.name <- paste0(file.out, sprintf("%06.0f", a), ".png")
  
  # Save current image
  ggsave(current.name,
         width = im.width,
         height = im.height,
         units = "in",
         dpi = res)
  
  # update progress bar
  setTxtProgressBar(pb, a)
}

# Save workspace for future reference
save.image(file = paste0(date.time, "_", file.out, ".RData"))

# Back to root directory
setwd(curr.dir)

# Close progress bar
close(pb)

# Clean up
# rm(circle, new.pos, a, angle, pb, t, y, im.height, im.width)