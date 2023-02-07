# This script downloads all the necessary data to run the scripts and render the manuscript
# To use, simply run this script once and it will download all the data for you.

# load libraries
library(here)
library(googledrive)


# download zipped data
download.file(
  url = "https://zenodo.org/record/7614189/files/language-experience-music_data.zip?download=1",
  destfile = here("data_download.zip"),
  mode = "wb",
  method = "curl"
  )

# unzip that bad-boi
unzip(here("data_download.zip"))

# remove random __MACOSX directory
unlink(here("__MACOSX"), recursive = TRUE)

# and remove the zip file itself
unlink(here("data_download.zip"))
 