###
#space
#website: https://vt-hydroinformatics.github.io/rgeowatersheds.html
install.packages("whitebox", repos="http://R-Forge.R-project.org")
install.packages("raster")
install.packages("tmap")
install.package("rayshader")

library(tidyverse)
library(raster)
library(sf)
library(whitebox)
library(tmap)
library(stars)
library(rayshader)
library(rgl)

whitebox::wbt_init()

knitr::knit_hooks$set(webgl = hook_webgl)

theme_set(theme_classic())

tmap_mode("view")

