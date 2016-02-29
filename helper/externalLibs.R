if (!require("devtools"))
  # in order to get the latest versions of leaflet and ggplot2
  install.packages("devtools")
library(devtools)

if (!require("shiny"))
  install.packages("shiny")
library(shiny)

if (!require("shinydashboard"))
  install.packages("shinydashboard")
library(shinydashboard)

if (!require("ggplot2"))
  # latest version fixes issues with scales
  devtools::install_github("hadley/ggplot2")
library(ggplot2)

if (!require("Cairo"))
  # fixes issues with ggplot2 on Linux
  install.packages("Cairo")
library(Cairo)

if (!require("extrafont"))
  # needed to include custom fonts in R
  install.packages("extrafont")
library(extrafont)

if (!require("leaflet"))
  # latest version required to fix issues with htmltools, causing browser incompatibilities
  devtools::install_github("rstudio/leaflet")
library(leaflet)

if (!require("RColorBrewer"))
  # includes pretty color palettes
  install.packages("RColorBrewer")
library(RColorBrewer)

if (!require("shinyjs"))
  # enables to use JavaScript in combination with R
  install.packages("shinyjs")
library(shinyjs)

if (!require("V8"))
  # needed to include own custom js functions
  install.packages("V8")
library(V8)

