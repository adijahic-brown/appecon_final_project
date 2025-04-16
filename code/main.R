# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(broom)
library(hdm)
library(haven)
library(foreign)
library(stringr) 

# Set to 1 to run the respective section of code
hyderabad_karnataka  <- 1
enterprise           <- 1
gem                  <- 1

# Set working directory. Should be set to the parent folder.
setwd("C:/Users/adijj/OneDrive/Documents/Brown Phd/Year1Sem2/Applied Econ/Hw/final project")

# Source the data cleaning script
source("code/datacleaning.R")

# Source the analysis script
source("code/analysis.R")
