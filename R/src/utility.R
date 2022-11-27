# Take snapshot of R environment
renv::snapshot()

# Restore R environment
# To reproduce the environment on another machine
renv::restore()

# Update R
installr::updateR()

# Update Packages
update.packages()

# Edit Global .Rprofile or .Renviron
usethis::edit_r_profile()
usethis::edit_r_environ()



# get my working directory
getwd()

# set my working directory
setwd("C:/Users/CamHa/OneDrive/Docs/Fantasy Football")

# verify working directory
getwd()

# you could remove all variables with this command remove(list = ls())
remove(list = ls())

# remove everything except functions
rm(list = setdiff(ls(), lsf.str()))



# install.packages('tidyquant', dependencies = TRUE)
# install.packages('DataEditR', dependencies = TRUE)
# library(tidyquant)
# library(DataEditR)
# 
# playerInfoEdit <- data_edit(playerInfo)
# install.packages('cfbfastR', dependencies = TRUE)
# install.packages("cli")
# install.packages("devtools")
# install.packages("ellipsis")
# install.packages("tidyverse", dependencies = TRUE)
# install.packages(c("httr","jsonlite"))
# install.packages("ggrepel", type = "binary")
# install.packages('Rcpp', dependencies = TRUE)
# install.packages('data.table', dependencies = TRUE)
# install.packages("ggimage", dependencies = TRUE)
# install.packages("tictoc")
# install.packages("qs")
# install.packages("gsisdecoder")
# install.packages("nflverse", dependencies = TRUE)
# install.packages('ffscrapr', repo = 'https://ffverse.r-universe.dev')
# if (!requireNamespace('pacman', quietly = TRUE)){
#   install.packages('pacman')
# }
