# ---------------------------------------------------------------------------------------
# WEB DATA COLLECTION WITH R
# Sascha Goebel
# R Packages
# Winter Semester 2020/21
# ---------------------------------------------------------------------------------------


#### INSTALL AND LOAD PACKAGES ==========================================================

# install pacman package if not installed -----------------------------------------------
suppressWarnings(if (!require("pacman")) install.packages("pacman"))

# load packages and install if not installed --------------------------------------------
pacman::p_load(dplyr,
               magrittr,
               purrr,
               lubridate,
               stringr,
               ggplot2,
               xml2,
               XML,
               flatxml,
               httr,
               rvest,
               robotstxt,
               RSelenium,
               progress,
               gtools,
               gender,
               tidygeocoder,
               rnaturalearth,
               rnaturalearthdata,
               readr,
               tidyr,
               legislatoR,
               rvest,
               polite,
               hrbrthemes,
               install = TRUE,
               update = FALSE)

# show loaded packages ------------------------------------------------------------------
cat("loaded packages\n")
print(pacman::p_loaded())

