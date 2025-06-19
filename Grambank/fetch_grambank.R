library(devtools)

if ("rcldf" %in% installed.packages()) {
  print("rcldf is installed")
} else{
  print("installing rcldf......")
  install_github("SimonGreenhill/rcldf", dependencies = TRUE)
}

library(rcldf)

#### this script downloads the grambank database as an R object using the rcldf pacakge (Greenhill SJ 2025)
# and saves it as a .rds####

grambank = rcldf::cldf(mdpath = "https://zenodo.org/records/7740140/files/grambank/grambank-v1.0.zip")
saveRDS(grambank, "Raw_data/grambank_database.rds")
