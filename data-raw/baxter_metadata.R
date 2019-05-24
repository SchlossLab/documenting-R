# code to prepare `baxter_metadata` dataset
library(usethis)
baxter_metadata <- get_metadata()
usethis::use_data(baxter_metadata, overwrite = TRUE)
