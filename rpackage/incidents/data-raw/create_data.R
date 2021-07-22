dat <- read.csv('contact_info.csv')
dat$X <- NULL
usethis::use_data(dat, overwrite = TRUE)
