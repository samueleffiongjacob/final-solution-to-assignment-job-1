install.packages("installr")

library(installr)

updateR()
packageStatus()
uninstall.R(r_version, GUI = TRUE)
uninstall.R("4.1.1") # will uninstall R
