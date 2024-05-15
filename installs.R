# Function to install and load a package if not already available
install_and_load <- function(package_to_install) {
  # Check if package is already loaded
  packageName <- paste(package_to_install, sep = "")
  if (!is.loaded(package_to_install)) {
    if (!require(packageName, character.only = TRUE)) install.packages(packageName, dependencies = TRUE)
  }
}

install_and_load("data.table")
install_and_load("ggplot2")
install_and_load("dplyr")
install_and_load("tidyr")
install_and_load("VIM")
install_and_load("naniar")
install_and_load("corrplot")
install_and_load("PerformanceAnalytics")
install_and_load("randomForest")
install_and_load("caret")
install_and_load("e1071")
install_and_load("ggforce")
install_and_load("plotly")
