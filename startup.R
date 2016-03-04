## INSTALL THESE DEPENDENCIES

# install packages for text mining
if(!"tm" %in% rownames(installed.packages())){
  install.packages("tm", dependencies = TRUE)
}
if(!"Rcampdf" %in% rownames(installed.packages())){
  install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
}
# install package to product plots
if(!"ggplot2" %in% rownames(installed.packages())){
  install.packages("ggplot2", dependencies = TRUE)
}
if(!"devtools" %in% rownames(installed.packages())){
  install.packages("devtools", dependencies = TRUE)
}
# install package to do arithmetic with dates
if(!"lubridate" %in% rownames(installed.packages())){
  install.packages("lubridate", dependencies = TRUE)
}
# install package to easily wrangle data
if(!"data.table" %in% rownames(installed.packages())){
  install.packages("data.table", dependencies = TRUE)
}
# install utils to pull last x requests
if(!"utils" %in% rownames(installed.packages())){
  install.packages("utils", dependencies = TRUE)
}
# install package for quantitative string comparison
if(!"qlcMatrix" %in% rownames(installed.packages())){
  install.packages("qlcMatrix", dependencies = TRUE)
}
# install package for quantitative string comparison
if(!"RSocrata" %in% rownames(installed.packages())){
  install.packages("RSocrata", dependencies = TRUE)
}
# install package for interactive application
if(!"shiny" %in% rownames(installed.packages())){
  install.packages("shiny", dependencies = TRUE)
}
# install package for utilities
if(!"geneorama" %in% rownames(installed.packages())){
  install.packages(devtools::install_github('geneorama/geneorama'))
}

