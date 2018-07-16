# FCS Generator
Script allowing the user to generate several fcs files with a given amount of markers (parameters / dimensions), events, populations and sub-populations.
    The populations can be generated either manually or automatically with a set of frequencies commands.
    Control and mutant files can be generated. Variable parameters must be given to differentiate the control files. 
    Mutant files are generated from control files. For each population, a reduction coefficient can be given to change its proportion in the selected mutant file.
	[You may find additional information here :](doc/temp.pdf)
 
	
## Requirements
  * software: R(Version 3.4.3 to 3.5), Rstudio(optional)
  * R packages: shiny, shinyjs, ggcyto, flowCore, gtools, Biobase , Rcpp, RcppArmadillo, microbenchmark
  
## Quick installation guide

  1. Run the following command in R/RStudio:
```
install.packages(c("shiny", "shinyjs", "gtools", "ggplot2", "Rcpp", "RcppArmadillo", "devtools", "microbenchmark"))
source("https://bioconductor.org/biocLite.R")
biocLite("Biobase")
biocLite("flowCore")
biocLite("ggcyto")
```
  >You may be asked to reload your environment, if so, accept.
  
  2. Run the next commands:
```
library("devtools")
install_github("isambens/fcsgenerator")
```

  
## Launching the shiny application

  1. Run the following commands in R/RStudio:
```
library("FCSGenerator")
FCSGenerator.run()
```