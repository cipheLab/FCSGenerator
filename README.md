# FCS Generator
Script allowing the user to generate several fcs files with a given amount of markers (parameters / dimensions), events, populations and sub-populations.
    The populations can be generated either manually or automatically with a set of frequencies commands.
    Control and mutant files can be generated. Variable parameters must be given to differentiate the control files. 
    Mutant files are generated from control files. For each population, a reduction coefficient can be given to change its proportion in the selected mutant file.

	
## Requirements
  * software: R(Version 3.4.3 to 3.5), Rstudio(optional)
  * R packages: shiny, shinyjs, ggcyto, flowCore, gtools, Biobase 
  
## Quick installation guide

  1. Run the following command in R/RStudio:
```
<<<<<<< HEAD
install.packages(c("shiny", "shinyjs", "gtools", "ggplot2"))
=======
install.packages(c("shiny", "shinyjs", "gtools""))
>>>>>>> parent of 6e9be70... OUI OUI

```
  >You may be asked to reload your environment, if so, accept.
  
  2. Run the next commands:
```
source("https://bioconductor.org/biocLite.R")
biocLite("Biobase")
biocLite("flowCore")
biocLite("ggcyto")
```

  
## Launching the shiny application

  1. Double click on the ciphold.Rproj file to open it. It will run a new R session with the working directory set to launch the shiny application.
  2. Run the following commands in R/RStudio:
```
<<<<<<< HEAD
library("FCSGenerator")
runApp("FCSGenerator.run()")
=======
library("shiny")
runApp("App/")
>>>>>>> parent of 6e9be70... OUI OUI
```
  3. The application should be running, if not, do all the steps from the "Setting up your environment" section in the good order. If it does not resolves the issue, please go to the "Known Issues" section.
  
### Notes: 
> You could also use **shiny::runApp("App/")**.