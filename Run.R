#devtools::install_github("sonsoleslp/tna", force=TRUE)
library("tna")

# INITIATION TO INSTALL IN JAMOVI

#install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
options(jamovi_home="/Applications/jamovi.app")
jmvtools::check()



### Update/Install module

# INSTALL INTO JAMOVI

setwd("/Users/mohammedsaqr/Documents/GitHub/JTNA1.2/TNAModule")
jmvtools::install()
