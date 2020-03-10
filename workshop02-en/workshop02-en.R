## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#",
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width=5, fig.height=5, fig.retina=3,
  fig.align = 'center'
)
options(repos=structure(c(CRAN="http://cran.r-project.org")))


## ----install_pkgs, echo = FALSE, results = "asis"-----------------------------
cat(
  qcbsRworkshops::first_slides(2, c('dplyr', 'tidyr', 'magrittr'))
)


## ---- eval = FALSE------------------------------------------------------------
## # This is a comment not a command


## ---- eval = FALSE------------------------------------------------------------
## # You can comment using this, but look below how to create section headers:
## 
## ## Heading name ####


## ---- eval = FALSE------------------------------------------------------------
## # Clear the R workspace
## rm(list = ls())
## ?rm
## ?ls


## ---- eval = FALSE------------------------------------------------------------
## A<-"Test" # Put some data in workspace
## A <- "Test" # Note that you can use spaces!
## A = "Test" # <- or = can be used equally
## 
## #Note that it is best practice to use "<-" for assigment instead of "="
## 
## # Check objects in the workspace
## ls()
## # [1] "A"
## 
## A
## # [1] "Test"
## 
## # clean workspace
## rm(list=ls())
## 
## A
## # Error in eval(expr, envir, enclos): object 'A' not found


## ---- eval = FALSE------------------------------------------------------------
## # Complete list of available data on base R
## library(help = "datasets")


## ---- eval = FALSE------------------------------------------------------------
## getwd()


## ---- eval = TRUE-------------------------------------------------------------
dir()


## ---- eval = TRUE-------------------------------------------------------------
CO2 <- read.csv("data/co2_good.csv", header=TRUE)


## ---- eval = FALSE------------------------------------------------------------
## ?read.csv


## ---- eval = FALSE------------------------------------------------------------
## ?read.csv2


## ---- eval = TRUE-------------------------------------------------------------
str(CO2)


## ---- eval = FALSE------------------------------------------------------------
## CO2 <- read.csv("data/co2_good.csv", header = FALSE)


## ---- eval = FALSE------------------------------------------------------------
## mydata[1,] # Extracts the first row
## mydata[2,3] # Extracts the content of row 2 / column 3
## mydata[,1] # Extracts the first column
## mydata[,1][2] # [...] can be also be used recursively
## mydata$Variable1 # Also extracts the first column


## ---- eval = TRUE-------------------------------------------------------------
# First lets make a copy of the dataset to play with
CO2copy <- CO2
# names() gives you the names of the variables present in the data frame
names(CO2copy)

# Changing from English to French names (make sure you have the same levels!)
names(CO2copy) <- c("Plante","Categorie", "Traitement", "conc", "absortion")
names(CO2copy)


## ---- eval = TRUE-------------------------------------------------------------
# Let's create an unique id for our samples:
# Don't forget to use "" for strings
CO2copy$uniqueID <- paste0(CO2copy$Plante,
                           "_",CO2copy$Categorie,
                           "_", CO2copy$Traitement)

# observe the results
head(CO2copy$uniqueID)


## ---- eval = FALSE------------------------------------------------------------
## # Let's standardize our variable "absortion" to relative values
## CO2copy$absortionRel <- CO2copy$absortion/max(CO2copy$absortion)
## 
## # Observe the results
## head(CO2copy$absortionRel)


## ---- eval = FALSE------------------------------------------------------------
## # Let's keep working with our CO2copy data frame
## 
## # Select only "Plante" and "absortionRel" columns. (Don't forget the ","!)
## CO2copy[,c("Plante", "absortionRel")]
## 
## # Subset data frame from rows from 1 to 50
## CO2copy[1:50,]


## ---- eval = F----------------------------------------------------------------
## # Select observations matching only the nonchilled Traitement.
## CO2copy[CO2copy$Traitement == "nonchilled",]
## 
## # Select observations with absortion higher or equal to 20
## CO2copy[CO2copy$absortion >= 20, ]
## 
## # Select observations with absortion higher or equal to 20
## CO2copy[CO2copy$Traitement  == "nonchilled" & CO2copy$absortion >= 20, ]
## 
## # We are done playing with the Dataset copy, lets erase it.
## CO2copy <- NULL


## ---- eval = FALSE------------------------------------------------------------
## summary(CO2)


## ---- eval = TRUE-------------------------------------------------------------
# Calculate the mean and the standard deviation of the CO2 concentration:
# Assign them to new variables
meanConc <- mean(CO2$conc)
sdConc <- sd(CO2$conc)


## ---- eval = TRUE-------------------------------------------------------------
# print() prints any given value to the R console
print(paste("the mean of concentration is:", meanConc))

print(paste("the standard deviation of concentration is:", sdConc))


## ---- eval = TRUE, fig.width=6, fig.height=6----------------------------------
# Let's plot a histogram to explore the distribution of "uptake"
hist(CO2$uptake)


## ---- eval = TRUE, fig.width=6, fig.height=6----------------------------------
# Increasing the number of bins to observe better the pattern
hist(CO2$uptake, breaks = 40)


## ---- eval = F----------------------------------------------------------------
## # Saving an R workspace file that stores all your objects
## save.image(file="data/co2_project_Data.RData")
## 
## 
## # Clear your memory
## rm(list = ls())
## 
## 
## # Reload your data
## load("data/co2_project_Data.RData")
## head(CO2) # Looking good!


## ---- echo = FALSE------------------------------------------------------------
head(CO2)


## ---- eval = FALSE------------------------------------------------------------
## write.csv(CO2, file = "data/co2_new.csv")


## ---- eval = TRUE-------------------------------------------------------------
CO2 <- read.csv("data/co2_broken.csv")
head(CO2)


## ---- eval = TRUE-------------------------------------------------------------
head(CO2)


## ---- eval = TRUE-------------------------------------------------------------
CO2 <- read.csv("data/co2_broken.csv",sep = "")


## ---- eval = TRUE-------------------------------------------------------------
head(CO2)


## ---- eval = TRUE-------------------------------------------------------------
CO2 <- read.csv("data/co2_broken.csv", sep = "", skip = 2)
head(CO2)


## ---- eval = TRUE-------------------------------------------------------------
str(CO2)


## ---- eval = TRUE-------------------------------------------------------------
unique(CO2$conc)


## ---- eval = TRUE-------------------------------------------------------------
CO2 <- read.csv("data/co2_broken.csv", sep = "", skip = 2,
                na.strings = c("NA","na","cannot_read_notes"))
str(CO2)


## ---- eval = FALSE------------------------------------------------------------
## str(CO2)


## ---- eval = TRUE-------------------------------------------------------------
levels(CO2$Treatment)
unique(CO2$Treatment)


## ---- eval = TRUE-------------------------------------------------------------
# Identify all rows that contain "nnchilled" and replace with "nonchilled"
CO2$Treatment[CO2$Treatment=="nnchilled"] <- "nonchilled"

# Identify all rows that contain "chiled" and replace with "chilled"
CO2$Treatment[CO2$Treatment=="chiled"] <- "chilled"


## -----------------------------------------------------------------------------
boxplot(uptake ~ Treatment, data = CO2)


## ---- eval = TRUE-------------------------------------------------------------
CO2 <- droplevels(CO2)
str(CO2)


## ---- eval = FALSE------------------------------------------------------------
## boxplot(uptake ~ Treatment, data = CO2)


## ---- echo = FALSE, fig.height = 3.5------------------------------------------
par(mar = c(4, 4, 0, 0))
boxplot(uptake ~ Treatment, data = CO2)


## ----tidyr--------------------------------------------------------------------
library(tidyr)


## ---- echo = FALSE, eval = TRUE-----------------------------------------------
# Let's create a new data frame
wide <- data.frame(Species = c("Oak", "Elm", "Ash"),
                   DBH = c(12, 20, 13),
                   Height = c(56, 85, 55))
wide


## ---- echo=FALSE, eval = TRUE-------------------------------------------------
long = gather(wide, Measurement, Value, DBH, Height)
long


## ---- eval = FALSE------------------------------------------------------------
## install.packages("tidyr")
## library(tidyr)


## ---- echo = TRUE-------------------------------------------------------------
wide <- data.frame(Species = c("Oak", "Elm", "Ash"),
                   DBH = c(12, 20, 13), Height = c(56, 85, 55))
wide


## -----------------------------------------------------------------------------
long = tidyr::gather(wide, Measurement, Value, DBH, Height)
long


## -----------------------------------------------------------------------------
long


## -----------------------------------------------------------------------------
wide2 = tidyr::spread(long, Measurement, Value)
wide2


## -----------------------------------------------------------------------------
set.seed(8)
messy <- data.frame(id = 1:4,
                    trt = sample(rep(c('control', 'farm'), each = 2)),
                    zooplankton.T1 = runif(4),
                    fish.T1 = runif(4),
                    zooplankton.T2 = runif(4),
                    fish.T2 = runif(4))
messy


## -----------------------------------------------------------------------------
messy.long <- tidyr::gather(messy, taxa, count, -id, -trt)
head(messy.long)


## ----separate-----------------------------------------------------------------
messy.long.sep <- tidyr::separate(messy.long, taxa,
                                  into = c("species", "time"), sep = "\\.")
head(messy.long.sep)


## ---- eval = FALSE------------------------------------------------------------
## ?airquality
## 
## data(airquality)


## -----------------------------------------------------------------------------
air.long <- tidyr::gather(airquality, variable, value, -Month, -Day)
head(air.long)


## -----------------------------------------------------------------------------
air.wide <- tidyr::spread(air.long, variable, value)
head(air.wide)


## ---- eval = TRUE-------------------------------------------------------------
library(dplyr)


## -----------------------------------------------------------------------------
ozone <- dplyr::select(airquality, Ozone, Month, Day)
head(ozone)


## -----------------------------------------------------------------------------
august <- dplyr::filter(airquality, Month == 8, Temp >= 90)
# same as: filter(airquality, Month == 8 & Temp >= 90)
head(august)


## -----------------------------------------------------------------------------
air_mess <- dplyr::sample_frac(airquality, 1)
head(air_mess)


## -----------------------------------------------------------------------------
air_chron <- dplyr::arrange(air_mess, Month, Day)
head(air_chron)


## -----------------------------------------------------------------------------
airquality_C <- dplyr::mutate(airquality, Temp_C = (Temp-32)*(5/9))
head(airquality_C)


## -----------------------------------------------------------------------------
library(magrittr)


## -----------------------------------------------------------------------------
june_C <- mutate(filter(airquality, Month == 6),
                 Temp_C = (Temp-32)*(5/9))


## -----------------------------------------------------------------------------
june_C <- airquality %>%
          dplyr::filter(Month == 6) %>%
          dplyr::mutate(Temp_C = (Temp-32)*(5/9))


## -----------------------------------------------------------------------------
month_sum <- airquality %>%
      group_by(Month) %>%
      summarise(mean_temp = mean(Temp),
                sd_temp = sd(Temp))
month_sum


## ----eval = 2-----------------------------------------------------------------
?ChickWeight
data(ChickWeight)


## -----------------------------------------------------------------------------
weight_diff <- ChickWeight %>%
               dplyr::group_by(Chick) %>%
               dplyr::summarise(weight_diff = max(weight) - min(weight))


## -----------------------------------------------------------------------------
head(weight_diff)


## -----------------------------------------------------------------------------
diet_summ <- ChickWeight %>%
             dplyr::group_by(Diet, Chick) %>%
             dplyr::summarise(weight_gain = last(weight) - first(weight)) %>%
             dplyr::group_by(Diet) %>%
             dplyr::summarise(mean_gain = mean(weight_gain))


## -----------------------------------------------------------------------------
diet_summ

