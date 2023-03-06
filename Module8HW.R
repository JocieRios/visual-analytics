# Loading packages
library(readxl)
library(ggplot2)

# Preparing Data for Analysis

# Making Main Data Table from CDC Data Table
main_df <- read_xlsx("table009.xlsx", range="A4:AR21")

## Correcting column names
### Removing extra characters
colnames(main_df) <- substr(colnames(main_df),1,4)
colnames(main_df)[1] <- "Age Range"

## Delete unneeded rows
main_df <- main_df[4:17,]

## Delete unneeded column
main_df <- main_df[,-43]

# Making variable for years column
years <- (1998:2018)
years  

# Making variable for young adult deaths 
deaths1 <- as.double(main_df[2,23:43])

# Making variable for adult deaths
deaths2 <- as.double(main_df[7,23:43])

# Making variable for older adult deaths
deaths3 <- as.double(main_df[10,23:43])

# Obtaining Data Frame of deaths throughout various ages

deathsDF <- data.frame(years, deaths1, deaths2, deaths3)
colnames(deathsDF)

head(deathsDF)

## NOTE: REMEMBER DEATHS ARE RATES, WITH DEATHS PROVIDED OCCURING PER 100,000 RESIDENT POPULATION (more like death proportion/rate)

# Preparing Plots with Regression using ggplot2 and Linear Models (lm)

## Young Adults Death Rates 
youngPlot <- ggplot(deathsDF, aes(x=years, y=deaths1)) +
  geom_point(color="darkorange") +
  stat_smooth(method="lm", color="yellow")

youngPlot

youngLM <- lm(formula= deaths1 ~ years, data= deathsDF)
summary(youngLM)

## Adults Death Rates
adultPlot <- ggplot(deathsDF, aes(x=years, y=deaths2)) +
  geom_point(color="brown") +
  stat_smooth(method="lm", color="beige")

adultPlot

adultLM <- lm(formula= deaths2 ~ years, data= deathsDF)
summary(adultLM)

## Older Adult Death Rates
oldPlot <- ggplot(deathsDF, aes(x=years, y=deaths3)) +
  geom_point(color="lightgreen") +
  stat_smooth(method="lm", color="lightblue") 

oldPlot

oldLM <- lm(formula= deaths3 ~ years, data= deathsDF)
summary(oldLM)



