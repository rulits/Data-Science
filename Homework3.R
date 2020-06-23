#Preliminaries

#remove all objects in environment
rm(list=ls())

#load libraries
library("utils")
library("tidyverse")

#set wd
setwd("C:/Users/o/Desktop/Micromaster/Data Analysis for Social Scientists/R Codes/Homework3")

#Getting the data
gender_data <- as_tibble(read.csv("Gender_StatsData.csv"))

#first 6 values
head(gender_data)

#last 6 values
tail(gender_data)

#n of rows and columns
dim(gender_data)

#filter only selected indicators
teenager_fr <-filter(gender_data, Indicator.Code == "SP.ADO.TFRT")

#delete the rest of the database we wont use
rm(gender_data)

#obtain the mean specific for the year 1975 without missing values
mean(teenager_fr$X1975,na.rm = TRUE)

#obtain the mean and std specific for the year 1960 without missing values
mean(teenager_fr$X1960,na.rm = TRUE)
sd(teenager_fr$X1960,na.rm = TRUE)

#obtain the mean and std specific for the year 1960 without missing values
mean(teenager_fr$X2000,na.rm = TRUE)
sd(teenager_fr$X2000,na.rm = TRUE)

#keep only the relevant Country.Code observations in teenager_fr
byincomelevel <- filter(teenager_fr,Country.Code%in%c("LIC","MIC","HIC", "WLD"))
colnames(byincomelevel)[1]<-"Country.Name"

#plot data with gather and select using pipe 
plotdata_bygroupyear <- gather(byincomelevel,Year,FertilityRate,X1960:X2015)%>%select(Year, Country.Name, Country.Code, FertilityRate)

#plot data with sperad and select using pipe
plotdata_byyear <- select(plotdata_bygroupyear, Country.Code,Year, FertilityRate) %>%spread(Country.Code,FertilityRate)
rm(plotdata_byyear)
plotdata_byyear <- spread(plotdata_bygroupyear,Country.Code,FertilityRate)

#plot bygroupyear for each income level alias group in the function
ggplot(plotdata_bygroupyear, aes(x=Year,y=FertilityRate,group=Country.Code,color=Country.Code),) +labs(title='Fertility Rate by Country-Income-Level over Time')+ geom_line()

plotdata_bygroupyear <- mutate(plotdata_bygroupyear, Year=as.numeric(str_sub(Year,-4)))



#Generating histdata_twoyears
histdata_twoyears <- select(teenager_fr, Country.Name, Country.Code, Indicator.Name, Indicator.Code, X1960,X2000)

histdata_twoyears <- gather(teenager_fr, Year, FertilityRate, X1960, X2000) %>%
  select(Year, Country.Name, Country.Code, FertilityRate)

histdata_twoyears <- filter(histdata_twoyears,!is.na(FertilityRate))

ggplot(histdata_twoyears, aes(x=FertilityRate)) + 
  geom_histogram(data=subset(histdata_twoyears, Year=="X1960"), 
                 color="darkred", fill="red", alpha=0.2) + 
  geom_histogram(data=subset(histdata_twoyears, Year=="X2000"), 
                 color="darkblue", fill="blue", alpha=0.2) 
ggsave("hist.png")

#Question 20
ggplot(histdata_twoyears, aes(x=FertilityRate, group=Year, color=Year, alpha=0.2)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(data=subset(histdata_twoyears, Year=="X1960"), color="darkred", fill="red", alpha=0.2, bw=5)+ 
  geom_density(data=subset(histdata_twoyears, Year=="X2000"), color="darkblue", fill="blue", alpha=0.2, bw=5)


f <- function(x) 6/5*(1/3+x)
z <- function(y) 6/5*(1/2+y**2)
l <- seq(0,1,.1)

plot(l,f(l), xlim=c(-2,4), xlab="x", ylab="y", col="red")
plot(l,z(l), xlim=c(-2,4), xlab="x", ylab="y", col="blue")
plot.function(l,z)
