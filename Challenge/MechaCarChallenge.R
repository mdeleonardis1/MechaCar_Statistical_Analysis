
## Deliverable 1 ##

#read in dataset
MechaCar_MPG <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F) 

## dplyr Library Import 
library(dplyr)
library(tidyverse)

## Check Data
head(MechaCar_MPG)

# Multiple linear Regression 

lm(mpg ~  + vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_MPG) #generate multiple linear regression model


# Summary results of MLR

summary(lm(mpg ~  + vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_MPG)) #generate summary statistics


## Deliverable 2 ##

Scoil_Data <- read.csv('Suspension_Coil.csv',stringsAsFactors = F)


## Check Data  

head(Scoil_Data)


## Plot by Lot 

plt <- ggplot(Scoil_Data,aes(x=Manufacturing_Lot,y=PSI)) #import dataset into ggplot2
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=0,hjust=1)) #add box plot and rotate x-axis labels 45 degrees


## Summary stat of PSI 

summary(Scoil_Data$"PSI")


## Summary Stats Grouped by Lot 

Lot_summary <- Scoil_Data %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),
                                                                        Median_PSI=median(PSI),Variance_PSI=var(PSI),SD_PSI = sd(PSI), .groups = 'keep') #create summary table


## Deliverable 3 ##

## t Test pop

?t.test()
## Deliverable 3 T-Tests on Suspension Coils

?shapiro.test()

# Test for Normality 
shapiro.test(Scoil_Data$PSI)

# P value less than .05 = PSi distribution is normal 

PSI_mu <- mean(Scoil_Data$PSI)

# RScripte t.test() function to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch.
t.test(x=Scoil_Data$PSI, mu=PSI_mu)


# Write three more RScripts in your MechaCarChallenge.RScript using the t.test() function and its subset() argument to determine if the PSI for each manufacturing lot is statistically different from the population mean of 1,500 pounds per square inch.
ss_Lot1 <- subset(Scoil_Data, Scoil_Data$Manufacturing_Lot == 'Lot1')
t.test(x=ss_Lot1$PSI, mu=PSI_mu)

ss_Lot2 <- subset(Scoil_Data, Scoil_Data$Manufacturing_Lot == 'Lot2')
t.test(x=ss_Lot2$PSI, mu=PSI_mu)

ss_Lot3 <- subset(Scoil_Data, Scoil_Data$Manufacturing_Lot == 'Lot3')
t.test(x=ss_Lot3$PSI, mu=PSI_mu)

