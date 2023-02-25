#Deliverable 1-----------------------------------------------------------

#3. load dplyr package
library(dplyr)

#4. read file
MechaCar <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

#5. perform linear regression
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data= MechaCar)

#6. perform summary
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data= MechaCar))



#Deliverable 2------------------------------------------------------------

#1,2. read file
Suspension <- read.csv(file='Suspension_coil.csv',check.names=F,stringsAsFactors = F)

#3. get a total summary
total_summary  <- Suspension %>% summarize(Mean=mean(PSI),Median=(PSI),Variance=var(PSI),SD=sd(PSI)) 

#4. create a lot summary
lot_summary <- Suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=(PSI),Variance=var(PSI),SD=sd(PSI))


#Deliverable 3 -----------------------------------------------------------

# 1. Peform t-test across all Lots
t.test(Suspension$PSI,mu = 1500)

# 2.Use t.test() function 3 more times 
#Peform t-test on Lot 1
t.test(subset(Suspension,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
# Peform t-test on Lot 2
t.test(subset(Suspension,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
# Peform t-test on Lot 3
t.test(subset(Suspension,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)

