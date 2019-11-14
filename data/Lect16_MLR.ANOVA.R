
################################################################
#                    Biostatistical Methods I                  #
#           Lecture 16: Multiple Linear Regression             #
#                          ANOVA Testing                       #
#                    Last updated: 11.12.2019                  #
################################################################


rm(list = ls())

# Load libraries
library(faraway)
library(broom)
library(dplyr)

# Read data 'Hospitals'
data_hosp<-read.csv("Hospital.csv")
names(data_hosp)


# MLR Model 1: Length of stay (LOS) vs number of BEDS and INFRISK
reg1<-lm(LOS~BEDS + INFRISK, data=data_hosp)
summary(reg1)

# Recode MEDSCHL: Yes:1 and No:0
data_hosp$MS<-ifelse(data_hosp$MEDSCHL==1,1,ifelse(data_hosp$MEDSCHL==2, 0, NA))

# MLR Model 2: BEDS, INFRISK, new MS (Medical School Affiliation: 1-Yes, 0-No), NURSE
# MLR 
reg2<-lm(LOS~BEDS + INFRISK + MS + NURSE, data=data_hosp)
summary(reg2)

# ANOVA for MLR (reg2)
# Tests for each term are conditioned for everything else above it.
anova(reg2)
summary(reg2)

# ANOVA for SLR
# What is the difference b/w regression and ANOVA?
reg_simple<-lm(LOS~BEDS, data=data_hosp)
summary(reg_simple)
anova(reg_simple)


# ANOVA test comparing Model 1 vs Model 2 (nested: small vs large model)
reg1<-lm(LOS~BEDS + INFRISK, data=data_hosp)
reg2<-lm(LOS~BEDS + INFRISK + MS + NURSE, data=data_hosp)

# Function to calculate the F-stat to compare Model 1 and Model 2
model_compare <- function(reg1, reg2){
  d1 <- dim(anova(reg1))[[1]]
  d2 <- dim(anova(reg2))[[1]]
  a1 <- anova(reg1)
  a2 <- anova(reg2)
  num <- (a1$`Sum Sq`[d1] - a2$`Sum Sq`[d2])/(a1$Df[d1] - a2$Df[d2])
  deno <- a2$`Sum Sq`[d2]/a2$Df[d2]

  return(num/deno)
}

model_compare(reg1,reg2)


# Built-in R function to compare the models
anova(reg1, reg2)

# Try to switch the order of the models
# anova(reg2, reg1)                       # Same idea, but flipped df.



# Likelihood ratio test for nested models
Delta <- -2*(logLik(reg1) - logLik(reg2))

# Calculate the p-value for the LRT
1-pchisq(Delta, 2)


# Calculate the 'partial' R in a model with 3 predictors
# Let us assume that we want to calculate the effect of INFRISK, given BEDS and MS already in the model
# Fit a regression with all 3 predictors 
# Note that INFRISK is added last: extract the SSR for INFRISK|BEDS+MS

reg3<-lm(LOS~BEDS+MS+INFRISK, data=data_hosp)
anova(reg3)
# SSR(INFRISK|BEDS+MS)=69.54


# Fit a regression only with BEDS+MS and extract the SSE
reg4<-lm(LOS~BEDS+MS, data=data_hosp)
anova(reg4)
# SSE(BEDS, MS)=338.77
# The partial R for INFRISK, given BEDS and MS in the model is equal to 69.54/338.7=0.21



