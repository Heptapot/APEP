rm(list=ls())
setwd("/Volumes/GoogleDrive/My Drive/R/Projects/APEP")

#installing necessary packages (don't forget "dependencies = T")
install.packages("mlogit", dependencies = T)
install.packages("stargazer", dependencies = T)
install.packages("mlogit", dependencies = T)
install.packages("naniar", dependencies = T)
#launche libraries
library(stargazer) 
library(broom)
library(foreign)
library(nnet)
library(stargazer)
library(car) 
library(nnet)
library(mlogit)
library(lme4)
library(lmerTest)
library(effects)
library(emmeans)
library(dplyr)
library(ggplot2)
library(naniar)
#reading in data
apep <- read.csv("apep.csv", header=T, sep=",", na.strings="", stringsAsFactors = T)
#get familiar with the data
View(apep)
head(apep)
str(apep)
#check how many missing values
summary(complete.cases(apep)) #each case has missing values
# # # GOING TO CLEANING DATA, SELECTING VARIABLES FOR ANALYSIS
# # Variables of interests:
# Q3 - gender (categorical variable, 3+) needs recoding into 3, possibly even 2. 
# Q21 - who you voted for in 2019 #Ableg (categorical variable, 3+)
# Q17_1 - Income (continous variable)
# Q13 - Education (interval variable, 3+)
# Q18 - religiosity (interval, 3+) -> recode into a binomial variable with 2 categories.
# Q20 - zipcodes (multinomial)
# Q4 - urban/suburban (binomial) #summary(is.na(apep$Q4))
# Q15 - work status (categorical) 
# Q16 - merital status (categorical)


#built a new training subset where I recode a few variables and excluded missing. 
trn <- na.omit(select(apep, gender = Q3, voted = Q21, zipcode = Q20, income = Q17_1, 
                      educ = Q13, religst = Q18, urbsub = Q4, wrksts = Q15, mertstat = Q16))
summary(complete.cases(trn)) #make sure there are no more NAs.

# # # Let's look at the dustribution of Income between men and women
ggplot(trn, aes(gender, income)) + geom_boxplot(aes(fill = gender)) 


# # # # # working gender variable.
str(trn$gender) #shows you what kind of variable this is. Rid of NAs, make 'man' reference level
summary(trn$gender) #shows you what categories there are with frequencies 
# to change labels as levels, I tarnsform the variable into a numeic one and back to a factor.
trn$gender <- as.factor(as.numeric(trn$gender))
summary(trn$gender) #check to see if it all went well.
#trn$gender <- droplevels(trn$gender)
# # # recode 'gender' into three categories with 'man' being reference.
trn$gender <- recode(trn$gender, '4'="man", '7'="woman", .default= "queer")
trn$gender <- relevel(trn$gender, ref = "man")
###'queer' category is too small for analysis. I declare it missing.
trn$gender <- recode_factor(trn$gender, queer = NA_character_) #declare "queer" missing
trn <- na.omit(trn) #I replace the old model with the new one where I don't have NAs for gender that has 2 categories now
unique(levels(trn$gender)) 
# # # Let's look at the dustribution of Income between men and women
ggplot(trn, aes(gender, income)) + geom_boxplot(aes(fill = gender)) 
###let's see if mean income is different for the genders
trn1 <- trn %>% 
  group_by(gender) %>% 
  mutate(MeanInc = mean(income))
ggplot(trn1, aes(gender, MeanInc)) + geom_col(aes(fill = gender))
  #income variable plotting
ggplot(data = trn1, aes(x = income)) + geom_density(aes(fill = gender))
qqnorm(trn$income)
qqline(trn$income)

#log transformation
ggplot(data = trn, aes(x = income)) + 
  geom_density()
qqnorm(trn$income)
qqline(trn$income)

plot(income~gender, data=trn)
# May be want to know whether income is different between males and females
t.test(trn[trn$gender=="man",]$income, trn[trn$gender=="woman",]$income)
# These summary dataframes can also be easily plotted
ggplot(data = trn, aes(y=income, x=gender))+geom_boxplot()

# Create a table showing overall male-female pay differences in base pay. 
summary_base <- group_by(trn, gender)
summary_base <- summarise(summary_base, meanBasePay = mean(income, na.rm = TRUE), medBasePay = median(income, na.rm = TRUE), cnt = sum(!(is.na(income)))) 
View(summary_base)

### religiosity
levels(trn$religst)
trn$religst <- as.numeric(trn$religst)
trn$religst <- recode(trn$religst, '1'=1, '2'=1, '3'=2, '4'=2)
trn$religst <- factor(trn$religst)

model1 <- lm(income ~ log(gender), data = trn)
summary(model1)
plot(resid(model1))
qqnorm(resid(model1))
qqline(resid(model1))

# Log transform
ggplot(data = trn, aes(x = log(income))) + 
  geom_density() 

ggplot(data = trn, aes(sample = log(income))) + stat_qq() + 
  stat_qq_line()



# Create an overall table of summary statistics for the data. 
stargazer(trn, type = "text", out = "summary.txt")


# # # I want to do a T.test of how man and women voted

summary(trn$voted)
trn$voted <- droplevels(trn$voted)

#trn$voted <- as.factor(as.numeric(trn$voted))
testvote <- trn[, trn$voted == 'The United Conservative Party of Alberta' & trn$voted == 'The New Democratic Party of Alberta']



###working with gender variable
str(apep$Q3)
names(apep$Q3)
summary(apep$Q3)
summary(is.na(apep$Q3))
View(apep$Q3) #learn how to recode

apep$sex <- trimws(apep$Q3)
apep$sex <- as.factor(apep$sex) #made sex a factor
apep$sex1 <- subset(apep$sex, !is.na(apep$sex))
#apep$sex <- ifelse(apep$sex == "Man", "Man", ifelse(apep$sex == "Woman", "Woman", 3))
apep$sex <- relevel(apep$sex, ref = "Man") #made man a reference level
apep$sex <- droplevels(apep$sex)
summary(apep$sex)
length(apep$sex)
table(is.na(apep$sex))
View(apep$sex)
str(apep$sex)

view(apep$Q21)
apep$vote <- trimws(apep$Q21)
apep$vote <- relevel(apep$Q21, ref = "The New Democratic Party of Alberta") #made man a reference level
table(is.na(apep$vote))
#vote <- subset(apep$vote, !is.na(apep$vote))
view(apep$vote)
str(apep$vote)
view(apep$sex)
str(apep$sex)


# Checking the output (dependent) variable
table(apep$vote)

mmmm1 <- as.numeric(apep$vote)
mmmm12 <- as.numeric(apep$sex)
mmm1 <- as.table(apep$vote, apep$sex)
mm1 <- mcnemar.test(mmm1)

m1 <- multinom(vote ~ sex, na.rm = T, data = apep)
summary(m1)
#calculating Z score
z <- summary(m1)$coefficients/summary(m1)$standard.errors
z
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 8
p
## extract the coefficients from the model and exponentiate
exp(coef(m1))

names(apep)
levels(apep$sex)






coefs <- coef(m1)
summary(coefs)
stargazer(m1, type="text")
pt(abs(m1$coefficients / m1$standard.errors), df=nrow(apep)-10, lower=FALSE)

### check for missing values 
colSums(is.na(apep))



###dependent variable is Q21 - What oarty you voted for in 2019 elections in Alberta
head(apep)
view(apep$Q21) #opens up the vector
summary(apep$Q21)
str(apep$Q21)

dim(apep) # size of the dataframe
nrow(apep) # To get the number of rows
ncol(apep) # To get the number of columns

colnames(apep) # To get the names of the columns
rownames(apep) # To get the names of the rows (though this isn't usually that informative)

apep$vote <- as.numeric(apep$Q21)
str(apep$vote)

view(apep$vote)
view(apep$q21)





ggplot(data = apep11, aes(x = Q21)) + 
  geom_density() #build a density plot


