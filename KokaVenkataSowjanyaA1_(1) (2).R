# Koka Venkata Sowjanya
# Due 11:59pm; Sep 21, 2020
# Assignment 1: North Carolina births

#=========================================================================

# Question 1 : Read the dataset straight from the OpenIntro webpage

###Answer:
## Reading the dataset from webpage
NC_Births <- read.table("https://www.openintro.org/data/csv/ncbirths.csv",sep = ",",header = TRUE)

## Displaying the first three records from the dataset
head(NC_Births,3)
#NC_Births[1:3,]

#=========================================================================

# Question 2 : Create a contingency table for these two variables: habit and premie
# Round the probabilities to four decimal places. round(x, digits = 0)

###Answer:
## Cross Classification of habit and premie

CC_Tab_Freq1 <- table(Mom_Type = NC_Births$habit, Birth_Outcome = NC_Births$premie)
CC_Tab_Freq1 #Cross Classification Table of counts (frequencies)

CC_Tab_Freq_Tot1 <- addmargins(table(Mom_Type = NC_Births$habit, Birth_Outcome = NC_Births$premie))
CC_Tab_Freq_Tot1 #Cross Classification Table of total (frequencies)

CC_Tab_Rel_Freq1 <- prop.table(table(Mom_Type = NC_Births$habit, Birth_Outcome = NC_Births$premie))
CC_Tab_Rel_Freq1 # Cross Classification Table of relative frequencies (Probabilities)
round(CC_Tab_Rel_Freq1,4) # Probabilities rounded to four decimal places

CC_Tab_Rel_Freq_Tot1 <- addmargins(prop.table(table(Mom_Type = NC_Births$habit, Birth_Outcome = NC_Births$premie)))
CC_Tab_Rel_Freq_Tot1 # Cross Classification Table of relative frequencies (Probabilities) with totals
round(CC_Tab_Rel_Freq_Tot1,4) # Probabilities rounded to four decimal places

#=========================================================================

# Question 3 : What is the probability that a woman who smokes during pregnancy will give birth prematurely?

###Answer:
## Probability that a woman who smokes during pregnancy will give birth prematurely
##  P(Birth_Outcome = premie|Mom_Type=smoker) = 0.0190/0.1263 = 0.1504
Q3 <- 0.0190/0.1263
#Probability that a woman who smokes during pregnancy will give birth prematurely is :
round(Q3,4)

#=========================================================================
# Question 4 : What is the probability that a woman who does not smoke during pregnancy will give birth prematurely?

###Answer:
## Probability that a woman who does not smoke during pregnancy will give birth prematurely
##  P(Birth_Outcome = premie|Mom_Type=nonsmoker) = 0.1333/0.8737 = 0.1526
Q4 <- 0.1333/0.8737
#Probability that a woman who does not smoke during pregnancy will give birth prematurely is:
round(Q4,4)

#=========================================================================

# Question 5: Compare the likelihood of giving birth prematurely, for the smoker versus the non-smoker.

###Answer:
PreBirth_OddsRatio <- Q3/Q4 #Odds ratio calculation
round(PreBirth_OddsRatio,4) #Rounding to four decimals

## Odds Ratio is 0.986
## The likelihood of giving birth prematurely, for the smoker versus non-smoker could be interpreted as no association and 
#smoker or non- smoker it is equally likely to have a premature baby as the odds ratio is almost equal to 1.
#=========================================================================

# Question 6: Having established the comparison from part 5 as a basis. Now compare the likelihood of lowbirthweight, for a mother who smokes versus a mother who does not.

###Answer:
## Cross Classification of habit and premie

CC_Tab_Freq2 <- table(Mom_Type = NC_Births$habit, Weight_Outcome = NC_Births$lowbirthweight)
CC_Tab_Freq2 #Cross Classification Table of counts (frequencies)

CC_Tab_Freq_Tot2 <- addmargins(table(Mom_Type = NC_Births$habit, Weight_Outcome = NC_Births$lowbirthweight))
CC_Tab_Freq_Tot2 #Cross Classification Table of total (frequencies)

CC_Tab_Rel_Freq2 <- prop.table(table(Mom_Type = NC_Births$habit, Weight_Outcome = NC_Births$lowbirthweight))
CC_Tab_Rel_Freq2 # Cross Classification Table of relative frequencies (Probabilities)
round(CC_Tab_Rel_Freq2,4) # Probabilities rounded to four decimal places

CC_Tab_Rel_Freq_Tot2 <- addmargins(prop.table(table(Mom_Type = NC_Births$habit, Weight_Outcome = NC_Births$lowbirthweight)))
CC_Tab_Rel_Freq_Tot2 # Cross Classification Table of relative frequencies (Probabilities) with totals
round(CC_Tab_Rel_Freq_Tot2,4) # Probabilities rounded to four decimal places

## Probability that a woman who does smokes during pregnancy will give birth to baby with lowbirthweight
##  P(Birth_Weight = lowbirthweight|Mom_Type=smoker) = 0.0180/0.1261 = 0.1427

BirthWeight_Smoker = 0.0180/0.1261
round(BirthWeight_Smoker,4) #Rounding to four decimals

## Probability that a woman who does not smoke during pregnancy will give birth to baby with lowbirthweight
##  P(Birth_Weight = lowbirthweight|Mom_Type=nonsmoker) = 0.0921/0.8739 = 0.1054

BirthWeight_NonSmoker = 0.0921/0.8739
round(BirthWeight_NonSmoker,4) #Rounding to four decimals

Weight_OddsRatio <- BirthWeight_Smoker/BirthWeight_NonSmoker #Odds ratio calculation
round(Weight_OddsRatio,4) #Rounding to four decimals

##Odds Ratio is 1.3544
## The likelihood of giving birth to a baby with lowbirthweight, for the smoker versus non-smoker could be interpreted as 
##A mom who is smoker is 1.3544 times more likely to have a low birth weight baby compared to non-smoker.

#=========================================================================

#Question 7: Create a boxplot that compares the distribution of birth weights for Low/Not Low Birthweights. The
#plot should look as close as possible to the one shown below. Research on your own what a “normal”
#birthweight should be. Provide your sources.

###Answer:
boxplot( NC_Births$weight~NC_Births$lowbirthweight, horizontal = TRUE) #Boxplot
##Source for normal birth weight
##https://www.uofmhealth.org/health-library/te6295#:~:text=The%20average%20birth%20weight%20for,usually%20lighter%20than%20later%20siblings.
##It explains the birth weight for babies is, 
##around 7.5 lb (3.5 kg), and weights between 5.5 lb (2.5 kg) and 10 lb (4.5 kg) are considered normal
#=========================================================================

#7a. Based on the plot, what seems to be the skewness for the distributions of weight at birth, for “low” and “not low”?. A one word answer suffices.

###Answer:
##The skewness for the distributions of weight at birth, for “low” is negatively skewed(left-skewed)

##The skewness for the distributions of weight at birth, for “not low” is  symmetrical
#=========================================================================
#7b. Does the classification for birth weight that you found agree with “North Carolina births” (2004) data? Explain in one sentence.

###Answer:
## Yes because in the North Carolina births” (2004) data,the "not low" birth weight classification has birth weight median value 
#around 7.3 which falls under normal weight of a baby according to the source and 
# For "low" birth weight classification there is a slight deviation and median value is around 5 which is slightly
# less than the normal weight(5.5 to 10) of the baby according to the source  

#=========================================================================

#Question 8: Create a boxplot that compares the distribution of weights by baby gender. 
#Does it seems as if the gender of the baby makes a difference in weight at birth? 
#Explain from your boxplot.

###Answer:

boxplot( NC_Births$weight~NC_Births$gender, horizontal = TRUE) #Boxplot

###Answer:
# From the box plot we can observe that there is only slight difference 
# in the central tendencies of weight distribution for male and female babies
## because,
# The measure of center(median) for males is 7.5 and for females is 7
# The measure of spread(IQR) for males is 2 (8.5 - 6.5) and for females is 1.6(7.8 - 6.2) 
# The distribution of females is skewed left(negative) and males is symmetrical
# The range of males is 11(12-1) and females is 10.3(10.8 - 0.5)


