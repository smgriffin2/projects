# Rscript for final project
# Sam Griffin, Jocelyn Cao
# LSC 660 Final Project
# Date began: 11/12/22
# Purpose: This Rscript features the analysis flow for successful 
# stimulus check and is the main Rscript for our analysis.
#_________________________________

# set working directory
setwd("~/Documents/lsc/lsc660/final_project")

# read csv
raw_data <- read.csv("final_data.csv")

##############################
#### PART1: clean-up data ####
##############################

semi_raw_data <- raw_data[raw_data$DistributionChannel != "preview",]
semi_raw_data1 <- semi_raw_data[semi_raw_data$Finished == "True",]
cleanish_data <- semi_raw_data1[c(19:46)]

# remove failed stimulus checks
cleaner_data <- cleanish_data[cleanish_data$Q11 != "I do not remember",]

clean_data <-cleaner_data[cleaner_data$Q11 == "Blake Miller, a journalist from the Williamsburg Gazette"
                              & cleaner_data$Stimuli_DO == "Human-stimulus" | cleaner_data$Q11 ==
                                "Heliograf, the Williamsburg Gazette's AI journalist" & cleaner_data$Stimuli_DO == "AI-stimulus",]

# Create string to numeric data conversion function
# for familiarty Q14, higher number = more familiar
# for likert scale, higher number = more agree
convert_to_numeric <- function(col){
  if (col == "Strongly Disagree" | col == "No" | col == "Not Sure" | col == "AI-stimulus") {
    col <- 0
  } else if ( col == "Somewhat Disagree" | col == "Yes" | col == "Human-stimulus" ) {
    col <- 1
  } else if ( col == "Neither Agree nor Disagree") {
    col <- 2
  } else if ( col == "High school or GED") {
    col <- 12
  } else if ( col == "Associates or other 2-yr equivalent" | col == "Trade School") {
    col <- 14
  } else if ( col == "Bachelors or 4 yr equivalent") {
    col <- 16
  } else if ( col == "Masters") {
    col <- 18
  } else {
    col <- 4
  }
}

# Create a familiarity column by converting sub-questions to numeric, then summing their output:
clean_data$Q14_1 <- sapply(clean_data$Q14_1, convert_to_numeric)
clean_data$Q14_2 <- sapply(clean_data$Q14_2, convert_to_numeric)
clean_data$Q14_3 <- sapply(clean_data$Q14_3, convert_to_numeric)
clean_data$ai_familiarity <- NA
clean_data$ai_familiarity <- rowSums(clean_data[ , c(10:12)])

# Create a trust column by converting sub-questions to numeric, then averaging their output:

clean_data$Q13_1 <- sapply(clean_data$Q13_1, convert_to_numeric)
clean_data$Q13_2 <- sapply(clean_data$Q13_2, convert_to_numeric)
clean_data$Q13_3 <- sapply(clean_data$Q13_3, convert_to_numeric)
clean_data$trust <- NA
clean_data$trust <- rowSums(clean_data[ , c("Q13_1", "Q13_2", "Q13_3")]) / 3

# Create a risk column by converting sub-questions to numeric, then averaging their output:
clean_data$Q12_1 <- sapply(clean_data$Q12_1, convert_to_numeric)
clean_data$Q12_2 <- sapply(clean_data$Q12_2, convert_to_numeric)
clean_data$Q12_5 <- sapply(clean_data$Q12_5, convert_to_numeric)
clean_data$risk <- NA
clean_data$risk <- rowSums(clean_data[ , c("Q12_1", "Q12_2", "Q12_5")]) / 3

# Create a efficacy column by converting sub-questions to numeric, then averaging their output:
clean_data$Q10_3 <- sapply(clean_data$Q10_3, convert_to_numeric)
clean_data$Q10_4 <- sapply(clean_data$Q10_4, convert_to_numeric)
clean_data$Q12_3 <- sapply(clean_data$Q12_3, convert_to_numeric)
clean_data$efficacy <- NA
clean_data$efficacy <- rowSums(clean_data[ , c("Q10_3", "Q10_4", "Q12_3")]) / 3

#############################
## Reliability tests for each instrument (risk, trust, efficacy, interest)
#############################

library(psych) # activate psych package
trust_alpha <- psych::alpha(clean_data[ , c("Q13_1", "Q13_2", "Q13_3")]) #reliability analysis on trust scale
risk_aplha <- psych::alpha(clean_data[ , c("Q12_1", "Q12_2", "Q12_5")]) #reliability analysis on risk scale
efficacy_alpha <- psych::alpha(clean_data[ , c("Q10_3", "Q10_4", "Q12_3")]) #reliability analysis on efficacy scale


#############################
####### BALANCE TABLE #######
#############################

# Relevant statistics and p-values placed in Table 1

# subsets of each condition:
# subsetting this way is unnessesary, but did it early and why re-write?
clean_data$Stimuli_DO <- sapply(clean_data$Stimuli_DO, convert_to_numeric)
ai <- clean_data[clean_data$Stimuli_DO == 0,]
human <- clean_data[clean_data$Stimuli_DO == 1,]

# length of subsets
n_ai <- nrow(ai)
n_human <- nrow(human)

# Recode age to numeric, assigning the average of the range, then t.test
clean_data$age <- NA
clean_data$age[clean_data$Q1 == "18-24"] <- 21
clean_data$age[clean_data$Q1 == "25-34"] <- 30
clean_data$age[clean_data$Q1 == "35-44"] <- 40
clean_data$age[clean_data$Q1 == "45-54"] <- 50
clean_data$age[clean_data$Q1 == "55-65"] <- 60

t.test(clean_data$age[clean_data$Stimuli_DO == 0], clean_data$age[clean_data$Stimuli_DO == 1]) #p-value =0.81

# Gender -- categorical variable, we do chi-square, make binary: woman or not woman
table(ai$Q2) # m/w/o 11/16/0
table(human$Q2) # m/w/o 21/22/2

chisq.test(rbind(c(16, 11), c(22, 23))) #p-value = 0.54

# Education 1) recode to years, highschool/GED = 12, Associates = 14, BA = 16, Masters = 18, 2) t-test
table(ai$Q6)
table(human$Q6)

ai$Q6 <- sapply(ai$Q6, convert_to_numeric)
human$Q6 <- sapply(human$Q6, convert_to_numeric)

t.test(ai$Q6, human$Q6) #p-value = 0.47

# Race -- categorical white or non-white
table(ai$Q5)
table(human$Q5)

chisq.test(rbind(c(17, 10), c(23, 22))) #p-value = 0.46

# Mental Health -- categorial yes or no
table(ai$Q8)
table(human$Q8)

chisq.test(rbind(c(22, 5), c(36, 9))) #p-value = 1.0

# AI Familiarity
mean(ai$ai_familiarity) #avg = 1.666
mean(human$ai_familiarity) # avg = 1.422

t.test(ai$ai_familiarity, human$ai_familiarity) # p-value = 0.18

###########################
### Multiple Regression ###
###########################

# convert years in school to numeric
clean_data$Q6 <- sapply(clean_data$Q6, convert_to_numeric)

# convert mental health to binary (yes = 1, no = 0)
clean_data$Q8 <- ifelse(clean_data$Q8 == "Yes", 1, 0)

# convert gender to binary variable (female = 1, male/third gender/other = 0)
clean_data$Q2 <- ifelse(clean_data$Q2 == "Woman", 1, 0)

# convert race to binary variable (white = 1, non-white = 0)
clean_data$Q5 <- ifelse(clean_data$Q5 == "White", 1, 0)

# Rename column names for better output
names(clean_data)[names(clean_data) == 'Stimuli_DO'] <- 'Source_Cue'
names(clean_data)[names(clean_data) == 'Q6'] <- 'Education'
names(clean_data)[names(clean_data) == 'Q8'] <- 'Mental_Health'
names(clean_data)[names(clean_data) == 'Q2'] <- 'Gender'
names(clean_data)[names(clean_data) == 'Q5'] <- 'Race'
names(clean_data)[names(clean_data) == 'ai_familiarity'] <- 'Ai_Familiarity'
names(clean_data)[names(clean_data) == 'age'] <- 'Age'

# Table 2: Trust multi regression
trust_fit <- lm(clean_data$trust~Source_Cue + Ai_Familiarity + Age + Mental_Health + Education + Gender + Race, data = clean_data)
summary(trust_fit)

# Table 3: Risk multi regression
risk_fit <- lm(clean_data$risk~Source_Cue + Ai_Familiarity + Age + Mental_Health + Education + Gender + Race, data = clean_data)
summary(risk_fit)

# Table 4: Efficacy multi regression
efficacy_fit <- lm(clean_data$efficacy~Source_Cue + Ai_Familiarity + Age + Mental_Health + Education + Gender + Race, data = clean_data)
summary(efficacy_fit)

##################################
#### Individual Trust Models #####
##################################

### Table 5: Trust in news org
trust_fit_newsorg <- lm(clean_data$Q13_1~Source_Cue + Ai_Familiarity + Age + Mental_Health + Education + Gender + Race, data = clean_data)
summary(trust_fit_newsorg)

### Table 6: Trust in writer
trust_fit_writer <- lm(clean_data$Q13_2~Source_Cue + Ai_Familiarity + Age + Mental_Health + Education + Gender + Race, data = clean_data)
summary(trust_fit_writer)

### Table 7: Trust in content
trust_fit_content <- lm(clean_data$Q13_3~Source_Cue + Ai_Familiarity + Age + Mental_Health + Education + Gender + Race, data = clean_data)
summary(trust_fit_content)

# packages for formatted regression table
library(gtsummary)
library(tidyverse)
library(survival)

# Trust table (table 2)
trust_table <- bold_p(tbl_regression(trust_fit))
modify_caption(trust_table, "**Table 2. Trust x Source Cue**")

# Risk table (table 3)
risk_table <- bold_p(tbl_regression(risk_fit))
modify_caption(risk_table, "**Table 3. Risk x Source Cue**")

# Efficacy table (table 4)
efficacy_table <- bold_p(tbl_regression(efficacy_fit))
modify_caption(efficacy_table, "**Table 4. Efficacy x Source Cue**")

# Trust item tables 6-9
org_table <- bold_p(tbl_regression(trust_fit_newsorg))
modify_caption(org_table, "**Table 7. Trust in News Org x Source Cue**")

writer_table <- bold_p(tbl_regression(trust_fit_writer))
modify_caption(writer_table, "**Table 8. Trust in News Writer x Source Cue**")

content_table <- bold_p(tbl_regression(trust_fit_content))
modify_caption(content_table, "**Table 9. Trust in News Content x Source Cue**")

