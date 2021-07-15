library(ggplot2)
library(dplyr)
library(GGally)
library(car)
library(arm)
library(pROC)
library(e1071)
library(caret)
moralMachine= read.csv("/Users/fengyi111/Desktop/2020-Fall/702/final-project/combined.csv")
## filter out observations where attribute level is random, as well as ones where there is NAN
## Age: default is young and at same time omission is young (killed), if saved means swerved; or default is old and defaul is not omission, if saved means served
## Social status
## Fitness
 
survey <-  read.csv("/Users/fengyi111/Desktop/2020-Fall/702/final-project/survey.csv")
 
survey = survey[, !names(survey) %in% c('ResponseID','ExtendedSessionID', 'Template','ScenarioTypeStrict','Intervention','PedPed','Barrier')] # 7 variables
 
 
 cols <- c("UserCountry3", "AttributeLevel", "ScenarioType",'DefaultChoiceIsOmission',
           'Saved','DescriptionShown','LeftHand','Review_education','Review_gender','Review_income')
 survey[cols] <- lapply(survey[cols], factor)  ## as.factor() could also be used

 ## drop empty value and nan values
 
# check empty string or space
survey_empty <- survey[apply(survey,1,function(x) any(x == ' '| x == '')),] # 309202 observations
# drop observations with empty value
survey_clean <- survey[!apply(survey,1,function(x) any(x == ' '| x == '')),]  # 2600540 observations
# drop levels where there are no values after cleaning the empty values
 survey_clean = droplevels(survey_clean)
# turns out no nan values left
survey_clean <- survey_clean[!apply(survey_clean,1,function(x) any(is.na(x))),] # 2600540 observations

# unify omission levels 
survey_clean<- transform(survey_clean, omission = ifelse(DefaultChoiceIsOmission== '1',DefaultChoice,NonDefaultChoice))

# create swerve column to represent whether the responder chose one group over another
survey_clean<- transform(survey_clean, swerve = ifelse((DefaultChoiceIsOmission== '1'& Saved == '0')|(DefaultChoiceIsOmission== '0'& Saved == '1'),0,1))

survey_clean$swerve <- factor(survey_clean$swerve)
survey_clean$omission <- factor(survey_clean$omission)
# some observations have age value out of range
survey_clean[survey_clean$Review_age>100,]
survey_clean[survey_clean$Review_age<0,]
survey_clean <- survey_clean[!(survey_clean$Review_age>100|survey_clean$Review_age<0),]

survey_clean$Review_age
mean(survey_clean$Review_age)
write.csv(survey_clean,'/Users/fengyi111/Desktop/2020-Fall/702/final-project/survey_clean.csv')


moralMachine<- read.csv("/Users/fengyi111/Desktop/2020-Fall/702/final-project/survey_clean.csv")

moralMachine = moralMachine[, !names(moralMachine) %in% c('X','ResponseID', 'ExtendedSessionID','UserID','Intervention','PedPed','Barrier',
                                                          'ScenarioOrder','CrossingSignal','AttributeLevel','ScenarioTypeStrict','NumberOfCharacters',
                                                          'DiffNumberOFCharacters','Template','DescriptionShown','LeftHand', 'Saved', 'DefaultChoice',
                                                          'NonDefaultChoice','DefaultChoiceIsOmission')] # 20 variables

cols <- c("UserCountry3", "Review_education", "ScenarioType",'Review_gender','Review_income','omission','swerve')
moralMachine[cols] <- lapply(moralMachine[cols], factor)  ## as.factor() could also be used

moralMachine$Review_age <- moralMachine$Review_age - mean(moralMachine$Review_age)



############## EDA ##############

## those who swerve and do not swerve are very close in numbers
table(moralMachine$swerve)
## chi-square test using country and swerve shows that the difference is not significant
tapply(moralMachine$swerve,moralMachine$UserCountry3,function(x) table(x)/sum(table(x)))
chisq.test(table(moralMachine[,c("swerve","UserCountry3")]),simulate.p.value = TRUE)


## omission and scenario type; some have less data but in general there are sufficient number of observations in each omission type
table(moralMachine$omission)
table(moralMachine$ScenarioType)
tapply(moralMachine$swerve,moralMachine$ScenarioType,function(x) table(x)/sum(table(x)))
chisq.test(table(moralMachine[,c("swerve","ScenarioType")]),simulate.p.value = TRUE)
tapply(moralMachine$swerve,moralMachine$omission,function(x) table(x)/sum(table(x)))
chisq.test(table(moralMachine[,c("swerve","omission")]),simulate.p.value = TRUE)

# number of observation in the three countries have significant difference, where RUS has about 10 times more
# observations than CHN, USA has about 50 times more observations
table(moralMachine$UserCountry3)



## responder attributes
# age
# The distribution of age for swerve of not swerve is very similar (a lot of outlier larger than 50)
ggplot(moralMachine, aes(x=swerve, y=Review_age)) + 
  geom_boxplot()
# religous - no significnat difference
ggplot(moralMachine, aes(x=swerve, y=Review_religious)) + 
  geom_boxplot()
# political - no significant difference
ggplot(moralMachine, aes(x=swerve, y=Review_political)) + 
  geom_boxplot()


# education; chi-square test not significant
tapply(moralMachine$swerve,moralMachine$Review_education,function(x) table(x)/sum(table(x)))
chisq.test(table(moralMachine[,c("swerve","Review_education")]),simulate.p.value = TRUE)

# income
tapply(moralMachine$swerve,moralMachine$Review_income,function(x) table(x)/sum(table(x)))
chisq.test(table(moralMachine[,c("swerve","Review_income")]),simulate.p.value = TRUE)
# gender
tapply(moralMachine$swerve,moralMachine$Review_gender,function(x) table(x)/sum(table(x)))
chisq.test(table(moralMachine[,c("swerve","Review_gender")]),simulate.p.value = TRUE)


# Interaction

# omission and swerve by country
# interaction effect in CHN is sifnificant
swerve_CHN <- moralMachine$swerve[moralMachine$UserCountry3=='CHN']
omission_CHN <- moralMachine$omission[moralMachine$UserCountry3=='CHN']
tapply(swerve_CHN,omission_CHN,function(x) table(x)/sum(table(x)))
chisq.test(table(swerve_CHN,omission_CHN))

# RUS is also significant
swerve_RUS <- moralMachine$swerve[moralMachine$UserCountry3=='RUS']
omission_RUS <- moralMachine$omission[moralMachine$UserCountry3=='RUS']
tapply(swerve_RUS,omission_RUS,function(x) table(x)/sum(table(x)))
chisq.test(table(swerve_RUS,omission_RUS))

# USA is also significant
swerve_USA <- moralMachine$swerve[moralMachine$UserCountry3=='USA']
omission_USA <- moralMachine$omission[moralMachine$UserCountry3=='USA']
tapply(swerve_USA,omission_USA,function(x) table(x)/sum(table(x)))
chisq.test(table(swerve_USA,omission_USA))


#  religious and swerve by gender
# may be significant
ggplot(moralMachine,aes(x=swerve, y=Review_religious, group=swerve)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Parityvs Baby Being Premature, by Smoke or Not",
       x="Premature or not ",y="Parity") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ Review_gender)

# 

model <- glm(swerve ~ omission + Review_religious+ Review_political + Review_income + Review_gender
             + Review_age, family =  binomial (link='logit'), data = moralMachine)
summary(model)
par(mar=c(1,1,1,1))
rawresid1 <- residuals(model,"resp")
binnedplot(x=fitted(model),y=rawresid1,xlab="Pred. probabilities",nclass = 100,
           col.int="red4",ylab="Avg.residuals",main="Binned residual plot",col.pts="navy")

binnedplot(x=moralMachine$Review_age,y=rawresid1,xlab="Pred. probabilities",nclass = 100,
           col.int="red4",ylab="Avg.residuals",main="Binned residual plot",col.pts="navy")

binnedplot(x=moralMachine$Review_religious,y=rawresid1,xlab="Pred. probabilities",nclass = 100,
           col.int="red4",ylab="Avg.residuals",main="Binned residual plot",col.pts="navy")

binnedplot(x=moralMachine$Review_political,y=rawresid1,xlab="Pred. probabilities",nclass = 100,
           col.int="red4",ylab="Avg.residuals",main="Binned residual plot",col.pts="navy")



Conf_mat6 <- confusionMatrix(as.factor(ifelse(fitted(model) >= 0.5, "1","0")),
                             as.factor(moralMachine$swerve),positive = "1")
Conf_mat6$table
Conf_mat6$overall["Accuracy"]
Conf_mat6$byClass[c('Sensitivity','Specificity')]

# Very high specificity and increased ROC 
roc(moralMachine$swerve,fitted(model),plot=T,print.thres="best",legacy.axes=T,
    print.ouc = T, col="red3")

# model selection
# stepwise; only omission
n <- nrow(moralMachine)
null_model <- glm(swerve~1,data=moralMachine,family=binomial)
model_step <- step(null_model,scope=formula(model),direction="both",
     trace=0)
# forward
model_forward <- step(null_model,direciton='forward',trace=0)
model_backward <- step(model,direciton='backward',trace=0)

# anova to test each main effect
# country -- not significant
model_country <- glm(swerve~omission + UserCountry3,data=moralMachine,family=binomial)
anova(model_backward,model_country,test='Chisq')
# income
model_income <- glm(swerve~omission + Review_income,data=moralMachine,family=binomial)
anova(model_backward,model_income,test='Chisq')
# education
model_education <- glm(swerve~omission + Review_education,data=moralMachine,family=binomial)
anova(model_backward,model_education,test='Chisq')
# gender
model_gender <- glm(swerve~omission + Review_gender,data=moralMachine,family=binomial)
anova(model_backward,model_gender,test='Chisq')
# religious
model_religious <- glm(swerve~omission + Review_religious,data=moralMachine,family=binomial)
anova(model_backward,model_religious,test='Chisq')
# political
model_political <- glm(swerve~omission + Review_political,data=moralMachine,family=binomial)
anova(model_backward,model_political,test='Chisq')

# with interaction
model_inter <- glm(swerve~omission + omission: UserCountry3,data=moralMachine,family=binomial)
anova(model_backward,model_inter,test='Chisq')

# final model
final_model <- glm(swerve~omission + omission: UserCountry3,data=moralMachine,family=binomial)
summary(final_model)
rawresid2 <- residuals(final_model,"resp")

binnedplot(x=fitted(final_model),y=rawresid2,xlab="Pred. probabilities",nclass = 100,
           col.int="red4",ylab="Avg.residuals",main="Binned residual plot",col.pts="navy")
par(mar=c(1,1,1,1))

Conf_mat_f <- confusionMatrix(as.factor(ifelse(fitted(final_model) >= 0.5, "1","0")),
                             as.factor(moralMachine$swerve),positive = "1")
Conf_mat_f$table
Conf_mat_f$overall["Accuracy"]
Conf_mat_f$byClass[c('Sensitivity','Specificity')]

# Very high specificity and increased ROC
par(mar=c(1,1,1,1))

roc(moralMachine$swerve,fitted(final_model),plot=T,print.thres="best",legacy.axes=T,
    print.ouc = T, col="red3")

# Multicolinearity 
# All below 10 which shows no evidence of multicolinearity 
check_vif <- final_model
vif(check_vif)