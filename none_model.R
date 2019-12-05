library(dplyr)
library(tidyr)
library(alr4)
library(ggplot2)
library(gridExtra)

setwd('/Users/rowlavel/Documents/Classes/Fall2019/LinearModels/S431_KKI_data_analysis')

kki_demographics = read.csv('data/KKI_demographicInfo.csv')
kki_handedness = read.csv('data/KKI_handedness.csv')
kki_mABC = read.csv('data/KKI_movementAssessmentBatteryforChildren.csv')
kki_SRS = read.csv('data/KKI_SocialResponsivenessScaleQuestionnaire.csv')
kki_WISC = read.csv('data/KKI_WechslerIntelligenceScaleforChildren.csv')
kki_gai = read.csv('data/KKI_GAI_from_WISC.csv')


full_dat = full_join(kki_demographics, kki_mABC)
full_dat = full_join(full_dat, kki_SRS)
full_dat = full_join(full_dat, kki_WISC)
full_dat = full_join(full_dat, kki_handedness)
full_dat = full_join(full_dat, kki_gai)

full_dat = full_dat %>% drop_na(c('SecondaryDiagnosis', 
                                  'CurrentlyNotTakingMeds', 
                                  'CurrentlyTakingAtomoxetine', 
                                  'CurrentlyTakingClonidine', 
                                  'mABC_TotalStandardScore', 
                                  'EdinburghHandedness_Integer',
                                  'GAI'))
full_dat = subset(full_dat, visit == 1)
full_dat = subset(full_dat, !is.na(WISC_VERSION) & !is.na(SRS_VERSION))
full_dat = subset(full_dat, ADHD_Subtype %in% c('Combined', 
                                                'Hyperactive/Impulsive', 
                                                'Inattentive', 'No dx'))

full_dat = filter(full_dat, full_dat$PrimaryDiagnosis=='None')

full_dat$hasADHD = as.integer(full_dat$ADHD_Subtype != 'No dx')
full_dat$hasAutism = as.integer(full_dat$PrimaryDiagnosis == 'Autism')

WISC4_full_dat = subset(full_dat, WISC_VERSION == 4)
WISC5_full_dat = subset(full_dat, WISC_VERSION == 5)

red_model = lm(SRS_TotalRawScore~mABC_TotalStandardScore, data=full_dat)
full_model = lm(SRS_TotalRawScore~
                  mABC_ManualDexterity.Component.StandardScore+
                  mABC_AimingAndCatching.Component.StandardScore+
                  mABC_Balance.Component.StandardScore,data=full_dat)

summary(red_model)
summary(full_model)
anova(red_model,full_model)
# take more complicated model (by components)
## we see an increase in adj R^2 value, a highish F statistic, and a small P-value
## reject the null and assume that the full model is better so we predict by components


# full model including GAI
none_model_full = lm(SRS_TotalRawScore~
                          mABC_ManualDexterity.Component.StandardScore+
                          mABC_AimingAndCatching.Component.StandardScore+
                          mABC_Balance.Component.StandardScore +
                          GAI + Gender + mABC_AGE + as.factor(SRS_VERSION)
                          ,data=full_dat)

# reduced model not including GAI
none_model_red = lm(SRS_TotalRawScore~
                         mABC_ManualDexterity.Component.StandardScore+
                         mABC_AimingAndCatching.Component.StandardScore+
                         mABC_Balance.Component.StandardScore +
                         Gender + mABC_AGE + as.factor(SRS_VERSION)
                         ,data=full_dat)

summary(none_model_full)
summary(none_model_red)
anova(none_model_full,none_model_red)
# we see a slight increase in adj R^2, and end up with a largeish F stat and small P value
# so we reject the null and assume that the full model is better than the reduced model
# so we decide to include GAI when analyzing 


full_dat$none_predict = predict(none_model_full)

ggplot(data=full_dat, aes(x=mABC_TotalStandardScore,y=SRS_TotalRawScore,col=as.factor(SRS_VERSION))) + 
  geom_jitter(alpha=0.3,width=0.2,height=0.2) + 
  geom_abline(slope=none_model_full$coefficients[2],
              intercept=none_model_full$coefficients[1], col='red') +
  geom_abline(slope=none_model_full$coefficients[2],
              intercept=none_model_full$coefficients[1]+none_model_full$coefficients[8], col='blue')

ggplot(full_dat, aes(x=none_predict, y=resid(none_model_full), col=as.factor(SRS_VERSION))) + geom_point()
qqnorm(resid(none_model_full))


resid = residuals(none_model_red)
GAI_regout = residuals(lm(GAI~ mABC_TotalStandardScore + SRS_VERSION, data=full_dat))
qplot(x = GAI_regout, y = resid) + geom_smooth(method='lm')







