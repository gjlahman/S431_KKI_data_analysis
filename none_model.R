library(dplyr)
library(tidyr)
library(alr4)
library(ggplot2)
library(gridExtra)

kki_demographics = read.csv('data/KKI_demographicInfo.csv')
kki_handedness = read.csv('data/KKI_handedness.csv')
kki_mABC = read.csv('data/KKI_movementAssessmentBatteryforChildren.csv')
kki_SRS = read.csv('data/KKI_SocialResponsivenessScaleQuestionnaire.csv')
kki_WISC = read.csv('data/KKI_WechslerIntelligenceScaleforChildren.csv')
kki_gai = read.csv('data/KKI_GAI_from_WISC')


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
                                  'EdinburghHandedness_Integer'))
full_dat = subset(full_dat, visit == 1)
full_dat = subset(full_dat, !is.na(WISC_VERSION) & !is.na(SRS_VERSION))
full_dat = subset(full_dat, ADHD_Subtype %in% c('Combined', 
                                                'Hyperactive/Impulsive', 
                                                'Inattentive', 'No dx'))

full_dat$hasADHD = as.integer(full_dat$ADHD_Subtype != 'No dx')
full_dat$hasAutism = as.integer(full_dat$PrimaryDiagnosis == 'Autism')

WISC4_full_dat = subset(full_dat, WISC_VERSION == 4)
WISC5_full_dat = subset(full_dat, WISC_VERSION == 5)

total_model = lm(SRS_TotalRawScore~mABC_TotalStandardScore, data=full_dat)
comp_model = lm(SRS_TotalRawScore~mABC_ManualDexterity.Component.StandardScore+mABC_AimingAndCatching.Component.StandardScore+mABC_Balance.Component.StandardScore,data=full_dat)

summary(total_model)
summary(comp_model)
anova(total_model,comp_model)
# take more complicated model (by components)


# including mabc_totalscore (by comp), gai, version, gender, age
full_dat_v1 = filter(full_dat,full_dat$SRS_VERSION==1,!is.na(full_dat$GAI),full_dat$PrimaryDiagnosis=='None')

none_model_v1_full = lm(SRS_TotalRawScore~
                          mABC_ManualDexterity.Component.StandardScore+
                          mABC_AimingAndCatching.Component.StandardScore+
                          mABC_Balance.Component.StandardScore +
                          GAI + Gender + mABC_AGE
                          ,data=full_dat_v1)

none_model_v1_red = lm(SRS_TotalRawScore~
                         mABC_ManualDexterity.Component.StandardScore+
                         mABC_AimingAndCatching.Component.StandardScore+
                         mABC_Balance.Component.StandardScore +
                         Gender + mABC_AGE
                         ,data=full_dat_v1)

summary(none_model_v1_full)
summary(none_model_v1_red)
anova(none_model_v1_full,none_model_v1_red)

full_dat_v1$none_predict = predict(none_model_v1_full,newdata=full_dat_v1)

ggplot(full_dat_v1, aes(x=mABC_TotalStandardScore, y=SRS_TotalRawScore)) + 
  geom_jitter(alpha=0.3,width=0.2,height=0.2) + geom_smooth() + 
  geom_line(data=full_dat_v1, aes(x=mABC_TotalStandardScore, none_predict), col='red')

ggplot(full_dat_v1, aes(x=none_predict, y=resid(none_model_v1_full))) + geom_point()
qqnorm(resid(none_model_v1_full))




