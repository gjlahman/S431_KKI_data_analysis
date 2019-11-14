library(dplyr)
library(tidyr)
library(alr4)
library(ggplot2)

kki_demographics = read.csv('data/KKI_demographicInfo.csv')
kki_handedness = read.csv('data/KKI_handedness.csv')
kki_mABC = read.csv('data/KKI_movementAssessmentBatteryforChildren.csv')
kki_SRS = read.csv('data/KKI_SocialResponsivenessScaleQuestionnaire.csv')
kki_WISC = read.csv('data/KKI_WechslerIntelligenceScaleforChildren.csv')


full_dat = full_join(kki_demographics, kki_mABC)
full_dat = full_join(full_dat, kki_SRS)
full_dat = full_join(full_dat, kki_WISC)
full_dat = full_join(full_dat, kki_handedness)
full_dat = full_dat %>% drop_na(c('SecondaryDiagnosis', 'CurrentlyNotTakingMeds', 'CurrentlyTakingAtomoxetine', 'CurrentlyTakingClonidine', 'mABC_TotalStandardScore', 'EdinburghHandedness_Integer'))
full_dat = subset(full_dat, visit == 1)
full_dat = subset(full_dat, !is.na(WISC_VERSION) & !is.na(SRS_VERSION))
full_dat = subset(full_dat, ADHD_Subtype %in% c('Combined', 'Hyperactive/Impulsive', 'Inattentive', 'No dx'))
WISC4_full_dat = subset(full_dat, WISC_VERSION == 4)
WISC5_full_dat = subset(full_dat, WISC_VERSION == 5)

ggplot(data=full_dat, aes(x=WISC4_MatrixReasoning, mABC_TotalStandardScore, col=PrimaryDiagnosis)) + geom_point() + geom_jitter()
ggplot(data=full_dat, aes(x=WISC4_BlockDesign, mABC_TotalStandardScore, col=PrimaryDiagnosis)) + geom_point() + geom_jitter()
ggplot(data=full_dat, aes(x=WISC4_PictureConcepts, mABC_TotalStandardScore, col=PrimaryDiagnosis)) + geom_point() + geom_jitter()
ggplot(data=full_dat, aes(x=WISC4_MatrixReasoning, SRS_TotalRawScore, col=PrimaryDiagnosis)) + geom_point() + geom_jitter()
ggplot(data=full_dat, aes(x=WISC4_BlockDesign, SRS_TotalRawScore, col=PrimaryDiagnosis)) + geom_point() + geom_jitter()
ggplot(data=full_dat, aes(x=WISC4_PictureConcepts, SRS_TotalRawScore, col=PrimaryDiagnosis)) + geom_point() + geom_jitter()

ggplot(data=full_dat, aes(x=EdinburghHandedness_Integer, mABC_AimingAndCatching.Component.StandardScore, col=PrimaryDiagnosis)) + geom_point() + geom_jitter()

ggplot(data=filter(full_dat, full_dat$SRS_VERSION==2), aes(x=ID, SRS_TotalRawScore, col=as.factor(round(mABC_AGE)))) + geom_point() + geom_jitter()


