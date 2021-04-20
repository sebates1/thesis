##########################################
#    WRAP data analysis: Descriptive     #
##########################################

## analysis of data not available to share online

library(reshape)
library(dplyr)
library(readxl)
library(data.table)
library(ggplot2)
library(Rmisc)
library(Hmisc)
library(xlsx)
library(wesanderson)


# Import data from excel
df_long <-read_excel("data/WRAP_data_2019.xlsx")

# Import data from csv file
# df_long <- read.csv("Data/WRAP_data_2019.csv")

# df_long <- WRAP_data_2019

# Check sample characteristics match original analysis (Ahern et al., 2017)

# number in each treamtent group
table(df_long$TreatmentGroup)

# Subset to baseline data only
baseline <- df_long[df_long$Timepoint=="A",]

# Age by group - means match 
aggregate(baseline$G_AgeAtTest_Attended_BL, list(gp=baseline$TreatmentGroup), mean, na.rm=T)
aggregate(baseline$G_AgeAtTest_Attended_BL, list(gp=baseline$TreatmentGroup), sd, na.rm=T)

# BMI by group - means match except 0.01 difference in CP52 mean (probable rounding)
aggregate(baseline$BMI, list(gp=baseline$TreatmentGroup), mean, na.rm=T)
aggregate(baseline$BMI, list(gp=baseline$TreatmentGroup), sd, na.rm=T)

# gender by group - frequencies match 
table(baseline$TreatmentGroup, baseline$Sex)

# Assign baseline values of treament group and sex to the id variable at every time point

# subset the baseline line data to just include ID number, sex and Treamtent group
sub_baseline <- baseline[, c("SerNo", "Sex", "TreatmentGroup", "G_AgeAtTest_Attended_BL" )]

# Merge the full data set with the sub_baseline set
df_long_tx <- merge(df_long, sub_baseline, by="SerNo")

# rename the column with Sex and Treatment group 
colnames(df_long_tx)[colnames(df_long_tx)=="Sex.y"]<-"Sex"
colnames(df_long_tx)[colnames(df_long_tx)=="TreatmentGroup.y"]<-"TreatmentGroup"
colnames(df_long_tx)[colnames(df_long_tx)=="G_AgeAtTest_Attended_BL.y"]<-"G_AgeAtTest_Attended_BL"

###Recoding variables to numerical values

# gender to number
df_long_tx$Sex <- recode(df_long_tx$Sex, "Male"=0, "Female"=1)

# Education recode -1 to NA
df_long_tx$LevelOfEducationScore[df_long_tx$LevelOfEducationScore==-1] <- NA

# household income
df_long_tx$Household_Income_num <- recode(df_long_tx$Household_Income, "£0-£9;999 pa"=1, "£10;000-£19;999 pa"=1, "£20;000-£29;999 pa"=2, 
                                       "£30;000-£39;999 pa"=2, "£40;000-£49;999 pa"=2, "£50;000-£59;999 pa"=3, "£60;000-£69;999 pa"=3, "£70;000+ pa"=3, "Prefer not to say"=9)  

# rename variables so shorter and easier to use
setnames(df_long_tx, old=c("TreatmentGroup","Timepoint", "RFCEI_RestraintScore","RFCEI_RigidRestraintScore", "RFCEI_FlexibleRestraintScore", "SRH_TotalScore", "DSRQ_AutonomousScore", 
                           "DSRQ_ControlledScore", "DSRQ_MotivationScore", "Household_Income_num",
                           "LevelOfEducationScore", "G_AgeAtTest_Attended_BL", "ESRQ_AutonomousScore", 
                           "ESRQ_ControlledScore", "ESRQ_MotivationScore" ),
         new=c("TXGROUP", "TIME", "TRES", "RRES", "FRES", "HABIT", "DSRA", "DSRC", "DSRM", "INCOME", "EDU", "AGE", "ESRA", "ESRC", "ESRM" ))

# Re-lable the time points from A-D to time in months
df_long_tx$TIME <- recode(df_long_tx$TIME, "A"= 0, "B"= 3, "C"= 12, "D"= 24)

# Recode the time points to numberical
df_long_tx$TXGROUP <- recode(df_long_tx$TXGROUP, "BI"= 1, "CP12"= 2, "CP52"= 3)

# Means of psychological variables at baseline
Psy_mean <- df_long_tx %>% group_by(TXGROUP, TIME) %>% summarise_at(vars(RRES, FRES, TRES,
              HABIT, HABIT_D, HABIT_E, DSRA, DSRC, DSRM), list(mean, sd), na.rm=TRUE)

# Rename columns to relect function
setnames(Psy_mean, old=c("RRES_fn1", "FRES_fn1", "TRES_fn1", "HABIT_fn1", "HABIT_D_fn1", "HABIT_E_fn1", "DSRA_fn1", "DSRC_fn1", "DSRM_fn1", 
                         "RRES_fn2", "FRES_fn2", "TRES_fn2", "HABIT_fn2","HABIT_D_fn2", "HABIT_E_fn2", "DSRA_fn2", "DSRC_fn2", "DSRM_fn2"),
         new=c("RRES_mean", "FRES_mean", "TRES_mean", "HABIT_mean", "HABIT_D_mean","HABIT_E_mean", "DSRA_mean", "DSRC_mean", "DSRM_mean", 
               "RRES_sd", "FRES_sd", "TRES_sd",  "HABIT_sd", "HABIT_D_sd", "HABIT_E_sd","DSRA_sd", "DSRC_sd", "DSRM_sd"))

# reorder so table reads mean sd mean sd and so on
Psy_mean <- Psy_mean[, c(1,2,3,8,4,9,5,10,6,11,7,12)]

# Export to excel
write.csv(Psy_mean, "Psy_mean_V2.csv")

# Save prepared data file
saveRDS(df_long_tx, "WRAP_ready.rds")

# Differences at baseline

# Restraint
an_res_bl <- aov(RFCEI_RestraintScore ~ TreatmentGroup, data=baseline)
summary(an_res_bl)
model.tables(an_res_bl, "means")

# Habit
an_habit_bl <- aov(SRH_TotalScore ~ TreatmentGroup, data=baseline)
summary(an_habit_bl)
model.tables(an_habit_bl, "means")

# Habit diet
an_habit_bl <- aov(HABIT_D ~ TreatmentGroup, data=baseline)
summary(an_habit_bl)
model.tables(an_habit_bl, "means")

# Habit exercise
an_habit_bl <- aov(HABIT_E ~ TreatmentGroup, data=baseline)
summary(an_habit_bl)
model.tables(an_habit_bl, "means")

# Autonmous diet self-regulation
an_DSRA_bl <- aov(DSRQ_AutonomousScore ~ TreatmentGroup, data=baseline)
summary(an_DSRA_bl)
model.tables(an_DSRA_bl, "means")

# Controlled diet self-regulation
an_DSRC_bl <- aov(DSRQ_ControlledScore ~ TreatmentGroup, data=baseline)
summary(an_DSRC_bl)
model.tables(an_DSRC_bl, "means")

# Amotivaion diet self-regulation
an_DSRM_bl <- aov(DSRQ_MotivationScore ~ TreatmentGroup, data=baseline)
summary(an_DSRM_bl)
model.tables(an_DSRM_bl, "means")

###Line graphs of change over time

# ensure error bar are easier to view by shifting positio
pd <- position_dodge(0.1)

# Recode the time points to intervention names for graphs
df_long_tx$TXGROUP <- recode(df_long_tx$TXGROUP, "1" = "Brief intervention", "2" ="12 week program", "3" ="52 week program")
df_long_tx$TXGROUP <- factor(df_long_tx$TXGROUP, levels = c("Brief intervention", "12 week program", "52 week program"))

# Subset the data to show just the variable of interest by treatment group and time (provides se, ci etc)
subsetBMI <- summarySE(data = df_long_tx, measurevar = "BMI", groupvars=c("TXGROUP", "TIME"), na.rm=TRUE)

# Create graph of variable against time, by treatment group, with error bars representing standard error
pBMI <- ggplot(subsetBMI, aes(x=TIME, y=BMI, color=TXGROUP, linetype =TXGROUP))+
  labs(x = "Time after baseline (months)") + geom_line(size = 0.5) + geom_point() +
  geom_errorbar(aes(ymin=BMI-se, ymax=BMI+se), size = 0.5, width=.1, position = pd)+ scale_x_discrete( limits=c(0,3,6,9,12,15,18,21,24))+ylim(31, 36) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
  axis.line = element_line(colour = "black"), legend.key = element_rect(fill = NA), legend.title = element_blank(), legend.text = element_text(size=19), legend.position="bottom", text = element_text(size=25), legend.key.width = unit(2,"cm"))+
  scale_color_brewer(palette="Dark2")
print(pBMI)

ggsave("pBMI.png")

# Subset to habit and summarise by score at each time point for each treatment group
subsetHABIT <- summarySE(data = df_long_tx, measurevar = "HABIT", groupvars=c("TXGROUP", "TIME"), na.rm=TRUE)

pHABIT <- ggplot(subsetHABIT, aes(x=TIME, y=HABIT, group=TXGROUP, color=TXGROUP))+
  labs(x = "Time after baseline (months)", y="Habit") + geom_line(aes(linetype=TXGROUP), size = 1.25) + geom_point() +
  geom_errorbar(aes(ymin=HABIT-se, ymax=HABIT+se), size = 1.25, width=.1, position = pd)+ scale_x_discrete( limits=c(0,3,6,9,12,15,18,21,24))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.title=element_blank(), legend.key = element_rect(fill = NA), legend.position="none", text = element_text(size=25))+
  scale_color_brewer(palette="Dark2")
print(pHABIT)
ggsave("pHABIT.png")

# Subset to habit (diet) and summarise by score at each time point for each treatment group
subsetHABIT_D <- summarySE(data = df_long_tx, measurevar = "HABIT_D", groupvars=c("TXGROUP", "TIME"), na.rm=TRUE)

pHABIT_D <- ggplot(subsetHABIT_D, aes(x=TIME, y=HABIT_D, group=TXGROUP, color=TXGROUP))+
  labs(x = "Time after baseline (months)", y="Habit Strength") + geom_line(aes(linetype=TXGROUP), size = 0.5) + geom_point() +
  geom_errorbar(aes(ymin=HABIT_D-se, ymax=HABIT_D+se), size = 0.5, width=.1, position = pd)+ scale_x_discrete( limits=c(0,3,6,9,12,15,18,21,24))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.title=element_blank(), legend.key = element_rect(fill = NA), legend.position="none", text = element_text(size=25))+
  scale_color_brewer(palette="Dark2")
print(pHABIT_D)
ggsave("pHABIT_D.png")

# Subset to habit (exercise) and summarise by score at each time point for each treatment group
subsetHABIT_E <- summarySE(data = df_long_tx, measurevar = "HABIT_E", groupvars=c("TXGROUP", "TIME"), na.rm=TRUE)

pHABIT_E <- ggplot(subsetHABIT_E, aes(x=TIME, y=HABIT_E, group=TXGROUP, color=TXGROUP))+
  labs(x = "Time after baseline (months)", y="Exercise habits") + geom_line(aes(linetype=TXGROUP), size = 1.25) + geom_point() +
  geom_errorbar(aes(ymin=HABIT_E-se, ymax=HABIT_E+se), size = 1.25, width=.1, position = pd)+ scale_x_discrete( limits=c(0,3,6,9,12,15,18,21,24))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.title=element_blank(), legend.key = element_rect(fill = NA), legend.position="bottom")+
  scale_color_brewer(palette="Dark2")
print(pHABIT_E)
ggsave("pHABIT_E.png")

# Subset to dietary restraint and summarise by score at each time point for each treatment group
subsetTRES <- summarySE(data = df_long_tx, measurevar = "TRES", groupvars=c("TXGROUP", "TIME"), na.rm=TRUE)

pTRES <- ggplot(subsetTRES, aes(x=TIME, y=TRES, group=TXGROUP, color=TXGROUP))+
  labs(x = "Time after baseline (months)", y="Dietary Restraint") + geom_line(aes(linetype=TXGROUP), size = 0.5) + geom_point() +
  geom_errorbar(aes(ymin=TRES-se, ymax=TRES+se), size = 0.5, width=.1, position = pd)+ scale_x_discrete( limits=c(0,3,6,9,12,15,18,21,24))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.title=element_blank(), legend.key = element_rect(fill = NA), legend.position="none", text = element_text(size=25))+
  scale_color_brewer(palette="Dark2")
print(pTRES)
ggsave("pTRES.png")

# Subset to rigid restraint and summarise by score at each time point for each treatment group
subsetRRES <- summarySE(data = df_long_tx, measurevar = "RRES", groupvars=c("TXGROUP", "TIME"), na.rm=TRUE)

pRRES <- ggplot(subsetRRES, aes(x=TIME, y=RRES, group=TXGROUP, color=TXGROUP))+
  labs(x = "Time after baseline (months)", y="Rigid Dietary Restraint") + geom_line(aes(linetype=TXGROUP), size = 1.25) + geom_point() +
  geom_errorbar(aes(ymin=RRES-se, ymax=RRES+se), size = 1.25, width=1.5, position = pd)+ scale_x_discrete( limits=c(0,3,6,9,12,15,18,21,24))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.title=element_blank(), legend.key = element_rect(fill = NA), legend.position="none", text = element_text(size=25))+
  scale_color_brewer(palette="Dark2")
print(pRRES)
ggsave("pRRES.png")

# Subset to flexible restraint and summarise by score at each time point for each treatment group
subsetFRES <- summarySE(data = df_long_tx, measurevar = "FRES", groupvars=c("TXGROUP", "TIME"), na.rm=TRUE)

pFRES <- ggplot(subsetFRES, aes(x=TIME, y=FRES, group=TXGROUP, color=TXGROUP))+
  labs(x = "Time after baseline (months)", y="Flexible Dietary Restraint") + geom_line(aes(linetype=TXGROUP), size = 1.25) + geom_point() +
  geom_errorbar(aes(ymin=FRES-se, ymax=FRES+se), size = 1.25, width=1.5, position = pd)+ scale_x_discrete( limits=c(0,3,6,9,12,15,18,21,24))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.title=element_blank(), legend.key = element_rect(fill = NA), legend.position="bottom")+
  scale_color_brewer(palette="Dark2")
print(pFRES)

# Subset to autonomous diet self-regulation and summarise by score at each time point for each treatment group
subsetDSRA <- summarySE(data = df_long_tx, measurevar = "DSRA", groupvars=c("TXGROUP", "TIME"), na.rm=TRUE)

pDSRA <- ggplot(subsetDSRA, aes(x=TIME, y=DSRA, group=TXGROUP, color=TXGROUP))+
  labs(x = "Time after baseline (months)", y="Diet self-regulation (autonomous)") + geom_line(aes(linetype=TXGROUP), size = 0.5) + geom_point() +
  geom_errorbar(aes(ymin=DSRA-se, ymax=DSRA+se), size = 0.5, width=.1, position = pd)+ scale_x_discrete( limits=c(0,3,6,9,12,15,18,21,24))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.title=element_blank(), legend.key = element_rect(fill = NA), legend.position="none", text = element_text(size=25))+
  scale_color_brewer(palette="Dark2")
print(pDSRA)
ggsave("pDSRA.png")

# Subset to controlled diet self-regulation and summarise by score at each time point for each treatment group
subsetDSRC <- summarySE(data = df_long_tx, measurevar = "DSRC", groupvars=c("TXGROUP", "TIME"), na.rm=TRUE)

pDSRC <- ggplot(subsetDSRC, aes(x=TIME, y=DSRC, group=TXGROUP, color=TXGROUP))+
  labs(x = "Time after baseline (months)", y="Diet self-regulation (controlled)") + geom_line(aes(linetype=TXGROUP), size = 0.5) + geom_point() +
  geom_errorbar(aes(ymin=DSRC-se, ymax=DSRC+se), size = 0.5, width=.1, position = pd)+ scale_x_discrete(limits=c(0,3,6,9,12,15,18,21,24))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.title=element_blank(), legend.key = element_rect(fill = NA), legend.position="none", text = element_text(size=25))+
  scale_color_brewer(palette="Dark2")
print(pDSRC)
ggsave("pDSRC.png")

# Subset to amotivation diet self-regulation and summarise by score at each time point for each treatment group
subsetDSRM <- summarySE(data = df_long_tx, measurevar = "DSRM", groupvars=c("TXGROUP", "TIME"), na.rm=TRUE)

pDSRM <- ggplot(subsetDSRM, aes(x=TIME, y=DSRM, group=TXGROUP, color=TXGROUP))+
  labs(x = "Time after baseline (months)", y="Diet self-regulation (motivation)") + geom_line(aes(linetype=TXGROUP), size = 0.5) + geom_point() +
  geom_errorbar(aes(ymin=DSRM-se, ymax=DSRM+se), size = 0.5, width=.1, position = pd)+ scale_x_discrete(limits=c(0,3,6,9,12,15,18,21,24))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.title=element_blank(), legend.key = element_rect(fill = NA), 
        legend.position="right", text = element_text(size=25),legend.key.size = unit(3, 'cm'), legend.text =element_text(size=30) )+
  scale_color_brewer(palette="Dark2")
print(pDSRM)
legend <- as_ggplot(get_legend(pDSRM))
pDSRM <- pDSRM + theme(legend.position='none')
ggsave("pDSRM.png")

# group the five plots into one figure
fig_moa_time <- plot_grid(plot_grid(
  pHABIT_D, pTRES, pDSRA, pDSRC, pDSRM,legend,  ncol = 2, align = "vh"))

ggsave("fig_moa_time.png", height = 30, width = 20)

save(df_long_tx, file = "data/df_long_tx.RData")

