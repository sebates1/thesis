#############################################
#  WRAP data analysis: Multiple Imputation  #
#############################################

## analysis of data not available to share online

library(mice)
library(data.table)
library(mice.1chain)
library(miceadds)

# Load WRAP data set
load("df_long_tx.RData")

# Subset the data to impute
mi_df <- df_long_tx[, c("SerNo", "TXGROUP", "TIME", "Sex", "INCOME", "EDU", "AGE",
                         "BMI", "TRES","RRES","FRES", "HABIT", "HABIT_D", "HABIT_E", "DSRA", "DSRC", "DSRM",  "ESRA", "ESRC", "ESRM")]

# Covert to wide
mi_df_wide <- reshape(mi_df, idvar = "SerNo", direction = "wide", timevar = "TIME", sep = "_")
mi_df_wide <- mi_df_wide[, c(1:19, 25:37, 43:55, 61:73)]

# Save data set ready for multiple imputation
saveRDS(mi_df_wide, "WRAP_wide.rds")

# rename baseline variables
setnames(mi_df_wide, old=c("TXGROUP_0","Sex_0", "INCOME_0","EDU_0", "AGE_0"), new=c("TXGROUP", "Sex", "INCOME","EDU", "AGE"))

# recode vairables

##Remove SerNo (not compatible with mplus)
mi_df_wide <- mi_df_wide[, c("TXGROUP", "Sex", "INCOME", "EDU", "AGE",
                             "BMI_0", "TRES_0", "RRES_0", "FRES_0", "HABIT_0", "HABIT_D_0", "HABIT_E_0","DSRA_0", "DSRC_0", "DSRM_0", "ESRA_0", "ESRC_0", "ESRM_0",
                             "BMI_3", "TRES_3", "RRES_3", "FRES_3", "HABIT_3", "HABIT_D_3", "HABIT_E_3","DSRA_3", "DSRC_3", "DSRM_3", "ESRA_3", "ESRC_3", "ESRM_3",
                             "BMI_12", "TRES_12", "RRES_12", "FRES_12", "HABIT_12", "HABIT_D_12", "HABIT_E_12","DSRA_12", "DSRC_12", "DSRM_12", "ESRA_12", "ESRC_12", "ESRM_12",
                             "BMI_24", "TRES_24", "RRES_24", "FRES_24", "HABIT_24", "HABIT_D_24", "HABIT_E_24","DSRA_24", "DSRC_24", "DSRM_24", "ESRA_24", "ESRC_24", "ESRM_24" )]

# Summarise missing data (i.e. how much for each variable)
setDT(mi_df_wide)[, lapply(.SD, function(x) sum(is.na(x))), by = "TXGROUP"]

# Check and update variable types (to factor)
str(mi_df_wide)
mi_df_wide$TXGROUP <- as.factor(mi_df_wide$TXGROUP)
mi_df_wide$Sex <- as.factor(mi_df_wide$Sex)
mi_df_wide$INCOME <- as.factor(mi_df_wide$INCOME)
mi_df_wide$EDU <- as.factor(mi_df_wide$EDU)

# Examine missing data pattern
md.pattern<-md.pattern(mi_df_wide)

# Save to text file
write.table(md.pattern, "md.pattern.txt")

# Save file before imputation
save(mi_df_wide, file = "mi_df_wide.RData")
load("mi_df_wide.RData")

#Run multiple impuatation to determine which variables have a correlation of at least 0.3 (mincor) and at least 25% of usable cases (minpuc)  
#imp<-mice(mi_df_wide, pred=quickpred(mi_df_wide,mincor=.3, minpuc=0.25), print=F)
#imp$pred

##Save prediction matrix to text file (which variables are predictive of others)
#write.table(imp$pred, "predmatrix.txt")

##Edit excel file to ensure that all time points are predicted by other values in that time point.
pred<-read.table("predmatrix.txt")
pred<-as.matrix(pred)

##Run multiple imputation##
imp<-mice(mi_df_wide, m = 40, seed=123, maxit=30, pred=pred, print=F)

##Check method
imp$meth

#Save the imputation file
write.mice.imputation(mi.res=imp, name="imputations" )
load("/Volumes/home/CM/Cmp16seb/ManWin/My Documents/Year 2/WRAP 2 year/imputation/imputation.Rdata")

# Check covergence
plot(imp, c("INCOME","EDU","AGE"))
plot(imp, c("BMI_3", "BMI_12", "BMI_24"))
plot(imp, c("DRES_0", "DRES_3", "DRES_12", "DRES_24"))
plot(imp, c("HABIT_0", "HABIT_3", "HABIT_12", "HABIT_24"))
plot(imp, c("DSRA_0", "DSRA_3", "DSRA_12", "DSRA_24"))
plot(imp, c("DSRC_0", "DSRC_3", "DSRC_12", "DSRC_24"))
plot(imp, c("DSRM_0", "DSRM_3", "DSRM_12", "DSRM_24"))
plot(imp, c("ESRA_0", "ESRA_3", "ESRA_12", "ESRA_24"))
plot(imp, c("ESRC_0", "ESRC_3", "ESRC_12", "ESRC_24"))
plot(imp, c("ESRM_0", "ESRM_3", "ESRM_12", "ESRM_24"))

# Check values are within expected range
stripplot(imp, AGE~.imp, pch=20, cex=2)
stripplot(imp, INCOME~.imp, pch=20, cex=2)
stripplot(imp, EDU~.imp, pch=20, cex=2)
stripplot(imp, BMI_3~.imp, pch=20, cex=2)
stripplot(imp, BMI_12~.imp, pch=20, cex=2)
stripplot(imp, BMI_24~.imp, pch=20, cex=2)
stripplot(imp, DRES_0~.imp, pch=20, cex=2)
stripplot(imp, DRES_3~.imp, pch=20, cex=2)
stripplot(imp, DRES__12~.imp, pch=20, cex=2)
stripplot(imp, DRES_24~.imp, pch=20, cex=2)
stripplot(imp, HABIT_0~.imp, pch=20, cex=2)
stripplot(imp, HABIT_3~.imp, pch=20, cex=2)
stripplot(imp, HABIT_12~.imp, pch=20, cex=2)
stripplot(imp, HABIT_24~.imp, pch=20, cex=2)
stripplot(imp, DSRA_0~.imp, pch=20, cex=2)
stripplot(imp, DSRA_3~.imp, pch=20, cex=2)
stripplot(imp, DSRA_12~.imp, pch=20, cex=2)
stripplot(imp, DSRA_24~.imp, pch=20, cex=2)
stripplot(imp, DSRC_0~.imp, pch=20, cex=2)
stripplot(imp, DSRC_3~.imp, pch=20, cex=2)
stripplot(imp, DSRC_12~.imp, pch=20, cex=2)
stripplot(imp, DSRC_24~.imp, pch=20, cex=2)
stripplot(imp, DSRM_0~.imp, pch=20, cex=2)
stripplot(imp, DSRM_3~.imp, pch=20, cex=2)
stripplot(imp, DSRM_12~.imp, pch=20, cex=2)
stripplot(imp, DSRM_24~.imp, pch=20, cex=2)
stripplot(imp, ESRA_0~.imp, pch=20, cex=2)
stripplot(imp, ESRA_3~.imp, pch=20, cex=2)
stripplot(imp, ESRA_12~.imp, pch=20, cex=2)
stripplot(imp, ESRA_24~.imp, pch=20, cex=2)
stripplot(imp, ESRC_0~.imp, pch=20, cex=2)
stripplot(imp, ESRC_3~.imp, pch=20, cex=2)
stripplot(imp, ESRC_12~.imp, pch=20, cex=2)
stripplot(imp, ESRC_24~.imp, pch=20, cex=2)
stripplot(imp, ESRM_0~.imp, pch=20, cex=2)
stripplot(imp, ESRM_3~.imp, pch=20, cex=2)
stripplot(imp, ESRM_12~.imp, pch=20, cex=2)
stripplot(imp, ESRM_24~.imp, pch=20, cex=2)

# Export to mplus
mids2mplus(imp, file.prefix = "imp", sep = "\t", dec = ".", silent = FALSE)

# Calcualte means and confidence intervals for imputed data sets

# get mean of BMI at 12 months by treatment group in each imputation
subsetBMI_12 <- summarySE(data = long1, measurevar = "BMI_12", groupvars=c("TXGROUP", ".imp"), na.rm=TRUE)
# get mean of the means and variance
BMI_12_sum<-aggregate(subsetBMI_12[, c("BMI_12", "sd", "ci", "se")] ,na.rm=T, list(subsetBMI_12$TXGROUP), mean)

# get mean of BMI at 24 months by treatment group in each imputation
subsetBMI_24 <- summarySE(data = long1, measurevar = "BMI_24", groupvars=c("TXGROUP", ".imp"), na.rm=TRUE)
# get mean of the means and variance
BMI_24_sum<-aggregate(subsetBMI_24[, c("BMI_24", "sd", "ci", "se")] ,na.rm=T, list(subsetBMI_12$TXGROUP), mean)

# mean of each imputation
subsetBMI_12_m <- summarySE(data = long1, measurevar = "BMI_12", groupvars=c(".id"), na.rm=TRUE)
subsetBMI_24_m <- summarySE(data = long1, measurevar = "BMI_24", groupvars=c(".id"), na.rm=TRUE)

subsetBMI_12_m<-as.data.frame(subsetBMI_12_m)
subsetBMI_24_m<-as.data.frame(subsetBMI_24_m)

# set up data frame to calculate change in BMI for each group
change <- cbind(mi_df_wide$TXGROUP, mi_df_wide$BMI_0, subsetBMI_12_m$BMI_12, subsetBMI_24_m$BMI_24)
colnames(change)<-c("TXGROUP", "BMI_0", "BMI_12", "BMI_24")

change<-as.data.frame(change)

change$year1<-change$BMI_12-change$BMI_0

change$year2<-change$BMI_24-change$BMI_12

# Summarise changes (to get standard error)
summarySE(data = change, measurevar = "year1", groupvars=c("TXGROUP"), na.rm=TRUE)
summarySE(data = change, measurevar = "year2", groupvars=c("TXGROUP"), na.rm=TRUE)

summarySE(data = change, measurevar = "BMI_0", groupvars=c("TXGROUP"), na.rm=TRUE)
summarySE(data = change, measurevar = "BMI_12", groupvars=c("TXGROUP"), na.rm=TRUE)
summarySE(data = change, measurevar = "BMI_24", groupvars=c("TXGROUP"), na.rm=TRUE)

# Summarise mechanisms of action accross imputations
habit_mi_0 <-summarySE(data = long1, measurevar = "HABIT_D_0", groupvars=c(".id"), na.rm=TRUE)
dres_mi_0 <-summarySE(data = long1, measurevar = "TRES_0", groupvars=c(".id"), na.rm=TRUE)
sres_mi_0 <-summarySE(data = long1, measurevar = "DSRA_0", groupvars=c(".id"), na.rm=TRUE)

habit_mi_12 <-summarySE(data = long1, measurevar = "HABIT_D_12", groupvars=c(".id"), na.rm=TRUE)
habit_mi_24 <-summarySE(data = long1, measurevar = "HABIT_D_24", groupvars=c(".id"), na.rm=TRUE)
dres_mi_12 <-summarySE(data = long1, measurevar = "TRES_12", groupvars=c(".id"), na.rm=TRUE)
dres_mi_24 <-summarySE(data = long1, measurevar = "TRES_24", groupvars=c(".id"), na.rm=TRUE)
sres_mi_12 <-summarySE(data = long1, measurevar = "DSRA_12", groupvars=c(".id"), na.rm=TRUE)
sres_mi_24 <-summarySE(data = long1, measurevar = "DSRA_24", groupvars=c(".id"), na.rm=TRUE)

# Join together in data frame
psy_mi <- cbind(wrap_2yr$TXGROUP_0, wrap_2yr$AGE_0, wrap_2yr$Sex_0, habit_mi_0$HABIT_D_0, habit_mi_12$HABIT_D_12, habit_mi_24$HABIT_D_24,
                dres_mi_0$TRES_0, dres_mi_12$TRES_12, dres_mi_24$TRES_24,
                sres_mi_0$DSRA_0, sres_mi_12$DSRA_12, sres_mi_24$DSRA_24)

colnames(psy_mi)<-c("TXGROUP",  "AGE", "SEX", "HABIT_0", "HABIT_12", "HABIT_24", "TRES_0","TRES_12", "TRES_24", "DSRA_0", "DSRA_12","DSRA_24")
psy_mi<-as.data.frame(psy_mi)

# save data frame
write.table(psy_mi, "psy_mi.txt")

# Add change in mechanisms of action
psy_mi$change_hab1<-psy_mi$HABIT_12 - psy_mi$HABIT_0
psy_mi$change_res1<-psy_mi$TRES_12 - psy_mi$TRES_0
psy_mi$change_sreg1<-psy_mi$DSRA_12 - psy_mi$DSRA_0

psy_mi$change_hab2<-psy_mi$HABIT_24 - psy_mi$HABIT_0
psy_mi$change_res2<-psy_mi$TRES_24 - psy_mi$TRES_0
psy_mi$change_sreg2<-psy_mi$DSRA_24 - psy_mi$DSRA_0

