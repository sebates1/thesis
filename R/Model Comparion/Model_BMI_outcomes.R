##########################################
# Adding MoA to SPHR model: BMI outcomes #
##########################################

## Some analysis of data that can't be shared online

library(ggpubr)
source("R/Functions.R")

# Import dataset with actual BMI values for year 1 and 2 (after multiple imputation)
df_rawData<-read.csv("debug_outputs/raw_mi_BMI.txt", sep="")  ## Raw data not available on github
df_rawData_tx0 <- df_rawData[which(df_rawData$TX==1),]
df_rawData_tx12 <- df_rawData[which(df_rawData$TX==2),]
df_rawData_tx52 <- df_rawData[which(df_rawData$TX==3),]

# Import dataset with actual BMI values for year 1 and 2 (after multiple imputation)
# Simulated population 
df_rawSim_tx0 <- read.csv("debug_outputs/raw_data_BI.txt", sep="",row.names = NULL)
df_rawSim_tx12 <- read.csv("debug_outputs/raw_data_12.txt", sep="",row.names = NULL)
df_rawSim_tx52 <- read.csv("debug_outputs/raw_data_52.txt", sep="",row.names = NULL)

# Import dataset with predicted BMI for years 1 and 2, for mean, conditional and psychological models for each treatment group
# Brief intervention
df_mean_tx0<- read.csv("debug_outputs/pop_mean_1.txt", sep="")
df_cond_tx0<- read.csv("debug_outputs/pop_cond_1.txt", sep="")
df_psy_tx0<- read.csv("debug_outputs/pop_psy_1_2021.txt", sep="")

# 12-week intervention
df_mean_tx12<- read.csv("debug_outputs/pop_mean_2.txt", sep="")
df_cond_tx12<- read.csv("debug_outputs/pop_cond_2.txt", sep="")
df_psy_tx12<- read.csv("debug_outputs/pop_psy_2_2021.txt", sep="")

# 52-week intervention
df_mean_tx52<- read.csv("debug_outputs/pop_mean_3.txt", sep="")
df_cond_tx52<- read.csv("debug_outputs/pop_cond_3.txt", sep="")
df_psy_tx52<- read.csv("debug_outputs/pop_psy_3_2021.txt", sep="")

# Combine the three models for each treatment group
df_allmodels_tx0<-cbind(df_mean_tx0,df_cond_tx0[,c("BMI_12_c", "BMI_24_c")],df_psy_tx0[,c("BMI_12_p", "BMI_24_p")])
df_allmodels_tx12<-cbind(df_mean_tx12,df_cond_tx12[,c("BMI_12_c", "BMI_24_c")],df_psy_tx12[,c("BMI_12_p", "BMI_24_p")])
df_allmodels_tx52<-cbind(df_mean_tx52,df_cond_tx52[,c("BMI_12_c", "BMI_24_c")],df_psy_tx52[,c("BMI_12_p", "BMI_24_p")])

# Create histograms with BMI distributions
p_histY1_tx0 <- f_BMI_hist(df_actual = df_rawSim_tx0, df_model = df_allmodels_tx0, year = 1, treatment_group = "Brief intervention" )
p_histY2_tx0 <- f_BMI_hist(df_actual = df_rawSim_tx0, df_model = df_allmodels_tx0, year = 2, treatment_group = "Brief intervention" )

p_histY1_tx12 <- f_BMI_hist(df_actual = df_rawSim_tx12, df_model = df_allmodels_tx12, year = 1, 
                            treatment_group = "12-week intervention" )+labs(title="Year 1")+
                            theme(plot.title = element_text(hjust = 0.5))+
                            theme(axis.title.y.left = element_blank())
p_histY2_tx12 <- f_BMI_hist(df_actual = df_rawSim_tx12, df_model = df_allmodels_tx12, year = 2, 
                            treatment_group = "12-week intervention" )+ ylab("12-week intervention")+
                            scale_y_continuous(position = 'right', sec.axis = dup_axis()) + 
                            theme(plot.title = element_text(hjust=0.5), axis.text.y.right = element_blank(),axis.ticks.y.right = element_blank(),
                            axis.title.y.left = element_blank())+labs(title="Year 2")+theme(plot.title = element_text(hjust = 0.5))

p_histY1_tx52 <- f_BMI_hist(df_actual = df_rawSim_tx52, df_model = df_allmodels_tx52, year = 1,
                            treatment_group = "52-week intervention" )+theme(axis.title.y.left = element_blank())
p_histY2_tx52 <- f_BMI_hist(df_actual = df_rawSim_tx52, df_model = df_allmodels_tx52, year = 2,
                            treatment_group = "52-week intervention" )+ ylab("52-week intervention")+
                            scale_y_continuous(position = 'right', sec.axis = dup_axis()) + theme(plot.title = element_text(hjust=0.5),
                            axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank(),axis.title.y.left = element_blank())


# Arrange graphs into one figure, annotate and save
bmi_spec_dist <- ggarrange(p_histY1_tx12, p_histY2_tx12, p_histY1_tx52, p_histY2_tx52,
                      ncol =2, nrow = 2, heights = 5, widths = 5, common.legend = T, legend = "right")

# bmi_spec_dist <- annotate_figure(bmi_spec_dist,
                          #left = text_grob("Density", rot=90),
                          #right = text_grob("12-week intervention                             52-week intervention", rot=270),
                          #top = text_grob("Year 1                          Year 2")
                          #)
# Save the plot
ggsave("p_bmi_dist.png", height = 8, width = 8) 

######  Chi-square to comapre distributions  ########

# Reformat datasets for analysis

# subset original data set (simulation) to columns needed (BMI columns)
df_rawSim_tx0 <- df_rawSim_tx0[,c(2:4)]
df_rawSim_tx12 <- df_rawSim_tx12[,c(2:4)]
df_rawSim_tx52 <- df_rawSim_tx52[,c(2:4)]

# subset original data set (raw data) to columns needed (BMI columns)
df_rawData_tx0<-df_rawData_tx0[,c("bmi", "BMI_c_12", "BMI_c_24")]
df_rawData_tx12<-df_rawData_tx12[,c("bmi", "BMI_c_12", "BMI_c_24")]
df_rawData_tx52<-df_rawData_tx52[,c("bmi", "BMI_c_12", "BMI_c_24")]

# Add variable (column) for model specification
df_mean_tx0$SPEC<-df_mean_tx12$SPEC<-df_mean_tx52$SPEC<-"mean"
df_cond_tx0$SPEC<-df_cond_tx12$SPEC<-df_cond_tx52$SPEC<-"cond"
df_psy_tx0$SPEC<-df_psy_tx12$SPEC<-df_psy_tx52$SPEC<-"psy"
df_rawSim_tx0$SPEC<-df_rawSim_tx12$SPEC<-df_rawSim_tx52$SPEC<-"actual"
df_rawData_tx0$SPEC<-df_rawData_tx12$SPEC<-df_rawData_tx52$SPEC<-"actual"

# list of databases
l_spec <- list(df_mean_tx0, df_cond_tx0, df_psy_tx0, df_mean_tx12, df_cond_tx12, df_psy_tx12,
               df_mean_tx52, df_cond_tx52, df_psy_tx52,df_rawSim_tx0, df_rawSim_tx12, df_rawSim_tx52,
               df_rawData_tx0, df_rawData_tx12, df_rawData_tx52)

# Rename the columns
for(i in 1:length(l_spec)){
  colnames(l_spec[[i]])<-c("BMI_0", "BMI_12", "BMI_24", "SPEC")
  }
  
# Combine columns to form dataset in long format
df_all_tx0_long<-rbind(l_spec[[13]], l_spec[[1]], l_spec[[2]], l_spec[[3]])
df_all_tx12_long<-rbind(l_spec[[14]], l_spec[[4]], l_spec[[5]], l_spec[[6]])
df_all_tx52_long<-rbind(l_spec[[15]], l_spec[[7]], l_spec[[8]], l_spec[[9]])

# Set model specification to factor
df_all_tx0_long$SPEC<-as.factor(df_all_tx0_long$SPEC)
df_all_tx12_long$SPEC<-as.factor(df_all_tx12_long$SPEC)
df_all_tx52_long$SPEC<-as.factor(df_all_tx52_long$SPEC)


# Chi squared function using custom function
chi_s_Y1_tx0 <- f_chi_sq(df_column = df_all_tx0_long$BMI_12, df_all_tx0_long)
chi_s_Y2_tx0 <- f_chi_sq(df_column = df_all_tx0_long$BMI_24, df_all_tx0_long)

chi_s_Y1_tx12 <- f_chi_sq(df_column = df_all_tx12_long$BMI_12, df_all_tx12_long)
chi_s_Y2_tx12 <- f_chi_sq(df_column = df_all_tx12_long$BMI_24, df_all_tx12_long)

chi_s_Y1_tx52 <- f_chi_sq(df_column = df_all_tx52_long$BMI_12, df_all_tx52_long)
chi_s_Y2_tx52 <- f_chi_sq(df_column = df_all_tx52_long$BMI_24, df_all_tx52_long)
