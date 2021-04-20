#################################################
# Adding MoA to SPHR model: SA graph BMI graphs #
#################################################

# Plotting the graphs for the intervention scenarios

# Read in csv file
df_SA_traj <- read.csv("data/Scenarios.csv",  row.names = NULL)
df_SA_traj_1<-df_SA_traj[,c(1,2,4,6)]
colnames(df_SA_traj_1)<-c("Month", "12-week intervention", "52-week intervention", "Brief intervention")

# Rearrange into long format
df_SA_traj_1 <- gather(df_SA_traj_1, Trajectory, BMI, c(2:4), factor_key=TRUE)

df_SA_traj_1$Trajectory<-factor(df_SA_traj_1$Trajectory, c("Brief intervention", "12-week intervention", "52-week intervention"))

# Plot graph
p_SA_traj_1 <- ggplot(df_SA_traj_1, aes(x=Month, y=BMI, group=Trajectory, color=Trajectory))+
  
  # Axis titles
  labs(x = "Time after baseline (months)", y = "BMI") + 
  ggtitle("Sensitivity Analysis 1") +
  geom_line(aes(linetype=Trajectory), size = 1) + 
  
  # set the theme for graph
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), legend.title=element_blank(), 
        legend.position="bottom", text = element_text(size=15),legend.key.width = unit(1.5, 'cm'))+
  
  # set colour of lines
  scale_color_manual(values=c("Brief intervention" = "grey80", "12-week intervention" = "cadetblue4", "52-week intervention" = "coral")) +

ggsave("p_SA_traj_1.png")

###########

# Read in csv file
df_SA_traj <- read.csv("data/Scenarios.csv",  row.names = NULL)
df_SA_traj_2<-df_SA_traj[,c(1,3,5,7)]
colnames(df_SA_traj_2)<-c("Month", "12-week intervention", "52-week intervention", "Brief intervention")

# Rearrange into long format
df_SA_traj_2 <- gather(df_SA_traj_2, Trajectory, BMI, c(2:4), factor_key=TRUE)

df_SA_traj_2$Trajectory<-factor(df_SA_traj_2$Trajectory, c("Brief intervention", "12-week intervention", "52-week intervention"))

# Plot graph
p_SA_traj_2 <- ggplot(df_SA_traj_2, aes(x=Month, y=BMI, group=Trajectory, color=Trajectory))+
  
  # Axis titles
  labs(x = "Time after baseline (months)", y = "BMI") + 
  geom_line(aes(linetype=Trajectory), size = 1) +
  ggtitle("Sensitivity Analysis 2") +
  
  # set the theme for graph
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), legend.title=element_blank(), 
        legend.position="bottom", text = element_text(size=15),legend.key.width = unit(1.5, 'cm'))+
  
  # set colour of lines
  scale_color_manual(values=c("Brief intervention" = "grey80", "12-week intervention" = "cadetblue4", "52-week intervention" = "coral")) +
  
  ggsave("p_SA_traj_2.png")

###

bmi_SA <- ggarrange(p_SA_traj_1, p_SA_traj_2,
                           ncol =1, nrow = 2, heights = 5, widths = 5, common.legend = T, legend = "bottom")

ggsave("bmi_SA.png", height = 12, width = 12) 

