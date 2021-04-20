##########################################
# Pre-trial modelling: Scenario results  #
##########################################

library(tidyr)

# Plotting the graphs for the intervention scenarios

# Read in csv file
df_scen_traj <- read.csv("data/scenario_trajectory.csv",  row.names = NULL)

# Rearrange into long format
df_scen_traj <- gather(df_scen_traj, effect_size, score, Control:Large, factor_key=TRUE)

# Add mechanism of action
df_scen_traj$MoA <- factor(df_scen_traj$MoA,levels=c("Habit strength","Dietary restraint",
                                                     "Autonomous diet self-regulation "))

# Plot graph
p_traj_scen <- ggplot(df_scen_traj, aes(x=Year, y=score, group=effect_size, color=MoA))+
  
  # Axis titles
  labs(x = "Time after baseline (months)", y = "Score") + 
  geom_line(aes(linetype=effect_size), size = 1) + 
  
  # set the theme for graph
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), legend.title=element_blank(), legend.key = element_rect(fill = NA), 
        legend.position="bottom", text = element_text(size=25),legend.key.width = unit(1.5, 'cm'))+
  
  # set colour of lines
  scale_color_manual(values=c("#FF9900", "#339900", "#0099FF")) +
  
  # stack graphs of different MoAs vertically
  facet_wrap(~MoA, dir="v", scales = "free") +

  guides(color=FALSE)

ggsave("p_traj_scen.png")

# Tables and plots of scenarios

load("data/a_scen.RData")
load("data/m_control.RData")
load("data/a_Oscen.RData")
load("data/m_Ocontrol.RData")

# create vector of scenario names
v_scen_names <- c("control", "small habit strength", "medium habit strength", "large habit strength",
                  "small dietary restraint", "medium dietary restraint", "large dietary restraint",
                  "small autonomous motivation","medium autonomous motivation","large autonomous motivation")

# create matrix with rows for scenarios and columns for outcomes
m_scen_results<-matrix(nrow = 10,ncol = 10, dimnames = list(v_scen_names,
                      c("BMI year 1", "BMI year 2", "BMI 2 year change", "Costs", "QALYs", "Incremental_costs", 
                        "Incremental QALYs", "JC_0", "JC_20","JC_30" )))

# populate each row other than first row with results from scenario array 
for(i in 1:(length(v_scen_names))-1){
  
  m_scen_results[i+1,]<- c(mean(a_scen[,4,i]), mean(a_scen[,5,i]), mean(a_scen[,4,i])-34.54, mean(a_scen[,2,i])/5000,
                         mean(a_scen[,3,i])/5000, mean(a_scen[,6,i]), mean(a_scen[,7,i]),(0*(mean(a_scen[,7,i]))-mean(a_scen[,6,i])),
                         (20000*(mean(a_scen[,7,i]))-mean(a_scen[,6,i])), (30000*(mean(a_scen[,7,i]))-mean(a_scen[,6,i])))
                         
                    }

# populate the first row of matrix with control results
m_scen_results["control",]<- c(mean(m_control[,4]), mean(m_control[,5]), mean(m_control[,4])-34.54, mean(m_control[,2])/5000,
                               mean(m_control[,3])/5000, NA,NA,NA,NA,NA)

# round values in matrix to 2 decimal spaces
m_scen_results<-round(m_scen_results,4)

write.csv(x = m_scen_results, file = "m_scen_results.csv")

########### create graph of all scenarios ###########

# bar graph of cost saving

# covert to data frame and add scenrio names as a value (removing the control row)
df_scen_results <- as.data.frame(m_scen_results[-1,])
df_scen_results$Scenarios <- as.factor(v_scen_names[-1])

# set order of scenarios 
df_scen_results$Scenarios <- factor(df_scen_results$Scenarios, df_scen_results$Scenarios)

# create plot of bar graph
p_costsav <- ggplot(df_scen_results, aes(x=Scenarios, y=JC_0, fill=Scenarios))+
  geom_bar(stat="identity", color="black")+
  scale_fill_brewer(palette="Paired")+
  ylab("Cost saving per person compared to a control intervention")+
  theme_minimal()+
  geom_hline(yintercept = 0) +
  annotate("text", x = 2, y = 1750, label = "Habit strength", size = 15)+
  annotate("text", x = 5, y = 1750, label = "Dietary restraint", size = 15)+
  annotate("text", x = 8, y = 1750, label = "Autonomous motivation", size = 15)+
  geom_text(aes(label = c("small", "medium", "large", "small", "medium",
              "large", "small", "medium", "large")), position = position_stack(vjust = 0.75), 
              angle=270, hjust = 0, size = 20) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.line.y = element_line(color="black", size = 0.5), legend.position = "none",
        text = element_text(size=35))

# save bar plot
ggsave("p_costsav.png", height = 15, width = 25)

# create cost effectivness plane
CE_scenarios <- ggplot() +
  # plot PSA results for each scenarios 
  geom_point(aes(x=m_hab_sm[,"inc_q"], y=m_hab_sm[,"inc_c"],col="1.Habit small"),size=2, alpha = 0.5, show.legend = T) +
  geom_point(aes(x=m_hab_md[,"inc_q"], y=m_hab_md[,"inc_c"],col="2.Habit medium"),size=2, alpha = 0.5, show.legend = T) +
  geom_point(aes(x=m_hab_lg[,"inc_q"], y=m_hab_lg[,"inc_c"],col="3.Habit large"),size=2, alpha = 0.5, show.legend = T) +
  geom_point(aes(x=m_res_sm[,"inc_q"], y=m_res_sm[,"inc_c"],col="4.Restraint small"),size=2, alpha = 0.5, show.legend = T) +
  geom_point(aes(x=m_res_md[,"inc_q"], y=m_res_md[,"inc_c"],col="5.Restrant medium"),size=2, alpha = 0.5, show.legend = T) +
  geom_point(aes(x=m_res_lg[,"inc_q"], y=m_res_lg[,"inc_c"],col="6.Restraint large"),size=2, alpha = 0.5, show.legend = T) +
  geom_point(aes(x=m_adsr_sm[,"inc_q"], y=m_adsr_sm[,"inc_c"],col="7.ADSR small"),size=2, alpha = 0.5, show.legend = T) +
  geom_point(aes(x=m_adsr_md[,"inc_q"], y=m_adsr_md[,"inc_c"],col="8.ADSR medium"),size=2, alpha = 0.5, show.legend = T) +
  geom_point(aes(x=m_adsr_lg[,"inc_q"], y=m_adsr_lg[,"inc_c"],col="9.ADSR large"),size=2, alpha = 0.5, show.legend = T) +
  
  # set colour palette for points
  scale_color_brewer(palette="Set3") +
  
  # plot mean cost and QALY for each scenario 
  geom_point(aes(x=mean(m_hab_sm[,"inc_q"]),y=mean(m_hab_sm[,"inc_c"])), shape = 49, size = 6) +
  geom_point(aes(x=mean(m_hab_md[,"inc_q"]),y=mean(m_hab_md[,"inc_c"])), shape = 50, size = 6) +
  geom_point(aes(x=mean(m_hab_lg[,"inc_q"]),y=mean(m_hab_lg[,"inc_c"])), shape = 51, size = 6) +
  geom_point(aes(x=mean(m_res_sm[,"inc_q"]),y=mean(m_res_sm[,"inc_c"])), shape = 52, size = 6) +
  geom_point(aes(x=mean(m_res_md[,"inc_q"]),y=mean(m_res_md[,"inc_c"])), shape = 53, size = 6) +
  geom_point(aes(x=mean(m_res_lg[,"inc_q"]),y=mean(m_res_lg[,"inc_c"])), shape = 54, size = 6) +
  geom_point(aes(x=mean(m_adsr_sm[,"inc_q"]),y=mean(m_adsr_sm[,"inc_c"])), shape = 55, size = 6) +
  geom_point(aes(x=mean(m_adsr_md[,"inc_q"]),y=mean(m_adsr_md[,"inc_c"])), shape = 56, size = 6) +
  geom_point(aes(x=mean(m_adsr_lg[,"inc_q"]),y=mean(m_adsr_lg[,"inc_c"])), shape = 57, size = 6) +

  # add lines for 0 costs and QALYs
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  
  # add line for willingness to pay threshold
  geom_line(aes(x=c(-0.1,0.35),y=c(-150,525), lty = "WTP Threshold £20,000")) +
  
  # set limits for x value
  xlim(-0.1,0.35) +
  
  # labels
  xlab("Incremental QALY") +
  ylab("Incremental Costs") +
  labs(title = "Cost-effectiveness of all scenarios")+
  
  # setting theme and legend setting
  theme_minimal() +
  theme(legend.position = "right", legend.title=element_blank(),
        text = element_text(size=20),legend.key.size = unit(1, 'cm'))

# print plot of all scenarios
CE_scenarios

# save plot
ggsave("CE_scen_all.png", height = 15, width = 20)  

############# plot habit-based scenarios ###########

p_CEscen_hab <- ggplot() +
  # plot PSA results for each scenarios 
  geom_point(aes(x=m_hab_sm[,"inc_q"], y=m_hab_sm[,"inc_c"],col="1.Habit strength small"),size=4, alpha = 0.5, show.legend = T) +
  geom_point(aes(x=m_hab_md[,"inc_q"], y=m_hab_md[,"inc_c"],col="2.Habit strength medium"),size=4, alpha = 0.5,show.legend = T) +
  geom_point(aes(x=m_hab_lg[,"inc_q"], y=m_hab_lg[,"inc_c"],col="3.Habit strength large"),size=4, alpha = 0.5,show.legend = T) +
  
  # set colour palette for points
  scale_color_brewer(palette="YlOrRd")+
  
  # plot mean cost and QALY for each scenario 
  geom_point(aes(x=mean(m_hab_sm[,"inc_q"]),y=mean(m_hab_sm[,"inc_c"]), shape = "1.Mean habit strength small"), size = 6) +
  geom_point(aes(x=mean(m_hab_md[,"inc_q"]),y=mean(m_hab_md[,"inc_c"]),shape = "2.Mean habit strength medium"),size = 6) +
  geom_point(aes(x=mean(m_hab_lg[,"inc_q"]),y=mean(m_hab_lg[,"inc_c"]),shape = "3.Mean habit strength large"),size = 6) +
  
  # add lines for 0 costs and QALYs
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  
  # add line for willingness to pay threshold
  geom_line(aes(x=c(-0.1,0.025),y=c(-2000,500), lty = "WTP Threshold £20,000")) +
  
  # set limits for axis
  xlim(-0.1,0.35) +
  ylim(-5000,500) +
  
  # labels
  labs(title = "Cost-effectiveness of intervention scenarios targetting habit strength")+

  # setting theme and legend 
  theme_minimal() +
  theme(legend.position = "right", axis.title = element_blank(), legend.title=element_blank(),
        text = element_text(size=25),legend.key.size = unit(1.5, 'cm'))

# print plot for habit scenarios
p_CEscen_hab

############# plot restraint-based scenarios ###############
p_CEscen_res <- ggplot() +
  
  # plot PSA results for each scenarios 
  geom_point(aes(x=m_res_sm[,"inc_q"], y=m_res_sm[,"inc_c"],col="4.Dietary restraint small"),size=4, alpha = 0.5, show.legend = T) +
  geom_point(aes(x=m_res_md[,"inc_q"], y=m_res_md[,"inc_c"],col="5.Dietary restrant medium"),size=4, alpha = 0.5, show.legend = T) +
  geom_point(aes(x=m_res_lg[,"inc_q"], y=m_res_lg[,"inc_c"],col="6.Dietary restraint large"),size=4, alpha = 0.5, show.legend = T) +
  
  # set colour palette for points
  scale_color_brewer(palette="Greens")+
  
  # plot mean cost and QALY for each scenario 
  geom_point(aes(x=mean(m_res_sm[,"inc_q"]),y=mean(m_res_sm[,"inc_c"]), shape = "4.Mean dietary restraint small"), size = 6) +
  geom_point(aes(x=mean(m_res_md[,"inc_q"]),y=mean(m_res_md[,"inc_c"]), shape = "5.Mean dietary restrant medium"), size = 6) +
  geom_point(aes(x=mean(m_res_lg[,"inc_q"]),y=mean(m_res_lg[,"inc_c"]), shape = "6.Mean dietary restraint large"), size = 6) +

  # add lines for 0 costs and QALYs
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  
  # add line for willingness to pay threshold
  geom_line(aes(x=c(-0.1,0.025),y=c(-2000,500), lty = "WTP Threshold £20,000")) +
  
  # add limits for axis
  xlim(-0.1,0.35) +
  ylim(-5000,500) +
  
  # lables
  labs(title = "Cost-effectiveness of intervention scenarios targetting dietary restaint")+
  
  # setting theme and legend setting
  theme_minimal() +
  theme(legend.position = "right", axis.title = element_blank(),legend.title=element_blank(),
        text = element_text(size=25),legend.key.size = unit(1.5, 'cm'))+
  guides(col = guide_legend(order = 1))

# print plot for restraint scenarios 
p_CEscen_res

############### plot autonomous motivated scenarios #################

p_CEscen_adsr <- ggplot() +
  
  # plot PSA results for each scenarios 
  geom_point(aes(x=m_adsr_sm[,"inc_q"], y=m_adsr_sm[,"inc_c"],col="7.Autonomous motivation small"),size=4, alpha = 0.5, show.legend = T) +
  geom_point(aes(x=m_adsr_md[,"inc_q"], y=m_adsr_md[,"inc_c"],col="8.Autonomous motivation medium"),size=4, alpha = 0.5, show.legend = T) +
  geom_point(aes(x=m_adsr_lg[,"inc_q"], y=m_adsr_lg[,"inc_c"],col="9.Autonomous motivation large"),size=4, alpha = 0.5, show.legend = T) +
  
  # set colour palette for points
  scale_color_brewer(palette="Blues")+
  
  # plot mean cost and QALY for each scenario 
  geom_point(aes(x=mean(m_adsr_sm[,"inc_q"]),y=mean(m_adsr_sm[,"inc_c"]), shape = "7.Mean autonomous motivation small"), size = 6) +
  geom_point(aes(x=mean(m_adsr_md[,"inc_q"]),y=mean(m_adsr_md[,"inc_c"]), shape = "8.Mean autonomous motivation medium"), size = 6) +
  geom_point(aes(x=mean(m_adsr_lg[,"inc_q"]),y=mean(m_adsr_lg[,"inc_c"]), shape = "9.Mean autonomous motivation large"), size = 6) +
  
  # add lines for 0 costs and QALYs
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  
  # add line for willingness to pay threshold
  geom_line(aes(x=c(-0.1,0.025),y=c(-2000,500), lty = "WTP Threshold £20,000")) +
  
  # add limits for axis
  xlim(-0.1,0.35) +
  ylim(-5000,500) +
  
  # labels
  labs(title = "Cost-effectiveness of intervention scenarios targetting autonomous motivation")+

  # setting theme and legend setting
  theme_minimal() +
  theme(legend.position = "right", axis.title = element_blank(),legend.title=element_blank(),
        text = element_text(size=25),legend.key.size = unit(1.5, 'cm'))

# print plot autonomous motivated scenarios
p_CEscen_adsr

# group the three plots into one figure
scen_plane <- plot_grid(
  p_CEscen_hab, p_CEscen_res,p_CEscen_adsr,
  labels = "AUTO", ncol = 1, align = "vh"
)

# annotate figure
scen_plane_fig <- annotate_figure(scen_plane,
                                        bottom = text_grob("Incremental QALYs",size = 20),
                                        left = text_grob("Incremental Costs", rot=90,size = 20))

# save figure of three plots
ggsave("p_scen_moa.png",height = 30,width = 20)


