############################
#       Functions          #
############################

# Density plot for BMI distribution
f_BMI_hist <- function(df_actual, df_model, year, treatment_group){
  
  if(year==1){ 
    
    p_hist <- ggplot()+
    
    # histogram of actual data
    geom_histogram(data = df_actual, aes(x = BMI_c_12, y= stat(density),  fill = "Study sample"),  alpha = 0.7,binwidth = 1) + 
    
    # Add density plot for BMI predicted by each model version
    geom_density(data = df_model, aes(x = BMI_12_m, y= stat(density),  color = "Mean"), alpha = 0.7, adjust = 1/3) +
    geom_density(data = df_model, aes(x = BMI_12_c, y= stat(density),   color = "Demographic"), alpha = 0.7, adjust = 1/3) +
    geom_density(data = df_model, aes(x = BMI_12_p, y= stat(density),  color = "Demographic plus MoA"), alpha = 0.7, adjust = 1/3) +
    
    # Set scale to percentage
    scale_y_continuous(labels = scales::percent) +
    
    # Axis labels
    xlab("BMI") +
    
    # Colour and theme
    scale_color_manual("Specification", values=c("Mean" = "black", "Demographic" = "cadetblue4", "Demographic plus MoA" = "coral"))+
    scale_fill_manual(" ", values=c("Study sample" = "grey60"))
#    theme(axis.title.y=element_blank()) 
      }
  
  if(year==2){  
    
    p_hist <- ggplot()+
    
    # histogram of actual data
    geom_histogram(data = df_actual, aes(x = BMI_c_24, y= stat(density),  fill = "Study sample"),  alpha = 0.7,binwidth = 1) + 
    
    # Add density plot for BMI predicted by each model version
    geom_density(data = df_model, aes(x = BMI_24_m, y= stat(density),  color = "Mean"), alpha = 0.7, adjust = 1/3) +
    geom_density(data = df_model, aes(x = BMI_24_c, y= stat(density),   color = "Demographic adjusted"), alpha = 0.7, adjust = 1/3) +
    geom_density(data = df_model, aes(x = BMI_24_p, y= stat(density),  color = "Psychological MoA"), alpha = 0.7, adjust = 1/3) +
    
    # Set scale to percentage
    scale_y_continuous(labels = scales::percent) +
    
    # Axis labels
    xlab("BMI") +
    
    # Colour and theme
    scale_color_manual("Specification", values=c("Mean" = "black", "Demographic adjusted" = "cadetblue4", "Psychological MoA" = "coral"))+
    scale_fill_manual(" ", values=c("Study sample" = "grey60"))
#    theme(axis.title.y=element_blank())
      }
  
  # print graph
  return(p_hist)
}

# Conduct chi square test on BMI distrubtion
f_chi_sq <- function(df_column, df){
  
  # set quartiles
  q<-quantile(df_column , seq(0, 1, .1), na.rm = T)
  
  # divide data into quartiles
  df$outcome_bin_12 <- cut(df_column, breaks=q, include.lowest=T)
  
  # create table of proportions of people with BMI in each catergory in each model specifiation
  tab <- table(df$outcome_bin_12, df$SPEC)
  tab <- (prop.table(x = tab, margin = 2))*100
  
  # chi square calculations
  mean_v_actual <- chisq.test(as.vector(tab[,"mean"]),p=as.vector(prop.table(tab[,"actual"])))
  cond_v_actual <- chisq.test(as.vector(tab[,"cond"]),p=as.vector(prop.table(tab[,"actual"])))
  psy_v_actual <- chisq.test(as.vector(tab[,"psy"]),p=as.vector(prop.table(tab[,"actual"])))
  
  output <- list(mean_v_actual,cond_v_actual,psy_v_actual)
  return(output)
}

#### grpahs comparing high and low of subgroups #####
plot_subgroup_ce<-function(high_q=df_subgp[,"HH_12_Q"], low_q = df_subgp[,"LH_12_Q"], 
                           high_c = df_subgp[,"HH_12_C"], low_c = df_subgp[,"LH_12_C"]){
  
  plot = ggplot() +
    # plot iteration results and mean estimate
    geom_point(aes(x=high_q, y=high_c,col="High"),size=1, show.legend = T, alpha = 0.6) +
    geom_point(aes(x=low_q, y=low_c,col="Low"),size=1, show.legend = T, alpha = 0.6) +
    geom_point(aes(x=mean(high_q, na.rm = T),y=mean(high_c, na.rm=T), shape="Mean high"), size=2,col="black", fill="black") +
    geom_point(aes(x=mean(low_q, na.rm = T),y=mean(low_c, na.rm=T), shape="Mean low"),size=2,col="black", fill="black") +
    # coordinate systems
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0)    +
    geom_line(aes(x=c(-0.05,0.05),y=c(-1000,1000), lty = "WTP Threshold £20,000")) +
    # labels
    xlab("Incremental QALY") +
    ylab("Incremental Costs") +
    labs(title = "Habit")+
    scale_color_manual(labels = c("High", "Low"), values = c("cadetblue4", "coral"))+
    scale_shape_manual(labels = c("Mean high", "Mean low"), values = c(23, 25))+
    scale_linetype()+
    theme_minimal() +
    theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank())+
    guides(colour = guide_legend(override.aes = list(size=3)))
  
  return(plot)
}


