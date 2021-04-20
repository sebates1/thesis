########################################################
# Adding MoA to SPHR model: cost-effectivness outcomes #
########################################################

library(Rmisc)
library(ggplot2)
library(tibble)

# Load custom functions
source("R/Functions.R")

# Load PSA results
load("data/PSAResults_m.RData")
load("data/PSAResults_c.RData")
load("data/PSAResults_p.RData")

##Costs and QALYs per person
m_CE_outcomes<-matrix(nrow = 3,ncol = 18, dimnames = list(c( "Direct", "Conditional", "Psychological"),
                                                        c("C_BI", "CLCI_BI", "CHCI_BI", "Q_BI", "QLCI_BI", "QHCI_BI",
                                                          "C_12", "CLCI_12", "CHCI_12", "Q_12", "QLCI_12", "QHCI_12",
                                                          "C_52", "CLCI_52", "CHCI_52", "Q_52", "QLCI_52", "QHCI_52"
                                                          )))


m_CE_outcomes["Direct",]<-c(mean(PSAResults_m$C_BI),sort(PSAResults_m$C_BI,T)[975], sort(PSAResults_m$C_BI,T)[25],
                               mean(PSAResults_m$Q_BI),sort(PSAResults_m$Q_BI,T)[975], sort(PSAResults_m$Q_BI,T)[25],
                               mean(PSAResults_m$C_12),sort(PSAResults_m$C_12,T)[975], sort(PSAResults_m$C_12,T)[25],
                               mean(PSAResults_m$Q_12),sort(PSAResults_m$Q_12,T)[975], sort(PSAResults_m$Q_12,T)[25],
                               mean(PSAResults_m$C_52),sort(PSAResults_m$C_52,T)[975], sort(PSAResults_m$C_52,T)[25],
                               mean(PSAResults_m$Q_52),sort(PSAResults_m$Q_52,T)[975], sort(PSAResults_m$Q_52,T)[25]
                               )
m_CE_outcomes["Conditional", ]<-c(mean(PSAResults_c$C_BI),sort(PSAResults_c$C_BI,T)[975], sort(PSAResults_c$C_BI,T)[25],
                               mean(PSAResults_c$Q_BI),sort(PSAResults_c$Q_BI,T)[975], sort(PSAResults_c$Q_BI,T)[25],
                               mean(PSAResults_c$C_12),sort(PSAResults_c$C_12,T)[975], sort(PSAResults_c$C_12,T)[25],
                               mean(PSAResults_c$Q_12),sort(PSAResults_c$Q_12,T)[975], sort(PSAResults_c$Q_12,T)[25],
                               mean(PSAResults_c$C_52),sort(PSAResults_c$C_52,T)[975], sort(PSAResults_c$C_52,T)[25],
                               mean(PSAResults_c$Q_52),sort(PSAResults_c$Q_52,T)[975], sort(PSAResults_c$Q_52,T)[25]
)

m_CE_outcomes["Psychological", ]<-c(mean(PSAResults_p$C_BI),sort(PSAResults_p$C_BI,T)[975], sort(PSAResults_p$C_BI,T)[25],
                                      mean(PSAResults_p$Q_BI),sort(PSAResults_p$Q_BI,T)[975], sort(PSAResults_p$Q_BI,T)[25],
                                      mean(PSAResults_p$C_12),sort(PSAResults_p$C_12,T)[975], sort(PSAResults_p$C_12,T)[25],
                                      mean(PSAResults_p$Q_12),sort(PSAResults_p$Q_12,T)[975], sort(PSAResults_p$Q_12,T)[25],
                                      mean(PSAResults_p$C_52),sort(PSAResults_p$C_52,T)[975], sort(PSAResults_p$C_52,T)[25],
                                      mean(PSAResults_p$Q_52),sort(PSAResults_p$Q_52,T)[975], sort(PSAResults_p$Q_52,T)[25]
)

m_CE_outcomes<-m_CE_outcomes/5000

# Percentage difference between the Condition, psychological and direct (mean)
pd_cond12 <- ((PSAResults_c$C_12-PSAResults_m$C_12)/PSAResults_m$C_12)*100
CI(pd_cond12)
pd_psy12 <- ((PSAResults_p$C_12-PSAResults_m$C_12)/PSAResults_m$C_12)*100
CI(pd_cond12)

pd_cond12q <- ((PSAResults_c$Q_12-PSAResults_m$Q_12)/PSAResults_m$Q_12)*100
CI(pd_cond12q)
pd_psy12q <- (((PSAResults_p$Q_12+(0.8*5000))-PSAResults_m$Q_12)/PSAResults_m$Q_12)*100
CI(pd_psy12q)

pd_cond52 <- ((PSAResults_c$C_52-PSAResults_m$C_52)/PSAResults_m$C_52)*100
CI(pd_cond52)
pd_psy52 <- ((PSAResults_p$C_52-PSAResults_m$C_52)/PSAResults_m$C_52)*100
CI(pd_psy52)

pd_cond52q <- ((PSAResults_c$Q_52-PSAResults_m$Q_52)/PSAResults_m$Q_52)*100
CI(pd_cond52q)
pd_psy52q <- (((PSAResults_p$Q_52+(0.8*5000))-PSAResults_m$Q_52)/PSAResults_m$Q_52)*100
CI(pd_psy52q)

# Calcalating number of PSA runs that are cost-effective for each model specification

# Mean model - 12 week vs BI
1000-(sum(PSAResults_m[,"C_12_BI"] > 0 & PSAResults_m[,"Q_12_BI"] > 0 & (PSAResults_m[,"ICER_12_BI"]*5000) > 20000) +
        sum(PSAResults_m[,"C_12_BI"] > 0 & PSAResults_m[,"Q_12_BI"] < 0)+
        sum(PSAResults_m[,"C_12_BI"] < 0 & PSAResults_m[,"Q_12_BI"] < 0))

# Demographic adjusted model - 12 week vs BI
1000-(sum(PSAResults_c[,"C_12_BI"] > 0 & PSAResults_c[,"Q_12_BI"] > 0 & (PSAResults_c[,"ICER_12_BI"]*5000) > 20000) +
        sum(PSAResults_c[,"C_12_BI"] > 0 & PSAResults_c[,"Q_12_BI"] < 0)+
        sum(PSAResults_c[,"C_12_BI"] < 0 & PSAResults_c[,"Q_12_BI"] < 0))      

# Psychological model - 12 week vs BI
1000-(sum(PSAResults_p[,"C_12_BI"] > 0 & PSAResults_p[,"Q_12_BI"] > 0 & (PSAResults_p[,"ICER_12_BI"]*5000) > 20000) +
        sum(PSAResults_p[,"C_12_BI"] > 0 & PSAResults_p[,"Q_12_BI"] < 0)+
        sum(PSAResults_p[,"C_12_BI"] < 0 & PSAResults_p[,"Q_12_BI"] < 0))

# Mean model - 52 week vs BI
1000-(sum(PSAResults_m[,"C_52_BI"] > 0 & PSAResults_m[,"Q_52_BI"] > 0 & (PSAResults_m[,"ICER_52_BI"]*5000) > 20000) +
        sum(PSAResults_m[,"C_52_BI"] > 0 & PSAResults_m[,"Q_52_BI"] < 0)+
        sum(PSAResults_m[,"C_12_BI"] < 0 & PSAResults_m[,"Q_52_BI"] < 0))

# Demographic adjusted model - 52 week vs BI
1000-(sum(PSAResults_c[,"C_52_BI"] > 0 & PSAResults_c[,"Q_52_BI"] > 0 & (PSAResults_c[,"ICER_52_BI"]*5000) > 20000) +
        sum(PSAResults_c[,"C_52_BI"] > 0 & PSAResults_c[,"Q_52_BI"] < 0)+
        sum(PSAResults_c[,"C_52_BI"] < 0 & PSAResults_c[,"Q_52_BI"] < 0))      

# Psychological model - 52 week vs BI
1000-(sum(PSAResults_p[,"C_52_BI"] > 0 & PSAResults_p[,"Q_52_BI"] > 0 & (PSAResults_p[,"ICER_52_BI"]*5000) > 20000) +
        sum(PSAResults_p[,"C_52_BI"] > 0 & PSAResults_p[,"Q_52_BI"] < 0)+
        sum(PSAResults_p[,"C_52_BI"] < 0 & PSAResults_p[,"Q_52_BI"] < 0))      

# create graphs
PSAResults_m<-as.matrix(PSAResults_m)/5000
PSAResults_c<-as.matrix(PSAResults_c)/5000
PSAResults_p<-as.matrix(PSAResults_p)/5000

plane_12_BI = ggplot() +
  # plot iteration results and mean estimate
  geom_point(aes(x=PSAResults_m[,"Q_12_BI"], y=PSAResults_m[,"C_12_BI"],col="Direct"),size=1, show.legend = T) +
  geom_point(aes(x=PSAResults_c[,"Q_12_BI"], y=PSAResults_c[,"C_12_BI"],col="Conditional"),size=1, show.legend = T) +
  geom_point(aes(x=PSAResults_p[,"Q_12_BI"], y=PSAResults_p[,"C_12_BI"],col="Psychological"),size=1, show.legend = T) +
  geom_point(aes(x=mean(PSAResults_m[,"Q_12_BI"], na.rm = T),y=mean(PSAResults_m[,"C_12_BI"], na.rm=T), shape="Mean direct"),size=2,col="black", fill="black") +
  geom_point(aes(x=mean(PSAResults_c[,"Q_12_BI"], na.rm = T),y=mean(PSAResults_c[,"C_12_BI"], na.rm=T), shape="Mean conditional"), size=2,col="black", fill="black") +
  geom_point(aes(x=mean(PSAResults_p[,"Q_12_BI"], na.rm = T),y=mean(PSAResults_p[,"C_12_BI"], na.rm=T), shape="Mean psychological"),size=2,col="black", fill="black") +
  # coordinate systems
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  geom_line(aes(x=c(-0.025,0.01),y=c(-500,200), lty = "WTP Threshold £20,000")) +
  # labels
  xlab("Incremental QALY") +
  ylab("Incremental Costs") +
  labs(title = "12-week programme vs. brief intervention")+
  scale_color_manual(labels = c( "Conditional", "Direct", "Psychological"), values = c("cadetblue4", "coral", "gray80"))+
  scale_shape_manual(labels = c("Mean conditional", "Mean direct", "Mean psychological"), values = c(23, 25, 21))+
  scale_linetype()+
  theme_minimal() +
  xlim(-0.025, 0.075)+
  ylim(-850, 200)+
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave("plane_12_BI.png")  

plane_52_BI = ggplot() +
  # plot iteration results and mean estimate
  geom_point(aes(x=PSAResults_m[,"Q_52_BI"], y=PSAResults_m[,"C_52_BI"],col="Direct"),size=1, show.legend = T) +
  geom_point(aes(x=PSAResults_c[,"Q_52_BI"], y=PSAResults_c[,"C_52_BI"],col="Conditional"),size=1, show.legend = T) +
  geom_point(aes(x=PSAResults_p[,"Q_52_BI"], y=PSAResults_p[,"C_52_BI"],col="Psychological"),size=1, show.legend = T) +
  geom_point(aes(x=mean(PSAResults_m[,"Q_52_BI"], na.rm = T),y=mean(PSAResults_m[,"C_52_BI"], na.rm=T), shape="Mean direct"),size=2,col="black", fill="black") +
  geom_point(aes(x=mean(PSAResults_c[,"Q_52_BI"], na.rm = T),y=mean(PSAResults_c[,"C_52_BI"], na.rm=T), shape="Mean conditional"), size=2,col="black", fill="black") +
  geom_point(aes(x=mean(PSAResults_p[,"Q_52_BI"], na.rm = T),y=mean(PSAResults_p[,"C_52_BI"], na.rm=T), shape="Mean psychological"),size=2,col="black", fill="black") +
  # coordinate systems
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  geom_line(aes(x=c(-0.025,0.01),y=c(-500,200), lty = "WTP Threshold £20,000")) +
  # labels
  xlab("Incremental QALY") +
  ylab("Incremental Costs") +
  labs(title = "52-week programme vs. brief intervention")+
  scale_color_manual(labels = c("Conditional","Direct",  "Psychological"), values = c("cadetblue4", "coral", "gray80"))+
  scale_shape_manual(labels = c("Mean conditional", "Mean direct", "Mean psychological"), values = c(23, 25, 21))+
  scale_linetype()+
  theme_minimal() +
  xlim(-0.025, 0.075)+
  ylim(-850, 200)+
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave("plane_52_BI.png")  

plane_52_12 = ggplot() +
  # plot iteration results and mean estimate
  geom_point(aes(x=PSAResults_m[,"Q_52_12"], y=PSAResults_m[,"C_52_12"],col="Direct"),size=1, show.legend = T) +
  geom_point(aes(x=PSAResults_c[,"Q_52_12"], y=PSAResults_c[,"C_52_12"],col="Conditional"),size=1, show.legend = T) +
  geom_point(aes(x=PSAResults_p[,"Q_52_12"], y=PSAResults_p[,"C_52_12"],col="Psychological"),size=1, show.legend = T) +
  geom_point(aes(x=mean(PSAResults_m[,"Q_52_12"], na.rm = T),y=mean(PSAResults_m[,"C_52_12"], na.rm=T), shape="Mean direct"),size=2,col="black", fill="black") +
  geom_point(aes(x=mean(PSAResults_c[,"Q_52_12"], na.rm = T),y=mean(PSAResults_c[,"C_52_12"], na.rm=T), shape="Mean conditional"), size=2,col="black", fill="black") +
  geom_point(aes(x=mean(PSAResults_p[,"Q_52_12"], na.rm = T),y=mean(PSAResults_p[,"C_52_12"], na.rm=T), shape="Mean psychological"),size=2,col="black", fill="black") +
  # coordinate systems
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  geom_line(aes(x=c(-0.025,0.01),y=c(-500,200), lty = "WTP Threshold £20,000")) +
  # labels
  xlab("Incremental QALY") +
  ylab("Incremental Costs") +
  labs(title = "52-week programme vs. 12-week programme")+
  scale_color_manual(labels = c("Conditional","Direct",  "Psychological"), values = c("cadetblue4", "coral", "gray80"))+
  scale_shape_manual(labels = c( "Mean conditional","Mean direct", "Mean psychological"), values = c(23, 25, 21))+
  scale_linetype()+
  theme_minimal() +
#  xlim(-0.025, 0.050)+
#  ylim(-600, 200)+
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave("plane_52_12.png")  

# Arrange plots together
full_plane_CE <- ggarrange(plane_12_BI, plane_52_BI,
                           labels = c("a", "b"),
                           ncol = 1, nrow = 2, heights = 5, widths = 5, common.legend = T, legend = "right")

# Add axis lables
full_plane_CE <- annotate_figure(full_plane_CE,
                                 bottom = text_grob("Incremental QALYs"),
                                 left = text_grob("Incremental Costs", rot=90))
ggsave("p_modelcomp_CE.png", width = 8, height = 12) 

# Import outcomes tables
outcomes00_c <- read.csv("outputs/outcomes00_c.txt", sep="", row.names = 1,nrows = 14)
outcomesAA_c <- read.csv("outputs/outcomesAA_c.txt", sep="", row.names = 1,nrows = 14)
outcomesBB_c <- read.csv("outputs/outcomesBB_c.txt", sep="", row.names = 1,nrows = 14)

outcomes00_p <- read.csv("outputs/outcomes00_p.txt", sep="", row.names = 1,nrows = 14)
outcomesAA_p <- read.csv("outputs/outcomesAA_p.txt", sep="", row.names = 1,nrows = 14)
outcomesBB_p <- read.csv("outputs/outcomesBB_p.txt", sep="", row.names = 1,nrows = 14)

outcomes00_m <- read.csv("outputs/outcomes00_m.txt", sep="", row.names = 1,nrows = 14)
outcomesAA_m <- read.csv("outputs/outcomesAA_m.txt", sep="", row.names = 1,nrows = 14)
outcomesBB_m <- read.csv("outputs/outcomesBB_m.txt", sep="", row.names = 1,nrows = 14)

# Set up table comapring 12week to control in both model specs
Costs_12_d<-tibble(.rows=8)
Costs_12_p<-tibble(.rows=8)
Costs_12_c<-tibble(.rows=8)

#Populate each table with condition name, model spec and incremental costs
Costs_12_c$cond <- c("Diabetes", "Cardiovascular", "Nephrology", "Retinopathy", "Neuropthy", "Cancer", "Osteoarthritis", "Depression")
Costs_12_c$spec <- "Conditional"
Costs_12_c$cost <- ((rowSums(outcomesAA_c[c(1:8),]))-(rowSums(outcomes00_c[c(1:8),])))/5000000

Costs_12_p$cond <- c("Diabetes", "Cardiovascular", "Nephrology", "Retinopathy", "Neuropthy", "Cancer", "Osteoarthritis", "Depression")
Costs_12_p$spec <- "Psychological"
Costs_12_p$cost <- ((rowSums(outcomesAA_p[c(1:8),]))-(rowSums(outcomes00_p[c(1:8),])))/5000000

Costs_12_d$cond <- c("Diabetes", "Cardiovascular", "Nephrology", "Retinopathy", "Neuropthy", "Cancer", "Osteoarthritis", "Depression")
Costs_12_d$spec <- "Direct"
Costs_12_d$cost <- ((rowSums(outcomesAA_m[c(1:8),]))-(rowSums(outcomes00_m[c(1:8),])))/5000000

# merge tables
costs_12<-rbind(Costs_12_d, Costs_12_c, Costs_12_p)

# Create bar chart of costs compared to control group for each model
p_costs_12 <- ggplot(costs_12, aes(fill=spec, y=cost, x=cond)) + 
  geom_bar(position="dodge", stat="identity", width=0.8, col="black", size=0.1)+
  labs(fill="Model Specification")+
  labs(title = "12-week programme vs. brief intervention")+
  scale_fill_manual(values=c("grey80", "cadetblue4", "coral"))+
  theme_minimal() +
  theme(legend.position = "bottom",text = element_text(size=10), axis.title = element_blank(), axis.text.x=element_text(angle=45, hjust=1))

#set up table comapring 52 week to control in both model specs
Costs_52_d<-tibble(.rows=8)
Costs_52_p<-tibble(.rows=8)
Costs_52_c<-tibble(.rows=8)

#Populate each table with condition name, model spec and incremental costs
Costs_52_c$cond <- c("Diabetes", "Cardiovascular", "Nephrology", "Retinopathy", "Neuropthy", "Cancer", "Osteoarthritis", "Depression")
Costs_52_c$spec <- "Conditional"
Costs_52_c$cost <- ((rowSums(outcomesBB_c[c(1:8),]))-(rowSums(outcomes00_c[c(1:8),])))/5000000

Costs_52_p$cond <- c("Diabetes", "Cardiovascular", "Nephrology", "Retinopathy", "Neuropthy", "Cancer", "Osteoarthritis", "Depression")
Costs_52_p$spec <- "Psychological"
Costs_52_p$cost <- ((rowSums(outcomesBB_p[c(1:8),]))-(rowSums(outcomes00_p[c(1:8),])))/5000000

Costs_52_d$cond <- c("Diabetes", "Cardiovascular", "Nephrology", "Retinopathy", "Neuropthy", "Cancer", "Osteoarthritis", "Depression")
Costs_52_d$spec <- "Direct"
Costs_52_d$cost <- ((rowSums(outcomesBB_m[c(1:8),]))-(rowSums(outcomes00_m[c(1:8),])))/5000000

# merge tables
costs_52<-rbind(Costs_52_d, Costs_52_c, Costs_52_p)

# Create bar chart of costs compared to control group for each model
p_costs_52 <- ggplot(costs_52, aes(fill=spec, y=cost, x=cond)) + 
  geom_bar(position="dodge", stat="identity", width=0.8, col="black", size=0.1)+
  labs(fill="Model Specification")+
  labs(title = "52-week programme vs. brief intervention")+
  scale_fill_manual(values=c("grey80","cadetblue4", "coral"))+
  theme_minimal() +
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank(), axis.text.x=element_text(angle=45, hjust=1))

#set up table comapring 52 week to 12 week in both model specs
Costs_52_12_c<-tibble(.rows=8)
Costs_52_12_p<-tibble(.rows=8)

#Populate each table with condition name, model spec and incremental costs
Costs_52_12_c$cond <- c("Diabetes", "Cardiovascular", "Nephrology", "Retinopathy", "Neuropthy", "Cancer", "Osteoarthritis", "Depression")
Costs_52_12_c$spec <- "Direct"
Costs_52_12_c$cost <- ((rowSums(outcomesBB_c[c(1:8),]))-(rowSums(outcomesAA_c[c(1:8),])))/5000

Costs_52_12_p$cond <- c("Diabetes", "Cardiovascular", "Nephrology", "Retinopathy", "Neuropthy", "Cancer", "Osteoarthritis", "Depression")
Costs_52_12_p$spec <- "Psychological"
Costs_52_12_p$cost <- ((rowSums(outcomesBB_p[c(1:8),]))-(rowSums(outcomesAA_p[c(1:8),])))/5000

# merge tables
costs_52_12<-rbind(Costs_52_12_c,Costs_52_12_p)

# Create bar chart of costs compared to 12-week group for each model
p_costs_52_12 <- ggplot(costs_52_12, aes(fill=spec, y=cost, x=cond)) + 
  geom_bar(position="dodge", stat="identity", width=0.8, col="black", size=0.1)+
  labs(title = "52-week programme vs. 12-week programme")+
  scale_fill_manual(values=c("cadetblue4", "coral"))+
  theme_minimal() +
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank(), axis.text.x=element_text(angle=45, hjust=1))

# Combine together for figure
cond_bar <- ggarrange(p_costs_12, p_costs_52,
                      ncol = 1, nrow = 2, heights = 5, widths = 5, common.legend = T, legend = "right")
cond_bar <- annotate_figure(cond_bar,
                            bottom = text_grob("Conditions"),
                            left = text_grob("Mean Incremental Costs per person", rot=90))
ggsave("cond_bar.png", height = 10, width = 6) 

# Incremental costs table
PSAResults_m<-as.data.frame(PSAResults_m)
PSAResults_c<-as.data.frame(PSAResults_c)
PSAResults_p<-as.data.frame(PSAResults_p)

# Set matrices for incremental costs, QALYs and net monetry benefit
inc_costs<-inc_qalys<-inc_nmb<-matrix(nrow = 3,ncol = 9, dimnames = list(c( "Direct", "Conditional", "Psychological"),
                                                              c("BI_12", "LCI_BI_12", "HCI_BI_12", 
                                                                "BI_52", "LCI_52_BI", "HCI_52_BI",
                                                                "12_52", "LCI_12_52", "HCI_12_52")))

# Create array of three tables
l_model <- list(PSAResults_m, PSAResults_c, PSAResults_p)

# For each model, calculate the costs, QALYs and NMB
  for(i in 1:length(l_model)){

  inc_costs[i,]<-c(mean(l_model[[i]][,"C_12_BI"]),sort(l_model[[i]][,"C_12_BI"],T)[975], sort(l_model[[i]][,"C_12_BI"],T)[25],
                   mean(l_model[[i]][,"C_52_BI"]),sort(l_model[[i]][,"C_52_BI"],T)[975], sort(l_model[[i]][,"C_52_BI"],T)[25],
                   mean(l_model[[i]][,"C_52_12"]),sort(l_model[[i]][,"C_52_12"],T)[975], sort(l_model[[i]][,"C_52_12"],T)[25]
                   )
  inc_qalys[i,]<-c(mean(l_model[[i]][,"Q_12_BI"]),sort(l_model[[i]][,"Q_12_BI"],T)[975], sort(l_model[[i]][,"Q_12_BI"],T)[25],
                   mean(l_model[[i]][,"Q_52_BI"]),sort(l_model[[i]][,"Q_52_BI"],T)[975], sort(l_model[[i]][,"Q_52_BI"],T)[25],
                   mean(l_model[[i]][,"Q_52_12"]),sort(l_model[[i]][,"Q_52_12"],T)[975], sort(l_model[[i]][,"Q_52_12"],T)[25]
                   )
  inc_nmb[i,]<-c(mean(l_model[[i]][,"NMB_12_BI"]),sort(l_model[[i]][,"NMB_12_BI"],T)[975], sort(l_model[[i]][,"NMB_12_BI"],T)[25],
               mean(l_model[[i]][,"NMB_52_BI"]),sort(l_model[[i]][,"NMB_52_BI"],T)[975], sort(l_model[[i]][,"NMB_52_BI"],T)[25],
               mean(l_model[[i]][,"NMB_52_12"]),sort(l_model[[i]][,"NMB_52_12"],T)[975], sort(l_model[[i]][,"NMB_52_12"],T)[25]
  )
  }

# Table of mean BMI and confidence intervals
BMI_mean_ci<-matrix(nrow = 4,ncol = 12, dimnames = list(c("Actual", "Mean", "Demographic", "Demographic plus MoA"),
                                                        c("mean_12_1", "LCI_12_1", "HCI_12_1","mean_12_2", "LCI_12_2", "HCI_12_2",
                                                          "mean_52_1", "LCI_52_1", "HCI_52_1", "mean_52_2", "LCI_52_2", "HCI_52_2")))

# Get confidence intervals
ci_tx12_12 <- CI(df_rawData_tx12$BMI_c_12)
ci_tx12_24 <- CI(df_rawData_tx12$BMI_c_24)

ci_tx52_12 <- CI(df_rawData_tx52$BMI_c_12)
ci_tx52_24 <- CI(df_rawData_tx52$BMI_c_24)

# Populate table
BMI_mean_ci["Actual", ] <- c(ci_tx12_12[[2]], ci_tx12_12[[1]], ci_tx12_12[[3]],
                             ci_tx12_24[[2]], ci_tx12_24[[1]], ci_tx12_24[[3]],
                             ci_tx52_12[[2]], ci_tx52_12[[1]], ci_tx52_12[[3]],
                             ci_tx52_24[[2]], ci_tx52_24[[1]], ci_tx52_24[[3]])

BMI_mean_ci["Mean", ] <- c(mean(PSAResults_m[, "BMI_yr1_12"]), sort(PSAResults_m[, "BMI_yr1_12"],T)[975], sort(PSAResults_m[, "BMI_yr1_12"],T)[25],
                             mean(PSAResults_m[, "BMI_yr2_12"]), sort(PSAResults_m[, "BMI_yr2_12"],T)[975], sort(PSAResults_m[, "BMI_yr2_12"],T)[25],
                             mean(PSAResults_m[, "BMI_yr1_52"]), sort(PSAResults_m[, "BMI_yr1_52"],T)[975], sort(PSAResults_m[, "BMI_yr1_52"],T)[25],
                             mean(PSAResults_m[, "BMI_yr2_52"]), sort(PSAResults_m[, "BMI_yr2_52"],T)[975], sort(PSAResults_m[, "BMI_yr2_52"],T)[25])

BMI_mean_ci["Demographic", ] <- c(mean(PSAResults_c[, "BMI_yr1_12"]), sort(PSAResults_c[, "BMI_yr1_12"],T)[975], sort(PSAResults_c[, "BMI_yr1_12"],T)[25],
                             mean(PSAResults_c[, "BMI_yr2_12"]), sort(PSAResults_c[, "BMI_yr2_12"],T)[975], sort(PSAResults_c[, "BMI_yr2_12"],T)[25],
                             mean(PSAResults_c[, "BMI_yr1_52"]), sort(PSAResults_c[, "BMI_yr1_52"],T)[975], sort(PSAResults_c[, "BMI_yr1_52"],T)[25],
                             mean(PSAResults_c[, "BMI_yr2_52"]), sort(PSAResults_c[, "BMI_yr2_52"],T)[975], sort(PSAResults_c[, "BMI_yr2_52"],T)[25])
PSAResults_p<-PSAResults_p/5000

BMI_mean_ci["Demographic plus MoA", ] <- c(mean(PSAResults_p[, "BMI_yr1_12"]), sort(PSAResults_p[, "BMI_yr1_12"],T)[975], sort(PSAResults_p[, "BMI_yr1_12"],T)[25],
                             mean(PSAResults_p[, "BMI_yr2_12"]), sort(PSAResults_p[, "BMI_yr2_12"],T)[975], sort(PSAResults_p[, "BMI_yr2_12"],T)[25],
                             mean(PSAResults_p[, "BMI_yr1_52"]), sort(PSAResults_p[, "BMI_yr1_52"],T)[975], sort(PSAResults_p[, "BMI_yr1_52"],T)[25],
                             mean(PSAResults_p[, "BMI_yr2_52"]), sort(PSAResults_p[, "BMI_yr2_52"],T)[975], sort(PSAResults_p[, "BMI_yr2_52"],T)[25])

df_BMI_mean_ci <- as.data.frame(BMI_mean_ci)
df_BMI_mean_ci$model_spec <- row.names(df_BMI_mean_ci)

# Rearrange data frame so that all BMI values in single data 
df_year1_sub <- df_BMI_mean_ci[,c(1,2,3,7,8,9,13)]
df_year1_sub$year <- "Year 1"
df_year1_sub_12 <- df_year1_sub[,c(1,2,3,7,8)]
df_year1_sub_12$tx <- "12-week intervention"
df_year1_sub_52 <- df_year1_sub[, c(4,5,6,7,8)]
df_year1_sub_52$tx <- "52-week intervention"

df_year2_sub <- df_BMI_mean_ci[,c(4,5,6,10,11,12,13)]
df_year2_sub$year <- "Year 2"
df_year2_sub_12 <- df_year2_sub[,c(1,2,3,7,8)]
df_year2_sub_12$tx <- "12-week intervention"
df_year2_sub_52 <- df_year2_sub[, c(4,5,6,7,8)]
df_year2_sub_52$tx <- "52-week intervention"

colnames(df_year1_sub_12)<-c("BMI", "L_CI", "H_CI", "model_spec", "year","treatment")
colnames(df_year1_sub_52)<-c("BMI", "L_CI", "H_CI", "model_spec", "year","treatment")
colnames(df_year2_sub_12)<-c("BMI", "L_CI", "H_CI", "model_spec", "year","treatment")
colnames(df_year2_sub_52)<-c("BMI", "L_CI", "H_CI", "model_spec", "year","treatment")
  
df_BMI_all<-rbind(df_year1_sub_12,df_year1_sub_52,df_year2_sub_12,df_year2_sub_52)
df_BMI_all$model_spec<-factor(df_BMI_all$model_spec, levels=unique(df_BMI_all$model_spec))

# Plot the BMI (bar graph)
p_BMI <- ggplot(df_BMI_all, aes(fill=model_spec, y=BMI, x=model_spec)) + 
  geom_bar(position="dodge", stat="identity", width=0.8, col="black", size=0.1)+
  ylab("Mean BMI")+
  xlab("Model Specification")+
  geom_errorbar(aes(ymin=L_CI, ymax=H_CI), width=.1,position=position_dodge(.9)) +
  scale_fill_manual(values = c("gray20", "coral", "cadetblue4", "gray80"))+
  theme_minimal() +
  facet_grid(treatment ~ year ) +
  coord_cartesian(ylim=c(31.5,35))+
  theme(legend.position = "none", legend.title=element_blank(),text = element_text(size=10), 
        axis.text.x=element_text(angle=45, hjust=1) )

ggsave("BMI_comp.png", width = 7, height = 6)


