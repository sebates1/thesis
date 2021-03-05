##########################################
##  Pre-trial modelling: User interface ##
##########################################

#### Load packages (use install.packages if required)

library(MASS)
library(VGAM)
library(mice)
library(dplyr)
library(shinydashboard)
library(shiny)
library(reshape2)
library(data.table)
library(ggplot2)
library(shinybusy)
library(openxlsx)


##################################################################
##                      User interface                          ##
##################################################################

ui <- dashboardPage(

    # Application title
    dashboardHeader(title="Pre-trial Health Economic Modelling ",titleWidth = 350),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
        
        width=350,
        sidebarMenu(

        # add list of scenarios to be selected
        radioButtons(inputId = "scen", label = h3("Select a single scenario:"),
                       choices = list("Small effect on habit" = 1, "Medium effect on habit" = 2, "Large effect on habit" = 3,
                                      "Small effect on dietary restraint" = 4, "Medium effect on dietary restraint" = 5, "Large effect on dietary restraint" = 6,
                                      "Small effect on autnomous motivation" = 7, "Medium effect on autnomous motivation" = 8, "Large effect on autnomous motivation" = 9), 
                       selected = 1),
        
        # add run model button (centred) 
        div(style="display:inline-block;width:100%;text-align: center;", actionButton(inputId = "run_model", label = "Run Model", icon("angle-right"))),
        
        # add menu items to select from side bar menu
        menuItem("Information", icon = icon("th"), tabName = "Information"),  
        menuItem("Results summary", icon = icon("th"), tabName = "summary"), 
        menuItem("Justifiable cost", icon = icon("th"), tabName = "JC"), 
        menuItem("PSA results", icon = icon("th"), tabName = "PSA") 
        
        )  # close sidebar menu
        
      ),   # close sidebar

        
      dashboardBody(
            
          tabItems(
          
            # tab 'information'
            tabItem(tabName = "Information", fluidRow(
              
              # add university of sheffield and wellcome trust logos
              img(src='sheff_logo.png', align = "right", height = '100px', width = '250px'), 
              img(src='wellcome-logo.png', align = "right", height = '100px', width = '100px'),
              
              # add additional space 
              HTML("<br><br><br>"),
              HTML("<br><br><br>"),
              
              # Title of front page (centred)
              h1(tags$b(div(style="display:inline-block;width:100%;text-align: center;",
                            "Pre-trial health economic modelling for
                            behavioural weight management interventions"))), 
              
              # Text giving info about user interface 
              h3(div(style="display:inline-block;width:100%;text-align: center;",
                     "This user interface enables you to select a small, medium or large change in 
                     one of three mechanisms of action (dietary restaint, habit stregnth and
               autonomous motivation) and view the impact on BMI trajectory and long-term cost-effectiveness.")),
            
              h3(div(style="display:inline-block;width:100%;text-align: center;",
                     "Select a single scenario on the left and view the results in the sections
                     labelled results summary, justifiable cost and PSA results.")),
              
              h3(div(style="display:inline-block;width:100%;text-align: center;",
                     ("Results have been generated using the School of Public Health (SPHR) microsiulation
                      model. Full details of the model results can be found here:"))),
              
              h3(div(style="display:inline-block;width:100%;text-align: center;",
                     (url <- a("Paper describing SPHR model", href="https://bmjopen.bmj.com/content/7/8/e014953")))),
              
              h3(div(style="display:inline-block;width:100%;text-align: center;",
                     ("All code for this app can be found here:"))),
              
              h3(div(style="display:inline-block;width:100%;text-align: center;",
                     (url <- a("Github repository", href="https://github.com/sebates1/thesis")))),
              
              
              h3(div(style="display:inline-block;width:100%;text-align: center;",
                     ("Due to the lenth 
                      of time taken to run the model, the scenarios have been pre-run. For specific scenarios,
                      and to find out more about the model,
                      please contact Sarah Bates (sebates1@sheffield.ac.uk)"))),
            
              # add additional space 
              HTML("<br><br><br>"),
              
              h5(div(style="display:inline-block;width:100%;text-align: center;",
                     ("This app was created as part of Sarah Bates's thesis 
                      which was supported by the Wellcome Trust [203970/Z/16/Z]"))),
              
            ) # end fluid row
          
          ), # end tab "information"
              
          # tab 'summary'
          tabItem(tabName = "summary", fluidRow(
              
             # box with line plot of predicted BMI  
             box(title="Predicted BMI", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, plotOutput("LineGraph", height=400)),
            
             # box with cost-effectiveness plot 
             box(title="Cost-effectiveness plane", height=460,  status="warning", solidHeader = TRUE,
                collapsible = TRUE, plotOutput("CE_plot")),
            
            ),   # close fluidrow
            
            fluidRow(
            
            # box with cost and QALY table
            box(title="Costs and QALYs",height=400,  status="warning", solidHeader = TRUE,
                collapsible = TRUE, tableOutput("costs_qalys")),
            
            # diabetes infobox
            infoBoxOutput("diab", width = 6),
            
            # CVD infobox
            infoBoxOutput("cardio", width = 6),
            
            # diabetes complication infobox
            infoBoxOutput("d_comp", width = 6),
          
            )    # close fluidrow
            
        ),   # close tab summary
           
        # tab justifiable cost
        tabItem(tabName = "JC", 
         
         fluidRow(
           
           # information about justifiable cost
           h3(div(style="display:inline-block;width:100%;text-align: center;",
                  ("The justifiable cost is the amount that can be spent on an intervention
                   with an expected intervention effect and incremental cost-effectiveness ratio (ICER)"))),
           h3(div(style="display:inline-block;width:100%;text-align: center;",
                  ("Below is the justifiable cost if the ICER is £0 (cost-saving intervetion)"))),
  
           # justifiable cost infobox cost saving
           infoBoxOutput("JC_cs", width = 6),
           
           # information about justifiable cost
           h3(div(style="display:inline-block;width:100%;text-align: center;",
                  ("The justifiable costs assuming an ICER of £20,000 and £30,000 per QALY are also calculated.
                   NICE’s ‘threshold,’ over which treatments are less likely to be recommended for 
                   use in the NHS, is typically between £20,000 and £30,000 per QALY."))),
           
           # justifiable cost infobox £20 000
           infoBoxOutput("JC_2", width = 6), 
           
           # justifiable cost infobox £30 000
           infoBoxOutput("JC_3", width = 6)
           
         )   # close fluidrow
         
      ),  # close tabitem
 
       # tab PSA    
       tabItem(tabName = "PSA", 
                    
                    fluidRow(
                      
                      # add download button 
                      div(style="display:inline-block;width:100%;text-align: center;", downloadButton("downloadData",
                      "Download PSA Results", tags$style(".skin-blue .sidebar a { color: #444; }")))
                      ,
                      
                    )   # close fluidrow
            
          )   # close tabitem
          
        )   # close all tab items
        
     )   # close dashboard body
    
)   # close user interface

##################################################################
##                      Server                                  ##
##################################################################

server <- function(input, output) { 
 
    # when action button is pressed...
    observeEvent(input$run_model,      
    ignoreNULL = F, { show_modal_spinner(spin="fingerprint", text = "Model running, please wait")
                 
    # load array of scenarios and outcomes, and control matrices
    load(file = "data/a_scen.RData")
    load(file = "data/m_control.RData")
    load(file = "data/a_Oscen.RData")
    load(file = "data/m_Ocontrol.RData")
    
    # select the selected scenario from the arrays
    m_scen <- a_scen[,,as.numeric(input$scen)]
    m_Oscen <- a_Oscen[,,as.numeric(input$scen)]
   
    # Create line graph of BMI over time
    output$LineGraph <- renderPlot({
       
        # remove the BMI data from the table
        BMI_results<-t(m_Oscen[c("BMI_B", "BMI"), ]/1000)
        # select BMI from control outcome table
        BMI_results_c<-t(m_Ocontrol[c("BMI_B", "BMI"), ]/1000)
        # merge two groups into one dataframe
        BMI_results<-cbind(BMI_results[,"BMI"], BMI_results_c[,"BMI"])
        # crop to 6 years
        BMI_results<-BMI_results[c(1:6),]
        # merge with baseline value of BMI
        BMI_results<-rbind(34.54295, BMI_results)
        # remane columns
        colnames(BMI_results)<-c("Intervention","No Intervention")
        # add a column for year 
        BMI_results <- cbind(BMI_results, "Year"=0:((nrow(BMI_results))-1))
        #Convert to data frame and into long format
        BMI_results<- as.data.frame(BMI_results)
        BMI_results <- melt(setDT(BMI_results), id.vars = c("Year"), variable.name = "Intervention")
        
        # create plot 
        BMI_results<- as.data.frame(BMI_results)
        pBMI <- ggplot(BMI_results, aes(x=Year, y=value, color=Intervention, linetype =Intervention))+
            labs(x = "Year", y = "BMI") + geom_line(size =1)  +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
                  axis.line = element_line(colour = "black"), legend.key = element_rect(fill = NA), legend.key.width = unit(2,"cm"), legend.title = element_blank(), legend.position="bottom", text = element_text(size=15))+
            scale_color_brewer(palette="Dark2")
        
        # print plot
        pBMI
      
   }) #Line graph plot end
    
    
    # Create cost-effectivness plane
    output$CE_plot <- renderPlot({
      
      m_scen <- as.data.frame(m_scen)
        CE_plot <- ggplot()+
        geom_point(aes(x = m_scen$inc_q, y = m_scen$inc_c), color = "orange", alpha = 0.5) +
          geom_point(aes(x=mean(m_scen$inc_q),y=mean(m_scen$inc_c)), pch=5, size=2,col="black", fill="black") +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        geom_line(aes(x=c(-0.025,0.01),y=c(-500,200), lty = "WTP Threshold £20,000")) +
        # labels
        xlab("Incremental QALYs") +
        ylab("Incremental Costs") +
        labs(title = "Cost-effectiveness Plane")+
        scale_linetype()+
        theme_minimal() +
        xlim(-0.025, 0.4)+
        ylim(-5000, 200)+
        theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10))+
        scale_fill_brewer(palette="Dark2")
        
        #print plot
        CE_plot
      
   }) #Cost-effectiveness plot end
    
    # cost and qalys tables
    output$costs_qalys<-renderTable({
    
    # calculate mean total and incremental costs and QALYs and ICER
    cost_Con <- mean(m_control[, "TOTAL_COST"])/5000
    cost_Int <- mean(m_scen[, "TOTAL_COST"])/5000
    inc_c <- cost_Int - cost_Con
    QALY_Con <- mean(m_control[, "QALY"])/5000
    QALY_Int <- mean(m_scen[, "QALY"])/5000
    inc_q<-QALY_Int-QALY_Con
    ICER<-inc_c/inc_q
    
    # set row names   
    rownames<-c("Total costs", "Total QALYs", "Incremental Costs", "Incremental QALYs", "ICER")
    
    # create dataframe 
    Control<-c(cost_Con, QALY_Con, NA, NA ,NA )
    Intervention<-c(cost_Int, QALY_Int, inc_c, inc_q,ICER)
    results<-data_frame(rownames, Control, Intervention)
    names(results)[names(results) == "rownames"] <- " "
    
    # print the table, rounded to two decimal places
    print(results, digits=2)
    
   })
    
    # create diabetes averted info box
    output$diab<-renderInfoBox({
      
       # calculate cases of diabetes averted
       diab_av<-(m_Ocontrol["DXT2",]-m_Oscen["DXT2",])/5000
       diab_av<-as.data.frame(diab_av)
       diab_av<-colSums(as.data.frame(diab_av))
      
        infoBox(
            "Diabetes cases averted (per 1000)", round(diab_av), color="light-blue", icon=icon("file-medical")
       )  # info box end
    })  # render info box end
    
    
    # create CVD cases averted info box
    output$cardio<-renderInfoBox({
      
      # calculate cases of CVD averted
      cardio_av<-((m_Ocontrol["CVD",]-m_Oscen["CVD",])/(5000))
      cardio_av<-as.data.frame(cardio_av)
      cardio_av<-colSums(as.data.frame(cardio_av))
      
      infoBox(
        "CVD cases averted (per 1000)", round(cardio_av), color="red", icon=icon("heartbeat")
      )   # info box end 
    })    # render info box end
    
    # create diabetes related complications averted info box
    output$d_comp<-renderInfoBox({
      
      # calculate cases of diabetes complications averted 
      Diab_c_00<-sum(m_Ocontrol["BLIND",],m_Ocontrol["ULCER",], m_Ocontrol["AMP",],m_Ocontrol["AMP2",],
                     m_Ocontrol["RENAL",])
      Diab_c_BB<-sum(m_Oscen["BLIND",],m_Oscen["ULCER",], m_Oscen["AMP",],m_Oscen["AMP2",], m_Oscen["RENAL",])
      d_comp_av<-(Diab_c_00-Diab_c_BB)/5000
      d_comp_av<-as.data.frame(d_comp_av)
      d_comp_av<-colSums(as.data.frame(d_comp_av))
      
      infoBox(
        "Diabetes complications averted (per 1000)", round(d_comp_av), color="yellow", icon=icon("stethoscope")
      )   # info box end
    })    # render info box end
    
    # create justifiable cost cost saving
    output$JC_cs<-renderInfoBox({
      
      infoBox(
        "JC:Cost saving (£) ", round((0*(mean(m_scen[,"inc_q"]))-mean(m_scen[,"inc_c"])), digits = 2), 
        color="yellow", icon=icon("coins"), width = 700)
        
    })    # info box end
    
    # create justifiable cost £20 000
    output$JC_2<-renderInfoBox({

      infoBox(
        "JC: ICER £20,000 (£) ", round((20000*(mean(m_scen[,"inc_q"]))-mean(m_scen[,"inc_c"])), digits = 2), 
        color="yellow", icon=icon("coins"))
      
    })    # info box end
    
    # create justifiable cost £30 000
    output$JC_3<-renderInfoBox({
      
      infoBox(
        "JC: ICER £20,000 (£)", paste0("£", round((30000*(mean(m_scen[,"inc_q"]))-mean(m_scen[,"inc_c"])), digits = 2)), 
        color="yellow", icon=icon("coins"))
      
    })    # info box end


### Adding in more detailed results table     
    
    # create outcomes table
    output$outcomes<-renderTable({
      
      # take relevant rows from the outcomes tables
      Costs00<-rowSums(m_Ocontrol[c(1:10,11,14),]/5000)
      CostsBB<-rowSums(m_Oscen[c(1:10,11,14),]/5000)
      
      # calculate difference between groups
      inc<-CostsBB-Costs00
      
      # outcome labels
      Outcomes<-c("Diabetes", "CVD", "Nephrology", "Retinopathy", "Neuropathy", "Cancer", "Osteoarthritis", "Depression", "Dementia", "Other", "Social", "QALYs")
      
      # create data frame with outcomes
      meanpp<-data_frame(Outcomes,Costs00, CostsBB, inc)
      
      # label tbale
      names(meanpp)[names(meanpp) == "Costs00"] <- "Costs and QALYs per person: Control"
      names(meanpp)[names(meanpp) == "CostsBB"] <- "Costs and QALYs per person: Intervention"
      names(meanpp)[names(meanpp) == "inc"] <- "Incremental costs and QALYs per person"
      
      # print table 
      meanpp
  
    })
    
    # download button
    output$downloadData <- downloadHandler(
      
      # create file 
      filename = function(){ 
        paste("PSA_", input$scen, Sys.Date(), ".xlsx", sep="")
      },
      
      # populate file with a sheet of PSA results for control and intervention
      content = function(filename) {
        Full_results<-list("Control_PSA" = m_control[,c(1:3)],"Intervention_PSA" = m_scen[,c(1:3,6,7)])
        write.xlsx(Full_results, filename, row.names = TRUE)
      }
      
    ) # end download button
    
    # end model spinner
    remove_modal_spinner()
  }) # Observe Event End
    
}

# Run the application 
shinyApp(ui = ui, server = server)
