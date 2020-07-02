#################
#################
###SHINY EPIDELTA
#################
#################
#Rosa Mulder
#May 18 2020

## https://github.com/rosamulder/EpiDelta_DEMO

################
###Load packages
################s
library(ggplot2)      
library(shiny)
library(shinyWidgets)   #to change background color
library(imager)         #to show images
library(shinyjs)        #to hide buttons



############
###Locations
############
dfloc     <- "data"                        
figureloc <- "plots"                        
logoloc   <- "logos"                        



#######
###Load
#######
load(file.path(dfloc,"epidelta_df_200108.RData"))



############
###CpG names
############
cpgs        <- rownames(epidelta)
names(cpgs) <- cpgs


cpgfigure <- function(cpg, ending) 
    file.path(figureloc, substring(cpg,1,6), paste0(cpg, ending))

#################
###USER INTERFACE
#################
ui <- tagList(               
  
  titlePanel(fluidRow(column(5, tags$h1(tags$strong(tags$code("THE EPIDELTA PROJECT"))), align="center"),
                        column(2, tags$img(src=file.path(logoloc,'Logo_EMC.png'), height = 137/2, width = 350/2, href="https://www.erasmus\
mc.nl/")),
                      column(1, tags$img(src=file.path(logoloc,'Logo_GenR.png'), height = 174/3, width = 240/3, href="https://generation\
r.nl/researchers/")),
                      column(2, tags$img(src=file.path(logoloc,'Logo_BristolUni.png'), height = 120/2.5, width = 418/2.5, href="https://\
www.bristol.ac.uk/")),
                      column(1, tags$img(src=file.path(logoloc,'Logo_ALSPAC.png'), height = 229/2, width = 145/2, href="http://www.brist\
ol.ac.uk/alspac/"))),
             windowTitle="epidelta project"),
  
  navbarPage("",
             tabPanel(HTML(paste("Reference:", 
                                 "[placeholder]", 
                                 sep="<br/>")),
                      ###Input objects
                      sidebarPanel(
                        useShinyjs(),   #to hide buttons
                        checkboxInput(inputId = "agree",label = strong("By checking this box, I declare that I have read and agree with the disclaimer:"),value = FALSE),
                        
                        htmlOutput(outputId="disclaimer"),
                        
                        textInput(inputId="typeCpG",label="CpG site of interest",placeholder="cg00029246",value="cg00029246"),
                        hidden(actionButton(inputId = "update",label = "Go")),
                        
                        selectInput(inputId="cpg_list_options",label="load results",
                                    choices=list("all"=1,
                                                 "M1 change estimate Bonferroni significant (1E-07)"=2,
                                                 "M1 inter-individual variation in change Bonferroni significant (1E-07)"=3,
                                                 "M2 nonlinear change at 6y or 9y Bonferroni significant (1E-07)"=4,
                                                 "M2 inter-individual variation from birth, 6y or 9y Bonferroni significant (1E-07)"=5,
                                                 "M3 stable sex differences Bonferroni significant (1E-07)"=6,
                                                 "M3 sex differences in change Bonferroni significant (1E-07)"=7)), 
                        hidden(downloadButton(outputId="download_button", label="download")),
                        
                        width=2)),
			
  mainPanel(fluidRow(column(4, offset=0.5, h4("Model 2 - including nonlinear changes"),
                            imageOutput(outputId = "Predicted_data_M2_bycohort")),
                     column(4, offset=1, h4("Model 3 - including sex differences in change"),
                            imageOutput(outputId = "Predicted_data_M3_bysex")),
                     column(2, offset=1, h4("Info"),
                            hidden(htmlOutput(outputId="info")),
                            hidden(actionButton(inputId="info_button", label="more info")),
                            hidden(actionButton(inputId="info_button_less", label="less info")))), #
            fluidRow(column(7, tableOutput("table_results")),
                     column(4, offset=1, hidden(htmlOutput(outputId="more_info")))))))
 

######################
###SERVER INPUT OUTPUT
######################
server <- function(input, output) {
  
  
  ##################
  ###Show disclaimer
  ##################
  output$disclaimer <- renderUI(HTML(paste0("Disclaimer","<br/>",
                                            "The data are provided ", "'as is'. ", 'The party providing the data ("Provider") makes no representations and extends no warranties of any kind, either expressed or implied with respect to the data, such as but not limited to any representation or warranty on accuracy, completeness, availability, accessibility, fitness for a particular purpose, or that the use of the data will not infringe any rights of third parties. The Provider shall not be liable for any liability or damages with respect to any claim by the recipient or any third party arising from the use and/or download of the data. Recipient shall indemnify and hold harmless the Provider and its trustees, officers, employees and students against any and all claims arising out of the use and/or download of the data by the recipient.')))
  
  
  ########################
  ###Enable use of website
  ########################
  observeEvent(input$agree, {
    if(input$agree)
      show("update") &
      show("download_button") &
      show("info") &
      show("info_button") &
      hide("disclaimer")
    else
      hide("update") &
      hide("download_button") &
      hide("info") &
      hide("info_button") &
      show("disclaimer")})
  
  
  ############
  ###Graph CpG
  ############
  data_cpg <- eventReactive(input$update, {
    input$typeCpG})
  
  
  #long plot from results                  
  output$Predicted_data_M2_bycohort <- renderImage({
    graphname_M2_bycohort <- cpgfigure(data_cpg(),'_M2_bycohort_200326.png')
    image_M2_bycohort     <- graphname_M2_bycohort
    
    list(src = image_M2_bycohort, 
         contentType = 'image/png',
         width = 400,
         height = 300,
         title = "Model 2 - nonlinear changes",            
         alt = paste(image_M2_bycohort, "Sorry something went wrong for this graph"))
  }, deleteFile = FALSE)
  
  
  output$Predicted_data_M3_bysex <- renderImage({
    
    graphname_M3_bysex    <- cpgfigure(data_cpg(),'_M3_bysex_200326.png')
    image_M3_bysex        <- graphname_M3_bysex
    
    list(src = image_M3_bysex, 
         contentType = 'image/png',
         width = 400,
         height = 300,
         title = "Model 3 - sex differences",
         alt = paste(image_M3_bysex, "Sorry something went wrong for this graph"))
  }, deleteFile = FALSE)
  
  
  output$table_results <- renderTable({
    
    mat           <- matrix(nrow=10, ncol=5)
    rownames(mat) <- c("M1 intercept","M1 change",
                       "M2 intercept", "M2 change", "M2 slope change at 6y", "M2 slope change at 9y",
                       "M3 intercept", "M3 change", "M3 sex", "M3 change by sex")
    colnames(mat) <- c("b", "se", "p","inter-individual variance, sd","inter-individual variance, p")
    
    
    mat[1,1]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M1.intercept.estimate"],digits=3,scientific=TRUE)
    mat[2,1]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M1.change.estimate"],digits=3,scientific=TRUE)
    mat[3,1]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.intercept.estimate"],digits=3,scientific=TRUE)
    mat[4,1]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.change.estimate"],digits=3,scientific=TRUE)
    mat[5,1]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.slopechange6.estimate"],digits=3,scientific=TRUE)
    mat[6,1]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.slopechange9.estimate"],digits=3,scientific=TRUE)
    mat[7,1]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.intercept.estimate"],digits=3,scientific=TRUE)
    mat[8,1]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.change.estimate"],digits=3,scientific=TRUE)
    mat[9,1]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.sex.estimate"],digits=3,scientific=TRUE)
    mat[10,1] <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.changebysex.estimate"],digits=3,scientific=TRUE)
    
    mat[1,2]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M1.intercept.se"],digits=3,scientific=TRUE)
    mat[2,2]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M1.change.se"],digits=3,scientific=TRUE)
    mat[3,2]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.intercept.se"],digits=3,scientific=TRUE)
    mat[4,2]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.change.se"],digits=3,scientific=TRUE)
    mat[5,2]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.slopechange6.se"],digits=3,scientific=TRUE)
    mat[6,2]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.slopechange9.se"],digits=3,scientific=TRUE)
    mat[7,2]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.intercept.se"],digits=3,scientific=TRUE)
    mat[8,2]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.change.se"],digits=3,scientific=TRUE)
    mat[9,2]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.sex.se"],digits=3,scientific=TRUE)
    mat[10,2] <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.changebysex.se"],digits=3,scientific=TRUE)
    
    mat[1,3]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M1.intercept.p"],digits=3,scientific=TRUE)
    mat[2,3]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M1.change.p"],digits=3,scientific=TRUE)
    mat[3,3]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.intercept.p"],digits=3,scientific=TRUE)
    mat[4,3]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.change.p"],digits=3,scientific=TRUE)
    mat[5,3]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.slopechange6.p"],digits=3,scientific=TRUE)
    mat[6,3]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.slopechange9.p"],digits=3,scientific=TRUE)
    mat[7,3]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.intercept.p"],digits=3,scientific=TRUE)
    mat[8,3]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.change.p"],digits=3,scientific=TRUE)
    mat[9,3]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.sex.p"],digits=3,scientific=TRUE)
    mat[10,3] <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.changebysex.p"],digits=3,scientific=TRUE)
    
    mat[1,4]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M1.intercept.rand.sd"],digits=3,scientific=TRUE)
    mat[2,4]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M1.change.rand.sd"],digits=3,scientific=TRUE)
    mat[3,4]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.intercept.rand.sd"],digits=3,scientific=TRUE)
    mat[4,4]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.change.rand.sd"],digits=3,scientific=TRUE)
    mat[5,4]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.slopechange6.rand.sd"],digits=3,scientific=TRUE)
    mat[6,4]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.slopechange9.rand.sd"],digits=3,scientific=TRUE)
    mat[7,4]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.intercept.rand.sd"],digits=3,scientific=TRUE)
    mat[8,4]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.change.rand.sd"],digits=3,scientific=TRUE)
    mat[9,4]  <- " "
    mat[10,4] <- " "
    
    mat[1,5]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M1.intercept.rand.p"],digits=3,scientific=TRUE)
    mat[2,5]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M1.change.rand.p"],digits=3,scientific=TRUE)
    mat[3,5]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.intercept.rand.p"],digits=3,scientific=TRUE)
    mat[4,5]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.change.rand.p"],digits=3,scientific=TRUE)
    mat[5,5]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.slopechange6.rand.p"],digits=3,scientific=TRUE)
    mat[6,5]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M2.slopechange9.rand.p"],digits=3,scientific=TRUE)
    mat[7,5]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.intercept.rand.p"],digits=3,scientific=TRUE)
    mat[8,5]  <- format(epidelta[rownames(epidelta)==data_cpg(),"M3.change.rand.p"],digits=3,scientific=TRUE)
    mat[9,5]  <- " "
    mat[10,5] <- " "
    mat 
  }, rownames=TRUE)
  
  
  ################
  ###Download data
  ################
  data_epideltaresults <- reactive({
    if(input$cpg_list_options==1){
      epideltadata <- epidelta
    }else if(input$cpg_list_options==2){
      epideltadata <- epidelta[epidelta$M1.change.p<1e-7,]
    }else if(input$cpg_list_options==3){
      epideltadata <- epidelta[epidelta$M1.change.rand.p<1e-7,]
    }else if(input$cpg_list_options==4){
      epideltadata <- epidelta[epidelta$M2.slopechange6.p<1e-7|epidelta$M2.slopechange9.p<1e-7,]
    }else if(input$cpg_list_options==5){
      epideltadata <- epidelta[epidelta$M2.change.rand.p<1e-7|epidelta$M2.slopechange6.rand.p<1e-7|epidelta$M2.slopechange9.rand.p<1e-7,]
    }else if(input$cpg_list_options==6){
      epideltadata <- epidelta[epidelta$M3.sex.p<1e-7,]
    }else if(input$cpg_list_options==7){
      epideltadata <- epidelta[epidelta$M3.changebysex.p<1e-7,]
    }
  })
  
  output$download_button <- downloadHandler(                
    filename = function(){
      paste("epidelta_", Sys.Date(),".txt", sep='\t')},
    
    content= function(file){write.table(data_epideltaresults(), file, row.names=TRUE)})
  
  
  ################
  ###Add some info
  ################
  info <- "<b>Info</b>
Three linear mixed models were tested

M1:  M<sub>ijk</sub> = &beta;<sub>0</sub> + u<sub>0i</sub> + &beta;<sub>1</sub>Age<sub>ij</sub> + u<sub>1i</sub>Age<sub>ij</sub> + 
u<sub>0k</sub> + covariates + &epsilon;<sub>ijk</sub>

M2:  M<sub>ijk</sub> = &beta;<sub>0</sub> + u<sub>0i</sub> + &beta;<sub>1</sub>Age<sub>ij</sub> + &beta;<sub>2</sub>(Age<sub>ij-6</sub>)<sup>+</sup> + 
&beta;<sub>3</sub>(Age<sub>ij-9</sub>)<sup>+</sup> + u<sub>1i</sub>Age<sub>ij</sub> + u<sub>2i</sub>(Age<sub>ij-6</sub>)<sup>+</sup> + u<sub>3i</sub>(Age<sub>ij-9</sub>)<sup>+</sup> + 
u<sub>0k</sub> + covariates + &epsilon;<sub>ijk</sub>

M3:  M<sub>ijk</sub> = &beta;<sub>0</sub> + u<sub>0i</sub> + &beta;<sub>1</sub>Age<sub>ij</sub> + u<sub>1i</sub>Age<sub>ij</sub> + 
&beta;<sub>2</sub>Sex<sub>i</sub>Age<sub>ij</sub> u<sub>0k</sub> + covariates + &epsilon;<sub>ijk</sub>

Covariates: batch, estimated white blood cells, gestational age, sex, cohort
The graphs display the predicted data based on the models


"
  info <- unlist(strsplit(info, split="\n"))
  info <- paste(info, collapse="<br/>")

  output$info <- renderUI(HTML(info))
  
  
  ###################
  ###Enable more info
  ###################
  observeEvent(input$info_button, {
    if(input$info_button)
      show("more_info") &
      show("info_button_less")
    else
      hide("more_info") &
      hide("info_button_less")
  })
  
  observeEvent(input$info_button_less, {
    if(input$info_button_less)
      hide("more_info") &
      hide("info_button_less")
    else
      show("more_info") &
      show("info_button_less")
  })
  
  
  #####################  
  ###Add some more info
  #####################
  info <- "
M1: This model was applied to identify CpGs that show an overall change in DNA methylation (DNAm) from birth to 18 years (i.e. fixed age effect), 
as well as CpGs with inter-individual differences in change during that time (i.e. random age effect).


Here, participants are denoted by i, time points by j, and sample plates by k.  M denotes DNAm level, &beta;<sub>0</sub> fixed intercept, u<sub>0i</sub> random intercept, &beta;<sub>1</sub> fixed age coefficient, u<sub>1i</sub> random age coefficient, u<sub>0k</sub> random intercept for sample plate. 
Hence, &beta;<sub>1</sub> represents the average change in DNAm per one year. Variability in this change amongst individuals was captured with u<sub>1i</sub>. 
To avoid problems with model identification, the random slope of age was uncorrelated to the random intercept (i.e. a diagonal random effects matrix was used).


M2: This model was applied to identify identify nonlinear changes in DNAm.

Where a<sup>+</sup> = a if a>0 and 0 otherwise, so that &beta;<sub>2</sub> represents the average change in DNAm per year from 6 years of age onward, after accounting for the change per year from birth onward, as denoted by &beta;<sub>1</sub>. Likewise, &beta;<sub>3</sub> represents the average change in DNAm per year from 9 years of age onward, after accounting for the change per year from 6 years of age onward. 

Hence, with those variables we are able to detect slope changes at 6 and 9 years old. These slope changes were used to identify different types of nonlinear patterns. With u<sub>2i</sub> and u<sub>3i</sub> the inter-individual variation in slope changes at 6 and 9 years were captured, respectively. 

General linear hypothesis testing was applied to our fitted models to determine if there were changes in DNAm per year from 6-9 years and from 9-18 years.


M3: This model was applied to identify CpGs for which DNAm changes differently over time for boys and girls.

Sex<sub>i</sub> denotes the sex of child i and was used to interpret stable sex differences, &beta;<sub>2</sub>Sex<sub>i</sub>Age<sub>ij</sub> denotes sex ifferences in DNAm change (i.e. Sex by Age interaction effect). 


"
  
  info <- unlist(strsplit(info, split="\n"))
  info <- paste(info, collapse="<br/>")
  
  output$more_info <- renderUI(HTML(info))
  
  
}

############
###SHINY APP
############
shinyApp(ui=ui, server=server)




