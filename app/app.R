#################
#################
###SHINY EPIDELTA
#################
#################

## Revision of https://github.com/rosamulder/EpiDelta_DEMO

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
#load(file.path(dfloc,"epidelta_df_200108.RData"))
load(file.path(dfloc,"epidelta_df_201208.RData"))



############
###CpG names
############
cpgs        <- rownames(epidelta)
names(cpgs) <- cpgs


cpgfigure <- function(cpg, ending) 
    file.path(figureloc, substring(cpg,1,6), paste0(cpg, ending))

##########
## p-value threshold for genome-wide significance
#############
p.threshold <- 1e-7

############
## Text
############

reference <- paste0("<div style='padding: 5px; font-size: 10pt; text-align: left'>",
		    "<b>Publication</b>: Mulder, R.H. <i>et al.</i> ",
		    "<a href='https://doi.org/10.1093/hmg/ddaa280' target='_blank'>",
 		    "Epigenome-wide change and variation in DNA methylation from birth to late adolescence", 
		    "</a>. ", 
		    "Hum Mol Genet. 2021 Mar 25;30(1):119-134.",
		    "</div>")

choices <- list("all"=1,
                "M1 change estimate Bonferroni significant (1E-07)"=2,
                "M1 inter-individual variation in change Bonferroni significant (1E-07)"=3,
                "M2 nonlinear change at 6y or 9y Bonferroni significant (1E-07)"=4,
                "M2 inter-individual variation from birth, 6y or 9y Bonferroni significant (1E-07)"=5,
                "M3 stable sex differences Bonferroni significant (1E-07)"=6,
                "M3 sex differences in change Bonferroni significant (1E-07)"=7)

declaration <- "By checking this box, I declare that I have read and agree with the disclaimer:"

disclaimer <- "Disclaimer<br/>The data are provided 'as is'. The party providing the data ('Provider') makes no representations and extends no warranties of any kind, either expressed or implied with respect to the data, such as but not limited to any representation or warranty on accuracy, completeness, availability, accessibility, fitness for a particular purpose, or that the use of the data will not infringe any rights of third parties. The Provider shall not be liable for any liability or damages with respect to any claim by the recipient or any third party arising from the use and/or download of the data. Recipient shall indemnify and hold harmless the Provider and its trustees, officers, employees and students against any and all claims arising out of the use and/or download of the data by the recipient."

info <- "<br><b>Info</b>
Three linear mixed models were tested

<b>M1</b>:  M<sub>ijk</sub> = &beta;<sub>0</sub> + u<sub>0i</sub> + &beta;<sub>1</sub>Age<sub>ij</sub> + u<sub>1i</sub>Age<sub>ij</sub> + u<sub>0k</sub> + covariates + &epsilon;<sub>ijk</sub>

<b>M2</b>:  M<sub>ijk</sub> = &beta;<sub>0</sub> + u<sub>0i</sub> + &beta;<sub>1</sub>Age<sub>ij</sub> + &beta;<sub>2</sub>(Age<sub>ij-6</sub>)<sup>+</sup> + &beta;<sub>3</sub>(Age<sub>ij-9</sub>)<sup>+</sup> + u<sub>1i</sub>Age<sub>ij</sub> + u<sub>2i</sub>(Age<sub>ij-6</sub>)<sup>+</sup> + u<sub>3i</sub>(Age<sub>ij-9</sub>)<sup>+</sup> + u<sub>0k</sub> + covariates + &epsilon;<sub>ijk</sub>

<b>M3</b>:  M<sub>ijk</sub> = &beta;<sub>0</sub> + u<sub>0i</sub> + &beta;<sub>1</sub>Age<sub>ij</sub> + u<sub>1i</sub>Age<sub>ij</sub> + &beta;<sub>2</sub>Sex<sub>i</sub>Age<sub>ij</sub> u<sub>0k</sub> + covariates + &epsilon;<sub>ijk</sub>

Covariates: batch, estimated white blood cells, gestational age, sex, cohort

The graphs display the predicted data based on the models.


"
info <- unlist(strsplit(info, split="\n"))
info <- paste(info, collapse="<br/>")

more_info <- "
<b>M1</b>: This model was applied to identify CpGs that show an overall change in DNA methylation (DNAm) from birth to 18 years (i.e. fixed age effect), as well as CpGs with inter-individual differences in change during that time (i.e. random age effect).

Here, participants are denoted by i, time points by j, and sample plates by k.  M denotes DNAm level, &beta;<sub>0</sub> fixed intercept, u<sub>0i</sub> random intercept, &beta;<sub>1</sub> fixed age coefficient, u<sub>1i</sub> random age coefficient, u<sub>0k</sub> random intercept for sample plate. Hence, &beta;<sub>1</sub> represents the average change in DNAm per one year. Variability in this change amongst individuals was captured with u<sub>1i</sub>. To avoid problems with model identification, the random slope of age was uncorrelated to the random intercept (i.e. a diagonal random effects matrix was used).

<b>M2</b>: This model was applied to identify identify nonlinear changes in DNAm.

Where a<sup>+</sup> = a if a>0 and 0 otherwise, so that &beta;<sub>2</sub> represents the average change in DNAm per year from 6 years of age onward, after accounting for the change per year from birth onward, as denoted by &beta;<sub>1</sub>. Likewise, &beta;<sub>3</sub> represents the average change in DNAm per year from 9 years of age onward, after accounting for the change per year from 6 years of age onward. 

Hence, with those variables we are able to detect slope changes at 6 and 9 years old. These slope changes were used to identify different types of nonlinear patterns. With u<sub>2i</sub> and u<sub>3i</sub> the inter-individual variation in slope changes at 6 and 9 years were captured, respectively. 

General linear hypothesis testing was applied to our fitted models to determine if there were changes in DNAm per year from 6-9 years and from 9-18 years.

<b>M3</b>: This model was applied to identify CpGs for which DNAm changes differently over time for boys and girls.

Sex<sub>i</sub> denotes the sex of child i and was used to interpret stable sex differences, &beta;<sub>2</sub>Sex<sub>i</sub>Age<sub>ij</sub> denotes sex ifferences in DNAm change (i.e. Sex by Age interaction effect). 

"  
more_info <- unlist(strsplit(more_info, split="\n"))
more_info <- paste(more_info, collapse="<br/>")
more_info <- paste("<div class='well'>",more_info,"</div>")


#################
###USER INTERFACE
#################
ui <- fluidPage(               
  

   titlePanel(fluidRow(column(5, tagList(h1(strong(code("THE EPIDELTA PROJECT"))),
					 HTML(reference)), align="center"),
                        column(6, img(src=file.path(logoloc,'Logo_EMC.png'),
                                           height = 137/2, width = 350/2, href="https://www.erasmusmc.nl/"),
                                   img(src=file.path(logoloc,'Logo_GenR.png'),
                                           height = 174/3, width = 240/3, href="https://generationr.nl/researchers/"),
                                   img(src=file.path(logoloc,'Logo_BristolUni.png'),
                                           height = 120/2.5, width = 418/2.5, href="https://www.bristol.ac.uk/"),
                                   img(src=file.path(logoloc,'Logo_ALSPAC.png'),
                                           height = 229/2, width = 145/2, href="http://www.bristol.ac.uk/alspac/"))),
              windowTitle="epidelta project"),
    
    sidebarLayout(
                        sidebarPanel(
                            useShinyjs(),   #to hide buttons
                            checkboxInput(inputId = "agree",label = strong(declaration),value = FALSE),
                            
                            htmlOutput(outputId="disclaimer"),
                            
                            hidden(textInput(inputId="typeCpG",label="CpG site of interest",placeholder="cg00029246",value="cg00029246")),
                            hidden(actionButton(inputId = "update",label = "Go")),
                            
                            hidden(selectInput(inputId="cpg_list_options",label="Load results",
                                        choices=choices)), 
                            hidden(downloadButton(outputId="download_button", label="download")),
                            htmlOutput(outputId="info"),
			    width=2),
               
               mainPanel(fluidRow(column(6, hidden(htmlOutput(outputId="model2_title")),
                                         imageOutput(outputId = "Predicted_data_M2_bycohort")),
                                  column(6, hidden(htmlOutput(outputId="model3_title")),
                                         imageOutput(outputId = "Predicted_data_M3_bysex"))),
			 fluidRow(column(12, h1(""))),
                         fluidRow(column(8, tableOutput("table_results")),
                                  column(4,
                                         hidden(actionButton(inputId="info_button", label="more info")),
                                         hidden(actionButton(inputId="info_button_less", label="less info")), 
                                         hidden(htmlOutput(outputId="more_info")))))))


######################
###SERVER INPUT OUTPUT
######################
server <- function(input, output) {
    
    

##################
###Show disclaimer
##################
    output$disclaimer <- renderUI(HTML(disclaimer))
    
    
########################
###Enable use of website
########################
    observeEvent(input$agree, {
        if(input$agree)
            (show("update") &
	     show("typeCpG") &
	     show("cpg_list_options") &
             show("download_button") &
             show("info") &
             hide("disclaimer"))
        else
            (hide("update") &
	     hide("typeCpG") &
	     hide("cpg_list_options") &
             hide("download_button") &
             hide("info") &
             hide("info_button") &
             show("disclaimer"))
    })
  
  
############
###Graph CpG
############
    hide("info_button")
    data_cpg <- eventReactive(input$update, {
        validate(
            need(input$typeCpG %in% rownames(epidelta),
                 paste(input$typeCpG, "is an unknown CpG site identifier.")))
	show("model2_title")
	show("model3_title")
	show("info_button")
        hide("more_info") 
        hide("info_button_less") 
        input$typeCpG
    })

    output$model2_title <- renderUI(HTML("<h3 style='white-space: nowrap'>Model 2 - including nonlinear changes</h4>"))
    ##long plot from results                  
    output$Predicted_data_M2_bycohort <- renderImage({
        graphname_M2_bycohort <- cpgfigure(data_cpg(),'_M2_bycohort_200326.png')
        image_M2_bycohort     <- graphname_M2_bycohort
        
        list(src = image_M2_bycohort, 
             contentType = 'image/png',
             width = 420,
             height = 420,
             title = "Model 2 - nonlinear changes",            
             alt = "Sorry something went wrong for this graph")
    }, deleteFile = FALSE)
    
    output$model3_title <- renderUI(HTML("<h3 style='white-space: nowrap'>Model 3 - including sex differences in change</h4>"))
    output$Predicted_data_M3_bysex <- renderImage({      
        graphname_M3_bysex    <- cpgfigure(data_cpg(),'_M3_bysex_200326.png')
        image_M3_bysex        <- graphname_M3_bysex
        
        list(src = image_M3_bysex, 
             contentType = 'image/png',
             width = 420,
             height = 420,
             title = "Model 3 - sex differences",
             alt = "Sorry something went wrong for this graph")
    }, deleteFile = FALSE)
    
    
    output$table_results <- renderTable({
        variable_selections <- list("M1"=c("intercept","change"),
                                    "M2"=c("intercept","change","slope change at 6y","slope change at 9y"),
                                    "M3"=c("intercept","change","sex (boy)","change by sex"))
        variable_selections <- unlist(lapply(names(variable_selections), function(model)
                                             paste(model, variable_selections[[model]], sep=".")))
        names(variable_selections) <- variable_selections
        variable_selections <- gsub(" ", "", variable_selections)
        variable_selections <- gsub("at([0-9]+)y", "\\1", variable_selections)
        variable_selections <- gsub("(boy)", "", variable_selections, fixed=T)
        
        stat_selections <- list("b"="estimate",
                                "se"="se",
                                "p"="p",
                                "inter-individual variance, sd"="rand.sd",
                                "inter-individual variance, p"="rand.p")

        sapply(stat_selections, function(stat)
               sapply(variable_selections, function(var) {
                   if (grepl("M3",var) && grepl("sex",var) && grepl("rand",stat))
                       return("")
                   colname <- paste(var, stat, sep=".")
                   format(epidelta[data_cpg(),colname],digits=3,scientific=TRUE)
               }))
    }, rownames=TRUE)
    
################
###Download data
################
    data_epideltaresults <- reactive({
        if(input$cpg_list_options==1){
            epideltadata <- epidelta
        }else if(input$cpg_list_options==2){
            epideltadata <- epidelta[epidelta$M1.change.p<p.threshold,]
        }else if(input$cpg_list_options==3){
            epideltadata <- epidelta[epidelta$M1.change.rand.p<p.threshold,]
        }else if(input$cpg_list_options==4){
            epideltadata <- epidelta[epidelta$M2.slopechange6.p<p.threshold|epidelta$M2.slopechange9.p<p.threshold,]
        }else if(input$cpg_list_options==5){
            epideltadata <- epidelta[epidelta$M2.change.rand.p<p.threshold|epidelta$M2.slopechange6.rand.p<p.threshold|epidelta$M2.slopechange9.rand.p<p.threshold,]
        }else if(input$cpg_list_options==6){
            epideltadata <- epidelta[epidelta$M3.sex.p<p.threshold,]
        }else if(input$cpg_list_options==7){
            epideltadata <- epidelta[epidelta$M3.changebysex.p<p.threshold,]
        }
        epideltadata
    })
    
    output$download_button <- downloadHandler(                
        filename = function() {
            paste("epidelta_", Sys.Date(),".txt", sep='')
        },        
        content = function(file){
            write.table(data_epideltaresults(), file, sep="\t", row.names=TRUE)
        })
    

################
###Add some info
################
    output$info <- renderUI(HTML(info))
    
    
###################
###Enable more info
###################
    observeEvent(input$info_button, {
        if(input$info_button)
            (show("more_info") &
             show("info_button_less") &
             hide("info_button"))
        else
            (hide("more_info") &
             hide("info_button_less") &
             show("info_button"))
    })
    
    observeEvent(input$info_button_less, {
        if(input$info_button_less)
            (hide("more_info") &
             hide("info_button_less") &
             show("info_button"))
        else
            (show("more_info") &
             show("info_button_less") &
             hide("info_button"))
    })
    
    
#####################  
###Add some more info
#####################  
    output$more_info <- renderUI(HTML(more_info))
    
}

############
###SHINY APP
############
shinyApp(ui=ui, server=server)




