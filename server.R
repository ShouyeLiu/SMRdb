##-----------------------------------------------------------------------------------------------------------
# SMRdb server.R
# Author: Jenny Whitehead (Pavlides)
# Date written: February 2016
#
# Updates:
# 2016-05-17 QQ plots removed - J Whitehead
# 2016-05-18 Regional Association plots functionality added - J Whitehead
# 2016-07-26 Regional Association refs code changed so that plot opens in new tab - J Whitehead
#
##------------------------------------------------------------------------------------------------------------

shinyServer(function(input,output,session){
  
# SMR Results Table
  
   smrdata <- reactive({
     input$loadData
     isolate(
       if (input$Trait != "" & input$Gene != "") {
         smrdata <- subset(dat,
                           dat$Trait == input$Trait & 
                             dat$Gene == input$Gene &
                             as.numeric(dat$p_SMR) < input$pSMRThreshold &
                             as.numeric(dat$p_HET) >= input$pHETThreshold)
       }  else {
         if (input$Trait == "") {
           smrdata <- subset(dat, dat$Gene == input$Gene &
                               as.numeric(dat$p_SMR) < input$pSMRThreshold &
                               as.numeric(dat$p_HET) >= input$pHETThreshold)
         } else {
           smrdata <- subset(dat, dat$Trait == input$Trait &
                               as.numeric(dat$p_SMR) < input$pSMRThreshold &
                               as.numeric(dat$p_HET) >= input$pHETThreshold) 
           
           
         }
       }
       
       
     )
     smrdata <- smrdata[order(p_SMR),]
     return(smrdata)
   })
    
  output$data <- DT::renderDataTable(
    smrdata(), rownames = FALSE, options = list(scrollX = TRUE),selection='none')
  
# Download handler to download the data to a .csv file
  output$downloadData <- downloadHandler(
    filename = paste0(Sys.Date(),"_SMRdata.csv"),
    content = function(file) {
      write.csv(smrdata(),file,row.names=F)
    }
  )
  
# Manhattan and QQ Plots
  
  output$manhattan <- renderPlot({
    input$loadData
    isolate(
      if (input$Trait != "") {
        manhattan(subset(dat, dat$Trait == input$Trait), chr = "ProbeChr", bp = "Probe_bp", p = "p_SMR", snp = "SNP",
                                           main = paste("Manhattan plot for",input$Trait),col=c("blue4","orange"),
                                           suggestiveline = -log10(8.4E-06), genomewideline = FALSE)
        }
      )
    })
  
  output$manplot <- renderText("Figure 1: Manhattan Plot of the SMR analysis results showing the -log10 p-value from the SMR test")
  
##------- 2016-05-17 Removed qqplot functionality -------##
#  output$qq <- renderPlot(qq(dat$p_SMR[dat$Trait == input$Trait], main = paste("QQ Plot for", input$Trait)))
#  output$qqplot <- renderText("Figure 2: QQ plot of the p_SMR values")
##-------------------------------------------------------##  
  
##------- 2016-05-18 Regional Association plots added -------##
##------- 2016-07-26 refs code changed so that plots open in new tab
# Regional Assocation Plots
  
  output$regassoc <- DT::renderDataTable({
    input$loadData
    isolate(
      if (input$Trait != "") {
          regassocplots <- subset(regassocdat,regassocdat$Trait == input$Trait)
          if (nrow(regassocplots)) {
 #        refs <- paste0("<a href=",regassocplots$Filename,">",regassocplots$Filename,"<a>") - changed 27/07/2016 so that plot opens in new tab
          refs <- paste0("<a href='",regassocplots$Filename,"'target='_blank'>",regassocplots$Filename,"<a>")  
          regassoc <- data.frame(Trait = regassocplots$Trait, Probe = regassocplots$Probe, Gene = regassocplots$Gene,
                           Chr = regassocplots$Chr, Association_Plot = refs)
    }})
  },escape=FALSE,rownames=FALSE,selection='none')
  
  
  
  
# GWAS Information
  
  output$TraitDef <- renderText(paste(gwaspubdat$Description[gwaspubdat$Trait == input$Trait]))
  output$Author <- renderText(paste(gwaspubdat$Author[gwaspubdat$Trait == input$Trait]))
  output$Publication <- renderText(HTML(paste0("<a href = '",gwaspubdat$Link[gwaspubdat$Trait == input$Trait],"'target='_blank'>",
                                               gwaspubdat$Title[gwaspubdat$Trait == input$Trait],"</a>")))
  output$Journal <- renderText(paste(gwaspubdat$Journal[gwaspubdat$Trait == input$Trait]))
  output$Url <- renderText(HTML(paste0("<a href = '",gwaspubdat$URL[gwaspubdat$Trait == input$Trait],"'target='_blank'>", 
                                 gwaspubdat$URL[gwaspubdat$Trait == input$Trait],"</a>")))
  

  
  session$onSessionEnded(function() {
    stopApp()
  })

  })