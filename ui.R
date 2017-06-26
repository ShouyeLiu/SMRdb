##------------------------------------------------------------------------------------------------
# SMRdb ui.R
# Author: Jenny Whitehead Pavlides - CNSG
# Date Written: February 2016
#
# Updates:
# 2016-05-17 QQ plots removed - J Whitehead
# 2016-05-18 Regional Association plots functionality added - J Whitehead
# 2016-05-29 Include help text on input fields; order gene list alphabetically - J Whitehead
# 2016-07-29 Update citation as name of paper changed - J Whitehead
#
##-----------------------------------------------------------------------------------------------

# Initialize the trait and gene input selections. These are initally set to the full 
# list of available traits/diseases or genes.

#traitData <- unique(as.character(dat$Trait))
#geneData <- unique(as.character(dat$Gene))


shinyUI(fluidPage(
  
  # display the title for the page
  
  fluidRow(h1(strong(img(src = "uq_pctg.png", height = 75, width = 360),"     SMR Results Database",img(src = "uq_imb_logo.png", height = 75, width = 270,align="right")))),
  
  sidebarLayout(
      
    # Create a sidebar panel in the UI for selectInputs
      sidebarPanel(
        helpText("The following data retrieval options are available: 
                 trait only; gene only or trait and gene."),
                 
        
      # Trait or Disease  
      #  selectInput("Trait",
      #     "Trait or Disease",
      #     c("",
      #       traitData)),
        selectizeInput("Trait","Trait or Disease",choices=traitData,
                       options=list(placeholder="Select a trait/disease",
                                    onInitialize = I('function() { this.setValue(""); }'),
                                    maxOptions=100),multiple=FALSE),
        
      # Genes   
      #  selectInput("Gene",
      #                 "Gene",
      #                 c("", geneData)),
        selectizeInput("Gene","Gene",choices=geneData,
                       options=list(placeholder="Select a gene",
                                    onInitialize = I('function() { this.setValue(""); }'),
                                    maxOptions=5164),multiple=FALSE),
      
      helpText("Please also take note of the default search threshholds. 
                 To return all results for one of the above options, please set p_SMR to 1 and p_HET to 0."),
      
   # p_SMR threshold
         numericInput("pSMRThreshold",
                        "p_SMR Threshold (< 8.4e-06)",
                        8.4e-06, min = 0, max = 1, step = 0.000001),
   
   # p_HEIDI threshold
         numericInput("pHETThreshold",
                       "p_HET Threshold (≥ 0.05)",
                       0.05, min = 0, max = 1, step = 0.01),
   
   
#          actionButton("loadData", "Get Data")), top = 75, width = 200),
          actionButton("loadData", "Get Data"),
          width = 3
      ),
  
  # Main panel  
      mainPanel(
          tabsetPanel(
#                tabPanel("Results Table", DT::dataTableOutput("data", width = 1000)
             tabPanel("Results Table", 
                      wellPanel(DT::dataTableOutput("data")),                        
                      
                      wellPanel(
                           p("Download the results as a .csv file."),
                           p("Note: To download the full set of SMR results for a given trait, please set p_SMR to 1 and p_HET to 0."),
                           downloadButton('downloadData','Download Data')
                         )
                      ),
             
                tabPanel("Manhattan Plot", 
                         wellPanel(plotOutput("manhattan"), 
                         br(),
                         textOutput("manplot"),
                         br()
##------- 2016-05-17 Removed qq plots as felt not needed -------##                                   
#                                   plotOutput("qq"),
#                                   br(),
#                                   textOutput("qqplot"))
                          )
                      ),

##------- 2016-05-18 Added Regional Association Plots -------##

               tabPanel("Regional Association Plots",
                         wellPanel(
                           p("Regional Association plots are only provided for those probes that pass both the SMR (p-value < 8.4e-06) and
                             the HEIDI (p-value ≥ 0.05) test.The upper plot of the figure shows the p-value for SNPs from the latest GWAS
                             for the given trait (brown dots). The diamonds represent the p-values of probes from the SMR analysis. 
                             Highlighted in red is the probe(s) of interest that passed the SMR and HEIDI tests. The lower plot of the figure
                             shows the eQTL p-values of SNPs from the Westra study for the probe of interest.")
                         ),
                         wellPanel(
                            DT::dataTableOutput("regassoc")
                        )

                ),
                tabPanel("GWAS information",
                         wellPanel(
                           p(strong("Trait or Disease")),
                           textOutput("TraitDef"),
                           br(),
                           p(strong("Publication")),
                           textOutput("Author"),
                           htmlOutput("Publication"),
                           textOutput("Journal"),
                           br(),
                           p(strong("Data downloaded from: ")),
                           htmlOutput("Url"))
                         ),
                tabPanel("SMR Overview", 
                        wellPanel(
                          p("Summary-based-results Mendelian Randomization (SMR) is a method
                            that applies the principles of mendelian randomization to identify
                            genes whose expression levels are associated with a complex trait 
                            or disease because of pleitropy using summary data from GWAS and eQTL
                            studies (Zhu et al. 2016, Nature Genetics). This method can be used to
                            prioritize genes at the GWAS loci for follow-up functional studies. The
                            method has been implemented in a user-friendly software tool which is
                            freely available at",
                            a(href="http://www.cnsgenomics.com/software/smr/","http://cnsgenomics.com/software/smr/",target="_blank"))
                        )
                        ),
                tabPanel("Citation and Contact Information",
                        wellPanel(
                           p(strong("SMR method and software tool:")),
                           p("Zhu Z, Zhang F, Bakshi A, Robinson MR, Powell J, Montgomery G, Goddard ME, Wray NR, Visscher PM, Yang J (2016)
                             Integration of summary data from GWAS and eQTL studies predict complex trait gene targets.",
                             a(href="http://www.nature.com/ng/journa;/vaop/ncurrent/full/ng.3538.html", "Nature Genetics, 48:481-7.", target="_blank")),
                           p(strong("SMR Database:")),
                           p("Jennifer M Whitehead Pavlides, Zhihong Zhu, Jacob Gratten, Allan F. McRae, Naomi R. Wray and Jian Yang (2016)
                              Predicting gene targets from integrative analysis of summary data from GWAS and eQTL studies for 28 human complex traits.",
                              a(href="http://www.ncbi.nlm.nih.gov/pmc/articles/PMC4979185/","Genome Medicine, 8:84", target="_blank"))),
                           
                        wellPanel(
                          p("The SMRdb query tool was developed by Jennifer Whitehead Pavlides. 
                            Please direct all queries or issues to Jennifer Whitehead Pavlides (j.pavlides@uq.edu.au) or ", a(href="https://scholar.google.com.au/citations?hl=en&user=aLuqQs8AAAAJ&view_op=list_works&sortby=pubdate", "Jian Yang", target="_blank"), " (jian.yang@uq.edu.au)")
                        ))), width = 9

           
      ))
 
)

)