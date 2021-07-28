#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("shiny")
#install.packages("shinyBS")
#install.packages("shinyjs")

options(shiny.sanitize.errors = FALSE)

library("shiny")
library("shinyBS")
library("shinyjs")
source("server.R")

# Define UI for application that calculate sample size for MRT with Flexible Designs
shinyUI(fluidPage(
    # Application title
    titlePanel(HTML("<strong>FlexiMRT-SS Calculator</strong>: A Sample Size Calculator for the Micro-Randomised Trials with Flexible Designs"), 
               windowTitle = "FlexiMRT-SS Calculator"),    
    useShinyjs(),
    
    ### Introduction of FlexiMRT-SS Calculator on left-hand side panel ###
    sidebarPanel(
        includeHTML("www/sidebar.Rhtml")
    ),
    
    ### Main Panel on the right-hand side ###
    mainPanel(
        tags$hr(),
        
        ### Study Setup ###
        
        h3("Number of Decision Time Point"),
        verticalLayout(
          numericInput("days",label = "Number of days for the study period",value = 180),
          numericInput("occ_per_day",label = "Number of decision time points per day",value = 1)
        ),                                  
        tags$hr(),
        
        h3("Number of Intervention (Not Control) Message Category"),
        verticalLayout(
            numericInput("messages_start",label = "Added at the first day of the study", value = 3 ),
            numericInput("messages_mid",label = "Added at the halfway in days of the study period", value = 1 )
        ),                                  
        tags$hr(),
        
        #### time-varying randomization probability ####
        h3("Randomization Probability"), 
        tabsetPanel(id = "ranPro",
                    
         tabPanel("Constant",
         br(),
                    
        verticalLayout(
            
            radioButtons(inputId="randomization_probability_choices", label = "Which of the randomization probability strategies would you like to use?", 
                         choices=list("Constant Control Probability"="choice_constant_probability_control", "Uniform Random Probability"="choice_uniform_random"),
                         selected = "choice_uniform_random"),
            
            ### type in the desired power if you want to calculate the sample size ###
            conditionalPanel(condition="input.randomization_probability_choices=='choice_constant_probability_control'
                             ",
                             sliderInput("prob.arm",label="Randomisation probability for either intervention message level at each decision time point ",min = 0, max = 1,value = 0.4)
                             
            )
            
        )#,
        
         ),
        
        tabPanel("Time-varying",
                 br(),
                 
                 fluidRow(helpText(textOutput("timevar_prob_text"))),
                 
                 fluidRow(
                     column(4,
                            
                            #conditionalPanel(condition="input.numbers =='re_dec'",
                                             fileInput('file1', 'Choose a .csv file containing time-varying randomization probabilities to upload',
                                                       accept = c('.csv')
                                             ),
                            bsButton("file.resetbutton", "Reset file upload", style = "link")
                            ),
                     
                     column(4,
                            ### downloading the sample file 
                            p("If you would like to use a template, you can download one here:"),
                            downloadButton("timevar_prob_template", "Download Template"),
                            br(),
                            textOutput("download_template_caption")
                     )
                 
        )
        
        )
        ),
        tags$hr(),
        
        #### Expected Availability ####
        h3("Expected Availability"),
        verticalLayout(
            ### Three patterns of expected availability to choose from 
            ### quadratic, constant and linear
            selectizeInput("tau_shape", label = "Select one of the following patterns for the expected availability", 
                           choices=list("Quadratic" = "quadratic","Linear"="linear","Constant"="constant"),
                           options = list(
                               placeholder = "Please select a pattern",
                               onInitialize = I('function() { this.setValue(0); }')
                           )
            ),
            ### Inputs for constant pattern of expected availability ###
            conditionalPanel(condition="input.tau_shape =='constant' ",
                             sliderInput("tau_constant_mean",label="Average of Expected Availability",min = 0, max = 1,value = 1)),
            
            ### Inputs for linear pattern of expected availability ###
            conditionalPanel(condition="input.tau_shape == 'linear' ",
                             sliderInput("tau_linear_mean",label="Average of Expected Availability",min = 0, max = 1,value = 0.5),
                             sliderInput("tau_linear_initial", label = "Initial Value of Expected Availability",
                                         min = 0, max = 1,value = 0.2)),
            
            ### Inputs for quadratic pattern of expected availability ###
            conditionalPanel(condition="input.tau_shape == 'quadratic' ",
                             sliderInput("tau_quadratic_mean",label="Average of Expected Availability",min = 0, max = 1,value = 0.5),
                             sliderInput("tau_quadratic_initial", label = "Initial value of Expected Availability",min =0, max = 1, value = 0.7),
                             numericInput("tau_quadratic_max", label = "Changing Point of Availability", value = 28)),
            
            ### Comments on constant pattern of expected availability ###
            conditionalPanel(condition="input.tau_shape == 'constant'",
                             p(em("Notes: ")," A simplistic constant availability pattern.")
                             
            ),
            ### Comments on linear pattern of expected availability ###
            conditionalPanel(condition="input.tau_shape == 'linear'",
                             p(em("Notes: "), "A linearly increasing pattern of expected availability might be used if participants
                                will find the intervention useful and thus more likely to turn the intervention on"),
                             p("A linearly decreasing pattern of expected availability might be used if participants learn more about the intervetion
                                and get bored through the course of the study and thus getting less likely to turn on the invervention.")
            ),
            ### Comments on quadratic pattern of expected availability ###
            conditionalPanel(condition="input.tau_shape == 'quadratic'",
                             p(em("Notes: "),"A quadratic pattern of availability. Here the changing point of availability refers to day of either maximal of minimal availability,
                                depending on the input values of initial and average availability")
            )
            
        ),
        tags$hr(),
        
        #### Specifying trend for Proximal Effect ####
        h3("Proximal Effect"),
        tabsetPanel(id = "proEff",
                    
        tabPanel("Category-same",
        br(),
        #verticalLayout(
            ### Four trends of proximal effect to choose from
            ### quadratic, constant, linear, linear then constant
            selectizeInput("beta_shape", label = "Select one of the following trends for the proximal effect", 
                           choices=list("Quadratic" = "quadratic",
                                        "Linear"="linear","Constant"="constant","Linear then Constant"="linear and constant"),
                           options = list(
                               placeholder = "Please select a trend",
                               onInitialize = I('function() { this.setValue(0); }')
                           )
            ),
            ### Inputs for quadratic trend of proximal effect ###
            conditionalPanel(condition="input.beta_shape =='quadratic'",
                             sliderInput("beta_quadratic_mean",label="The standardised effect size or margin of error of average proximal effect",min = 0, max = 1,value = 0.1),
                             numericInput("beta_quadratic_max", label = "Day of maximal proximal effect", value = 28),
                             numericInput("beta_quadratic_initial", label = "The standardized effect size or margin of error of initial proximal effect",value = 0.01),
                             p(em("Notes"),": The quadratic form of a proximal effect might be used if you expect that 
                                                        initially participants will enthusiastically engage in the apps and thus the 
                                                         proximal effect will get higher. Then, as the study goes on, some participants are 
                                                         likely to disengage or begin to ignore the activity suggestions and hence a downward trend.")
            ),
            ### Inputs for constant trend of proximal effect ###
            conditionalPanel(condition="input.beta_shape =='constant'",
                             numericInput("beta_constant_mean", label = "The standardized effect size or margin of error of average proximal effect",min = 0, max = 1, value = 0.1),
                             p(em("Notes"),": The proximal effect stays constant over the study.")
            ),
            ### Inputs for linear trend of proximal effect ###
            conditionalPanel(condition="input.beta_shape == 'linear' ",
                             sliderInput("beta_linear_mean",label="The standardized effect size or margin of error of average proximal effect",min = 0, max = 1,value = 0.1),
                             numericInput("beta_linear_initial", label = "The standardized effect size or margin of error of initial proximal effect",value = 0.01),
                             p(em("Notes"),": The linearly increasing form of a proximal effect may be used if participants
                                                         will get more enthusiastically engage in the apps and thus the proximal effect will increase as the 
                                                         study goes."),
                             p("The linearly decreasing form of a proximal effect may be used if participants
                                                        are likely to disengage the activity suggestionss and thus the proximal effect will decrease as the 
                                                         study goes.")
            ),
            ### Inputs for linear then constant trend of proximal effect ###
            conditionalPanel(condition="input.beta_shape =='linear and constant'",
                             sliderInput("beta_linearconst_mean",label="The standardized effect size or margin of error of average proximal effect",min = 0, max = 1,value = 0.1),
                             numericInput("beta_linearconst_max", label = "Day of maximal proximal effect", value = 28),
                             numericInput("beta_linearconst_initial", label = "The standardized effect size or margin of error of initial proximal effect",value = 0.01),
                             p(em("Notes"),": The linear then constant form of a proximal effect might be used, e.g., if you expect that 
                                                         participants will be benefit from the messages
                                                         and thus the proximal effect will get higher until reach maximum value then maintain it for the rest of the
                                                         study period.")
            )
            
        #)
        ),
        tabPanel("Category-varying",
                 br(),
                 ### Four trends of proximal effect to choose from
                 ### quadratic, constant, linear, linear then constant
                 selectizeInput("beta_shape_var", label = "Select one of the following trends for the proximal effect", 
                                choices=list("Quadratic" = "quadratic",
                                             "Linear"="linear","Constant"="constant","Linear then Constant"="linear and constant"),
                                options = list(
                                    placeholder = "Please select a trend",
                                    onInitialize = I('function() { this.setValue(0); }')
                                )
                 ),
                 
                 fluidRow(
                     column(4,
                            fileInput('beta_var', 'Choose a .csv file, where each row represents an intervention category, 
                                             containing the category-vary standardized effect size or margin of error of average and initial proximal effects (min=0 and max=1), 
                                             and the day that average proximal effect reaching its maximum or minimum to upload',
                                      accept = c('.csv')
                            ),
                            bsButton("file_beta_var.resetbutton", "Reset file upload", style = "link")
                     ),
                     column(4,
                            ### downloading the sample file 
                            p("If you would like to use a template, you can download one here:"),
                            downloadButton("catvar_beta_template", "Download Template"),
                            br(),
                            textOutput("download_template_caption_beta_var")
                     )
                     
                 ),
                 p(em("Notes"),": The linear then constant form of a proximal effect might be used, e.g., if you expect that 
                                                         participants will be benefit from the messages
                                                         and thus the proximal effect will get higher until reach maximum value then maintain it for the rest of the
                                                         study period.")
                 
                 ### Inputs for quadratic trend of proximal effect ###
                 
                 
                 
                 ### Inputs for constant trend of proximal effect ###
                 
                 
                 ### Inputs for linear trend of proximal effect ###
                 
                 
                 ### Inputs for linear then constant trend of proximal effect ###
                 #conditionalPanel(condition="input.beta_shape_var =='linear and constant'",
                                  
                 #)     
        
        )
        ),
        tags$hr(),
        
        #### Specifying sample size calculation method, i.e. power or confidence interval ####
        h3("Sample Size Calculation Method"),
        verticalLayout(
            selectizeInput(inputId="method", label="Select a Sample Size Method", 
                           choices=list("Power"="power", "Precision"="confidence interval") 
                           )
        ),
        tags$hr(),
        
        #### Specifying distribution type of test statistics ####
        h3("Test Statistics Distribution Type"),
        verticalLayout(
            selectizeInput(inputId="test", label="Distribution Type", 
                           choices=list(
                               "Hotelling T2 N-q-1"="hotelling N-q-1",
                                        "Hotelling T2 N"="hotelling N", 
                                        "Hotelling T2 N-1"="hotelling N-1", 
                                        "Chi-square"="chi"
                                        ) 
            )
        ),
        tags$hr(),
        
        ### Specifying whether you are interested in calculating the sample size, power or coverage probability ###
        h3("Result"),
        verticalLayout(
            ### Choices to choose from "sample size", "power" or "coverage probability" ###
            radioButtons(inputId="result_choices", label = "Are you interested in finding sample size, power or coverage probability?", 
                         choices=list("Sample Size"="choice_sample_size","Power"="choice_power", "Coverage Probability"="choice_coverage_probability"),
                         selected = "choice_sample_size"),
            
            ### type in the desired power if you want to calculate the sample size ###
            conditionalPanel(condition="input.result_choices=='choice_sample_size'
                             && input.method=='power'
                             ",
                             numericInput(inputId="power", label = HTML("Desired Power"), value = 0.8)
            ),
            
            ### Note that the desired precision to calculate the sample size can be obtained from the level of significance, i.e. 1-sigLev ###
            
            
            ### type in the sample size if you want to calculate the power attained ###
            conditionalPanel(condition="input.result_choices=='choice_power'
                             && input.method=='power'
                             ",
                             numericInput("sample_size_power", label = HTML("Number of Participants"), value = 50)
            ),
            
            ### type in the sample size if you want to calculate the precision attained ###
            conditionalPanel(condition="input.result_choices=='choice_coverage_probability'
                             && input.method=='confidence interval'            
                             ",
                             numericInput("sample_size_precision", label = HTML("Number of Participants"), value = 50)
                             
            ),
            
            numericInput(inputId="sigLev", label = HTML("Significance Level"), value = 0.05),
            
            ### Output warnings if you type in wrong format for "desired power" ###
            conditionalPanel(condition="input.result_choices=='choice_sample_size'
                             && input.method=='power'
                             ",
                             textOutput("choice_power_sample_size_warning")
            ),
            
            
            
            ### Output warnings if you type in wrong format for "Number of participants" ###
            conditionalPanel(condition="input.result_choices=='choice_power'",
                             textOutput("choice_power_warning")
            ),
            
            conditionalPanel(condition="input.result_choices=='choice_coverage_probability'",
                             textOutput("choice_coverage_probability_warning")
            ),
            
            ### Output warnings if you type in wrong format for "Significance level" ###
            textOutput("significance_warning"),
            
            #### choice to calculate sample size (action buttons) ####
            actionButton("size_power_cp","Get Result"),
            uiOutput("result_size_power_cp")
            
        ),
        tags$hr(),
        
        ##### Action Button to Obtain the Results #####
        
        
        
        
        
        ### Output warnings if you didn't choose any patterns for expected availability ###
        conditionalPanel(condition="input.tau_shape !='constant'
                                               && input.tau_shape != 'quadratic'
                                               && input.tau_shape != 'linear'",
                         uiOutput("result_warning_tau")
        ),
        
        ### Output warnings if you didn't choose any patterns for proximal treatment effect ###
        conditionalPanel(condition="input.beta_shape !='constant'
                                               && input.beta_shape != 'linear'
                                               && input.beta_shape != 'quadratic'
                                               && input.beta_shape != 'linear and constant",
                         uiOutput("result_warning_beta")
        ),
        
        ### Output warnings if you didn't choose any methods for sample size calculation ###
        conditionalPanel(condition="input.method !='power'
                                               && input.method != 'confidence interval'",
                         uiOutput("result_warning_method")
        ),
        
        ### Output warnings if you didn't choose any distribution types for test statistics ###
        conditionalPanel(condition="input.test !='chi'
                                               && input.test != 'hotelling N'
                                               && input.test != 'hotelling N-1'
                                               && input.test != 'hotelling N-q-1",
                         uiOutput("result_warning_test")
        ),
        
       
    
       
        
        #### A TOOLTIPS: to give you tips for typeing in numeric inputs. ####
        
        bsTooltip(id = "days", title = "integer greater than 0",placement="right", trigger = "focus"),
        bsTooltip(id = "occ_per_day", title = "integer greater than 0",placement="right", trigger = "focus"),
        bsTooltip(id = "messages_start", title = "integer greater than or equal to 0",placement="right", trigger = "focus"),
        bsTooltip(id = "messages_mid", title = "integer greater than or equal to 0",placement="right", trigger = "focus"),
        bsTooltip(id = "sigLev", title = "Input must range from 0-1" ,placement="right", trigger = "focus"),
        bsTooltip(id = "power", title = "Input must range from 0-1" ,placement="right", trigger = "focus"),
        bsTooltip(id = "beta_quadratic_initial", title = "Input must range from 0-1, and less than or equal to Average Standardized Effect",placement="right", trigger = "focus"),
        bsTooltip(id = "beta_quadratic_max", title = "Input must be integer greater than or equal to 0",placement="right", trigger = "focus"),
        bsTooltip(id = "beta_linear_initial", title = "Input must range from 0-1",placement="right", trigger = "focus"),
        bsTooltip(id = "beta_linearconst_initial", title = "Input must range from 0-1, and less than or equal to Average Standardized Effect",placement="right", trigger = "focus"),
        bsTooltip(id = "beta_linearconst_max", title = "Input must be integer greater than or equal to 0",placement="right", trigger = "focus"),
        bsTooltip(id = "tau_quadratic_max", title = "Input must be integer greater than or equal to 0",placement="right", trigger = "focus"),
        bsTooltip(id = "sample_size_power", title = "Input must be integer greater than or equal to 1",placement="right", trigger = "focus"),
        bsTooltip(id = "sample_size_precision", title = "Input must be integer greater than or equal to 1",placement="right", trigger = "focus"),
        
        collapsable = TRUE,
        footer = HTML("<p style = 'font-size:12px'> Please direct correspondence to kenny.xu@duke-nus.edu.sg</a></p>")
        
        
    ),
        
)
)
