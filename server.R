#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("SampleSizeMLMRT.R")

# Define server logic required to draw a histogram
shinyServer(
  function(input, output) {
    
  ### generating the warnings for numeric input: "Desired Power" ###
  output$choice_power_sample_size_warning <-renderText(
    {      
    validate(
      need(input$power >= 0,"Error: Please specify the power greater than or equal to 0"), 
      need(input$power <= 1,"Error: Please specify the power less than or equal to 1")
    )
  }
  )
  
  ### generating the warnings for numeric input: "Significance level" ###
  output$significance_warning <-renderText(
    {      
      validate(
        need(input$sigLev >= 0,"Error: Please specify the level of significance greater than or equal to 0"), 
        need(input$sigLev <= 1,"Error: Please specify the level of significance less than or equal to 1")
      )
    }
  )
  
  ### generating the warnings for numeric input: "Number of Participants for Power Calculation" ###
  output$choice_power_warning <-renderText(
    {      
    validate(
      need(input$sample_size_power > 0,"Error: Please specify Number of Participants greater than 0"), 
      need(input$sample_size_power == round(input$sample_size_power),"Error: Please enter integer value for Number of Participants")
    )
  }
  )
  
  ### generating the warnings for numeric input: "Number of Participants for Coverage Probability Calculation" ###
  output$choice_coverage_probability_warning <-renderText(
    {      
    validate(
      need(input$sample_size_precision > 0,"Error: Please specify Number of Participants greater than 0"), 
      need(input$sample_size_precision == round(input$sample_size_precision),"Error: Please enter integer value for Number of Participants")
    )
  }
  )
  
  ### generating warnings if you don't choose a pattern of expected availability. ###
  output$result_warning_tau<- renderUI({
    validate(
      need(input$tau_shape == "constant"
           || input$tau_shape =="linear"
           || input$tau_shape == "quadratic",
           "Please select a pattern for expected availability")
    )
  })
  
  ### generating warnings if you don't choose a trend of proximal treatment effect. ###
  output$result_warning_beta <- renderUI({
    validate(
      need(input$beta_shape == "constant"
           || input$beta_shape =="linear"
           || input$beta_shape == "quadratic"
           || input$beta_shape == "linear and constant",
           "Please select a pattern for proximal treatment effect")
    )
  })
  
  ### generating warnings if you don't choose any methods for sample size calculation. ###
  output$result_warning_method <- renderUI(
    {
    validate(
      need(input$method == "power" 
           || input$method == "confidence interval",
           "Please select a sample size calulation method")
    )
  }
  )
  
  ### generating warnings if you don't choose any distribution types for test statistics. ###
  output$result_warning_test <- renderUI(
    {
      validate(
        need(input$test == "chi"
             || input$test == "hotelling N"
             || input$test == "hotelling N-1"
             || input$test == "hotelling N-q-1",
             "Please select a distribution type for the test statistics")
      )
    }
  )
  
  ### Generate the current result of sample size 
  ### Constant trend of expected availability
  ### Linear then constant trend of proximal treatment effect
  ### Power method
  ### Hotelling t-square type of distribution for the test statistics with 
  ### denominator degree of freedom N 
  Result_size_power_cp <- eventReactive(input$size_power_cp, 
                               {  ### Generate this current result of sample size if the corresponding 
                                  ### action button is pressed
                                 
    days = input$days
    occ_per_day = input$occ_per_day
    messages_start = input$messages_start
    messages_mid = input$messages_mid
    aa_each = c( messages_start, messages_mid )
    aa_day = c( 1, ( floor(days/2) + 1 ), days ) 
    aa_freq = length( aa_day ) - 1;
    aa_day_aa = rep( ( aa_day )[ 1:aa_freq ], aa_each )
    
    tau_shape = input$tau_shape
    if( tau_shape == "quadratic"  )
    {
      tau_mean = input$tau_quadratic_mean
      tau_initial = input$tau_quadratic_initial
      tau_quadratic_max = input$tau_quadratic_max
    }
    else if( tau_shape == "linear"  )
    {
      tau_mean = input$tau_linear_mean
      tau_initial = input$tau_linear_initial
      tau_quadratic_max = days
    }
    else if( tau_shape == "constant"  )
    {
      tau_mean = input$tau_constant_mean
      tau_initial = input$tau_constant_mean
      tau_quadratic_max = 1
    }
    else{ stop( "Error: Please specify a pattern for the expected availability" ) }
    
    beta_shape = input$beta_shape
    if( beta_shape == "linear and constant" ){
      beta_mean = rep( input$beta_linearconst_mean, sum( aa_each ) )
      beta_initial = rep( input$beta_linearconst_initial, sum( aa_each ) )
      beta_quadratic_max = aa_day_aa - 1 + input$beta_linearconst_max
    }
    else if( beta_shape == "quadratic"  )
    {
      beta_mean = rep( input$beta_quadratic_mean, sum( aa_each ) )
      beta_initial = rep( input$beta_quadratic_initial, sum( aa_each ) )
      beta_quadratic_max = aa_day_aa - 1 + input$beta_quadratic_max
    }
    else if( beta_shape == "linear"  )
    {
      beta_mean = rep( input$beta_linear_mean, sum( aa_each ) )
      beta_initial = rep( input$beta_linear_initial, sum( aa_each ) )
      beta_quadratic_max = aa_day_aa - 1 + days
    }
    else if( beta_shape == "constant"  )
    {
      beta_mean = rep( input$beta_constant_mean, sum( aa_each ) )
      beta_initial = rep( input$beta_constant_mean, sum( aa_each ) )
      beta_quadratic_max = aa_day_aa - 1 + 1
    }
    else{ stop( "Error: Please specify a trend for the proximal effect" ) }
    
    method = input$method
    test = input$test
    sigLev = input$sigLev
    if( sigLev >= 0 & sigLev <= 1 ){ sigLev = sigLev } 
    else{stop("Please specify the level of significance between range 0 to 1 inclusive")}
    result = input$result_choices
    
    prob.arm = input$prob.arm
    prob.con = 1 - prob.arm
    prob = matrix( 0, days, ( 1 + sum(aa_each) ) )
    prob[ , 1 ] = rep( prob.con, days )
    for( l in 1:( length( aa_day ) - 1 ) )
    {
      prob[ ( aa_day[ l + 0 ] + 0 ):( aa_day[ l + 1 ] - 1 ), 2:( 1 + cumsum( aa_each )[ l ] ) ] = prob.arm/( cumsum( aa_each )[ l ] )
    }
    prob[ ( aa_day[ length( aa_day ) ] ), 2:( 1 + sum( aa_each ) ) ] = prob.arm/( sum( aa_each ) )
    
    if( method == "power" )
    {
      if( result == "choice_sample_size" )
      {
        pow = input$power
        if( pow >= 0 & pow <= 1 ){ pow = pow } 
        else{stop("Please specify the power between range 0 to 1 inclusive")}
        
        MRTN <- SampleSize_MLMRT( days=days, occ_per_day=occ_per_day, aa.day.aa = aa_day_aa, prob=prob, beta_shape=beta_shape, beta_mean=beta_mean, beta_initial=beta_initial, 
                                  beta_quadratic_max=beta_quadratic_max, tau_shape=tau_shape, tau_mean=tau_mean, tau_initial=tau_mean, tau_quadratic_max=beta_quadratic_max, 
                                  sigma=1, pow=pow, sigLev=sigLev, method = method, test = test, result = result )
        N <- MRTN$N
        HTML(paste("<h4 style = 'color:blue';> The required sample size is ", N, "to attain", input$power*100,"% power when the significance level is",input$sigLev,".")) 
      }
      else if( result == "choice_power" )
      {
        N <- input$sample_size_power
        SS <- N
        MRTN <- SampleSize_MLMRT( days=days, occ_per_day=occ_per_day, aa.day.aa = aa_day_aa, prob=prob, beta_shape=beta_shape, beta_mean=beta_mean, beta_initial=beta_initial, 
                                 beta_quadratic_max=beta_quadratic_max, tau_shape=tau_shape, tau_mean=tau_mean, tau_initial=tau_mean, tau_quadratic_max=beta_quadratic_max, 
                                 sigma=1, pow=0.8, sigLev=sigLev, method = method, test = test, result = result, SS =SS )  
        pow <- round( MRTN$P, digits = 2 )
        HTML(paste("<h4 style = 'color:blue';> The sample size", N, "gives", pow*100,"% power when the significance level is",input$sigLev,".")) 
      }
      else{ stop( paste( "Define the correct result, i.e. sample size calculation or power calculation" ) ) }
    }
    else if( method == "confidence interval" )
    {
      if( result == "choice_sample_size" )
      {
        pow=0.8
        MRTN <- SampleSize_MLMRT( days=days, occ_per_day=occ_per_day, aa.day.aa = aa_day_aa, prob=prob, beta_shape=beta_shape, beta_mean=beta_mean, beta_initial=beta_initial, 
                                  beta_quadratic_max=beta_quadratic_max, tau_shape=tau_shape, tau_mean=tau_mean, tau_initial=tau_mean, tau_quadratic_max=beta_quadratic_max, 
                                  sigma=1, pow=pow, sigLev=sigLev, method = method, test = test, result = result)
        N <- MRTN$N
        CP <- ( 1 - sigLev )*100
        HTML(paste("<h4 style = 'color:blue';> The required sample size is ", N, "to attain", CP,"% coverage probability.")) 
      }
      else if( result == "choice_coverage_probability" )
      {
        N <- input$sample_size_precision
        SS <- N
        MRTN <- SampleSize_MLMRT( days=days, occ_per_day=occ_per_day, aa.day.aa = aa_day_aa, prob=prob, beta_shape=beta_shape, beta_mean=beta_mean, beta_initial=beta_initial, 
                                  beta_quadratic_max=beta_quadratic_max, tau_shape=tau_shape, tau_mean=tau_mean, tau_initial=tau_mean, tau_quadratic_max=beta_quadratic_max, 
                                  sigma=1, pow=0.8, sigLev=sigLev, method = method, test = test, result = result, SS = SS )
        CP <- round(MRTN$CP, digits = 2)  
        HTML(paste("<h4 style = 'color:blue';> The sample size", N, "obtains", CP*100,"coverage probability.")) 
      }
      else{ stop( paste( "Define the correct result, i.e. sample size calculation or coverage probability calculation" ) ) }
    }
  
  }
  )
  
  output$result_size_power_cp <- renderUI(
    {
    validate(
      need(input$days == round(input$days),"Error: Please enter an integer value for duration of the study"),
      need(input$days > 0 ,"Error: Please specify the duration of the study greater than 0"),
      
      need(input$occ_per_day == round(input$occ_per_day),"Error: Please enter an integer for the number of occasions per day"),
      need(input$occ_per_day > 0 ,"Error: Please specify the number of occasions per day greater than 0"),
     
      #need(input$sigLev >= 0,"Error: Please specify the significance level greater than or equal to 0"),
      #need(input$sigLev <= 1,"Error: Please specify the significance Level less than or equal to 1"),
      
      #need(input$power >= 0 ,"Error: Please specify power greater than or equal to 0"),
      #need(input$power <= 1 ,"Error: Please specify the power less than or equal to 1"),
      
      need(input$beta_linearconst_mean > 0,"Error: Please specify an average standardized effect greater than 0"),
      need(input$beta_linearconst_mean <= 1,"Error: Please specify an average standardized effect less than or equal to 1"),
      need(input$beta_linearconst_initial >= 0, "Error: Please specify the standardized Initial Effect greater than or equal to 0"),
      need(input$beta_linearconst_initial <= input$beta_linearconst_mean, "Error: Please specify the standardized Initial Effect less than or equal to average standardized effect"),
      need(input$beta_linearconst_max == round(input$beta_linearconst_max), "Error: Please enter an integer value for the maximum proximal effect day"),
      need(input$beta_linearconst_max >= 1, "Error: Please specify the maximum proximal effect day greater than or equal to 1"),
      need(input$beta_linearconst_max <= input$days, "Error: Please specify the maximum proximal effect day less than or equal to the duration"),
      
      need(input$beta_quadratic_mean > 0,"Error: Please specify an average standardized effect greater than 0"),
      need(input$beta_quadratic_mean <= 1,"Error: Please specify an average standardized effect less than or equal to 1"),
      need(input$beta_quadratic_initial >= 0, "Error: Please specify the standardized Initial Effect greater than or equal to 0"),
      need(input$beta_quadratic_initial <= input$beta_linearconst_mean, "Error: Please specify the standardized Initial Effect less than or equal to average standardized effect"),
      need(input$beta_quadratic_max == round(input$beta_quadratic_max),
           "Error: Please enter an integer value for the maximum proximal effect day"),
      need(input$beta_quadratic_max >= 1, "Error: Please specify the maximum proximal effect day greater than or equal to 1"),
      need(input$beta_quadratic_max <= input$days, "Error: Please specify the maximum proximal effect day less than or equal to the duration"),
      
      need(input$beta_linear_mean > 0,"Error: Please specify an average standardized effect greater than 0"),
      need(input$beta_linear_mean <= 1,"Error: Please specify an average standardized effect less than or equal to 1"),
      need(input$beta_linear_initial >= 0, "Error: Please specify the standardized Initial Effect greater than or equal to 0"),
      need(input$beta_linear_initial <= input$beta_linearconst_mean, "Error: Please specify the standardized Initial Effect less than or equal to average standardized effect"),
      
      need(input$beta_constant_mean > 0,"Error: Please specify an average standardized effect greater than 0"),
      need(input$beta_constant_mean <= 1,"Error: Please specify an average standardized effect less than or equal to 1"),
      
      need(input$tau_quadratic_mean > 0, "Error: Please specify the mean of availability greatert than 0"),
      need(input$tau_quadratic_mean <= 1, "Error: Please specify the mean of availability less than or equal to 1"),
      need(input$tau_quadratic_initial > 0, "Error: Please specify the Initial availability greater than 0"),
      need(input$tau_quadratic_initial >= input$tau_quadratic_mean, "Error: Please specify the Initial availability greater than or equal to the average availability"),
      need(input$tau_quadratic_max == round(input$tau_quadratic_max), "Error: Please enter an integer value for the maximum availability day"),
      need(input$tau_quadratic_max >= 1, "Error: Please specify the maximum availability day greater than or equal to 1"),
      need(input$tau_quadratic_max <= input$days, "Error: Please specify the maximum availability day less than or equal to the duration"),
      
      need(input$tau_linear_mean > 0, "Error: Please specify the mean of availability greatert than 0"),
      need(input$tau_linear_mean <= 1, "Error: Please specify the mean of availability less than or equal to 1"),
      need(input$tau_linear_initial > 0, "Error: Please specify the Initial availability greater than 0"),
      need(input$tau_linear_initial >= input$beta_linear_mean, "Error: Please specify the Initial availability greater than or equal to the average availability"),
      
      need(input$tau_constant_mean > 0, "Error: Please specify the mean of availability greatert than 0"),
      need(input$tau_constant_mean <= 1, "Error: Please specify the mean of availability less than or equal to 1")
    )
    Result_size_power_cp()
  }
  )
  

}
)
