library(shiny)
library(shinythemes)
library(ggplot2)
library(tibble)

server <- shinyServer(function(input, output) {
    
    # How to use this tool
    output$notice <- renderUI({
        
        intro <- "This is a R shiny application developed for verifying Central Limit Theorem."
        
        CLT2 <- "The central limit theorem (CLT) is a statistical theory"
        CLT3 <- "that states that given a sufficiently large sample size from a population with a finite level of variance,"
        CLT4 <- "the mean of all samples from the same population will be approximately equal to the mean of the population."
        CLT5 <- "Furthermore, all of the samples will follow an approximate normal distribution pattern," 
        CLT6 <- "with all variances being approximately equal to the variance of the population divided by each sample's size."
        CLT <- paste(CLT2, CLT3, CLT4, CLT5, CLT6)
        
        note1 <- "For Bernoulli distribution, random variable is binary (either 0 or 1), parameter p is probablility of success in a trial."
        note2 <- "For Poisson distribution, random variable is the number of events in a interval, parameter lambda is the average of that number."
        note3 <- "For Geometric distribution, random variable is the number of failure, parameter p is probablility of success in a trial."
        note4 <- "For Normal distribution, parameter mu denotes population mean, and parameter sigma denotes population standard deviation."
        note5 <- "For Uniform distribution, parameter a denotes the left boundary, and parameter b denotes the right boundary."
        note6 <- "For Gamma distribution, parameter alpha denotes the shape, parameter beta denotes the rate."
        note <- paste(note1, note2, note3, note4, note5, note6)
        
        choice0 <- "The Sidebar Panel is the input panel."
        choice1 <- "Specify the type of distribution, the values of paramters, the size of random sample and the number of simulation"
        choice2 <- "in the Simulation Choice Section to control what simulation you want to run;"
        choice3 <- "Also, specify your preferences of outputs such as the number of bins in the histogram, the color of the histogram bars"
        choice4 <- "and the color of the approxiamtely normal curve in the Outputs Choice Section to control what output you want to see."
        choice5 <- "Notice that inputs in the Simulation Choice Section are delayed interactive,"
        choice6 <- "which means you need to click the update button after those choices to run the application;"
        choice7 <- "Whereas, inputs in the Outputs Choice Section are interactive directly to the plot,"
        choice8 <- "which means once you do not need to click the update button to change the appearance of the plot."
        choice9 <- "If you like, you can download the plot in pdf format using download link in the Plot Download Section."
        side <- paste(choice0, choice1, choice2, choice3, choice4, choice5, choice6, choice7, choice8, choice9)
        
        tab0 <- "The Main Panel is the output panel."
        tab1 <- "There are 5 Tab Panels in this application. This one just give you instructions how to use the application."
        tab2 <- "The 2nd Panel 'Validity of Paramters' checks the validity of parameters for the distribution you choose."
        tab3 <- "Make sure all parameters are valid if you are not familiar with a distribution."
        tab4 <- "The 3rd Panel 'Random Sampling Results' shows the simulated random samples."
        tab5 <- "Each row denotes one simulation, and each column denote one sample."
        tab6 <- "The 4th Panel 'Summary of Sample Mean' gives some statistics of random sample mean."
        tab7 <- "Especially, the actual mean and standard error of the sample mean are expected to be close to theoretical ones."
        tab8 <- "The 5th Panel 'Plot of Sample Mean' shows the histogram of sample mean and aprroximately Normal curve."
        tab <- paste(tab0, tab1, tab2, tab3, tab4, tab5, tab6, tab7, tab8)
        
        text <- paste('<p align="justify">', intro, CLT, note, side, tab, '</p>', sep = '<br/>  <br/>')
        HTML(text)
        
    })
    
    
    # Check validity of the paramters
    output$validity <- renderUI({
        
        if ( input$sample_dist == "Bernoulli" )  {
            if ( !is.na(as.numeric(input$prob) ) & as.numeric(input$prob) > 0 & as.numeric(input$prob) < 1 ) {
                validity <- "The parameter you entered is valid."
            }  else  {
                validity <- "The parameter you entered is invalid.<br/> <br/>
                Probablity must be a numeric value between 0 and 1."
            }
            
            } else if ( input$sample_dist == "Poisson" )  {
                if ( !is.na(as.numeric(input$lambda) ) & as.numeric(input$lambda) > 0 )  {
                    validity <- "The parameter you entered is valid."
                }  else  {
                    validity <- "The parameter you entered is invalid.<br/> <br/>
                    Lambda must be a numeric value greater than 0."
                }
                
                } else if ( input$sample_dist == "Geometric" )  {
                    if ( !is.na(as.numeric(input$prob2) ) & as.numeric(input$prob2) > 0 & as.numeric(input$prob2) < 1 )  {
                        validity <- "The parameter you entered is valid."
                    }  else  {
                        validity <- "The parameter you entered is invalid.<br/> <br/>
                        Probablity must be a numeric value between 0 and 1."
                    }
                    
                    } else if ( input$sample_dist == "Normal" ) {
                        if ( !is.na(as.numeric(input$mu) ) )  {
                            validity_1 <- "The mean parameter you entered is valid."
                        } else {
                            validity_1 <- "The mean parameter you entered is invalid."
                        }
                        if ( !is.na(as.numeric(input$mu) & as.numeric(input$sigma) > 0) ) {
                            validity_2 <- "The standard deviation parameter you entered is valid."
                        } else {
                            validity_2 <- "The standard deviation parameter you entered is invalid."
                        } 
                        validity <- paste(validity_1, validity_2, sep = "<br/> <br/>")
                        
                    } else if ( input$sample_dist == "Uniform" ) {
                        if ( is.na(as.numeric(input$min_a) ) | is.na(as.numeric(input$min_a) ) )  {
                            validity <- "At least one paramter you entered is not numeric."
                        } else if ( as.numeric(input$min_a) >= as.numeric(input$min_a) ) {
                            validity <- "Parameter a must be less than Parameter b."
                        } else {
                            validity <- "The parameter you entered is valid."
                        }
                        
                        
                    } else {
                        if ( !is.na((as.numeric(input$alpha) ) ) )  {
                            validity_1 <- "The mean parameter you entered is valid."
                        } else {
                            validity_1 <- "The mean parameter you entered is invalid."
                        }
                        if ( as.numeric(input$beta) > 0 & !is.na(as.numeric(input$beta) ) ) {
                            validity_2 <- "The standard deviation parameter you entered is valid."
                        } else {
                            validity_2 <- "The standard deviation parameter you entered is invalid."
                        } 
                        validity <- paste(validity_1, validity_2, sep = '<br/> <br/>')
                        
                    }
        
        HTML(validity)
        
    })
    
    
    
    # Return the random sample
    rsample <- eventReactive(input$update, {
        
        if (isolate(input$sample_dist == "Bernoulli") ) {
            rsample <- isolate(rbinom(n = input$sample_size * input$simulation,
                                      size = 1, prob = as.numeric(input$prob) ) )
            
        } else if (isolate(input$sample_dist == "Poisson") )  {
            rsample <- isolate(rpois(n = input$sample_size * input$simulation, 
                                     lambda = as.numeric(input$lambda) ) )
            
        } else if (isolate(input$sample_dist == "Geometric") )  {
            rsample <- isolate(rgeom(n = input$sample_size * input$simulation, 
                                     prob = as.numeric(input$prob2) ) )
            
        } else if (isolate(input$sample_dist == "Normal") ) {
            rsample <- isolate(rnorm(n = input$sample_size * input$simulation, 
                                     mean = as.numeric(input$mu), sd = as.numeric(input$sigma) ) )
            
        } else if (isolate(input$sample_dist == "Uniform") ) {
            rsample <- isolate(runif(n = input$sample_size * input$simulation, 
                                     min = as.numeric(input$min_a), max = as.numeric(input$max_b) ) )
            
        } else {
            rsample <- isolate(rgamma(n = input$sample_size * input$simulation, 
                                      shape = as.numeric(input$alpha), rate = as.numeric(input$beta) ) )
        }
        
        rsample 
        
    })
    
    
    # Calculate mean and variance of original distribution and theoretical mean and variance of sample mean
    ori_stat <- eventReactive(input$update, {
        
        if (isolate(input$sample_dist == "Bernoulli") )  {
            ori_mean <- isolate(as.numeric(input$prob) ) 
            ori_sd <- isolate(sqrt(as.numeric(input$prob) * (1 - as.numeric(input$prob) ) ) )
            
        } else if (isolate(input$sample_dist == "Poisson") )  {
            ori_mean <- isolate(as.numeric(input$lambda) )
            ori_sd <- isolate(sqrt(as.numeric(input$lambda) ) )
            
        } else if (isolate(input$sample_dist == "Geometric") )  {
            ori_mean <- isolate( (1 - as.numeric(input$prob2) ) / as.numeric(input$prob2) ) 
            ori_sd <- isolate(sqrt( (1 - as.numeric(input$prob2) ) / as.numeric(input$prob2)^2 ) )
            
        } else if (isolate(input$sample_dist == "Normal") ) {
            ori_mean <- isolate(as.numeric(input$mu) )
            ori_sd <- isolate(as.numeric(input$sigma) )
            
        } else if (isolate(input$sample_dist == "Uniform") ) {
            ori_mean <- isolate((as.numeric(input$min_a) + as.numeric(input$max_b) ) / 2 )
            ori_sd <- isolate(sqrt( (as.numeric(input$max_b) - as.numeric(input$min_a) )^2 / 12  ) )
            
        } else {
            ori_mean <- isolate(as.numeric(input$alpha) / as.numeric(input$beta) )
            ori_sd <- isolate(sqrt( as.numeric(input$alpha) / as.numeric(input$beta)^2 ) )
            
        }
        
        theo_mean <- ori_mean
        theo_se <- ori_sd / sqrt( isolate(input$sample_size) )
        ori_stat <- c(ori_mean, ori_sd, input$sample_size, theo_mean, theo_se)
        ori_stat
        
    })
    
    
    # Return the random sample matrix
    rsamplematrix <- reactive({
        matrix(rsample(), nrow = isolate(input$simulation) )
    })
    
    
    # show simulation results in a table format
    output$rsample_df <- renderDataTable({
        rsampledf <- as.data.frame(rsamplematrix() )
        rownames(rsampledf) <- paste("simulation", isolate(1:input$simulation) )
        colnames(rsampledf) <- paste("sample", isolate(1:input$sample_size) )
        rsampledf <- rownames_to_column(rsampledf, var = "Simulation")
        rsampledf
    })
    
    
    # Calculate mean and variance of sampling mean and output summary table of them with therotical counterparts
    output$summary_simu <- renderTable({
        sample_mean <- rowMeans(rsamplematrix())
        theo_stat <- c(mean(sample_mean), sd(sample_mean) )
        Value = signif(c(ori_stat(), theo_stat), digits = 3)
        Statistics = c("Population Mean", "Population Standard Deviation", "Sample Size", 
                       "Theoretical Mean of Sample Mean", "Theoretical Standard Error of Sample Mean",
                       "Actual Mean of Sample Mean", "Actual Standard Error of Sample Mean")
        summary_table <- as.data.frame(cbind(Statistics, Value) )
        summary_table
    })
    
    
    # present histogram of sample mean and theoretical aprroximately normal density according to CLT
    output$output_hist <- renderPlot({
        sample_mean <- rowMeans(rsamplematrix())
        theo_mean <- isolate(ori_stat()[4] )
        theo_se <- isolate(ori_stat()[5] )
        
        hist_nc <- 
        ggplot(data.frame(sample_mean), aes(x = sample_mean) ) +
            geom_histogram(bins = input$bins, fill = input$hist_color, 
                           color = "white", aes(y = ..density..) ) +
            stat_function(fun = dnorm, args = list(theo_mean, theo_se),
                          size = 1.5, linetype = 2, color = input$curve_color ) +
            labs(title = "Central Limit Theorem Verification\n",
                 subtitle = "(Histogram of Random Sample Mean and Dasded Theoretical Aprroximately Normal Curve)",
                 x = "Random Sample Mean", y = 'Density') +
            theme(plot.title = element_text(size = 20, face = "bold", color = "blue", 
                                            hjust = 0.5, vjust = 5)) +
            theme(plot.subtitle = element_text(size = 16, face = "bold", color = "orange", 
                                               hjust = 0.5, vjust = 3)) +
            theme(axis.title = element_text(size = 15, face = "italic", color = "purple") ) +
            theme(axis.text = element_text(size = 12, color = "forestgreen") ) +
            theme(plot.margin = unit(rep(0.5, 4), "cm") ) 
        
        ggsave("plot.pdf", hist_nc)
        hist_nc
    })
    
    output$downloadplot <- downloadHandler(
        filename = function() {
            "plot.pdf"
        },
        content = function(file) {
            file.copy("plot.pdf", file, overwrite = TRUE)
        }
    )   
})
