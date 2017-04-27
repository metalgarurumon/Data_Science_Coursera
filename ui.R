library(shiny)
library(shinythemes)
library(ggplot2)
library(tibble)

ui <- shinyUI(fluidPage(
    theme = shinytheme("united"),
    withMathJax(),
    
    titlePanel("Central Limit Theorem Simulation"),
    
    sidebarLayout(
        sidebarPanel(
            
            h3("Simulation Choice Section:"),
            
            selectInput("sample_dist", "Population Distribution where each sample is from",
                        choices = list('Discrete Distribution' = c("Bernoulli", "Poisson", "Geometric"), 
                                       'Continous Distribution' = c("Normal", "Uniform", "Gamma") ) ),
            
            conditionalPanel(condition = 'input.sample_dist == "Bernoulli"', 
                             h4("$$f(x) = p^x (1-p)^{1-x}, \\,\\textrm{for}\\, x = 0, 1$$"),
                             textInput("prob", "Parameter (p)") ),
            
            conditionalPanel(condition = 'input.sample_dist == "Poisson"', 
                             h4("$$f(x) = e^{-\\lambda} \\frac{\\lambda^x}{x!}, 
                                \\,\\textrm{for}\\, x = 0, 1,... $$"),
                             textInput("lambda", "Parameter (lambda)") ),
            
            conditionalPanel(condition = 'input.sample_dist == "Geometric"', 
                             h4("$$f(x) = p (1-p)^x, \\,\\textrm{for}\\, x = 0, 1,... $$"),
                             textInput("prob2", "Parameter (p)") ),
            
            conditionalPanel(condition = 'input.sample_dist == "Normal"', 
                             h4("$$f(x) = \\frac{1}{\\sqrt{2\\pi}\\sigma} 
                                e^{-\\frac{(x-\\mu)^2}{2\\sigma^2} } $$"),
                             textInput("mu", "Parameter (mu)"),
                             textInput("sigma", "parameter (sigma)") ),
            
            conditionalPanel(condition = 'input.sample_dist == "Uniform"', 
                             h4("$$f(x) = \\frac{1}{b-a}, \\,\\textrm{for}\\, a<x<b $$"),
                             textInput("min_a", "Parameter (a)"),
                             textInput("max_b", "parameter (b)") ),
            
            conditionalPanel(condition = 'input.sample_dist == "Gamma"', 
                             h4("$$f(x) = \\frac{\\beta^\\alpha}{\\Gamma(\\alpha)} 
                                x^{\\alpha - 1} e^{-\\beta x},  
                                \\,\\textrm{for}\\, x >0 $$"),
                             textInput("alpha", "Parameter (alpha)"),
                             textInput("beta", "parameter (beta)") ),
            
            sliderInput("sample_size", "Size of each random sample", 
                        value = 30, min = 10, max = 150, step = 1),
            
            sliderInput("simulation", "The number of simulation",
                        value = 100, min = 100, max = 1000, step = 1),
            
            HTML(paste("<br/>", h3("Outputs Choice Section:") ) ),
            
            numericInput("bins", "Number of bins in the histogram\n(max: 50)", 
                         value = 20, min = 1, max = 50, step = 1),
            
            selectInput("hist_color", "Speficy the colour of bins in the histogram",
                        choices = c("steelblue", "yellowgreen", "magenta", "orchid", "violetred") ),
            
            selectInput("curve_color", "Specify the color of aprroximately Normal curve",
                        choices = c("black", "brown", "red", "blue", "purple") ),
            
            actionButton("update", "Update Simulation"),
            
            HTML(paste("<br/> <br/>", h3("Plot Download Section:") ) ),
            
            downloadLink("downloadplot", "Download Plot")
            
            ),
        
        mainPanel(
            tabsetPanel(type = "pills", 
                        
                        tabPanel("Instructions", br(), 
                                 htmlOutput(outputId = "notice")), 
                        
                        tabPanel("Validity of Paramters", br(), 
                                 htmlOutput(outputId = "validity")), 
                        
                        tabPanel("Random Sampling Results", br(), 
                                 dataTableOutput("rsample_df") ),
                        
                        tabPanel("Summary of Sample Mean", br(), 
                                 tableOutput(outputId = "summary_simu")),
                        
                        tabPanel("Plot of Sample Mean", br(), 
                                 plotOutput(outputId = "output_hist"))
            )
        )
            )
    
        ))