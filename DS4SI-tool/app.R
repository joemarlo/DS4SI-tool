library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(gridExtra)
library(plotly)
library(rlang)
library(quantreg) # for weighted box
# library(shinydashboard)
# library(shinyBS)
# options(scipen = 999999)

# ggplot theme
theme_set(theme_minimal())

# todo --------------------------------------------------------------------

# filtering outliers (maybe by selecting points on plot?) 
    # https://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html
    # https://shiny.rstudio.com/gallery/plot-interaction-basic.html
# how to download data and plots when finished? 
    # https://shiny.rstudio.com/gallery/generating-reports.html
    # https://yihui.shinyapps.io/DT-info/
# filter data table 
    # https://shiny.rstudio.com/gallery/basic-datatable.html
# getting sliders to sum to 100
    # https://stackoverflow.com/questions/38372906/constrain-two-sliderinput-in-shiny-to-sum-to-100
    # although should just have weights
# this dashboard could be useful
    # https://rstudio.github.io/shinydashboard/
# table should only be filtered once and then used for everything else
# should there be an advanced plotting tab? 
# fix slider finicky behavior
    # probably should round all values in the data cleaning process
# plotly + shiny
    # https://plotly-r.com/linking-views-with-shiny.html
# implement plot brushing
    # https://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html
# should implement weights by allowing user to select # top sites and those sites are
    # ranked by the weighted score
# need to implment table on final page
# need reset button on filters and weights
# add checkmark box for inlcuding only filtered sites in site exploration tab

# notes -------------------------------------------------------------------

# if there are any issues in the future, it's most likely due to the quantile sliders;
#  best strategy is just to delete them and move on with your life

# 2020-07-05 can't get the back and forth behavior between the value slider and the quantile slider
#   to work as it gets stuck in a loop. This could replicates it. There is a possibly related issue
#   that stats::ecdf() doesn't return a 0 for the minimal value
# x1 <- get_quantile(final_table$pct_hs, 0.60)
# x2 <- get_value(final_table$pct_hs, x1)
# x3 <- get_quantile(final_table$pct_hs, x2)
# x4 <- get_value(final_table$pct_hs, x3)
# x5 <- get_quantile(final_table$pct_hs, x4)
# x6 <- get_value(final_table$pct_hs, x5)

# load data ---------------------------------------------------------------

final_table <- read_csv("jpta_cleaned.csv")
# welcome_text <- read_file("welcome_text.txt")

# custom functions --------------------------------------------------------

get_quantile = function(distribution, value){
    # function to return the quantile amount
    
    # find the value in distribution that is closest to value
    closest_value_in_distribution <- distribution[which.min(abs(distribution - value))]

    # find the quantile of this closest value
    closest_quantile <- ecdf(distribution)(closest_value_in_distribution)
    return(closest_quantile)
} 

get_value <- function(distribution, probs){
    # function to get the quantile, but make sure it matches an actual value in the distribution
    # similar to stats::quantile() but returns the quantile to the closest value in the distribution
    
    # get the actual quantile
    actual_quantile <- as.numeric(quantile(distribution, probs = probs))
    
    # find the value in distribution that is closest to value
    closest_prob_in_distribution <- distribution[which.min(abs(distribution - actual_quantile))]
    
    # closest_value <- distribution[all_quantiles  == closest_quantile_in_distribution]
    
    return(closest_prob_in_distribution)
}


# custom variables --------------------------------------------------------

# custom HTML code for collapsiible
#  see http://jsfiddle.net/thurstanh/emtAm/2/
HTML_for_collapsible_1 <- '<details><summary>View quantile</summary>'
HTML_for_collapsible_2 <- '</details><br>'

# create df of min and maxes to use in slider calculations
min_max_df <- final_table %>% 
    select(unemp, pct_hs, income, comfort, cost) %>% 
    pivot_longer(cols = everything()) %>% 
    group_by(name) %>% 
    summarize(min = floor(min(value) * 0.999 * 100) / 100,
              max = ceiling(max(value) * 1.001 * 100) / 100) %>%
    as.data.frame() %>% 
    `rownames<-`(.[,'name']) %>% 
    select(-name)
min_max_df['cost',] <- round(min_max_df['cost',], 0)

    
# app ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = "my-shiny.css",

    # download roboto and inconsolata font
    HTML(
        '<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'
    ),
    HTML(
        '<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Inconsolata">'
    ),

    # choose default slider skin
    chooseSliderSkin(skin = "Flat",
                     color = "#5c5980"),

    # <br> for spacing
    br(),
    
    navlistPanel(widths = c(2, 10),
        tabPanel("Welcome",
                 mainPanel(width = 10,
                           
                           # Output: Tabset w/ plot, summary, and table ----
                           tabsetPanel(
                               type = "tabs",
                               tabPanel("Welcome",
                                        includeMarkdown("welcome_text.md")),
                               tabPanel("Tool instructions", 
                                        h1("Tool instructions"))
                           ))
        ),
                 
        tabPanel("1. Site exploration",
                 sidebarLayout(
                     sidebarPanel(width = 4,
                          selectInput("plot_type", "Plot type:", multiple = FALSE,
                                      choices = c("Scatter", "Histogram"),
                                      selected = "Scatter"),
                          selectInput("x_variable", "X: ", multiple = FALSE,
                                      choices = colnames(final_table),
                                      selected = "unemp"),
                          selectInput("y_variable", "Y: ", multiple = FALSE,
                                      choices = colnames(final_table),
                                      selected = "cost"),
                          selectInput("size_variable", "Size: ", multiple = FALSE,
                                      choices = colnames(final_table),
                                      selected = "comfort"),
                          selectInput("fill_variable", "Fill color: ", multiple = FALSE,
                                      choices = colnames(final_table),
                                      selected = "cost" ),
                          selectInput("facet_variable", "Facet variable: ",multiple = FALSE,
                                      choices = c("none", "region", "urban", "other_prog"),
                                      selected = "none"),
                          sliderInput("alpha_variable", "Opacity: ", min = 0.1, max = 1, value = 0.7, step = 0.1)
                 ),
                 mainPanel(width = 6,
                     # plotlyOutput('advanced_plotly'),
                     plotOutput('advanced_ggplot', 
                                brush = brushOpts(id = "plot1_brush")),
                     br(),
                     h4("Highlight data points on the above plot to view their information below"),
                     DT::dataTableOutput("brush_info"),
                     h6("Highlighting is not possible when using a facet variable")
                 )
                 )
        ),

        tabPanel("2. Site selection",

    sidebarLayout(
        sidebarPanel(width = 4,

            # text output of title with number of sites selected
            htmlOutput("n_sites_selected"),
            
            br(),

            tabsetPanel(type = 'tabs',

                # tab one containing the filters
                tabPanel("Filters",
                     br(),
                    # these should be reactive -> move to server side?
                    selectInput("region_slider", "Region:", multiple = TRUE,
                                choices = unique(as.character(final_table$region)),
                                selected = unique(as.character(final_table$region))),
                    selectInput("urban_slider", "Urban:", multiple = TRUE,
                                choices = rev(unique(as.character(final_table$urban))),
                                selected = rev(unique(as.character(final_table$urban)))),
                    selectInput("other_program_slider", "Other program available:", multiple = TRUE,
                                choices = rev(unique(as.character(final_table$other_prog))),
                                selected = rev(unique(as.character(final_table$other_prog)))),
                    
                    br(),
                    
                    # unemployment rate sliders
                    sliderInput("unemp_slider", "Unemployment: ",
                                min = min_max_df['unemp', 'min'],
                                max = min_max_df['unemp', 'max'],
                                value = c(min_max_df['unemp', 'min'], 
                                          min_max_df['unemp', 'max'])),
                    HTML(HTML_for_collapsible_1),
                    sliderInput(inputId = "unemp_quantile", label = NULL, min = 0, max = 1,
                                value = c(0, 1)),
                    HTML(HTML_for_collapsible_2),
                    
                    # high school graduation rate sliders
                    sliderInput("hs_slider", "High school graduation rate: ",
                                min = min_max_df['pct_hs', 'min'],
                                max = min_max_df['pct_hs', 'max'],
                                value = c(min_max_df['pct_hs', 'min'], min_max_df['pct_hs', 'max'])),
                    HTML(HTML_for_collapsible_1),
                    sliderInput(inputId = "hs_quantile", label = NULL, min = 0, max = 1,
                                value = c(get_quantile(final_table$pct_hs, min_max_df['pct_hs', 'min']),
                                          get_quantile(final_table$pct_hs, min_max_df['pct_hs', 'max']))),
                    HTML(HTML_for_collapsible_2),

                    # income sliders
                    sliderInput("income_slider", "Income: ", #round = -0.2,
                                min = min_max_df['income', 'min'],
                                max = min_max_df['income', 'max'],
                                value = c(min_max_df['income', 'min'], min_max_df['income', 'max'])),
                    HTML(HTML_for_collapsible_1),
                    sliderInput(inputId = "income_quantile", label = NULL, min = 0, max = 1,
                                value = c(get_quantile(final_table$income, min_max_df['income', 'min']),
                                          get_quantile(final_table$income, min_max_df['income', 'max']))),
                    HTML(HTML_for_collapsible_2),

                    # comfort sliders
                    sliderInput("comfort_slider", "Comfort: ", #round = -0.2,
                                min = min_max_df['comfort', 'min'],
                                max = min_max_df['comfort', 'max'],
                                value = c(min_max_df['comfort', 'min'], min_max_df['comfort', 'max'])),
                    HTML(HTML_for_collapsible_1),
                    sliderInput(inputId = "comfort_quantile", label = NULL, min = 0, max = 1,
                                value = c(get_quantile(final_table$comfort, min_max_df['comfort', 'min']),
                                          get_quantile(final_table$comfort, min_max_df['comfort', 'max']))),
                    HTML(HTML_for_collapsible_2),

                    # cost sliders
                    sliderInput("cost_slider", "Cost: ", #round = -0.2,
                                min = min_max_df['cost', 'min'],
                                max = min_max_df['cost', 'max'],
                                value = c(min_max_df['cost', 'min'], min_max_df['cost', 'max'])),
                    HTML(HTML_for_collapsible_1),
                    sliderInput(inputId = "cost_quantile", label = NULL, min = -0.001, max = 1,
                                value = c(get_quantile(final_table$cost, min_max_df['cost', 'min']),
                                          get_quantile(final_table$cost, min_max_df['cost', 'max']))),
                    HTML(HTML_for_collapsible_2),
                    
                    # manually exclude sites
                    selectInput("sites_excl", "Exclude sites manually by site ID:", multiple = TRUE,
                                choices = sort(unique(as.character(final_table$site_id)))),
                    ),

                # tab two containing the weight selections
                tabPanel("Weights",
                         br(),
                         sliderInput("slider1", "Unemployment: ", min = 0, max = 100, value = 50, step = 1),
                         sliderInput("slider2", "High school graduation rate: ", min = 0, max = 100, value = 50, step = 1),
                         sliderInput("slider3", "Income: ", min = 0, max = 100, value = 50, step = 1),
                         sliderInput("slider4", "Comfort: ", min = 0, max = 100, value = 50, step = 1),
                         sliderInput("slider4", "Site cost: ", min = 0, max = 100, value = 50, step = 1)

                )
            )
            ),

        mainPanel(width = 6,
                  
                  # Output: Tabset w/ plot, summary, and table ----
                  tabsetPanel(
                      type = "tabs",
                      tabPanel("Plots",
                               plotOutput("densities", height = 900)),
                      tabPanel("Table of selected sites", DT::dataTableOutput('table'))
                  ))
    )),
    
    tabPanel("3. Summary of selected sites",
             mainPanel(width = 9,
                 htmlOutput("summary_text"),
                 plotOutput("boxplots", height = 800),
                 br(),
                 downloadButton("downloadData", "Download the data"),
                 br(),
                 br()
             ))
    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    
    # code for sliders to react to it's respective quantile slider ------------
    
    # closure to define server side absolute <-> quantile slider behavior
    absolute_quantile_observeEvent <- function(slider_name, quantile_name, variable_name) {
        
        # when quantile slider changes, update the original slider
        # observeEvent(input[[quantile_name]], {
        #     updateSliderInput(session = session, inputId = slider_name,
        #                       value = c(get_value(distribution = final_table[[variable_name]],
        #                                           probs = input[[quantile_name]][1]),
        #                                 get_value(distribution = final_table[[variable_name]],
        #                                           probs = input[[quantile_name]][2]))
        #     )
        # })
        
        # when original slider changes, update the quantile slider
        observeEvent(input[[slider_name]],  {
            updateSliderInput(session = session, inputId = quantile_name,
                              value = c(get_quantile(value = input[[slider_name]][1], 
                                                     distribution = final_table[[variable_name]]),
                                        get_quantile(value = input[[slider_name]][2], 
                                                     distribution = final_table[[variable_name]])))
        })
    }
    
    # the non-closure version of this code is:
        # #unemployment
        # observeEvent(input$unemp_quantile,  {
        #     updateSliderInput(session = session, inputId = "unemp_slider",
        #                       value = c(as.numeric(quantile(final_table$unemp, probs = input$unemp_quantile[1])),
        #                                 as.numeric(quantile(final_table$unemp, probs = input$unemp_quantile[2])))
        #     )
        # })
        
        # observeEvent(input$unemp_slider,  {
        #     updateSliderInput(session = session, inputId = "unemp_quantile",
        #                       value = c(get_quantile(input$unemp_slider[1], final_table$unemp),
        #                                 get_quantile(input$unemp_slider[2], final_table$unemp)))
        # })
    
    # make the quantile slider dependent on the input slider
    absolute_quantile_observeEvent("unemp_slider", "unemp_quantile", "unemp")
    absolute_quantile_observeEvent("hs_slider", "hs_quantile", "pct_hs")
    absolute_quantile_observeEvent("income_slider", "income_quantile", "income")
    absolute_quantile_observeEvent("comfort_slider", "comfort_quantile", "comfort")
    absolute_quantile_observeEvent("cost_slider", "cost_quantile", "cost")
    

    # reactive function to return the current table filtered based on user inputs
    filtered_table <- reactive({
        
        # filter dataframe
        data <- final_table
        data <- data[data$region %in% input$region_slider,]
        data <- data[data$urban %in% input$urban_slider,]
        data <- data[data$other_prog %in% input$other_program_slider,]
        data <- data[data$unemp >= input$unemp_slider[1] & data$unemp <= input$unemp_slider[2],]
        data <- data[data$pct_hs >= input$hs_slider[1] & data$pct_hs <= input$hs_slider[2],]
        data <- data[data$income >= input$income_slider[1] & data$income <= input$income_slider[2],]
        data <- data[data$comfort >= input$comfort_slider[1] & data$comfort <= input$comfort_slider[2],]
        data <- data[data$cost >= input$cost_slider[1] & data$cost <= input$cost_slider[2],]
        data <- data[!data$site_id %in% input$sites_excl,]
        
        return(data)
    })
    
    # number of current sites selected to use in header
    output$n_sites_selected <- renderText({
        paste0(
            '<h4>',
            nrow(filtered_table()),
            ' sites are currently selected to be approached</h4>'
        )
    })

    # display the table in the 'table of selected sites' tab
    output$table <- DT::renderDataTable(DT::datatable(
        filtered_table(), rownames = FALSE, options = list(
            # sets n observations shown
            pageLength = 20,
            # removes option to change n observations shown
            lengthChange = FALSE,
            # removes the search bar
            sDom  = '<"top">lrt<"bottom">ip')
        ) %>%
        formatRound(5:8, 2) %>%
        formatRound(9, 0))

    # the plots
    output$densities <- renderPlot({

        # bar plot of categorical variables
        p1 <- filtered_table() %>%
            select(region, urban, other_prog) %>%
            mutate(urban = as.character(urban),
                   other_prog = as.character(other_prog)) %>%
            pivot_longer(cols = everything()) %>%
            mutate(name = factor(name, levels = c("region", 'urban', 'other_prog'))) %>%
            ggplot(aes(x = value)) +
            geom_bar(fill = 'grey60', alpha = 0.8) +
            facet_wrap(~name, scales = 'free_x', ncol = 2) +
            labs(x = NULL,
                 y = NULL) +
            theme(axis.text.x = element_text(angle = 40, hjust = 1))

        # density plot of numeric variables
        p2 <- filtered_table() %>%
            select(unemp, pct_hs, income, comfort, cost) %>%
            pivot_longer(cols = everything()) %>%
            mutate(name = factor(name, levels = c("unemp", 'pct_hs', 'income', 'comfort', 'cost'))) %>%
            ggplot(aes(x = value)) +
            geom_density(fill = 'grey60', alpha = 0.8, color = 'white') +
            facet_wrap(~name, scales = 'free', ncol = 2) +
            labs(x = NULL,
                 y = NULL)

        # render both plots vertically
        grid.arrange(p1, p2, ncol = 1)

    })
    
    # text for summary page
    output$summary_text <- renderText({
        
        n_sites <- nrow(filtered_table())
        mean_acceptance <- mean(filtered_table()$comfort)
        expected_cost <- sum(filtered_table()$comfort * filtered_table()$cost)
        
        paste0(
            '<h4>',
            n_sites,
            ' sites have been selected to be approached. 
            These sites have a mean probability of accepting the invitation of ',
            round(mean_acceptance, 2), 
            '. The expected final sample of sites that will accept is ',
            floor(n_sites * mean_acceptance), ' sites, and have a total expected cost of ',
            scales::label_dollar(accuracy = 1)(expected_cost), '</h4>'
        )
    })
    
    # the summary plots
    output$boxplots <- renderPlot({

        # boxplots of the population, sites to approach, and expected sample
        
        # create the three groups
        population <- final_table %>% mutate(weight = 1, group = "Population")
        sites_to_approach <- filtered_table() %>% mutate(weight = 1, group = "Sites to \n approach")
        expected_sample <- filtered_table() %>% mutate(weight = comfort, group = 'Expected \n sample')
        
        bind_rows(population, sites_to_approach, expected_sample) %>% 
            select(cost, unemp, pct_hs, income, comfort, group, weight) %>% 
            mutate(group = factor(group,
                                 levels = c("Expected \n sample", "Sites to \n approach", "Population"),
                                 labels = c("Expected \n sample", "Sites to \n approach", "Population"))) %>%
            rename('Site cost ($)' = cost,
                   'County level \n unemployment (%)' = unemp,
                   'County level high \n school grad rate (%)' = pct_hs,
                   'County level \n median income ($)' = income,
                   'Site probability \n of participation' = comfort) %>%
            pivot_longer(cols = -c("group", "weight")) %>%  
            ggplot(aes(x = group, y = value, weight = weight)) +
            geom_boxplot() +
            stat_summary(fun = mean, geom = "errorbar",
                         aes(ymax = ..y.., ymin = ..y..),
                         width = .75, linetype = "dashed") +
            facet_wrap(~name, scale = "free", ncol = 2) +
            labs(x = NULL, 
                 y = NULL)
    })
    
    # create dataset with identifier indicating if site is to be approached
    data_to_download <- reactive({
        final_table %>%
        mutate(site_to_approach = site_id %in% filtered_table()$site_id)
    })
    
    # download button for data_to_download
    output$downloadData <- downloadHandler(
        filename <- "sites.csv",
        content <- function(file) {
            write.csv(data_to_download(), file, row.names = FALSE)
        }
    )
    
    # # draft of advanced plotting
    # output$advanced_plotly <- renderPlotly(
    #     plot_ly(
    #         x = filtered_table()$unemp,
    #         y = filtered_table()$cost, 
    #         type = 'scatter',
    #         mode = 'markers')
    # )
    
    # draft of advanced plotting
    output$advanced_ggplot <- renderPlot({
        
        p <- final_table %>% 
            ggplot(aes_string(x = input$x_variable, y = input$y_variable,
                       size = input$size_variable, fill = input$fill_variable,
                       color = input$fill_variable)) +
            geom_point(alpha = input$alpha_variable)
        
        if(input$facet_variable != "none"){
            # facet_variable <- parse_quo(input$facet_variable, env = caller_env())
            p <- p + facet_wrap(~get(input$facet_variable), scales = 'free')
        } 
        
        # show plot
        p
    })
    
    
    # table of brushed data points from plot
    output$brush_info <- DT::renderDataTable(
        
        # show only if there isn't faceting
        if (input$facet_variable == "none") {
            DT::datatable(
                brushedPoints(final_table, input$plot1_brush),
                rownames = FALSE,
                options = list(
                    # sets n observations shown
                    pageLength = 10,
                    # removes option to change n observations shown
                    lengthChange = FALSE,
                    # removes the search bar
                    sDom  = '<"top">lrt<"bottom">ip'
                )
            ) %>%
                formatRound(5:8, 2) %>%
                formatRound(9, 0)
        })

}

# Run the application
shinyApp(ui = ui, server = server)
