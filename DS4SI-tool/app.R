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

options(
    ggplot2.continuous.colour = "viridis",
    ggplot2.continuous.fill = "viridis",
    ggplot2.discrete.colour = "viridis",
    ggplot2.discrete.fill = "viridis"
)

# todo --------------------------------------------------------------------

# DONE filtering outliers (maybe by selecting points on plot?) 
    # https://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html
    # https://shiny.rstudio.com/gallery/plot-interaction-basic.html
# DONE how to download data and plots when finished? 
    # https://shiny.rstudio.com/gallery/generating-reports.html
    # https://yihui.shinyapps.io/DT-info/
# DONE filter data table 
    # https://shiny.rstudio.com/gallery/basic-datatable.html
# DONE getting sliders to sum to 100
    # https://stackoverflow.com/questions/38372906/constrain-two-sliderinput-in-shiny-to-sum-to-100
    # although should just have weights
# REMOVE this dashboard could be useful
    # https://rstudio.github.io/shinydashboard/
# DONE table should only be filtered once and then used for everything else
# DONE should there be an advanced plotting tab? 
# DONE fix slider finicky behavior
    # probably should round all values in the data cleaning process
# REMOEV plotly + shiny
    # https://plotly-r.com/linking-views-with-shiny.html
# DONE implement plot brushing
    # https://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html
# should implement weights by allowing user to select # top sites and those sites are
    # ranked by the weighted score
# need to implement table on final page
# need reset button on filters and weights
# REMOVE add checkmark box for inlcuding only filtered sites in site exploration tab
# DONE need to maintain an "excluded list" of sites that can be manually added to by
    # various different UI elements
# DONE how to push user to another tab
    # https://shiny.rstudio.com/reference/shiny/1.1.0/updateTabsetPanel.html
    # https://shiny.rstudio.com/reference/shiny/1.1.0/showTab.html
# DONE removing tabs
    # https://shiny.rstudio.com/reference/shiny/1.1.0/insertTab.html
# fix data table overflow
# DONE data table editing
  # https://rstudio.github.io/DT/shiny.html
# REMOVE need to "include" button for excluded sites on Final Selection page
# should probably split this into seperate client and server files
# add button to refresh sample
# stratify sampling isn't yet working; need to implement dyanmic UI first
  # https://www.r-bloggers.com/dynamic-ui-elements-in-shiny/

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

HTML_for_collapsible_Weighting_1 <- '<details><summary>View details</summary>'
HTML_for_collapsible_Weighting_2 <- '</details><br>'

# violet color
violet_col <- "#5c5980"

# set list of variables for plotting options
numeric_vars <- c("unemp", "pct_hs", "income", "comfort", "cost")
categorical_vars <- c('region', 'urban', 'other_prog')

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
    HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
    HTML('<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Inconsolata">'),

    # choose default slider skin
    chooseSliderSkin(skin = "Flat",
                     color = "#221146"), # "#112446"),
    
    # top left title
    titlePanel("DS4SI Site Selection"),
    
    # <br> for spacing
    br(),
    
    navlistPanel(id = "nav", widths = c(2, 10),
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
        
        tabPanel(title = "1. Site exploration",
                 sidebarLayout(
                     sidebarPanel(width = 4,
                          selectInput("plot_type", "Plot type:", multiple = FALSE,
                                      choices = c("Scatter", "Histogram", "Density", "Boxplot"),
                                      selected = "Scatter"),
                          selectInput("x_variable", "X: ", multiple = FALSE,
                                      choices = numeric_vars,
                                      selected = "unemp"),
                          conditionalPanel(
                              condition = "input.plot_type == 'Scatter'",
                              selectInput("y_variable", "Y: ", multiple = FALSE,
                                          choices = numeric_vars,
                                          selected = "cost"),
                              selectInput("fill_variable", "Fill color: ", multiple = FALSE,
                                          choices = c(numeric_vars, categorical_vars, "cluster"),
                                          selected = "cost" ),
                              conditionalPanel(
                                  condition = "input.fill_variable == 'cluster'",
                                  sliderInput("n_clusters", "Number of clusters: ", 
                                              min = 2, max = 10, value = 4, step = 1),
                                  HTML('Hartigan-Wong K-means clustering based on selected X and Y variables. Not recommended when faceting.<br><br>')
                              ),
                              selectInput("size_variable", "Size: ", multiple = FALSE,
                                          choices = c(numeric_vars, categorical_vars),
                                          selected = "comfort"),
                              selectInput("regression", "Linear regression: ", multiple = FALSE,
                                          choices = c('none', 'include'),
                                          selected = 'none')
                          ),
                          conditionalPanel(
                              condition = "input.plot_type == 'Boxplot'",
                              selectInput("group_variable", "Grouping: ", multiple = FALSE,
                                          choices = c("none", categorical_vars))
                          ),
                          selectInput("facet_variable", "Facet variable: ", multiple = FALSE,
                                      choices = c("none", categorical_vars),
                                      selected = "none"),
                          conditionalPanel(
                              condition = "input.plot_type == 'Scatter'",
                              sliderInput("alpha_variable", "Opacity: ", 
                                      min = 0.1, max = 1, value = 0.6, step = 0.1)
                          ),
                          HTML('<details><summary>Advanced filters</summary>'),
                          selectInput(inputId = "plot_Data", label = "Dataset: ", multiple = FALSE,
                                      choices = c("All sites", "Sites to approach")),
                          HTML(HTML_for_collapsible_2)
                 ),
                 mainPanel(width = 6,
                     plotOutput('advanced_ggplot', 
                                brush = brushOpts(id = "plot1_brush")),
                     br(),
                     htmlOutput("brush_text"),
                     DT::dataTableOutput("brush_info")
                 )
                 )
        ),

        HTML("<h5>2. Site selection</h5>"),
        
            tabPanel(title = HTML("&nbsp &nbsp Filtering"),

    sidebarLayout(
        sidebarPanel(width = 4,

            # text output of title with number of sites selected
            htmlOutput("n_sites_selected"),
            
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
                    HTML(HTML_for_collapsible_2)
            ),

        mainPanel(width = 6,
                  
                  # Output: Tabset w/ plot, summary, and table ----
                  tabsetPanel(
                      type = "tabs",
                      tabPanel("Plots",
                               plotOutput("densities", height = 650)),
                      tabPanel("Table of selected sites", DT::dataTableOutput('table')),
                      tabPanel("Table of excluded sites", DT::dataTableOutput('excluded_table'))
                      )
                  )
    )
    ),
    
    tabPanel(title = HTML("&nbsp &nbsp Weighting"),
             sidebarLayout(
                 sidebarPanel(width = 4,
                              
                              # text output of title with number of sites selected
                              # htmlOutput("n_sites_selected_2"),
                              h4("Create a weighted score for each site by setting the importance of each continuous variable below."),
                              
                              br(),
  
                              sliderInput("slider1", "Unemployment: ", min = 0, max = 100, value = 50, step = 1),
                              HTML(HTML_for_collapsible_Weighting_1),
                              p("Higher employment == ..."),
                              HTML(HTML_for_collapsible_Weighting_2),
                              sliderInput("slider2", "High school graduation rate: ", min = 0, max = 100, value = 50, step = 1),
                              sliderInput("slider3", "Income: ", min = 0, max = 100, value = 50, step = 1),
                              sliderInput("slider4", "Comfort: ", min = 0, max = 100, value = 50, step = 1),
                              sliderInput("slider4", "Site cost: ", min = 0, max = 100, value = 50, step = 1)
                 ),
             
             mainPanel(width = 6,
                       tabPanel("Table of selected sites", DT::dataTableOutput('table_2'))
                       )
             )
    ),
    
    tabPanel(title = HTML("&nbsp &nbsp Sampling"),
             sidebarLayout(
               sidebarPanel(width = 4,
                            
                            # text output of title with number of sites selected
                            # htmlOutput("n_sites_selected_2"),
                            h4("Create simple and stratified random samples"),
                            
                            br(),
                            
                            selectInput(inputId = "sample_dataset", label = "Dataset: ", multiple = FALSE,
                                        choices = c("All sites", "Sites to approach")),
                            selectInput(inputId = "simple_or_stratified", label = "Simple or stratified sample: ", multiple = FALSE,
                                        choices = c("simple", "stratified")),
                            conditionalPanel(
                              condition = "input.simple_or_stratified == 'stratified'",
                              selectizeInput("strata_variables", "Strata variable (limited to two): ", 
                                          multiple = TRUE,
                                          options = list(maxItems = 2),
                                          choices = categorical_vars,
                                          selected = NULL),
                              sliderInput("strata_prob_1", label = "Probability: ", min = 0, max = 1, value = 0.5, step = 0.01),
                              # uiOutput("strata_probs_UI_1"),
                              # uiOutput("strata_probs_UI_2"),
                              conditionalPanel(
                                # show only if there are two variables selected in stratefied_variables
                                condition = "input.strata_variables.length == 2",
                                sliderInput("strata_prob_2", label = "Probability: ", min = 0, max = 1, value = 0.5, step = 0.01)
                                )
                            ),
                            # this should have a dynamic max
                            sliderInput("sample_n", "Sample size: ", min = 0, max = 400, value = 50, step = 1),
                            
               ),
               
               mainPanel(width = 6,
                         tabPanel("Table of sampled sites", DT::dataTableOutput('random_sample_table'))
               )
             )
    ),
    
    tabPanel(title = HTML("&nbsp &nbsp Final selection"),
             
             sidebarLayout(
                 sidebarPanel(width = 4,
                              
                              # text output of title with number of sites selected
                              # htmlOutput("n_sites_selected_3"),
                              # br(),
                              # br(),
                              # 
                              h4("Final exclusions"),
                              br(),
                              # manually exclude sites
                              selectInput("sites_excl", "Exclude sites manually by site ID:", multiple = TRUE,
                                          choices = sort(unique(as.character(final_table$site_id)))),
                              br(),
                              h5("Exclude sites by selecting rows on the table: "),
                              br(),
                              
                              # save row selections
                              actionButton(inputId = "save_row_selections", label = "Exclude selected rows"),
                              br(),
                              br(),
                              br(),
                              hr(),
                              htmlOutput("summary_final_selection"),
                              br(),
                              actionButton(inputId = "send_invitations_button", 
                                           label = HTML('Send invitations<br>
                                                        <p style="font-size: 0.6em; font-weight: 10;">
                                                        Cannot be undone</p>')),
                              br(),
                              br()
                 ),
                 
                 mainPanel(width = 6,
                           tabsetPanel(
                             type = "tabs",
                             tabPanel("Table of selected sites", DT::dataTableOutput('table_3')),
                             tabPanel("Table of excluded sites", DT::dataTableOutput('excluded_table_2'))
                           )
                 )
             )),
    
    tabPanel("3. Results",
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



# Define server logic
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
        
        # exclude rows from manual input
        data <- data[!data$site_id %in% input$sites_excl,]
        
        # exclude rows from row selection
        data <- data[!data$site_id %in% dd$all_row_selections,]
        
        return(data)
    })
    
    # number of current sites selected to use in header
    n_sites_text <- reactive({
        paste0('<h4>',
        nrow(filtered_table()),' 
        sites are currently selected to be approached</h4>'
    )})
    
    # must duplicate for each call b/c Shiny can't handle two of the same thing
    output$n_sites_selected <- renderText({n_sites_text()})
    output$n_sites_selected_2 <- renderText({n_sites_text()})
    output$n_sites_selected_3 <- renderText({n_sites_text()})
    
    # text for final selection page
    output$summary_final_selection <- renderText({
      
      n_sites <- nrow(filtered_table())
      mean_acceptance <- mean(filtered_table()$comfort)
      expected_cost <- sum(filtered_table()$comfort * filtered_table()$cost)
      
      paste0(
        '<h4>',
        n_sites,
        ' sites are currently selected to be approached. Of these, ',
        floor(n_sites * mean_acceptance), ' sites are expected to accept the invitation, and have a total expected cost of ',
        scales::label_dollar(accuracy = 1)(expected_cost), '.</h4>'
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
    
    # display the table in the 'table of selected sites' tab within the weighting page
    output$table_2 <- DT::renderDataTable(DT::datatable(
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
    
    # display the table in the 'table of selected sites' tab within the final selection page
    output$table_3 <- DT::renderDataTable(DT::datatable(
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
    
    # can access the above data using input$tableId_cell_info
    # output$selected_rows <- DT::renderDataTable(DT::datatable(
    #   filtered_table()[input$table_2_rows_selected,], rownames = FALSE, options = list(
    #     # sets n observations shown
    #     pageLength = 20,
    #     # removes option to change n observations shown
    #     lengthChange = FALSE,
    #     # removes the search bar
    #     sDom  = '<"top">lrt<"bottom">ip')
    # ) %>%
    #   formatRound(5:8, 2) %>%
    #   formatRound(9, 0))
    
    # display the table in the 'table of excluded sites' tab
    output$excluded_table <- DT::renderDataTable(DT::datatable(
        anti_join(final_table, filtered_table()), rownames = FALSE, options = list(
            # sets n observations shown
            pageLength = 20,
            # removes option to change n observations shown
            lengthChange = FALSE,
            # removes the search bar
            sDom  = '<"top">lrt<"bottom">ip')
    ) %>%
        formatRound(5:8, 2) %>%
        formatRound(9, 0))

    # display the table in the 'table of excluded sites' tab: Final selection tab
    output$excluded_table_2 <- DT::renderDataTable(DT::datatable(
      anti_join(final_table, filtered_table()), rownames = FALSE, options = list(
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
            geom_bar(fill = violet_col, alpha = 0.9) +
            facet_wrap(~name, scales = 'free_x', ncol = 3) +
            labs(x = NULL,
                 y = NULL) +
            theme(axis.text.x = element_text(angle = 40, hjust = 1))

        # histograms plot of numeric variables
        p2 <- filtered_table() %>%
            select(unemp, pct_hs, income, comfort, cost) %>%
            pivot_longer(cols = everything()) %>%
            mutate(name = factor(name, levels = c("unemp", 'pct_hs', 'income', 'comfort', 'cost'))) %>%
            ggplot(aes(x = value)) +
            geom_histogram(fill = violet_col, alpha = 0.9, color = 'white', bins = 20) +
            facet_wrap(~name, scales = 'free_x', ncol = 3) +
            labs(x = NULL,
                 y = NULL)

        # render both plots vertically
        grid.arrange(p1, p2, ncol = 1, heights = c(1, 2))

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
            scales::label_dollar(accuracy = 1)(expected_cost), '.</h4><br>'
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
            geom_boxplot(fill = violet_col, alpha = 0.35) +
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
    
    # site exploration
    output$advanced_ggplot <- renderPlot({
        
        # set which dataset to use
        if (input$plot_Data == "Sites to approach"){
            plot_data <- filtered_table()
        } else {
            plot_data <- final_table
        }
        
        # add kmeans cluster to dataframe
        if(input$plot_type == 'Scatter' & input$fill_variable == "cluster"){
            km <- plot_data %>% 
                select(input$x_variable, input$y_variable) %>% 
                kmeans(centers = input$n_clusters, iter.max = 50, nstart = 5)
            plot_data$cluster <- as.factor(km$cluster)
        }
        
        # create ase ggplot object
        p <- plot_data %>% 
            ggplot(aes_string(x = input$x_variable))
        
        # scatter
        if (input$plot_type == 'Scatter'){
            p <- p +
                geom_point(aes_string(y = input$y_variable,
                                      fill = input$fill_variable,
                                      size = input$size_variable,
                                      color = input$fill_variable),
                           alpha = input$alpha_variable)
            
            # add regression line
            if(input$regression == 'include'){
                p <- p + geom_smooth(aes(y = get(input$y_variable)), 
                                     method = "lm")
            } 
            
            # add cluster centers
            if(input$fill_variable == "cluster"){
                p <- p +
                    geom_point(data = as_tibble(km$centers),
                               aes_string(x = colnames(km$centers)[1],
                                          y = colnames(km$centers)[2]),
                               color = violet_col,
                               shape = 4, size = 8, stroke = 1.5)
            }
            
        }
        
        # histogram
        if (input$plot_type == 'Histogram'){
            p <- p + geom_histogram(color = 'white') +
                labs(y = NULL)
        }
        
        # density
        if (input$plot_type == 'Density'){
            p <- p + geom_density() +
                labs(y = NULL)
        }
        
        # boxplot
        if (input$plot_type == 'Boxplot'){
            p <- p + 
                geom_boxplot(
                if(input$group_variable != 'none') aes_string(y = input$group_variable)
            ) +
                coord_flip() +
                labs(y = NULL)
        }
        
        # add faceting
        if(input$facet_variable != "none"){
            p <- p + facet_wrap(~get(input$facet_variable))
        } 
        
        # show plot
        p
    })
    
    
    # text above the brush table
    output$brush_text <- renderText({
        
        if (input$facet_variable == "none" & input$plot_type == 'Scatter') {
            txt <- "<h4>Highlight data points on the above plot to view their information below</h4>"
        } else {
            txt <- NULL
        }
        
        return(txt)
        
    })
    
    # table of brushed data points from plot
    output$brush_info <- DT::renderDataTable(
        
        # show only if there isn't faceting
        if (input$facet_variable == "none" & input$plot_type == 'Scatter') {
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

    # take action when the submit invitations button is clicked
    observeEvent(input$send_invitations_button, {
      
      # move user to the final tab
      updateNavlistPanel(session = session, inputId = "nav", selected = "3. Results")
      
      # hide old tabs
      hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Filtering"), session = session)
      hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Weighting"), session = session)
      hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Sampling"), session = session)
      hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Final selection"), session = session)
    })
  
    # save row selections when clicked
    dd = reactiveValues(select = NULL)
    observeEvent(input$save_row_selections, {
      
      # get current selected rows
      dd$current_row_selections <- filtered_table()$site_id[input$table_3_rows_selected]
      
      # maintain list of all rows that have been selected
      dd$all_row_selections <- sort(unique(append(dd$all_row_selections, dd$current_row_selections)))
      
    })
    
    # table of sites that were excluded by selecting rows on the table
    # output$x4 <- DT::renderDataTable(DT::datatable(
    #   
    #   tibble(`Excluded sites` = as.integer(dd$all_row_selections)),
    #   rownames = FALSE, options = list(
    #     pageLength = 10,
    #     lengthChange = FALSE,
    #     sDom  = '<"top">lrt<"bottom">ip')
    # )
    # )
    
    #   tibble(`Excluded sites` = as.integer(dd$all_row_selections))
    # })

    # dynamic UI for strata probability inputs
    # FIX THIS HERE
    # sliderInput("strata_prob_1", label = "Probability: ", min = 0, max = 1, value = 0.5, step = 0.1),
    # output$strata_probs_UI_1 <- renderUI({
    #   strata_1_vars <- unique(final_table[, input$strata_variables[[1]]])
    #   lapply(strata_1_vars, function(var){
    #     sliderInput(inputId = paste0(var, "id"), label = paste0(var, "label"),
    #                 min = 0, max = 1, value = 0.5, step = 0.1)
    #     })
    #   # checkboxGroupInput("cities", "Choose Cities", cities)
    # })
    
    # # update slider probability name based on user input
    observeEvent(input$strata_variables, {
      updateSliderInput(session, "strata_prob_1", label = paste0(input$strata_variables[1], " probability: "))
      updateSliderInput(session, "strata_prob_2", label = paste0(input$strata_variables[2], " probability: "))
    })

    # random sampling
    random_sample <- reactive({
      
      # set which dataset to use
      if (input$sample_dataset == "Sites to approach"){
        sample_data <- filtered_table()
      } else {
        sample_data <- final_table
      }
      
      # if simple
      if (input$simple_or_stratified == "simple"){
        sampled_data <- sample_n(tbl = sample_data, size = input$sample_n, replace = FALSE) 
      } else {
        # sample_data %>% 
        #   group_by(input$strata_variables) %>% 
        #   sample_n()
        sampled_data <- sample_n(tbl = sample_data, size = input$sample_n, replace = FALSE) 
      }
      
      return(sampled_data)
    })
    
    # display the table in the sampling tab
    output$random_sample_table <- DT::renderDataTable(DT::datatable(
      random_sample(), rownames = FALSE, options = list(
        # sets n observations shown
        pageLength = 20,
        # removes option to change n observations shown
        lengthChange = FALSE,
        # removes the search bar
        sDom  = '<"top">lrt<"bottom">ip')
    ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0))
    

}

# Run the application
shinyApp(ui = ui, server = server)
