library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(gridExtra)
library(rlang)
library(shinyjs)
library(quantreg) # for weighted box plot
library(viridis) # for better colors for color blind people
# options(scipen = 999999)
set.seed(44)

# ggplot settings ---------------------------------------------------------

# build custom theme
theme_custom <- function()
  theme_minimal() +
  theme(
    strip.background = element_rect(
      fill = "gray95",
      color = 'white'),
    strip.text = element_text(
      color = "gray30",
      size = 11,
      face = "bold"
    )
  )

# set custom theme
theme_set(theme_custom())

# set default continuous colors
options(
    ggplot2.continuous.colour = "viridis",
    ggplot2.continuous.fill = "viridis"
)

# set default discrete colors
scale_colour_discrete <- function(...) {
  scale_color_viridis(..., discrete = TRUE)
}


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
# allow for user to save datasets so they can move them to each panel as they wish
# rename last selection tab as "manual adjustments" and create new tab with
  # send invitations button


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

scale_01 <- function(x) {
 # function scales the vector between 0 and 1
  (x - min(x)) / (max(x) - min(x))
}

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

custom_datatable <- function(...){
  # wrapper around DT::datatable so commonly used arguments
  # can be set as global defaults
  
  DT::datatable(rownames = FALSE, 
                options = list(
                  # sets n observations shown
                  pageLength = 20,
                  # removes option to change n observations shown
                  lengthChange = FALSE,
                  # removes the search bar
                  sDom  = '<"top">lrt<"bottom">ip',
                  # enable side scroll so table doesn't overflow
                  scrollX = TRUE
                ),
                ...
  )
}

draw_histograms <- function(data){
  
  # function takes in a dataset and then draws histograms for continuous
    # variables and barplots for categoricals. Returns a single grid.arrange
    # object to pass to renderPlot({})
  
  # bar plot of categorical variables
  p1 <- data %>%
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
  p2 <- data %>%
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
  
}


score_attributes <- function(data){
  
  # function returns a table of the score attributes
  # total cost, generalizability, causality, sample size
  
  # total cost
  total_cost <- sum(data$cost)
  total_cost <- scales::dollar(total_cost)
  
  # generalizability TBD
  general_score <- as.character("NA")
  
  # causality
  causality_score <- as.character("NA")
  
  # sample size
  n_sites <- nrow(data)
  n_sites <- scales::comma(n_sites)
  
  # build the final table
  metrics_table <- data.frame(Metric = c(total_cost, general_score, causality_score, n_sites))
  rownames(metrics_table) <- c("Total cost", "Generalizability score", "Causality score", "Sample size")
  
  # return the table
  return(metrics_table)
  
}

n_sites_text <- function(data){
  # function to output HTML text indicating
    # how many rows are a dataframe
  
  paste0('<h4>',
         nrow(data),' 
        sites are currently selected to be approached</h4>'
  )
}


determine_x_pos <- function(value){
  # for ggplot
  # determines x position for segments
  # when grouping
  
  case_when(
    value == FALSE ~ 1,
    value == TRUE ~ 2,
    value == "Northcentral" ~ 1,
    value == "Northeast" ~ 2,
    value == "South" ~ 3,
    value == "West" ~ 4
  )
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
# convert this to base R
min_max_df <- final_table %>% 
    select(unemp, pct_hs, income, comfort, cost) %>% 
    pivot_longer(cols = everything()) %>% 
    group_by(name) %>% 
    summarize(min = floor(min(value) * 0.999 * 100) / 100,
              max = ceiling(max(value) * 1.001 * 100) / 100,
              .groups = "drop") %>%
    as.data.frame() %>% 
    `rownames<-`(.[,'name']) %>% 
    select(-name)
min_max_df['cost',] <- round(min_max_df['cost',], 0)

    
# app ----------------------------------------------------------------------


# Define UI for application
ui <- fluidPage(

    # initialize shinyjs
    # can this be removed?
    useShinyjs(),
    
    # download roboto and inconsolata font
    HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
    HTML('<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Inconsolata">'),

    # custom CSS file
    includeCSS("www/my-shiny.css"),
    
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
                                        includeMarkdown("tool_instructions.md"))
                           ))
        ),
        
        HTML('<div><h5>1. Site attributes</h5></div>'),
        
        tabPanel(title = HTML("&nbsp &nbsp Data description"),
                 sidebarLayout(
                   sidebarPanel(width = 4,
                                includeMarkdown("data_description.md")
                                ),
                   mainPanel(width = 6,
                             plotOutput("intro_plots", height = 650)
                   )
                 )
        ),
                                
                 
        
        tabPanel(title = HTML("&nbsp &nbsp Data exploration"),
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
                            condition = "input.plot_type == 'Histogram'",
                            sliderInput("n_bins", "Number of bins: ", 
                                        min = 5, max = 50, value = 20, step = 1)
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

        HTML("<div><h5>2. Site selection</h5></div>"),
        
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
                    sliderInput(inputId = "cost_quantile", label = NULL, min = 0, max = 1,
                                value = c(get_quantile(final_table$cost, min_max_df['cost', 'min']),
                                          get_quantile(final_table$cost, min_max_df['cost', 'max']))),
                    HTML(HTML_for_collapsible_2)
            ),

        mainPanel(width = 6,
                  
                  # Output: Tabset w/ plot, summary, and table ----
                  tabsetPanel(
                      type = "tabs",
                      tabPanel("Plots",
                               plotOutput("filtered_plots", height = 650)),
                      tabPanel("Table of selected sites", DT::dataTableOutput('table')),
                      tabPanel("Table of excluded sites", DT::dataTableOutput('excluded_table'))
                      )
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
                            
                            selectInput(inputId = "sample_dataset", label = "Dataset to sample from: ", multiple = FALSE,
                                        choices = c("All sites", "Sites to approach")),
                            selectInput(inputId = "simple_or_stratified", label = "Simple or stratified sample: ", multiple = FALSE,
                                        choices = c("simple", "stratified")),
                            conditionalPanel(
                              condition = "input.simple_or_stratified == 'stratified'",
                              selectizeInput("strata_variables", "Strata variable (limited to two): ", 
                                             multiple = TRUE,
                                             options = list(maxItems = 2),
                                             choices = categorical_vars,
                                             selected = categorical_vars[1]),
                              # sliderInput("strata_prob_1", label = "Probability: ", min = 0, max = 1, value = 0.5, step = 0.01),
                              # uiOutput("strata_probs_UI_1"),
                              # uiOutput("strata_probs_UI_2"),
                              # conditionalPanel(
                              #   # show only if there are two variables selected in stratefied_variables
                              #   condition = "input.strata_variables.length == 2",
                              #   sliderInput("strata_prob_2", label = "Probability: ", min = 0, max = 1, value = 0.5, step = 0.01)
                              #   )
                            ),
                            # this maximum input varies based on inputs; see code inside random_sample reactive
                            sliderInput("sample_n", "Sample size: ", min = 0, max = 400, value = 400, step = 1),
                            conditionalPanel(
                              condition = "input.simple_or_stratified == 'stratified'",
                              htmlOutput("n_strata"),
                              br(),
                              tableOutput("strata_table"),
                            ),
                            actionButton(inputId = "run_sampling", label = "Update sample")
                            
               ),
               
               mainPanel(width = 6,
                         
                         # Output: Tabset w/ plot, summary, and table ----
                         tabsetPanel(
                           type = "tabs",
                           tabPanel("Plots",
                                    plotOutput("sampling_plots", height = 650)), 
                           tabPanel("Table of selected sites", DT::dataTableOutput('random_sample_table')),
                           tabPanel("Table of excluded sites", DT::dataTableOutput('random_sample_excluded_table'))
                         )
               )
             )
    ),
    
    tabPanel(title = HTML("&nbsp &nbsp Weighting"),
             sidebarLayout(
                 sidebarPanel(width = 4,
                              
                              # text output of title with number of sites selected
                              # htmlOutput("n_sites_selected_2"),
                              h4("Create a weighted score for each site by setting the importance of each continuous variable below"),
                              
                              br(),
                              
                              selectInput(inputId = "weight_dataset", label = "Dataset: ", multiple = FALSE,
                                          choices = c("All sites", "Sites to approach")),
  
                              sliderInput("weight_unemp", "Unemployment: ", min = 1, max = 100, value = 50, step = 1),
                              sliderInput("weight_pct_hs", "High school graduation rate: ", min = 1, max = 100, value = 50, step = 1),
                              sliderInput("weight_income", "Income: ", min = 1, max = 100, value = 50, step = 1),
                              sliderInput("weight_comfort", "Comfort: ", min = 1, max = 100, value = 50, step = 1),
                              sliderInput("weight_cost", "Site cost: ", min = 1, max = 100, value = 50, step = 1),
                              # this maximum input varies based on dataset
                              sliderInput("weight_n", "Only include top n sites: ", min = 0, max = 400, value = 400, step = 1)
                 ),

             mainPanel(width = 6,
                       DT::dataTableOutput('table_2')
                       )
             )
    ),
    
    tabPanel(title = HTML("&nbsp &nbsp Manual exclusions"),
             
             sidebarLayout(
                 sidebarPanel(width = 4,
                              
                              # text output of title with number of sites selected
                              # htmlOutput("n_sites_selected_3"),
                              # br(),
                              # br(),
                              # 
                              h4("Manually exclude sites"),
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
    
    tabPanel(title = "3. Send invitations",
             sidebarLayout(
               sidebarPanel(width = 4,
                            br(),
                            selectInput(inputId = "invitations_data", label = "Dataset: ", multiple = FALSE,
                                        choices = c("All sites", "Sites to approach")),
                            htmlOutput("summary_final_selection"),
                            br(),
                            tableOutput("send_scores_table"),
                            br(),
                            actionButton(inputId = "send_invitations_button", 
                                         label = HTML('Send invitations<br>
                                          <p style="font-size: 0.6em; font-weight: 10;">
                                         Once sent, site selection will no longer be available</p>'
                                         )
                            ),
                            br()
               ),
               mainPanel(width = 6,
                         plotOutput("send_plots", height = 650)
               )
             )
    ),
    
    
    tabPanel(title = "4. Results",
             
             sidebarLayout(
               sidebarPanel(width = 4,
                            htmlOutput("summary_text"),
                            tableOutput("key_metrics_table"),
                            br(),
                            br(),
                            actionButton(inputId = "run_simulation",
                                         label = 'How does your random draw compare to 200 simulated draws?'),
                            br(),
                            br(),
                            downloadButton("downloadData", "Download the data"),
                            br(),
                            br(),
                            actionButton(inputId = "restart_button", 
                                         label = 'Erase these data and restart the selection process')
               ),
             
               mainPanel(width = 6,
                       tabsetPanel(
                         id = "results_tabs",
                         type = "tabs",
                         tabPanel("Summary statistics", 
                                  tableOutput("summary_table")),
                         tabPanel("Plots",
                                  plotOutput("final_hist_plots", height = 650)),
                         tabPanel("Sample vs. population",
                                  plotOutput("samp_v_pop_plots", height = 650)),
                         tabPanel("Sites that accepted",
                                  DT::dataTableOutput('accepted_table'))
                         )
                       )
               )
             
    )
    )
)



# Define server logic
server <- function(input, output, session) {

    # hide tab on start
    hideTab(inputId = "nav", target = "4. Results", session = session)
    
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
    
    # must duplicate for each call b/c Shiny can't handle two of the same thing
    output$n_sites_selected <- renderText(n_sites_text(filtered_table()))
    
  
    # text for final selection page
    output$summary_final_selection <- renderText({
      
      data <- switch(input$invitations_data,
                     "All sites" = final_table,
                     "Sites to approach" = filtered_table()
      )
      
      n_sites <- nrow(data)
      mean_acceptance <- mean(data$comfort)
      expected_cost <- sum(data$comfort * data$cost)
      
      paste0(
        '<h4>',
        n_sites,
        ' sites are currently selected to be approached. Of these, ',
        floor(n_sites * mean_acceptance), ' sites are expected to accept the invitation, and have a total expected cost of ',
        scales::label_dollar(accuracy = 1)(expected_cost), '.</h4>'
      )
      })

    # display the table in the 'table of selected sites' tab
    output$table <- DT::renderDataTable(
      custom_datatable(
          filtered_table(),
          selection = 'none'
        ) %>%
        formatRound(5:8, 2) %>%
        formatRound(9, 0))
    
    # select which dataset to use on weighting tab
    weight_data <- reactive({
      
      switch(input$weight_dataset,
             "All sites" = final_table,
             "Sites to approach" = filtered_table()
             # "Sites that accepted" = sites_that_accepted
      )
      
    })
    
    # update sample_n slider max so it's not larger than the dataset
    observeEvent(nrow(weight_data()), {
      updateSliderInput(session, "weight_n", max = nrow(weight_data()))
    })
    
    # display the table in the 'table of selected sites' tab within the weighting page
    output$table_2 <- DT::renderDataTable(
      custom_datatable({
        
        data <- weight_data()
        
        # scale the variables
        numeric_vars_scaled <- data[, numeric_vars]
        numeric_vars_scaled <- apply(numeric_vars_scaled, MARGIN = 2, scale_01)
        
        # vector of weights
        weights <- c(input$weight_unemp, input$weight_pct_hs, input$weight_income, 
                     input$weight_comfort, input$weight_cost)
        
        # calculate score per each row
        data$site_score <- apply(numeric_vars_scaled, MARGIN = 1, function(row) sum(row * weights))
        
        # filter to just the top rows selected by the user
        data <- data %>% arrange(desc(site_score)) %>% head(n = input$weight_n)
        
        # make site_score as first column in dataframe
        data <- data[, c('site_score', setdiff(colnames(data), "site_score"))]
        
        data
    }, selection = 'none' ) %>%
        formatRound(6:9, 2) %>%
        formatRound(10, 0) %>% 
        formatRound(1, 1)
      )
    
    # display the table in the 'table of selected sites' tab within the final selection page
    output$table_3 <- DT::renderDataTable(
      custom_datatable(
        filtered_table()
        ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0))
    
    # display the table in the 'table of excluded sites' tab
    output$excluded_table <- DT::renderDataTable(
      custom_datatable(
        anti_join(final_table, filtered_table()),
        selection = 'none'
        ) %>%
        formatRound(5:8, 2) %>%
        formatRound(9, 0))

    # display the table in the 'table of excluded sites' tab: Final selection tab
    output$excluded_table_2 <- DT::renderDataTable(
      custom_datatable(
        anti_join(final_table, filtered_table())
      ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0))
    
 
    # plots on data description page
    output$intro_plots <- renderPlot({draw_histograms(final_table)})
    
    # plots on filtering page
    output$filtered_plots <- renderPlot({draw_histograms(filtered_table())})
    
    # plots on send invitations page
    output$send_plots <- renderPlot({
      draw_histograms({
        
        switch(input$invitations_data,
               "All sites" = final_table,
               "Sites to approach" = filtered_table()
        )
        
      })})
    
  
    
    # site exploration
    output$advanced_ggplot <- renderPlot({
        
      # set which dataset to use
      plot_data <- switch(input$plot_Data,
                          "All sites" = final_table,
                          "Sites to approach" = filtered_table(),
                          "Sites that accepted" = sites_that_accepted
      )
        
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
            p <- p + geom_histogram(color = 'white', bins = input$n_bins, 
                                    fill = violet_col, alpha = 0.9) +
                labs(y = NULL)
        }
        
        # density
        if (input$plot_type == 'Density'){
            p <- p + geom_density(fill = violet_col, alpha = 0.5) +
                labs(y = NULL)
        }
        
        # boxplot
        if (input$plot_type == 'Boxplot'){
            p <- p + 
                geom_boxplot(fill = violet_col, alpha = 0.5,
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
                selection = "none",
                options = list(
                    # sets n observations shown
                    pageLength = 10,
                    # removes option to change n observations shown
                    lengthChange = FALSE,
                    # removes the search bar
                    sDom  = '<"top">lrt<"bottom">ip',
                    # enable side scroll so table doesn't overflow
                    scrollX = TRUE
                )
            ) %>%
                formatRound(5:8, 2) %>%
                formatRound(9, 0)
        })

    # take action when the submit invitations button is clicked
    observeEvent(input$send_invitations_button, {
      
      # move user to the final tab
      updateNavlistPanel(session = session, inputId = "nav", selected = "4. Results")
      
      # hide old tabs
      hide(selector = "li.navbar-brand") # this hides the HTML(2. Site selection) text
      hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Filtering"), session = session)
      hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Weighting"), session = session)
      hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Sampling"), session = session)
      hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Manual exclusions"), session = session)
      hideTab(inputId = "nav", target = "3. Send invitations", session = session)
      showTab(inputId = "nav", target = "4. Results", session = session)
      
      # update Results tab
      sent_invitations_data <- switch(input$invitations_data,
                     "All sites" = final_table,
                     "Sites to approach" = filtered_table()
      )
      
      # flip a coin with prob = comfort to see which sites accepted
      accepted_boolean <- rbinom(n = nrow(sent_invitations_data), size = 1, prob = sent_invitations_data$comfort) == 1
      sites_that_accepted <<- sent_invitations_data[accepted_boolean,]
      sent_invitations_data <<- sent_invitations_data
      
      # final data frame of of all sites with indicator if site was sent inviation and if accepted
      # assign variable to global environment so it can be used in other functions
      final_table$sent_invitation <- final_table$site_id %in% sent_invitations_data$site_id
      final_table$accepted <- final_table$site_id %in% sites_that_accepted$site_id
      final_results <<- final_table
      
    })
    
    # table of key metrics for the send invitations page
    output$send_scores_table <- renderTable(
      score_attributes({
        
        switch(input$invitations_data,
               "All sites" = final_table,
               "Sites to approach" = filtered_table()
        )
        
      }), 
      rownames = TRUE, align = 'r'
    )
    
    
    # table of key metrics for the Results page
    output$key_metrics_table <- renderTable(
      score_attributes(sites_that_accepted), 
      rownames = TRUE, align = 'r'
    )
    
    # summary table for the Results page
    output$summary_table <- renderTable({
      
      data <- final_results
      
      # build dataframe with identifer for the group it comes from
        # df should nrow should be n(population) + n(sites sent invitation) + n(sites accepted)
      data$group <- 'Population'
      data <- data %>% select(-c('sent_invitation', 'accepted'))
      sites_that_accepted$group <- 'Accepted'
      sent_invitations_data$group <- 'Sent invitation'
      data <- rbind(data, sites_that_accepted, sent_invitations_data)
      
      # calculate mean numeric stats per group
      mean_unemp <- aggregate(x = data$unemp, by = list(data$group), FUN = 'mean')$x
      mean_hs <- aggregate(x = data$pct_hs, by = list(data$group), FUN = 'mean')$x
      mean_income <- aggregate(x = data$income, by = list(data$group), FUN = 'mean')$x
      mean_cost <- aggregate(x = data$cost, by = list(data$group), FUN = 'mean')$x
      mean_comfort <- aggregate(x = data$comfort, by = list(data$group), FUN = 'mean')$x
      mean_urban <- aggregate(x = data$urban, by = list(data$group), FUN = 'mean')$x
      mean_other_program <- aggregate(x = data$other_prog, by = list(data$group), FUN = 'mean')$x
    
      # calculate % of sites per region per group
      n_sites <- tapply(data$region, list(data$region, data$group), function(tbl) length(tbl))
      mean_sites <- apply(n_sites, 1, function(row) row / table(data$group))
      
      # bind the stats into one table
      summary_table <- t(cbind(mean_unemp, mean_hs, mean_income, mean_cost, mean_comfort, mean_urban, mean_other_program, mean_sites))
      
      # set row names
      rownames(summary_table) <- c(
                "Mean unemployment",
                "Mean HS rate",
                "Mean income",
                "Mean cost",
                "Mean comfort",
                "% urbanicity",
                "% with other program",
                "% Northcentral",
                "% Northeast",
                "% South",
                "% West"
      )
      
      # rearrange columns
      summary_table <- summary_table[, c('Accepted', 'Sent invitation', 'Population')]
      
      # return the table
      summary_table
      
    }, rownames = TRUE
    )
    
  
    # save row selections when button is clicked
    dd <- reactiveValues(select = NULL)
    observeEvent(input$save_row_selections, {
      
      # get current selected rows
      dd$current_row_selections <- filtered_table()$site_id[input$table_3_rows_selected]
      
      # maintain list of all rows that have been selected
      dd$all_row_selections <- sort(unique(append(dd$all_row_selections, dd$current_row_selections)))
      
    })
    
    

# Sampling page -----------------------------------------------------------

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
    # observeEvent(input$strata_variables, {
    #   updateSliderInput(session, "strata_prob_1", label = paste0(input$strata_variables[1], " probability: "))
    #   updateSliderInput(session, "strata_prob_2", label = paste0(input$strata_variables[2], " probability: "))
    # })

    # random sampling
    # random_sample <- reactive({
    # 
    #   # set which dataset to use
    #   if (input$sample_dataset == "Sites to approach"){
    #     sample_data <- filtered_table()
    #   } else {
    #     sample_data <- final_table
    #   }
    # 
    #   # if simple
    #   if (input$simple_or_stratified == "simple"){
    #     # update sample_n slider max so it's not larger than the dataset
    #     observeEvent(nrow(sample_data), {
    #       updateSliderInput(session, "sample_n", max = nrow(sample_data))
    #     })
    # 
    #     # sample the data
    #     sampled_data <- sample_n(tbl = sample_data, size = input$sample_n, replace = FALSE)
    # 
    #   } else {
    # 
    #     # stratified sampling
    #     # find unique combinations of variables
    #     unique_groups <- distinct(select(sample_data, input$strata_variables))
    # 
    #     # calculate the sample size per each unique group
    #     sample_size_per_group <- floor(input$sample_n / nrow(unique_groups))
    # 
    #     # split the data into the unique groups
    #     split_groups <- split(x = sample_data,
    #                           f = select(sample_data, input$strata_variables))
    # 
    #     # calculate the minimum strata size
    #     min_group_size <- min(sapply(split_groups, nrow))
    # 
    #     # update sample_n slider max so it can't produce strata samples that are greater than
    #     #   the minimum strata size
    #     max_n <- floor(min_group_size * nrow(unique_groups))
    #     observeEvent(max_n, {
    #       updateSliderInput(session, "sample_n", max = max_n)
    #     })
    # 
    #     # make sure the sample size is not greater than any of the unique groups
    #     validate(
    #       need(min_group_size >= sample_size_per_group,
    #             "Please decrease sample size or remove a strata variable. At least one strata is smaller than sample size per strata."
    #       )
    #     )
    # 
    #     # sample n rows per group
    #     sampled_row_indices <- tapply(X = 1:nrow(sample_data),
    #                                   INDEX = select(sample_data, input$strata_variables),
    #                                   FUN = function(group){
    #       sample(x = group, size = sample_size_per_group, replace = FALSE)
    #     })
    #     sampled_row_indices <- as.vector(unlist(sampled_row_indices))
    # 
    #     # final sampled data
    #     sampled_data <- sample_data[sampled_row_indices,]
    # 
    #     # # show text below sample size slider indicating n per strata
    #     output$n_strata <- renderText({paste0("The minimum strata size is ", min_group_size,
    #                                           " and the sample size per strata is ", sample_size_per_group,
    #                                           ". The resulting sample size is ", nrow(sampled_data), ".")})
    # 
    #     # table of strata combinations with count of site per strata
    #     output$strata_table <- renderTable(
    #       sampled_data %>%
    #         group_by_at(vars(input$strata_variables)) %>%
    #         tally() %>%
    #         rename(sample_n = n)
    #       )
    # 
    #   }
    # 
    #   return(sampled_data)
    # })
    # 
    
    # random sampling
    random_sample <- function(){
      
      # set which dataset to use
      if (input$sample_dataset == "Sites to approach"){
        sample_data <- filtered_table()
      } else {
        sample_data <- final_table
      }
      
      # if simple
      if (input$simple_or_stratified == "simple"){
        # update sample_n slider max so it's not larger than the dataset
        observeEvent(nrow(sample_data), {
          updateSliderInput(session, "sample_n", max = nrow(sample_data))
        })
        
        # sample the data
        sampled_data <- sample_n(tbl = sample_data, size = input$sample_n, replace = FALSE)
        
      } else {
        
        # stratified sampling
        # find unique combinations of variables
        unique_groups <- distinct(select(sample_data, input$strata_variables))
        
        # calculate the sample size per each unique group
        sample_size_per_group <- floor(input$sample_n / nrow(unique_groups))
        
        # split the data into the unique groups
        split_groups <- split(x = sample_data,
                              f = select(sample_data, input$strata_variables))
        
        # calculate the minimum strata size
        min_group_size <- min(sapply(split_groups, nrow))
        
        # update sample_n slider max so it can't produce strata samples that are greater than
        #   the minimum strata size
        max_n <- floor(min_group_size * nrow(unique_groups))
        observeEvent(max_n, {
          updateSliderInput(session, "sample_n", max = max_n)
        })
        
        # make sure the sample size is not greater than any of the unique groups
        validate(
          need(min_group_size >= sample_size_per_group,
               "Please decrease sample size or remove a strata variable. At least one strata is smaller than sample size per strata."
          )
        )
        
        # sample n rows per group
        sampled_row_indices <- tapply(X = 1:nrow(sample_data),
                                      INDEX = select(sample_data, input$strata_variables),
                                      FUN = function(group){
                                        sample(x = group, size = sample_size_per_group, replace = FALSE)
                                      })
        sampled_row_indices <- as.vector(unlist(sampled_row_indices))
        
        # final sampled data
        sampled_data <- sample_data[sampled_row_indices,]
        
        # # show text below sample size slider indicating n per strata
        output$n_strata <- renderText({paste0("The minimum strata size is ", min_group_size,
                                              " and the sample size per strata is ", sample_size_per_group,
                                              ". The resulting sample size is ", nrow(sampled_data), ".")})
        
        # table of strata combinations with count of site per strata
        output$strata_table <- renderTable(
          sampled_data %>%
            group_by_at(vars(input$strata_variables)) %>%
            tally() %>%
            rename(sample_n = n)
        )
        
      }
      
      return(sampled_data)
    }

    # update current random sample on click    
    current_sample <- reactiveValues(select = NULL)
    current_sample$table <- final_table
    observeEvent(input$run_sampling, {
      current_sample$table <- random_sample()
    })
    
    # display the table in the sampling tab    
    output$random_sample_table <- DT::renderDataTable(
      custom_datatable(
        current_sample$table,
        selection = 'none'
        ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0)
    )
    
    # display excluded table in the sampling tab
    output$random_sample_excluded_table <- DT::renderDataTable(
      
      custom_datatable(
        anti_join(final_table, current_sample$table),
        selection = 'none'
      ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0)
    )
    
    # the plots for sampling page
    output$sampling_plots <- renderPlot({draw_histograms(current_sample$table)})
    
    

# results page ------------------------------------------------------------

    # text for Results page page
    output$summary_text <- renderText({
      
      data <- final_results[final_results$accepted,]
      
      # calculate summary stats
      n_sites <- nrow(data)
      expected_cost <- sum(data$comfort * data$cost)
      
      # paste together the sentence
      sentence <- paste0(
        '<h2>Congrats! ', n_sites, ' sites accepted the invitation</h2>'
      )
      
      return(sentence)
    })
    
    # insert tab after running simulation
    observeEvent(input$run_simulation, {
      
      # insert the tab
      appendTab(inputId = "results_tabs",
                tabPanel("Actual vs. expected", 
                         plotOutput("actual_vs_expected_plots", height = 650)),
                select = TRUE
      )
      
      # add text
      insertUI(selector = "#run_simulation",
               where = "afterEnd",
               ui = h4("One moment ... Simulation results will appear on the 'Actual vs. expected' tab"))
      
      # remove button
      removeUI(selector = "#run_simulation")
      
    })
    
    # the histograms
    output$final_hist_plots <- renderPlot({draw_histograms(sites_that_accepted)})
    
    # sample vs population plots
    output$samp_v_pop_plots <- renderPlot({
      
      # stack a dataframe with groups per each population (will contain duplicates)
      population_sites <- final_results
      sent_sites <- final_results[final_results$sent_invitation,]
      accepted_sites <- final_results[final_results$accepted,]
      
      # add identifier for each population
      population_sites$population <- "Population"
      sent_sites$population <- "Sent_invitation"
      accepted_sites$population <- "Accepted_invitation"
      
      # bind the three datasets together
      data_for_plot <- rbind(population_sites, sent_sites, accepted_sites)
      
      # barplots
      p1 <- data_for_plot %>% 
        select(population, all_of(categorical_vars)) %>% 
        mutate_all(as.character) %>% 
        pivot_longer(cols = -c("population")) %>%
        group_by(population, name, value) %>%
        tally() %>%
        group_by(population, name) %>% 
        mutate(prop = n / sum(n)) %>%
        mutate(name = factor(name, levels = categorical_vars)) %>% 
        ggplot(aes(x = value, y = prop, group = population, fill = population)) +
        geom_col(position = 'dodge', color = 'white', alpha = 0.6) +
        scale_fill_viridis_d() +
        facet_wrap(~name, scales = 'free_x') +
        labs(x = NULL,
             y = NULL) +
        theme(legend.position = 'none',
              axis.text.x = element_text(angle = 40, hjust = 1))
      
      # density plots
      p2 <- data_for_plot %>% 
        select(population, all_of(numeric_vars)) %>% 
        pivot_longer(cols = -c("population")) %>% 
        ggplot(aes(x = value, group = population, fill = population)) +
        geom_density(alpha = 0.24) +
        facet_wrap(~name, scales = 'free') +
        scale_fill_viridis_d() +
        labs(x = NULL,
             y = NULL) +
        theme(legend.title = element_blank(),
              legend.position = c(0.82, 0.25))
      
      # render both plots vertically
      grid.arrange(p1, p2, ncol = 1, heights = c(1, 2))
      
    })
    
    # table of sites that accepted for the Results table
    output$accepted_table <- DT::renderDataTable(
      
      custom_datatable(
        sites_that_accepted,
        selection = 'none'
      ) %>%
        formatRound(5:8, 2) %>%
        formatRound(9, 0)
      
      )
    
    
    # actual vs final plots
    output$actual_vs_expected_plots <- renderPlot({
      
      data <- final_results
      sent_invitations_data <- data[data$sent_invitation,]
      
      # flip a coin with prob = comfort to see which sites accepted
      list_of_accepted_dataframes <- list()
      nsims <- 200
      for (i in 1:nsims){
        # sample the data and return T/F for indices that accepted
        accepted_boolean <- rbinom(n = nrow(sent_invitations_data), size = 1, prob = sent_invitations_data$comfort) == 1  
        
        # subset the data based on the indices
        accepted_data <- sent_invitations_data[accepted_boolean,]
        
        # add identifier for use in plotting
        accepted_data$sim <- i
        
        # add dataframe to list of total dataframes
        list_of_accepted_dataframes[[i]] <- accepted_data
      }
      
      # convert sites_that_accepted to long format - categoricals only
      sites_that_accepted_categorical <- sites_that_accepted %>% 
        mutate(sim = "Actual") %>% 
        select(sim, all_of(categorical_vars)) %>% 
        mutate_all(as.character) %>% 
        pivot_longer(cols = -c("sim")) %>% 
        group_by(sim, name, value) %>% 
        tally() %>%
        mutate(prop = n / sum(n),
               x_pos = determine_x_pos(value),
               name = factor(name, categorical_vars))
      
      # combine and plot - categoricals only
      p1 <- bind_rows(list_of_accepted_dataframes) %>%
        select(sim, all_of(categorical_vars)) %>%
        mutate_all(as.character) %>%
        pivot_longer(cols = -c("sim")) %>%
        group_by(sim, name, value) %>%
        tally() %>%
        mutate(prop = n / sum(n),
               x_pos = determine_x_pos(value),
               name = factor(name, categorical_vars)) %>% 
        ggplot(aes(x = value, y = prop, group = sim)) +
        geom_jitter(alpha = 0) +
        geom_segment(aes(x = x_pos - 0.25, xend = x_pos + 0.25,
                         y = prop, yend = prop), alpha = 0.025) +
        geom_segment(data = sites_that_accepted_categorical,
                     aes(x = x_pos - 0.25, xend = x_pos + 0.25,
                         y = prop, yend = prop),
                     color = "white", size = 1.1) +
        geom_segment(data = sites_that_accepted_categorical,
                     aes(x = x_pos - 0.25, xend = x_pos + 0.25,
                         y = prop, yend = prop), color = "#302f42",
                     size = 1.2, linetype = "dotted") +
        facet_wrap(~name, scales = 'free_x') +
        labs(x = NULL,
             y = NULL) +
        theme(axis.text.x = element_text(angle = 40, hjust = 1))
      
      # convert sites_that_accepted to long format - numerics only
      sites_that_accepted_numeric <- sites_that_accepted %>%
        mutate(sim = "Actual") %>%
        select(sim, all_of(numeric_vars)) %>%
        pivot_longer(cols = -c("sim"))

      # combine and plot - numerics only
      p2 <- bind_rows(list_of_accepted_dataframes) %>%
        select(sim, all_of(numeric_vars)) %>%
        pivot_longer(cols = -c("sim")) %>%
        ggplot(aes(x = value, group = sim)) +
        geom_line(stat = "density", alpha = 0.025, color = 'black') +
        geom_density(data = sites_that_accepted_numeric, color = "white", size = 1.1) +
        geom_density(data = sites_that_accepted_numeric, color = "#302f42", size = 1.3, linetype = "dotted") +
        facet_wrap(~name, scales = 'free') +
        labs(x = NULL,
             y = NULL)
      
      # render both plots vertically
      grid.arrange(p1, p2, ncol = 1, heights = c(1, 2))
      
    })
    
    # download button for data_to_download
    output$downloadData <- downloadHandler(
      filename <- "sites.csv",
      content <- function(file) {
        write.csv(final_results, file, row.names = FALSE)
      }
    )
    
    # restart the session based on button click
    observeEvent(input$restart_button, {
      session$reload()
    } )
    
}

# Run the application
shinyApp(ui = ui, server = server)
