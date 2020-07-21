library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(gridExtra)
# library(rlang)
library(shinyjs)
library(quantreg) # for weighted box plot
library(viridis) # for better colors for color blind people
# options(scipen = 999999)
set.seed(44)


# load other files --------------------------------------------------------

source("R/variables.R")
source("R/ggplot_settings.R")
source("R/functions.R")

    
# app ----------------------------------------------------------------------

# Define UI for application
ui <- fluidPage(

    # initialize shinyjs
    useShinyjs(),
    
    # download roboto and inconsolata font
    HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
    HTML('<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Inconsolata">'),

    # custom CSS file
    includeCSS("www/custom_css.css"),

    # choose default slider skin
    chooseSliderSkin(skin = "Flat",
                     color = "#221146"), # "#112446"),
    
    # top left title
    titlePanel("DS4SI Site Selection"),
    
    br(),
    
    navlistPanel(id = "nav", widths = c(2, 10),
        
        tabPanel("Welcome",
                 mainPanel(width = 10,
                           tabsetPanel(
                               type = "tabs",
                               tabPanel("Welcome",
                                        includeMarkdown("markdowns/welcome_text.md")),
                               tabPanel("Tool instructions", 
                                        includeMarkdown("markdowns/tool_instructions.md"))
                           ))
        ),
        
        tabPanel(title = "4. Results",
                 
                 sidebarLayout(
                   sidebarPanel(
                     width = 4,
                     htmlOutput("summary_text"),
                     tableOutput("key_metrics_table"),
                     br(),br(),
                     actionButton(inputId = "run_simulation",
                                  label = 'How does your random draw compare to 250 simulated draws?'),
                     br(),br(),
                     downloadButton("downloadData", "Download the data"),
                     br(),br(),
                     actionButton(inputId = "restart_button",
                                  label = 'Erase these data and restart the selection process')
                   ),
                   
                   mainPanel(
                     width = 6,
                     tabsetPanel(
                       id = "results_tabs",
                       type = "tabs",
                       tabPanel("Summary statistics",
                                tableOutput("summary_table")),
                       tabPanel("Plots",
                                plotOutput("final_hist_plots", height = 650)),
                       tabPanel(
                         "Sample vs. population",
                         plotOutput("samp_v_pop_plots", height = 650)
                       ),
                       tabPanel("Sites that accepted",
                                DT::dataTableOutput('accepted_table'))
                     )
                   )
                 )), 
        
        HTML('<div><h5>1. Site attributes</h5></div>'),
        
        tabPanel(title = HTML("&nbsp &nbsp Data description"),
                 sidebarLayout(
                   sidebarPanel(width = 4, includeMarkdown("markdowns/data_description.md")),
                   mainPanel(width = 6, plotOutput("intro_plots", height = 650))
                 )), 
                 
        tabPanel(title = HTML("&nbsp &nbsp Data exploration"),
                 sidebarLayout(
                   sidebarPanel(
                     width = 4,
                     h4("Explore the data using scatter plots, histograms, density plots, and boxplots"),
                     br(),
                     uiOutput("exploration_selection_data_spawn"),
                     selectInput(
                       inputId ="plot_type",
                       label = "Plot type:",
                       multiple = FALSE,
                       choices = c("Scatter", "Histogram", "Density", "Boxplot"),
                       selected = "Scatter"
                     ),
                     selectInput(
                       inputId ="x_variable",
                       label = "X: ",
                       multiple = FALSE,
                       choices = numeric_vars,
                       selected = numeric_vars[5]
                     ),
                     conditionalPanel(
                       condition = "input.plot_type == 'Scatter'",
                       selectInput(
                         inputId = "y_variable",
                         label = "Y: ",
                         multiple = FALSE,
                         choices = numeric_vars,
                         selected = numeric_vars[3]
                       ),
                       selectInput(
                         inputId = "fill_variable",
                         label = "Fill color: ",
                         multiple = FALSE,
                         choices = c(numeric_vars, categorical_vars, "cluster"),
                         selected = numeric_vars[2]
                       ),
                       conditionalPanel(
                         condition = "input.fill_variable == 'cluster'",
                         sliderInput(
                           inputId = "n_clusters",
                           label = "Number of clusters: ",
                           min = 2,
                           max = 10,
                           value = 4,
                           step = 1
                         ),
                         HTML(
                           'Hartigan-Wong K-means clustering based on selected X and Y variables. Not recommended when faceting.<br><br>'
                         )
                       ),
                       selectInput(
                         inputId = "size_variable",
                         label = "Size: ",
                         multiple = FALSE,
                         choices = c(numeric_vars, categorical_vars),
                         selected = numeric_vars[1]
                       ),
                       selectInput(
                         inputId = "regression",
                         label = "Linear regression: ",
                         multiple = FALSE,
                         choices = c('none', 'include'),
                         selected = 'none'
                       )
                     ),
                     conditionalPanel(
                       condition = "input.plot_type == 'Histogram'",
                       sliderInput(
                         inputId = "n_bins",
                         label = "Number of bins: ",
                         min = 5,
                         max = 50,
                         value = 20,
                         step = 1
                       )
                     ),
                     conditionalPanel(
                       condition = "input.plot_type == 'Boxplot'",
                       selectInput(
                         inputId = "group_variable",
                         label = "Grouping: ",
                         multiple = FALSE,
                         choices = c("none", categorical_vars)
                       )
                     ),
                     selectInput(
                       inputId = "facet_variable",
                       label = "Facet variable: ",
                       multiple = FALSE,
                       choices = c("none", categorical_vars),
                       selected = "none"
                     ),
                     conditionalPanel(
                       condition = "input.plot_type == 'Scatter'",
                       sliderInput(
                         inputId = "alpha_variable",
                         label = "Opacity: ",
                         min = 0.1,
                         max = 1,
                         value = 0.6,
                         step = 0.1
                       )
                     ),
                     div(
                       id = "plot_Data_div",
                       HTML('<details><summary>Advanced filters</summary>'),
                       selectInput(
                         inputId = "plot_Data",
                         label = "Dataset: ",
                         multiple = FALSE,
                         choices = NULL
                       ),
                       br(),br(),br(),br(),
                       HTML(HTML_for_collapsible_2)
                     )
                   ),
                   
                   mainPanel(
                     width = 6,
                     plotOutput('advanced_ggplot',
                                brush = brushOpts(id = "plot1_brush")),
                     br(),
                     htmlOutput("brush_text"),
                     DT::dataTableOutput("brush_info")
                   )
                 )),
        
        HTML("<div><h5>2. Site selection</h5></div>"),
        
        
        
        tabPanel(title = HTML("&nbsp &nbsp Filtering"),
                 
                 sidebarLayout(
                   sidebarPanel(width = 4,
                                
                                # text output of title with number of sites selected
                                htmlOutput("filtering_html_n_sites_selected"),
                                br(),
                                selectInput(inputId = "filter_dataset", 
                                            label = "Dataset to filter: ", 
                                            multiple = FALSE, 
                                            choices = NULL),
                                br(),
                                # numeric sliders generated on the server side
                                uiOutput(outputId = "filtering_select_categorical"),
                                # numeric sliders generated on the server side
                                uiOutput(outputId = "filtering_sliders_numeric"),
                                # save dataset
                                br(),
                                textInput(inputId = "filtering_data_save_name", 
                                          value = "my_dataset", 
                                          label = "Name and save your dataset"),
                                actionButton(inputId = "filtering_data_save_button", 
                                             label = "Save my_dataset")
                   ),
                   
                   mainPanel(width = 6,
                             
                             tabsetPanel(
                               type = "tabs",
                               tabPanel("Plots",
                                        plotOutput("filtered_plots", height = 650)),
                               tabPanel("Table of selected sites", DT::dataTableOutput('filtering_selected_table')),
                               tabPanel("Table of excluded sites", DT::dataTableOutput('filtering_excluded_table'))
                             )
                   )
                 )
        ),
        
        tabPanel(title = HTML("&nbsp &nbsp Sampling"),
                 sidebarLayout(
                   sidebarPanel(width = 4,
                                
                                h4("Create simple and stratified random samples"),
                                br(),
                                selectInput(inputId = "sample_dataset", 
                                            label = "Dataset to sample: ", 
                                            multiple = FALSE,
                                            choices = NULL),
                                selectInput(inputId = "simple_or_stratified", 
                                            label = "Simple or stratified sample: ", 
                                            multiple = FALSE,
                                            choices = c("simple", "stratified")),
                                conditionalPanel(
                                  condition = "input.simple_or_stratified == 'stratified'",
                                  selectizeInput("strata_variables", "Strata variable (limited to two): ", 
                                                 multiple = TRUE,
                                                 options = list(maxItems = 2),
                                                 choices = categorical_vars,
                                                 selected = categorical_vars[2]),
                                  HTML("<strong>Sample size per strata:</strong>"),
                                  uiOutput("sampling_strata_sliders"),
                                  br()
                                ),
                                conditionalPanel(
                                  condition = "input.simple_or_stratified == 'stratified'",
                                  htmlOutput("n_strata"),
                                  br(),
                                  actionButton(inputId = "sample_reset_sliders", label = "Reset sliders"),
                                  br(), br()
                                ),
                                conditionalPanel(
                                  condition = "input.simple_or_stratified == 'simple'",
                                  # this maximum input varies based on inputs; see code inside random_sample reactive
                                  sliderInput("sample_n", "Sample size: ", min = 0, max = population_n, value = population_n, step = 1)
                                ),
                                actionButton(inputId = "run_sampling", label = "Sample the data"),
                                # save dataset
                                br(), br(),
                                textInput("sampling_data_save_name", value = "my_dataset", label = "Name and save your dataset"),
                                actionButton("sampling_data_save_button", label = "Save my_dataset")
                                
                   ),
                   
                   mainPanel(width = 6,
                             
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
                                h4("Create a weighted score for each site by setting the importance of each continuous variable below"),
                                br(),
                                selectInput(inputId = "weight_dataset", label = "Dataset to apply weights to: ", multiple = FALSE,
                                            choices = NULL),
                                uiOutput("weighting_sliders"),
                                # this maximum input varies based on dataset
                                sliderInput(inputId = "weight_n", 
                                            label = "Only include top n sites: ", 
                                            min = 0, 
                                            max = population_n, 
                                            value = population_n, 
                                            step = 1),
                                # save dataset
                                br(),
                                textInput("weighting_data_save_name", value = "my_dataset", label = "Name and save your dataset"),
                                actionButton("weighting_data_save_button", label = "Save my_dataset")
                   ),
                   
                   mainPanel(width = 6,
                             DT::dataTableOutput('weighting_selected_table')
                   )
                 )
        ),
        
        tabPanel(title = HTML("&nbsp &nbsp Manual exclusions"),
                 
                 sidebarLayout(
                   sidebarPanel(width = 4,
                                
                                h4("Manually exclude sites"),
                                br(),
                                selectInput(inputId = "manual_dataset", label = "Dataset to apply exclusions to: ", 
                                            multiple = FALSE, choices = NULL),
                                selectInput("sites_excl", "Exclude sites manually by site ID:", multiple = TRUE,
                                            choices = sort(unique(as.character(population_dataset$site_id)))),
                                br(),
                                HTML("<strong>Exclude sites by selecting rows on the table: </strong><br>"),
                                br(),
                                actionButton(inputId = "save_row_selections", label = "Exclude selected rows"),
                                br(),br(),
                                # save dataset
                                textInput("manual_data_save_name", value = "my_dataset", label = "Name and save your dataset"),
                                actionButton("manual_data_save_button", label = "Save my_dataset")
                   ),
                   
                   mainPanel(width = 6,
                             tabsetPanel(
                               type = "tabs",
                               tabPanel("Table of selected sites", DT::dataTableOutput('manual_table_selected')),
                               tabPanel("Table of excluded sites", DT::dataTableOutput('manual_table_excluded'))
                             )
                   )
                 )),
        
        tabPanel(title = "3. Send invitations",
                 sidebarLayout(
                   sidebarPanel(width = 4,
                                br(),
                                selectInput(inputId = "invitations_data", label = "Dataset: ", multiple = FALSE,
                                            choices = NULL), #c("All sites", "Sites to approach")),
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
        )
    )
)



# server ------------------------------------------------------------------

# Define server logic
server <- function(input, output, session) {
  
  # hide tab on start
  hideTab(inputId = "nav", target = "4. Results", session = session)
  
  # load Rdata that contains score_generalizability() function
  load("R/score_generalizability.RData", envir = .GlobalEnv)
  
  
  # saving datasets ---------------------------------------------------------
  
  # initialize list of saved datasets
  datasets_available <- reactiveValues(data = NULL, data_names = NULL)
  datasets_available$data <- list(population_dataset)
  datasets_available$data_names <- c("Population")
  
  # update list of dataframes in the dataset dropdowns
  # this applies to every page that has a dataset dropdown
  observeEvent(datasets_available$data, {
    
    dataset_selector_ids <- c(
      "filter_dataset", 
      "sample_dataset", 
      "weight_dataset",
      "manual_dataset",
      "invitations_data",
      "plot_Data"
    )
    
    lapply(dataset_selector_ids, function(id){
      updateSelectInput(session = session, inputId = id,
                        choices = datasets_available$data_names)  
    })
  })
  
  # list of the prefixes for saving datasets on each page
  # prefix_name = id of the inputText of the dataset name
  # prefix_button = id of the actionButton to save the dataset
  data_save_name_prefixes <- c(
    "filtering_data_save",
    "sampling_data_save",
    "weighting_data_save",
    "manual_data_save"
  )
  
  # loop through each of the prefixes and take action
  # this code chunk controls most of the saving of datasets
  lapply(data_save_name_prefixes, function(id){
    
    save_name <- paste0(id, "_name")
    save_button <- paste0(id, "_button")
    
    # make save button label equal to the input'ed dataset name 
    observeEvent(input[[save_name]], {
      
      updateActionButton(session = session, 
                         inputId = save_button,
                         label = paste0("Save ", str_trim(input[[save_name]]))
      )
    })
    
    # save dataset on button click
    observeEvent(input[[save_button]], {
      
      # clean up name for logic checks
      clean_save_name <- str_to_lower(str_trim(input[[save_name]]))
      
      # make sure input'ed dataset name is not already used
      # this prevents the saving of datasets if the name already exists in
      # datasets_available$data_names
      validate(
        need(!(clean_save_name %in% str_to_lower(datasets_available$data_names)),
             "Dataset name already used")
      )
      
      # add input data to list of of dataframes 
      dataset_to_save <- switch(id,
                                "filtering_data_save" = filtered_table(),
                                "sampling_data_save" = current_sample(),
                                "weighting_data_save"= weighting_current_data(),
                                "manual_data_save" = manual_selected_data())
      datasets_available$data <- c(datasets_available$data, list(dataset_to_save))
      
      # add input string to list of dataset names
      datasets_available$data_names <- c(datasets_available$data_names,
                                         str_trim(input[[save_name]]))
      
      # destroy the user entered text
      updateTextInput(session = session,
                      inputId = save_name,
                      value = NA)
      
    })
    
    # make save_name text red if its a duplicate of a name
    # already in datasets_available$data_names
    observe({
      
      # make names lower case and remove white space so 
      # save_name is 1:1 with data_names
      clean_save_name <- str_to_lower(str_trim(input[[save_name]]))
      dataset_names <- str_to_lower(datasets_available$data_names)
      
      # make text red
      if(clean_save_name %in% dataset_names) {
        runjs(
          paste0('document.getElementById("',
                 save_name,
                 '").style.color = "red"')
        )
      }
      
      # javascript permanently changes text color so it needs to change it back
      # to black when the input text is valid
      if(!clean_save_name %in% dataset_names) {
        runjs(
          paste0('document.getElementById("',
                 save_name,
                 '").style.color = "#363636"')
        )
      }
    })
    
  })
  
  
  # description page --------------------------------------------------------
  
  # plots on data description page
  output$intro_plots <- renderPlot({draw_histograms(population_dataset)})
  
  
  # exploration page --------------------------------------------------------
  
  # select which dataset to use on data exploration tab
  exploration_selected_data <- reactive({
    
    # if the send invitations button has been triggered then used the 
    # "stacked_results" dataframe
    if ("stacked_results" %in% datasets_available$data_names){
      data <- datasets_available$data[[match("stacked_results", datasets_available$data_names)]]
      
      # apply filters from checkmark boxes
      data <- data[data$site_group %in% input$exploration_checkboxes,]
      
      return(data)
      
    } else {
      # otherwise use the user selected dataframe on the dropdown
      datasets_available$data[[match(input$plot_Data, datasets_available$data_names)]]
      }
  })
  
  
  # site exploration plots
  output$advanced_ggplot <- renderPlot({
    
    # set which dataset to use
    plot_data <- exploration_selected_data()
    
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
                             method = "lm",
                             formula = 'y ~ x')
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
      p <- p + facet_wrap(input$facet_variable, labeller = label_both)
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
      
      custom_datatable(
        brushedPoints(population_dataset, input$plot1_brush),
        selection = "none"
      ) %>%
        formatRound(5:8, 2) %>%
        formatRound(9, 0)
    })
  
  
  # filtering page ----------------------------------------------------------
  
  # text at top of page
  # output$filtering_html_n_sites_selected <- renderText(n_sites_text(filtered_table()))
  output$filtering_html_n_sites_selected <- renderText(
    paste0('<h4>',
           'Apply filters to the data using the inputs below. ',
           scales::comma_format()(nrow(filtered_table())),' 
             sites are currently selected to be approached</h4>'
    )
  )
  
  # select which dataset to use on filtering tab
  filter_selected_data <- reactive({
    datasets_available$data[[match(input$filter_dataset, datasets_available$data_names)]]
  })
  
  # generate sliders for each categorical variable
  output$filtering_select_categorical <- renderUI({
    tagList(
      pmap(
        .l = list(categorical_vars, categorical_choices),
        .f = function(variable, var_choices) {
          selectInput(
            inputId = paste0("filtering_select_", variable),
            label = paste0(variable, ": "),
            multiple = TRUE,
            choices = var_choices,
            selected = var_choices
          )
        }
      ))
  })    
  
  # generate sliders for each numeric variable
  output$filtering_sliders_numeric <- renderUI({
    tagList(
      pmap(
        .l = list(numeric_vars, min_max_df$min, min_max_df$max),
        .f = function(variable, var_min, var_max) {
          sliderInput(
            inputId = paste0("filtering_slider_", variable),
            label = paste0(variable, ": "),
            min = var_min,
            max = var_max,
            value = c(var_min, var_max)
          )
        }
      ))
  })
  
  # reactive function to return the current table filtered based on user inputs
  filtered_table <- reactive({
    
    # filter dataframe
    data <- filter_selected_data()
    data <- data[data$region %in% input$filtering_select_region,]
    data <- data[data$urban %in% input$filtering_select_urban,]
    data <- data[data$other_prog %in% input$filtering_select_other_prog,]
    data <- data[data$unemp >= input$filtering_slider_unemp[1] & 
                   data$unemp <= input$filtering_slider_unemp[2],]
    data <- data[data$pct_hs >= input$filtering_slider_pct_hs[1] & 
                   data$pct_hs <= input$filtering_slider_pct_hs[2],]
    data <- data[data$income >= input$filtering_slider_income[1] & 
                   data$income <= input$filtering_slider_income[2],]
    data <- data[data$comfort >= input$filtering_slider_comfort[1] & 
                   data$comfort <= input$filtering_slider_comfort[2],]
    data <- data[data$cost >= input$filtering_slider_cost[1] & 
                   data$cost <= input$filtering_slider_cost[2],]
    
    return(data)
  })
  
  # histograms and bar plots on 'plots' tab
  output$filtered_plots <- renderPlot({draw_histograms(filtered_table())})
  
  # display the table in the 'table of selected sites' tab
  output$filtering_selected_table <- DT::renderDataTable(
    custom_datatable(
      filtered_table(),
      selection = 'none'
    ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0))
  
  # display the table in the 'table of excluded sites' tab
  output$filtering_excluded_table <- DT::renderDataTable(
    custom_datatable(
      anti_join(population_dataset, filtered_table()),
      selection = 'none'
    ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0))
  
  
  # sampling page -----------------------------------------------------------
  
  # select which dataset to use on sampling tab
  sample_selected_data <- reactive({
    datasets_available$data[[match(input$sample_dataset, datasets_available$data_names)]]
  })
  
  # update sample_n slider max so it's not larger than the dataset
  observeEvent(nrow(sample_selected_data()), {
    updateSliderInput(session = session, 
                      inputId = 'sample_n', 
                      value = nrow(sample_selected_data()),
                      max = nrow(sample_selected_data()))
  })
  
  # table of strata combinations that exist for the selected dataset
  strata_combos <- reactive({
    sample_selected_data() %>%
      group_by_at(vars(input$strata_variables)) %>%
      tally() %>%
      unite("strata_combos", input$strata_variables, sep = "_")
  })
  
  # generate sliders for each strata combinations
  output$sampling_strata_sliders <- renderUI({
    tagList(
      map2(.x = strata_combos()$strata_combos,
           .y = strata_combos()$n,
           .f = function(combo, max_n) {
             sliderInput(
               inputId = combo, 
               label = str_replace(combo, "_", ":"), 
               value = min(strata_combos()$n), 
               min = 0, max = max_n, step = 1) 
           })
    )
  })
  
  # on button click, reset the sliders to the starting position
  observeEvent(input$sample_reset_sliders, {
    
    # get current list of sliders
    slider_ids <- strata_combos()$strata_combos
    
    # update the position of the sliders to the maximum amount that still
    # allows equality across the sliders
    lapply(slider_ids, function(slider){
      updateSliderInput(session = session, inputId = slider, value = min(strata_combos()$n))
    })
    
  })
  
  # random sampling
  current_sample <- eventReactive(input$run_sampling, {
    
    data <- sample_selected_data()
    
    # if simple sampling
    if (input$simple_or_stratified == "simple"){
      
      # sample the data
      sampled_data <- slice_sample(data, n = input$sample_n, replace = FALSE)
      
      return(sampled_data)
      
    } else {
      # stratified sampling
      
      # split the data into the unique groups
      split_groups <- split(x = data,
                            f = select(data, input$strata_variables),
                            sep = "_")
      
      # reorder list so it matches strata_combos() order
      split_groups <- split_groups[strata_combos()$strata_combos]
      
      # list of current slider input values
      sample_size_per_group <- reactiveValuesToList(input)[strata_combos()$strata_combos]
      
      # sample n rows per strata
      sampled_data <- map2_dfr(.x = split_groups,
                               .y = sample_size_per_group,
                               .f = function(strata, strata_size){
                                 
                                 slice_sample(strata, n = strata_size, replace = FALSE)
                                 
                               })
      
      return(sampled_data)
      
    }
  })
  
  # show text below sample size slider indicating total sample size
  output$n_strata <- renderText({
    slider_sum <- sum(unlist(reactiveValuesToList(input)[strata_combos()$strata_combos]))
    paste0("The total selected sample size is ", slider_sum)
  })    
  
  # the plots for sampling page
  output$sampling_plots <- renderPlot({draw_histograms(current_sample())})
  
  # display the table in the sampling tab    
  output$random_sample_table <- DT::renderDataTable(
    custom_datatable(
      current_sample(),
      selection = 'none'
    ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0)
  )
  
  # display excluded table in the sampling tab
  output$random_sample_excluded_table <- DT::renderDataTable(
    custom_datatable(
      anti_join(population_dataset, 
                current_sample()),
      selection = 'none'
    ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0)
  )
  
  
  # weighting page ----------------------------------------------------------
  
  # select which dataset to use on weighting page
  weight_selected_data <- reactive({
    datasets_available$data[[match(input$weight_dataset, datasets_available$data_names)]]
  })
  
  # update weight_n slider max so it's not larger than the dataset
  observeEvent(nrow(weight_selected_data()), {
    updateSliderInput(session, "weight_n",
                      value = nrow(weight_selected_data()),
                      max = nrow(weight_selected_data())
    )
  })
  
  # generate sliders for each variable
  output$weighting_sliders <- renderUI({
    tagList(
      lapply(numeric_vars, function(variable) {
        sliderInput(
          inputId = paste0("weighting_slider_", variable),
          label = paste0(variable, ": "),
          min = 1,
          max = 100,
          value = 50
        )
      }
      ))
  })
  
  # current weighted dataset based on inputs
  weighting_current_data <- reactive({
    
    data <- weight_selected_data()
    
    # scale the variables
    numeric_vars_scaled <- data[, numeric_vars]
    numeric_vars_scaled <- apply(numeric_vars_scaled, MARGIN = 2, scale_01)
    
    # vector of weights (ordering must match the numeric_vars_scaled column order)
    weights <- c(input$weighting_slider_comfort, input$weighting_slider_cost, 
                 input$weighting_slider_income, input$weighting_slider_pct_hs,
                 input$weighting_slider_unemp)
    
    # calculate score per each row
    data$site_score <- apply(numeric_vars_scaled, MARGIN = 1, function(row) sum(row * weights))
    
    # filter to just the top rows selected by the user based on site_score
    data <- data[order(-data$site_score), ][1:input$weight_n,]
    
    # make site_score as first column in dataframe
    data <- data[, c('site_score', setdiff(colnames(data), "site_score"))]
    
    # round the score
    data$site_score <- round(data$site_score, 1)
    
    return(data)
  })
  
  # display the table in the 'table of selected sites' tab within the weighting page
  output$weighting_selected_table <- DT::renderDataTable(
    DT::datatable(
      weighting_current_data(),
      selection = 'none',
      rownames = FALSE,
      options = list(
        # sets n observations shown
        pageLength = 20,
        # removes option to change n observations shown
        lengthChange = FALSE,
        # removes the search bar
        sDom  = '<"top">lrt<"bottom">ip',
        # default sort by site score
        order = list(0, 'desc'),
        # enable side scroll so table doesn't overflow
        scrollX = TRUE
      )
    ) %>%
      formatRound(6:9, 2) %>%
      formatRound(10, 0)
  )
  
  
  # manual exclusions page --------------------------------------------------
  
  # select which dataset to use on manual exclusison page
  manual_selected_data <- reactive({
    selected_data <- datasets_available$data[[match(input$manual_dataset, datasets_available$data_names)]]
    
    # exclude rows from manual input
    selected_data <- selected_data[!selected_data$site_id %in% input$sites_excl,]
    
    # exclude rows based on row selection
    selected_data <- selected_data[!selected_data$site_id %in% dd$all_row_selections,]
    
    return(selected_data)
  })
  
  # save row selections when button is clicked
  # TODO i think there is an issue here as the row selections are a global variable
  # when there should be specific to each table. Fix could be to reset the row selections where
  # input$manual_dataset changes?
  dd <- reactiveValues(select = NULL)
  observeEvent(input$save_row_selections, {
    
    # get current selected rows
    dd$current_row_selections <- manual_selected_data()$site_id[input$manual_table_selected_rows_selected]
    
    # maintain list of all rows that have been selected
    dd$all_row_selections <- sort(unique(append(dd$all_row_selections, dd$current_row_selections)))
    
  })
  
  # display the table in the 'table of selected sites' tab within the final selection page
  output$manual_table_selected <- DT::renderDataTable(
    custom_datatable(
      manual_selected_data()
    ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0))
  
  # display the table in the 'table of excluded sites' tab: Final selection tab
  output$manual_table_excluded <- DT::renderDataTable(
    custom_datatable(
      anti_join(population_dataset, manual_selected_data())
    ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0))
  
  
  # send invitations page ---------------------------------------------------
  
  # select which dataset to use on send invitations tab
  sent_invitations_data <- reactive({
    datasets_available$data[[match(input$invitations_data, datasets_available$data_names)]]
  })
  
  # text for final selection page
  output$summary_final_selection <- renderText({
    
    data <- sent_invitations_data()
    
    n_sites <- nrow(data)
    mean_acceptance <- mean(data$comfort)
    expected_cost <- sum(data$comfort * data$cost)
    
    paste0(
      '<h4>',
      scales::comma_format()(n_sites),
      ' sites are currently selected to be approached. Of these, ',
      floor(n_sites * mean_acceptance), ' sites are expected to accept the invitation, and have a total expected cost of ',
      scales::label_dollar(accuracy = 1)(expected_cost), '.</h4>'
    )
  })
  
  
  # table of key metrics for the send invitations page
  output$send_scores_table <- renderTable(
    score_attributes({
      sent_invitations_data()
    }), 
    rownames = TRUE, align = 'r'
  )
  
  # take action when the submit invitations button is clicked
  observeEvent(input$send_invitations_button, {
    
    # move user to the final tab
    updateNavlistPanel(session = session, inputId = "nav", selected = "4. Results")
    showTab(inputId = "nav", target = "4. Results", session = session)
    
    # hide old tabs
    hide(selector = "li.navbar-brand") # this hides the HTML(2. Site selection) text
    hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Data description"), session = session)
    hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Filtering"), session = session)
    hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Weighting"), session = session)
    hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Sampling"), session = session)
    hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Manual exclusions"), session = session)
    hideTab(inputId = "nav", target = "3. Send invitations", session = session)
    
    # save which dataset was used to send invitations
    sent_invitations_data <<- sent_invitations_data()
    
    # flip a coin with prob = comfort to see which sites accepted
    accepted_boolean <- rbinom(n = nrow(sent_invitations_data), 
                               size = 1, 
                               prob = sent_invitations_data$comfort) == 1
    sites_that_accepted <<- sent_invitations_data[accepted_boolean,]
    
    # final data frame of of all sites with indicator if site was sent inviation and if accepted
    # assign variable to global environment so it can be used in other functions
    population_dataset$sent_invitation <- population_dataset$site_id %in% sent_invitations_data$site_id
    population_dataset$accepted <- population_dataset$site_id %in% sites_that_accepted$site_id
    final_results <<- population_dataset
    
    # create a stacked dataframe with observation per population, sent invitations, and accepted invitations
    # i.e. nrow(...) should be nrow(population) + nrow(sent_invitations) + nrow(accepted)
    tmp1 <- sent_invitations_data
    tmp1$site_group <- "Sent_invitation"
    tmp2 <- sites_that_accepted
    tmp2$site_group <- "Accepted_invitation"
    tmp3 <- population_dataset[, setdiff(colnames(population_dataset), c("sent_invitation", "accepted"))]
    tmp3$site_group <- "Population"
    stacked_results <- rbind(tmp1, tmp2, tmp3)
    rm(tmp1, tmp2, tmp3)
    
    # factor the site_group variable so its in the right order
    stacked_results$site_group <- factor(stacked_results$site_group,
                                         levels = c("Accepted_invitation",
                                                    "Sent_invitation",
                                                    "Population"))
    
    # add stacked_results to list of dataframes available
    datasets_available$data <- c(datasets_available$data, 
                                 list(stacked_results))
    datasets_available$data_names <- c(datasets_available$data_names,
                                       "stacked_results")
    
    # on the data exploration page, added a grouping variable that represents 
      # population, sent invitations, and accepted invitations sites
    updateSelectInput(session = session, inputId = "plot_Data",
                      selected = "stacked_results")
    updateSelectInput(session = session, inputId = "facet_variable",
                      choices = c("none", "site_group", categorical_vars),
                      selected = "site_group")
    updateSelectInput(session = session, inputId = "group_variable",
                      choices = c("none", "site_group", categorical_vars))
    
    # create the three checkmark boxes on the data exploration page
    output$exploration_selection_data_spawn <- renderUI({
      tagList(
        checkboxGroupInput(inputId = "exploration_checkboxes",
                           label = "Sites to include:",
                           choices = list("Population" = "Population",
                                          "Sent invitation" = "Sent_invitation",
                                          "Accepted invitation" = "Accepted_invitation"),
                           selected = c("Population", "Sent_invitation", "Accepted_invitation")
                           ),
        br()
      )
    })
    
    # remove dataset selector element from the data exploration page
    removeUI(selector = "#plot_Data_div")
    
  })
  
  # plots on send invitations page
  output$send_plots <- renderPlot({draw_histograms({sent_invitations_data()})})
  
  
  # results page ------------------------------------------------------------
  
  # text for Results page page
  output$summary_text <- renderText({
    
    data <- final_results[final_results$accepted,]
    
    # calculate summary stats
    n_sites <- nrow(data)
    expected_cost <- sum(data$comfort * data$cost)
    
    # paste together the sentence
    sentence <- paste0(
      '<h2>Congrats! ', scales::comma_format()(n_sites), ' sites accepted the invitation</h2>'
    )
    
    return(sentence)
  })
  
  # table of key metrics for the Results page
  output$key_metrics_table <- renderTable(
    score_attributes(sites_that_accepted), 
    rownames = TRUE, align = 'r'
  )
  
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
  
  # summary table for the Results page
  output$summary_table <- renderTable({
    
    data <- final_results
    
    # create list of dataframes representing the three groups
    list_of_tables <- list(sites_that_accepted, sent_invitations_data, data)
    
    # calculate mean numeric stats per group
    numeric_means <- sapply(list_of_tables, function(df){
      apply(df[, numeric_vars], MARGIN = 2, FUN = mean)
    })
    
    # calculate proportions for categorgical per group
    categorical_proportions <- sapply(list_of_tables, function(df) {
      
      # calculate percent other prog / urbancity
      prog_urban <- apply(df[, c("other_prog", "urban")], MARGIN = 2, function(col) {
        table(col)["TRUE"] / length(col)
      })
      
      # calculate proportion per region
      regions <- apply(df[, "region"], MARGIN = 2, function(col) {
        table(col) / length(col)
      })
      
      # bind data into a matrix
      all_categoricals <- rbind(as.matrix(prog_urban), regions)
      
      return(all_categoricals)
    })
    
    # add row names
    rownames(categorical_proportions) <- c("other_prog", "urban", 
                                           sort(unique(population_dataset$region)))
    
    # bind into one table and round the figures
    summary_table <- rbind(numeric_means, categorical_proportions)
    summary_table <- round(summary_table, 2)
    
    # set column and row names
    colnames(summary_table) <- c('Accepted', 'Sent invitation', 'Population')
    rownames(summary_table) <- c(
      "Mean comfort",
      "Mean cost",
      "Mean income",
      "Mean HS rate",
      "Mean unemployment",
      "% with other program",
      "% urbanicity",
      "% Northcentral",
      "% Northeast",
      "% South",
      "% West"
    )
    
    return(summary_table)
    
  }, rownames = TRUE
  )
  
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
    
    # reorder population variables
    data_for_plot$population <- factor(data_for_plot$population,
                                       levels = c("Accepted_invitation",
                                                  "Sent_invitation",
                                                  "Population"))
    
    # barplots
    p1 <- data_for_plot %>% 
      select(population, all_of(categorical_vars)) %>% 
      mutate_at(categorical_vars, as.character) %>% 
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
  
  
  # actual vs expected plots
  output$actual_vs_expected_plots <- renderPlot({
    
    data <- final_results
    sent_invitations_data <- data[data$sent_invitation,]
    
    # flip a coin with prob = comfort to see which sites accepted
    list_of_accepted_dataframes <- list()
    nsims <- 250
    for (i in 1:nsims){
      # sample the data and return T/F for indices that accepted
      accepted_boolean <- rbinom(n = nrow(sent_invitations_data), 
                                 size = 1, 
                                 prob = sent_invitations_data$comfort) == 1  
      
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
                   size = 1.3, linetype = "dotted") +
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
      geom_density(data = sites_that_accepted_numeric, 
                   color = "white", size = 1.1) +
      geom_density(data = sites_that_accepted_numeric, 
                   color = "#302f42", size = 1.3, linetype = "dotted") +
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
