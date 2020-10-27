
# Define UI for application
ui <- fluidPage(
  
  # initialize shinyjs
  useShinyjs(),

  # download roboto font
  HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
  
  # set default slider skin
  chooseSliderSkin(skin = "Flat",
                   color = "#221146"),
  
  # load custom CSS file
  includeCSS("www/custom_css.css"),
  
  # set top left title
  titlePanel(title = h1("NYU DS4SI"),
             windowTitle = "NYU DS4SI"),
  
  br(),
  
  navlistPanel(id = "nav", 
               widths = c(2, 10),

# welcome page ------------------------------------------------------------

               tabPanel("Welcome",
                        mainPanel(width = 10,
                                  tabsetPanel(
                                    type = "tabs",
                                    tabPanel("Welcome",
                                             includeMarkdown("markdowns/welcome_text.md")),
                                    tabPanel("Assignment One",
                                             includeMarkdown("markdowns/assignment_1.md")),
                                    tabPanel("Assignment Two",
                                             includeMarkdown("markdowns/assignment_2.md")),
                                    tabPanel("Assignment Four",
                                             includeMarkdown("markdowns/assignment_4.md"),
                                             actionButton(inputId = "welcome_button_jump_to_Results",
                                                          label = 'I already did Site Selection! Take me to Reproducibility.'))
                                  ))
               ),


# upload data page --------------------------------------------------------

              tabPanel(title = HTML("&nbsp &nbsp Upload dataset"),
                       sidebarLayout(
                         sidebarPanel(
                           width = 4,
                           h4("Upload your dataset that you downloaded in Assignment One"),
                           br(),
                           div(id = "upload_file_div",
                             fileInput(inputId = "upload_file", 
                                       label = "Find the CSV",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values",
                                                  ".csv"),
                                       buttonLabel = "Browse",
                                       placeholder = "sites_sent_invitations.csv"
                                       )
                           ),
                           uiOutput("upload_UI_selection_siteid"),
                           uiOutput("upload_UI_switch_use_site_selection_data"),
                           htmlOutput("upload_html_n_sites"),
                           # UNCOMMENT THIS CHUNK IF REMOVING HARDCODED PERSUASION SCORE
                           # numericInput(inputId = "upload_numeric_persuasion",
                           #              label = "Enter your persuasion score from Assignment Two",
                           #              value = 75,
                           #              min = 1,
                           #              max = 100,
                           #              step = 1),
                           br(),
                           actionButton(inputId = "upload_button_get_results",
                                        label = "Get the final results"),
                           ),
                         mainPanel(
                           width = 6,
                           tabsetPanel(
                             id = "upload_tabs",
                             type = "tabs",
                             tabPanel("Plots",
                                      absolutePanel(id = "upload_panel_message",
                                                    HTML("Please upload a dataset or, if you came directly from Site selection, flip the<br>Site selection switch to use that dataset"),
                                                    style = "z-index: -2;"),
                                      plotOutput("upload_plot_hist", height = 650)),
                             tabPanel("Sites invited",
                                      DT::dataTableOutput('upload_table'))
                             )
                           )
                         )),

# results page ------------------------------------------------------------

               tabPanel(title = HTML("&nbsp &nbsp Summary results"),
                        sidebarLayout(
                          sidebarPanel(
                            width = 4,
                            htmlOutput("results_text_summary"),
                            br(),
                            includeMarkdown("markdowns/results_next_steps.md"),
                            br(),
                            downloadButton("results_button_download", "Download the tables, plots, and site data"),
                            br(),br(),
                            actionButton(inputId = "results_button_restart",
                                         label = 'Erase these data and restart the selection process')
                          ),
                          mainPanel(
                            width = 6,
                            tabsetPanel(
                              id = "results_tabs",
                              type = "tabs",
                              tabPanel("Summary metrics and attributes",
                                       tableOutput("results_table_summary")),
                              tabPanel("Plots",
                                       plotOutput("results_plot_hist", height = 650)),
                              tabPanel("Sampled vs. population",
                                       plotOutput("results_plot_samp_v_pop", height = 650)),
                              tabPanel("Sites that accepted",
                                       DT::dataTableOutput('results_table_accepted'))
                            )
                          )
                        )), 
               
               HTML('<div><h5>1. Site attributes</h5></div>'),

# data description page ---------------------------------------------------

               tabPanel(title = HTML("&nbsp &nbsp Data description"),
                        sidebarLayout(
                          sidebarPanel(width = 4, includeMarkdown("markdowns/data_description.md")),
                          mainPanel(width = 6, plotOutput("description_plots", height = 650))
                        )), 

# data exploration page ---------------------------------------------------

               tabPanel(title = HTML("&nbsp &nbsp Data exploration"), #htmlOutput("exploration_tab_name"),
                        sidebarLayout(
                          sidebarPanel(
                            width = 4,
                            h4("Explore the data using scatter plots, histograms, density plots, and boxplots"),
                            br(),
                            uiOutput("exploration_selection_data_spawn"),
                            selectInput(
                              inputId ="exploration_select_plot_type",
                              label = "Plot type:",
                              multiple = FALSE,
                              choices = c("Scatter", "Histogram", "Density", "Boxplot"),
                              selected = "Scatter"
                            ),
                            selectInput(
                              inputId ="exploration_variable_x",
                              label = "X: ",
                              multiple = FALSE,
                              choices = numeric_vars,
                              selected = numeric_vars[5]
                            ),
                            conditionalPanel(
                              condition = "input.exploration_select_plot_type == 'Scatter'",
                              selectInput(
                                inputId = "exploration_variable_y",
                                label = "Y: ",
                                multiple = FALSE,
                                choices = numeric_vars,
                                selected = numeric_vars[3]
                              ),
                              selectInput(
                                inputId = "exploration_variable_fill",
                                label = "Fill color: ",
                                multiple = FALSE,
                                choices = c(numeric_vars, categorical_vars, "Cluster"),
                                selected = numeric_vars[6]
                              ),
                              conditionalPanel(
                                condition = "input.exploration_variable_fill == 'Cluster'",
                                selectInput(
                                  inputId = "exploration_variable_cluster",
                                  label = "Clustering algorithm: ",
                                  multiple = FALSE,
                                  choices = c('k-means', 'Hierarchical'),
                                  selected = 'k-means'
                                ),
                                sliderInput(
                                  inputId = "exploration_variable_n_clusters",
                                  label = "Number of clusters: ",
                                  min = 2,
                                  max = 10,
                                  value = 4,
                                  step = 1
                                ),
                                HTML(
                                  'Clustering using only selected X and Y variables. Not recommended when faceting.<br><br>'
                                )
                              ),
                              selectInput(
                                inputId = "exploration_variable_size",
                                label = "Size: ",
                                multiple = FALSE,
                                choices = numeric_vars,
                                selected = numeric_vars[1]
                              ),
                              selectInput(
                                inputId = "exploration_variable_regression",
                                label = "Linear regression: ",
                                multiple = FALSE,
                                choices = c('None', 'Include'),
                                selected = 'None'
                              ),
                              bsPopover(id = 'exploration_variable_regression',
                                        title = "Linear regression",
                                        content = 'Apply a linear regression (y ~ x) to each subgroup of your plot. If you facet on a variable, then the regressions will be calculated per each facet group.',
                                        placement = 'top'),
                            ), 
                            conditionalPanel(
                              condition = "input.exploration_select_plot_type == 'Histogram'",
                              sliderInput(
                                inputId = "exploration_variable_n_bins",
                                label = "Number of bins: ",
                                min = 5,
                                max = 50,
                                value = 20,
                                step = 1
                              )
                            ),
                            conditionalPanel(
                              condition = "input.exploration_select_plot_type == 'Boxplot'",
                              selectInput(
                                inputId = "exploration_variable_group",
                                label = "Grouping: ",
                                multiple = FALSE,
                                choices = c("None", categorical_vars)
                              )
                            ),
                            selectInput(
                              inputId = "exploration_variable_facet",
                              label = "Facet variable: ",
                              multiple = FALSE,
                              choices = c("None", categorical_vars),
                              selected = "None"
                            ),
                            bsPopover(id = 'exploration_variable_facet',
                                      title = "Facet variable",
                                      content = 'Faceting splits the data by one or more variables and then plots these subsets.',
                                      placement = 'top'),
                            conditionalPanel(
                              condition = "input.exploration_variable_facet != 'None'",
                              selectInput(
                                inputId = "exploration_variable_facet_second",
                                label = "Second facet variable: ",
                                multiple = FALSE,
                                choices = c("None", categorical_vars),
                                selected = "None"
                              )
                            ),
                            conditionalPanel(
                              condition = "input.exploration_select_plot_type == 'Scatter'",
                              sliderInput(
                                inputId = "exploration_variable_alpha",
                                label = "Opacity: ",
                                min = 0.1,
                                max = 1,
                                value = 0.5,
                                step = 0.1
                              )
                            ),
                            HTML('<details><summary>Advanced</summary>'),
                            div(
                              id = "exploration_dataset_div",  
                              selectInput(
                                inputId = "exploration_dataset",
                                label = "Dataset: ",
                                multiple = FALSE,
                                choices = NULL
                              ),
                            ),
                            actionButton(inputId = "exploration_button_download", 
                                         label = "Download the plot"),
                            br(),
                            HTML('</details><br>')
                          ),
                          
                          mainPanel(
                            width = 6,
                            plotOutput('exploration_plot',  height = 600,
                                       brush = brushOpts(id = "plot1_brush")),
                            br(),
                            htmlOutput("brush_text"),
                            DT::dataTableOutput("brush_info")
                          )
                        )),
               
               HTML("<div><h5>2. Site selection</h5></div>"),

# filtering page ----------------------------------------------------------

               tabPanel(title = HTML("&nbsp &nbsp Filtering"),
                        sidebarLayout(
                          sidebarPanel(width = 4,
                                       
                                       htmlOutput("filtering_html_n_sites_selected"),
                                       br(),
                                       selectInput(inputId = "filtering_dataset", 
                                                   label = "Dataset to filter: ", 
                                                   multiple = FALSE, 
                                                   choices = NULL),
                                       bsPopover(id = "filtering_dataset",
                                                 title = 'Tip!',
                                                 content = 'Once you filter your data, save it and it will become avilable here.'),
                                       br(),
                                       uiOutput(outputId = "filtering_select_categorical"),
                                       uiOutput(outputId = "filtering_sliders_numeric"),
                                       br(),
                                       textInput(inputId = "filtering_data_save_name", 
                                                 value = "my_dataset", 
                                                 label = "Name and save your dataset:"),
                                       actionButton(inputId = "filtering_data_save_button", 
                                                    label = "Save my_dataset")
                          ),
                          mainPanel(width = 6,
                                    
                                    tabsetPanel(
                                      type = "tabs",
                                      tabPanel("Plots",
                                               plotOutput("filtering_plot_hist", height = 650)),
                                      tabPanel("Table of selected sites", DT::dataTableOutput('filtering_table_selected')),
                                      tabPanel("Table of excluded sites", DT::dataTableOutput('filtering_table_excluded'))
                                    )
                          )
                        )
               ),

# sampling page -----------------------------------------------------------

               tabPanel(title = HTML("&nbsp &nbsp Sampling"),
                        sidebarLayout(
                          sidebarPanel(width = 4,
                                       h4("Create simple or stratified random samples"),
                                       br(),
                                       selectInput(inputId = "sampling_dataset", 
                                                   label = "Dataset to sample: ", 
                                                   multiple = FALSE,
                                                   choices = NULL),
                                       uiOutput("sampling_select_simple_or_stratified"),
                                       conditionalPanel(
                                         condition = "input.sampling_select_simple_or_stratified == 'Stratified'",
                                         selectizeInput(inputId = "sampling_select_strata_variables", 
                                                        label = "Strata variable (limited to two): ", 
                                                        multiple = TRUE,
                                                        options = list(maxItems = 2),
                                                        choices = categorical_vars,
                                                        selected = categorical_vars[2]),
                                         HTML("<strong>Sample size per stratum:</strong>"),
                                         uiOutput("sampling_strata_sliders"),
                                         br(),
                                       ),
                                       conditionalPanel(
                                         condition = "input.sampling_select_simple_or_stratified == 'Stratified'",
                                         htmlOutput("n_strata"),
                                         br(),
                                         actionButton(inputId = "sampling_reset_sliders", label = "Reset sliders"),
                                         br(), br()
                                       ),
                                       conditionalPanel(
                                         condition = "input.sampling_select_simple_or_stratified == 'Simple'",
                                         sliderInput(inputId = "sampling_slider_simple_n", 
                                                     label = "Sample size: ", 
                                                     min = 1, 
                                                     max = population_n, 
                                                     value = population_n, 
                                                     step = 1)
                                       ),
                                       actionButton(inputId = "sampling_button_run_sampling", label = "Sample the data"),
                                       br(), br(),
                                       textInput(inputId = "sampling_data_save_name", 
                                                 value = "my_dataset", 
                                                 label = "Name and save your dataset:"),
                                       actionButton("sampling_data_save_button", label = "Save my_dataset")
                          ),
                          mainPanel(width = 6,
                                    
                                    tabsetPanel(
                                      type = "tabs",
                                      tabPanel("Plots",
                                               absolutePanel(id = "sampling_panel_message",
                                                             HTML("'Sample the data' to generate the plots"),
                                                             style = "z-index: -2;"),
                                               plotOutput("sampling_plots", height = 650)), 
                                      tabPanel("Table of selected sites", DT::dataTableOutput('sampling_table_selected')),
                                      tabPanel("Table of excluded sites", DT::dataTableOutput('sampling_table_excluded'))
                                    )
                          )
                        )
               ),

# manual exclusions page --------------------------------------------------

               tabPanel(title = HTML("&nbsp &nbsp Manual exclusions"),
                        sidebarLayout(
                          sidebarPanel(width = 4,
                                       h4("Manually exclude sites by their site ID"),
                                       br(),
                                       selectInput(inputId = "manual_dataset", 
                                                   label = "Dataset to apply exclusions to: ", 
                                                   multiple = FALSE, 
                                                   choices = NULL),
                                       selectInput(inputId = "manual_select_sites_excl", 
                                                   label = "Exclude sites manually by site ID:", 
                                                   multiple = TRUE,
                                                   choices = sort(unique(as.character(population_dataset$`Site ID`)))),
                                       br(),
                                       HTML("<strong>Exclude sites by selecting rows on the table: </strong><br>"),
                                       br(),
                                       actionButton(inputId = "manual_button_save_row_selections", 
                                                    label = "Exclude selected rows"),
                                       br(),br(),
                                       textInput(inputId = "manual_data_save_name", 
                                                 label = "Name and save your dataset:", 
                                                 value = "my_dataset", ),
                                       actionButton(inputId = "manual_data_save_button", 
                                                    label = "Save my_dataset"),
                                       br(), br(),
                                       htmlOutput(outputId = "manual_text_reminder")
                          ),
                          mainPanel(width = 6,
                                    tabsetPanel(
                                      type = "tabs",
                                      tabPanel("Table of selected sites", DT::dataTableOutput('manual_table_selected')),
                                      tabPanel("Table of excluded sites", DT::dataTableOutput('manual_table_excluded'))
                                    )
                          )
                        )),

# send invitations page ---------------------------------------------------

               tabPanel(title = "3. Send invitations",
                        sidebarLayout(
                          sidebarPanel(width = 4,
                                       br(),
                                       selectInput(inputId = "invitations_dataset", 
                                                     label = "Dataset to send invitations: ", 
                                                     multiple = FALSE,
                                                     choices = NULL),
                                       bsPopover(id = 'invitations_dataset',
                                                 title = "Select your dataset here!",
                                                 content = 'These are the datasets you saved on the previous pages. Select which one contains the sites you wish to invite to participate in the trial.',
                                                 placement = 'top'),
                                       h4("This selection of sites has the below metrics. Actual results may vary. Keep in mind you should aim to have 100 sites accept the invitation to participate."),
                                       br(),
                                       tableOutput("invitations_table_scores"),
                                       br(),br(),
                                       actionButton(inputId = "invitations_button_send",
                                                    label = HTML(invitations_HTML_send)),
                                       br()
                          ),
                          mainPanel(
                            width = 6,
                            tabsetPanel(
                              id = "invitations_tabs",
                              type = "tabs",
                              tabPanel("Plots",
                                       plotOutput("invitations_plots", height = 650)),
                              tabPanel("Sites vs. population",
                                       plotOutput("invitations_plot_sites_v_pop", height = 650)),
                              tabPanel("Sites to send invitations",
                                       DT::dataTableOutput('invitations_table_send')),
                              tabPanel(title = "Expected attributes",
                                       absolutePanel("One moment while the simulation runs",
                                                     style = "z-index: -2;"),
                                       plotOutput("invitations_plot_expected_attributes", height = 650),
                                       absolutePanel(id = "invitations_plot_att_box", 
                                                     class = "floating_message",
                                                     top = "auto", left = "auto", right = 25, bottom = 25,
                                                     width = "30%", height = "auto", draggable = FALSE,
                                                     invitations_plot_att_box_message,
                                                     actionButton(inputId = 'invitations_button_exit_att_box',
                                                                  label = 'Close this box'))),
                              tabPanel(title = "Expected metrics",
                                       absolutePanel("One moment while the simulation runs",
                                                     style = "z-index: -2;"),
                                       plotOutput("invitations_plot_expected_attributes_metrics", height = 433),
                                       absolutePanel(id = "invitations_plot_metrics_box", 
                                                     class = "floating_message",
                                                     top = "auto", left = "auto", right = 50, bottom = 25,
                                                     width = "50%", height = "auto", draggable = FALSE,
                                                     invitations_plot_metrics_box_message,
                                                     actionButton(inputId = 'invitations_button_exit_metrics_box',
                                                                  label = 'Close this box')))
                            )
                          )
                        )
               ),

        # these are the fake nav text
        HTML('<div><h5>4. Results</h5></div>'),
        HTML('<div><h5>&nbsp &nbsp Upload dataset</h5></div>'),
        HTML('<div><h5>&nbsp &nbsp Summary results</h5></div>')
        # HTML('<div><h5>&nbsp &nbsp Data exploration</h5></div>')
  )
)
