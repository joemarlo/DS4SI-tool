
# Define UI for application
ui <- fluidPage(
  
  # initialize shinyjs
  useShinyjs(),

  # download roboto font
  HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
  
  # choose default slider skin
  chooseSliderSkin(skin = "Flat",
                   color = "#221146"),
  
  # load custom CSS file
  includeCSS("www/custom_css.css"),
  
  # set top left title
  titlePanel("DS4SI Site Selection"),
  
  br(),
  
  navlistPanel(id = "nav", 
               widths = c(2, 10),
               
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
                            htmlOutput("results_text_summary"),
                            tableOutput("results_table_scores"),
                            br(),br(),
                            actionButton(inputId = "results_button_run_simulation",
                                         label = 'How does your random draw compare to 250 simulated draws?'),
                            br(),br(),
                            downloadButton("results_button_download_data", "Download the data"),
                            br(),br(),
                            actionButton(inputId = "results_button_restart",
                                         label = 'Erase these data and restart the selection process')
                          ),
                          
                          mainPanel(
                            width = 6,
                            tabsetPanel(
                              id = "results_tabs",
                              type = "tabs",
                              tabPanel("Summary statistics",
                                       tableOutput("results_table_summary")),
                              tabPanel("Plots",
                                       plotOutput("results_plot_hist", height = 650)),
                              tabPanel("Sample vs. population",
                                       plotOutput("results_plot_samp_v_pop_", height = 650)),
                              tabPanel("Sites that accepted",
                                       DT::dataTableOutput('results_table_accepted'))
                            )
                          )
                        )), 
               
               HTML('<div><h5>1. Site attributes</h5></div>'),
               
               tabPanel(title = HTML("&nbsp &nbsp Data description"),
                        sidebarLayout(
                          sidebarPanel(width = 4, includeMarkdown("markdowns/data_description.md")),
                          mainPanel(width = 6, plotOutput("description_plots", height = 650))
                        )), 
               
               tabPanel(title = htmlOutput("exploration_tab_name"),
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
                                choices = c(numeric_vars, categorical_vars, "cluster"),
                                selected = numeric_vars[2]
                              ),
                              conditionalPanel(
                                condition = "input.exploration_variable_fill == 'cluster'",
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
                                choices = c('none', 'include'),
                                selected = 'none'
                              )
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
                                choices = c("none", categorical_vars)
                              )
                            ),
                            selectInput(
                              inputId = "exploration_variable_facet",
                              label = "Facet variable: ",
                              multiple = FALSE,
                              choices = c("none", categorical_vars),
                              selected = "none"
                            ),
                            conditionalPanel(
                              condition = "input.exploration_variable_facet != 'none'",
                              selectInput(
                                inputId = "exploration_variable_facet_second",
                                label = "Second facet variable: ",
                                multiple = FALSE,
                                choices = c("none", categorical_vars),
                                selected = "none"
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
                            div(
                              id = "exploration_dataset_div",
                              HTML('<details><summary>Advanced filters</summary>'),
                              selectInput(
                                inputId = "exploration_dataset",
                                label = "Dataset: ",
                                multiple = FALSE,
                                choices = NULL
                              ),
                              br(),br(),br(),br(),
                              HTML('</details><br>')
                            )
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
               
               tabPanel(title = HTML("&nbsp &nbsp Filtering"),
                        
                        sidebarLayout(
                          sidebarPanel(width = 4,
                                       
                                       htmlOutput("filtering_html_n_sites_selected"),
                                       br(),
                                       selectInput(inputId = "filtering_dataset", 
                                                   label = "Dataset to filter: ", 
                                                   multiple = FALSE, 
                                                   choices = NULL),
                                       br(),
                                       uiOutput(outputId = "filtering_select_categorical"),
                                       uiOutput(outputId = "filtering_sliders_numeric"),
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
                                               plotOutput("filtering_plot_hist", height = 650)),
                                      tabPanel("Table of selected sites", DT::dataTableOutput('filtering_table_selected')),
                                      tabPanel("Table of excluded sites", DT::dataTableOutput('filtering_table_excluded'))
                                    )
                          )
                        )
               ),
               
               tabPanel(title = HTML("&nbsp &nbsp Sampling"),
                        sidebarLayout(
                          sidebarPanel(width = 4,
                                       
                                       h4("Create simple and stratified random samples"),
                                       br(),
                                       selectInput(inputId = "sampling_dataset", 
                                                   label = "Dataset to sample: ", 
                                                   multiple = FALSE,
                                                   choices = NULL),
                                       selectInput(inputId = "sampling_select_simple_or_stratified", 
                                                   label = "Simple or stratified sample: ", 
                                                   multiple = FALSE,
                                                   choices = c("simple", "stratified")),
                                       conditionalPanel(
                                         condition = "input.sampling_select_simple_or_stratified == 'stratified'",
                                         selectizeInput(inputId = "strata_variables", 
                                                        label = "Strata variable (limited to two): ", 
                                                        multiple = TRUE,
                                                        options = list(maxItems = 2),
                                                        choices = categorical_vars,
                                                        selected = categorical_vars[2]),
                                         HTML("<strong>Sample size per strata:</strong>"),
                                         uiOutput("sampling_strata_sliders"),
                                         br(),
                                       ),
                                       conditionalPanel(
                                         condition = "input.sampling_select_simple_or_stratified == 'stratified'",
                                         htmlOutput("n_strata"),
                                         br(),
                                         actionButton(inputId = "sample_reset_sliders", label = "Reset sliders"),
                                         br(), br()
                                       ),
                                       conditionalPanel(
                                         condition = "input.sampling_select_simple_or_stratified == 'simple'",
                                         sliderInput(inputId = "sampling_slider_simple_n", 
                                                     label = "Sample size: ", 
                                                     min = 1, 
                                                     max = population_n, 
                                                     value = population_n, 
                                                     step = 1)
                                       ),
                                       actionButton(inputId = "sampling_button_run_sampling", label = "Sample the data"),
                                       # save dataset
                                       br(), br(),
                                       textInput("sampling_data_save_name", value = "my_dataset", label = "Name and save your dataset"),
                                       actionButton("sampling_data_save_button", label = "Save my_dataset"),
                                       bsTooltip(id = "sampling_select_simple_or_stratified",
                                                 title = sampling_message,
                                                 placement = 'top')
                          ),
                          
                          mainPanel(width = 6,
                                    
                                    tabsetPanel(
                                      type = "tabs",
                                      tabPanel("Plots",
                                               plotOutput("sampling_plots", height = 650)), 
                                      tabPanel("Table of selected sites", DT::dataTableOutput('sampling_table_selected')),
                                      tabPanel("Table of excluded sites", DT::dataTableOutput('sampling_table_excluded'))
                                    )
                          )
                        )
               ),
               
               # tabPanel(title = HTML("&nbsp &nbsp Weighting"),
               #          sidebarLayout(
               #            sidebarPanel(width = 4,
               #                         h4("Create a weighted score for each site by setting the importance of each continuous variable below"),
               #                         br(),
               #                         selectInput(inputId = "weighting_dataset", 
               #                                     label = "Dataset to apply weights to: ", 
               #                                     multiple = FALSE,
               #                                     choices = NULL),
               #                         uiOutput("weighting_sliders"),
               #                         sliderInput(inputId = "weighting_slider_n", 
               #                                     label = "Only include top n sites: ", 
               #                                     min = 1, 
               #                                     max = population_n, 
               #                                     value = population_n, 
               #                                     step = 1),
               #                         # save dataset
               #                         br(),
               #                         textInput("weighting_data_save_name", value = "my_dataset", label = "Name and save your dataset"),
               #                         actionButton("weighting_data_save_button", label = "Save my_dataset")
               #            ),
               #            
               #            mainPanel(width = 6,
               #                      DT::dataTableOutput('weighting_selected_table')
               #            )
               #          )
               # ),
               
               tabPanel(title = HTML("&nbsp &nbsp Manual exclusions"),
                        
                        sidebarLayout(
                          sidebarPanel(width = 4,
                                       
                                       h4("Manually exclude sites"),
                                       br(),
                                       selectInput(inputId = "manual_dataset", 
                                                   label = "Dataset to apply exclusions to: ", 
                                                   multiple = FALSE, 
                                                   choices = NULL),
                                       selectInput(inputId = "manual_select_sites_excl", 
                                                   label = "Exclude sites manually by site ID:", 
                                                   multiple = TRUE,
                                                   choices = sort(unique(as.character(population_dataset$site_id)))),
                                       br(),
                                       HTML("<strong>Exclude sites by selecting rows on the table: </strong><br>"),
                                       br(),
                                       actionButton(inputId = "manual_button_save_row_selections", 
                                                    label = "Exclude selected rows"),
                                       br(),br(),
                                       textInput(inputId = "manual_data_save_name", 
                                                 label = "Name and save your dataset", 
                                                 value = "my_dataset", ),
                                       actionButton(inputId = "manual_data_save_button", 
                                                    label = "Save my_dataset")
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
                                       selectInput(inputId = "invitations_dataset", 
                                                     label = "Dataset to send invitations: ", 
                                                     multiple = FALSE,
                                                     choices = NULL),
                                       bsPopover(id = 'invitations_dataset',
                                                 title = "Select your dataset here!",
                                                 content = 'These are the datasets you saved on the previous pages. Select which one contains the sites you wish to invite to participate in the trial.',
                                                 placement = 'top'),
                                       htmlOutput("invitations_text_summary"),
                                       br(),
                                       uiOutput("invitations_table_button"),
                                       br()
                          ),
                          mainPanel(width = 6,
                                    plotOutput("invitations_plots", height = 650)
                          )
                        )
               )
  )
)
