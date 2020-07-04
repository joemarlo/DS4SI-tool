library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(plotly)

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

# load data ---------------------------------------------------------------

sites.DF <- read_csv("jpta_cleaned.csv")


# joe's assignment code ---------------------------------------------------

#remove outliers
###need to throw out site 188###
sites.DF[which.min(sites.DF$distance),]
sites.DF <- sites.DF[sites.DF$site_id != 6573,]

#function "scales" the data between 0:1
calc.proportion <- function(distribution){
    (distribution - min(distribution)) / (max(distribution) - min(distribution))
}

C.sites.DF <- sites.DF %>% select(site_id, cost, unemp, pct_hs, income, comfort)
proportions <- apply(X = C.sites.DF, MARGIN = 2, FUN = calc.proportion)
proportions <- proportions[, 2:ncol(proportions)]
colnames(proportions) <- c("cost.P", "unemp.P", "pct_hs.P", "income.P", "comfort.P")
proportions <- as.data.frame(proportions)

#need to make sure percentiles are ordered correctly; high income proportion may mean more income
# cost: down
# unemp: up
# pct_hs: down
# income: down
# comfort: up

#categories to apply the weight against is determined by position in DF
weights <- c(1.25, 1.5, 1, 1.5, 1)

#reverse percentiles for relevant categories; 
#    sum P to generate a single score based on weights
#    bind back to original DF
sites.ranked.DF <- proportions %>%
    mutate(cost.P = 1 - cost.P,
           pct_hs.P = 1 - pct_hs.P,
           income.P = 1 - income.P) %>%
    rowwise() %>%
    # mutate(site_score = sum(c(cost.P, unemp.P, pct_hs.P, income.P, comfort.P))) %>%
    mutate(site_score = sum(c(cost.P, unemp.P, pct_hs.P, income.P, comfort.P) * weights)) %>%
    cbind(sites.DF, .) %>%
    ungroup()

#top sites
top.sites.DF <- sites.ranked.DF %>%
    filter(other_prog == F) %>% #remove sites that have another program
    arrange(desc(site_score)) %>%
    mutate(Exp_site_ct = cummean(comfort)*row_number()) %>%
    filter(Exp_site_ct <= 35) #filters the DF so we expect that we'll have 35 sites after accounting for comfort

#total expected sites
mean(top.sites.DF$comfort) * nrow(top.sites.DF)

#add identifier for top site to the main DF
sites.ranked.DF$top_site <- sites.ranked.DF$site_id %in% top.sites.DF$site_id
rm(C.sites.DF, proportions, top.sites.DF, weights)

final_table <- sites.DF

# app ----------------------------------------------------------------------


# shinyApp(
#     ui = fluidPage(
#         tabsetPanel(
#             tabPanel("Filters", fluid = TRUE,
#                      sidebarLayout(
#                          sidebarPanel(selectInput("Country", "Select Country", choices = "", selected = "")),
#                          mainPanel(
#                              htmlOutput("Attacks")
#                          )
#                      )
#             ),
#             tabPanel("Weights", fluid = TRUE,
#                      sidebarLayout(
#                          sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
#                          mainPanel(fluidRow(
#                              column(7,  plotlyOutput("")),
#                              column(5, plotlyOutput(""))   
#                          )
#                          )
#                      )
#             )
#         )
#     ), 
#     server = function(input, output) {
#         
#     }
# )


# Define UI for application that draws a histogram
ui <- fluidPage(
    # theme = "my-shiny.css",

    # jQuery to keep text at top when scrolling
    # tags$script(
    #     HTML(
    #         "$(window).scroll(function(e){
    #     var $el = $('.subheader-container');
    #     var isPositionFixed = ($el.css('position') == 'fixed');
    #     if ($(this).scrollTop() >= 110 && !isPositionFixed){
    #         $el.css({'position': 'fixed', 'top': '0px', 'display': 'block'});
    #         }
    #     if ($(this).scrollTop() < 110 && isPositionFixed){
    #     $el.css({'position': 'static', 'top': '0px', 'display': 'none'});
    #     }
    # });"
    #     )
    # ),
    #
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

    # text output of title with probability results
    # htmlOutput("probability_results"),
    # htmlOutput("probability_results_scroll"),

    br(),

    sidebarLayout(
        sidebarPanel(
            
            tabsetPanel(type = 'tabs',
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
            sliderInput("unemp_slider", "Unemployment: ", min = 0.05, max = 0.15,
                        value = c(0.05, 0.15), step = 0.01),
            sliderInput("hs_slider", "High school graduation rate: ", min = 0.5, max = 0.9,
                        value = c(0.5, 0.9), step = 0.01),
            sliderInput("income_slider", "Income: ", min = 40, max = 75,
                        value = c(40, 75), step = 1),
            sliderInput("comfort_slider", "Comfort: ", min = 0.08, max = 0.95,
                        value = c(0.08, 0.95), step = 0.01),
            sliderInput("cost_slider", "Cost: ", min = 770, max = 3100,
                        value = c(770, 3100), step = 10)
            ),
            
            
            tabPanel("Weights",
                     br(),
            sliderInput("slider1", "Unemployment: ", min = 0, max = 100, value = 0, step = 1),
            sliderInput("slider2", "High school graduation rate: ", min = 0, max = 100, value = 0, step = 1),
            sliderInput("slider3", "Income: ", min = 0, max = 100, value = 0, step = 1),
            sliderInput("slider4", "Comfort: ", min = 0, max = 100, value = 0, step = 1),
            sliderInput("slider4", "Site cost: ", min = 0, max = 100, value = 0, step = 1)

            ))),
        
        mainPanel(
            
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Plots", plotOutput("scatter")), # height = "500px"),
                    tabPanel("Table of selected sites", DT::dataTableOutput('table')), # height = "500px"),
                    tabPanel("Summary plots", plotOutput("boxplots")))
                    # tabPanel("Table", tableOutput("table"))

        # Show a plot of the generated distribution
        # mainPanel(DT::dataTableOutput('table'), # height = "500px")
        #           plotOutput("boxplots"))
    )

    # text for bottom of the page
    # HTML(
    #     '<div class="belowplot" >
    #      <p>Probability estimates based on 5,000 simulations @ alpha = 0.05</p>
    #      <p>See
    #      <a href="https://www.marlo.works/posts/a-b-testing/" target="_blank">marlo.works/A-B-Testing</a>
    #       for more info</p>
    #      </div>'
    # )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {


    # the table
    output$table <- DT::renderDataTable(DT::datatable({
        data <- final_table

        # filters
        data <- data[data$region == input$region_slider,]
        data <- data[data$urban == input$urban_slider,]
        data <- data[data$other_prog == input$other_program_slider,]
        data <- data[data$unemp >= input$unemp_slider[1] & data$unemp <= input$unemp_slider[2],]
        data <- data[data$pct_hs >= input$hs_slider[1] & data$pct_hs <= input$hs_slider[2],]
        data <- data[data$income >= input$income_slider[1] & data$income <= input$income_slider[2],]
        data <- data[data$comfort >= input$comfort_slider[1] & data$comfort <= input$comfort_slider[2],]
        data <- data[data$cost >= input$cost_slider[1] & data$cost <= input$cost_slider[2],]

        data
    }, options = list(
        # sets n observations shown
        pageLength = 10,
        # removes option to change n observations shown
        lengthChange = FALSE,
        # removes the search bar
        sDom  = '<"top">lrt<"bottom">ip')) %>%
        formatRound(5:8, 2) %>%
        formatRound(9, 0))

    # the plots
    output$scatter <- renderPlot({
        
        # filters
        data <- final_table
        data <- data[data$region == input$region_slider,]
        data <- data[data$urban == input$urban_slider,]
        data <- data[data$other_prog == input$other_program_slider,]
        data <- data[data$unemp >= input$unemp_slider[1] & data$unemp <= input$unemp_slider[2],]
        data <- data[data$pct_hs >= input$hs_slider[1] & data$pct_hs <= input$hs_slider[2],]
        data <- data[data$income >= input$income_slider[1] & data$income <= input$income_slider[2],]
        data <- data[data$comfort >= input$comfort_slider[1] & data$comfort <= input$comfort_slider[2],]
        data <- data[data$cost >= input$cost_slider[1] & data$cost <= input$cost_slider[2],]
        
        data %>% 
            select(unemp, pct_hs, income, comfort, cost) %>% 
            pivot_longer(cols = everything()) %>% 
            ggplot(aes(x = value)) +
            geom_density() +
            facet_wrap(~name, scales = 'free')

    })
    
    # the summary plots
    output$boxplots <- renderPlot({

        # use table output as the inferred selected sites to approach
        selected_sites <- sites.DF[input$table_rows_all,]


        # draw the plot
        sites.DF[input$table_rows_all,] %>%
            select(unemp, pct_hs, income, comfort, cost) %>%
            pivot_longer(cols = everything()) %>%
            ggplot(aes(y = value)) +
            geom_boxplot() +
            facet_wrap(~name, scales = 'free_y')
    })
}

# Run the application
shinyApp(ui = ui, server = server)



# old code ----------------------------------------------------------------
# 
# server <- function(input, output) {
#     
#     # look up variables from the slider text to the actuql value
#     sample_size <- reactive({sample_size_values[input$sample_size == sample_size_text]})
# 
#     # filter dataset to inputs and pull out probability
#     probs <- reactive({
#         summarized_results %>%
#             filter(n_checks == input$n_stops,
#                    n_comparisons == input$n_comparisons,
#                    effect_size == input$effect_size,
#                    sample_size == sample_size(),
#                    std_dev == input$spread) %>%
#             pull(Probability_of_finding_an_effect)
#     })
# 
#     # html for text that stays at the top
#     output$probability_results <- renderText({
# 
#         paste0(
#             '<h2>A/B testing: pulling it all together
#             <div>
#             <span class="subheader">Probability of finding at least one effect: &nbsp </span>
#             <span class="emphasis"> ~',
#             round(min(0.99, max(0.05, probs())), 2),
#             '</span>
#             </div>
#             </h2>'
#         )
# 
#     })
# 
#     # html for text that scrolls
#     output$probability_results_scroll <- renderText({
# 
#         # this contains '.subheader-container' which is responsive to scroll position
#         paste0(
#             '<h2>
#             <div class="subheader-container">
#             <span class="subheader">Probability of finding at least one effect: &nbsp </span>
#             <span class="emphasis"> ~',
#             round(min(0.99, max(0.05, probs())), 2),
#             '</span>
#             </div>
#             </h2>'
#         )
#     })
# 
#     output$distPlot <- renderPlot({
# 
#         # simulate random distributions
#         dat <-
#             tibble(
#                 As = as.vector(replicate(input$n_comparisons, rnorm(
#                     n = sample_size(),
#                     mean = 45,
#                     sd = 45 * input$spread
#                 ))),
#                 Bs = as.vector(replicate(input$n_comparisons, rnorm(
#                     n = sample_size(),
#                     mean = 45 * (1 + input$effect_size),
#                     sd = 45 * input$spread
#                 )))
#             )
# 
#         # dynamically adjust facet labels so its readable on mobile
#         if (input$n_comparisons == 1){
#             dat$ID <- "A single comparison between A and B"
#         } else if (input$n_comparisons <= 5){
#             dat$ID <- factor(
#                 rep(paste0("Comparison ", 1:input$n_comparisons), each = sample_size()),
#                 levels = paste0("Comparison ", 1:input$n_comparisons))
#         } else {
#             dat$ID <- factor(
#                 rep(c("Comp. 1", 2:input$n_comparisons), each = sample_size()),
#                 levels =  c("Comp. 1", 2:input$n_comparisons))
#         }
# 
#         # calculate means for each distribution
#         means <- dat %>%
#             group_by(ID) %>%
#             summarize(meanA = mean(As),
#                       meanB = mean(Bs))
# 
#         # facet font size; responsive to number of facets so its readable on mobile
#         facet_font <- if_else(input$n_comparisons > 5, 10, 12)
# 
#         # draw the plot
#         fill_colors <- c("A" = "grey20", "B" = "#4e917e")
#         dat %>%
#             ggplot() +
#             geom_density(aes(x = As, fill = "A"), alpha = 0.6) +
#             geom_density(aes(x = Bs, fill = "B"), alpha = 0.6) +
#             geom_vline(data = means, aes(xintercept = meanA), color = 'black') +
#             geom_vline(data = means, aes(xintercept = meanB), color = '#6fd9bb') +
#             scale_x_continuous(labels = NULL, lim = c(20, 70)) +
#             scale_y_continuous(labels = NULL) +
#             scale_fill_manual(values = fill_colors) +
#             facet_wrap(~ID) +
#             labs(title = "Distributions of A and B",
#                  x = NULL,
#                  y = NULL) +
#             theme(strip.text = element_text(size = facet_font))
# 
#     })
# }

