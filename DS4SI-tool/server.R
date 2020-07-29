
# Define server logic
server <- function(input, output, session) {
  
  # hide the Results tab on start
  hideTab(inputId = "nav", target = "4. Results", session = session)
  
  # name the data exploration tab panel (this name is later changed to 
    # 'sites exploration' once the send invitations button is clicked)
  output$exploration_tab_name <- renderText({"&nbsp &nbsp Data exploration"})
  
  # saving datasets ---------------------------------------------------------
  
  # initialize list of saved datasets
  datasets_available <- reactiveValues(data = NULL, data_names = NULL)
  datasets_available$data <- list(population_dataset)
  datasets_available$data_names <- c("Population")
  
  # update list of dataframes in the dataset dropdowns
  # this applies to every page that has a dataset dropdown
  observeEvent(datasets_available$data, {
    
    # list of dropdown ids
    dataset_selector_ids <- c(
      "filtering_dataset", 
      "sampling_dataset", 
      # "weighting_dataset",
      "manual_dataset",
      "invitations_dataset",
      "exploration_dataset"
    )
    
    # update the dropdown options
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
    # "weighting_data_save",
    "manual_data_save"
  )
  
  # loop through each of the prefixes and take action
  # this code chunk controls most of the saving of datasets
  lapply(data_save_name_prefixes, function(id){
    
    save_name <- paste0(id, "_name")
    save_button <- paste0(id, "_button")
    
    # make save button label equal to the inputted dataset name 
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
      
      # make sure inputted dataset name is not already used and is not length 0
      # this prevents the saving of datasets if the name already exists in
      # datasets_available$data_names
      validate(
        need(!(clean_save_name %in% str_to_lower(datasets_available$data_names)) &
               length(clean_save_name) > 0,
             "Dataset name already used")
      )
      
      # get the dataset for this page
      dataset_to_save <- switch(
        id,
        "filtering_data_save" = filtering_data(),
        "sampling_data_save" = sampling_data(),
        # remove site_score column before saving
        # "weighting_data_save" = weighting_data()[, setdiff(colnames(weighting_data()), 'site_score')],
        "manual_data_save" = manual_selected_data()
      )
      
      # launch popup confirmation if sites are less than 100
      if (nrow(dataset_to_save) < min_sites_to_approach) {
        show_alert(
          title = "Less than 100 sites are currently selected",
          text = "You must approach at least 100 sites",
          type = "warning",
          btn_colors = "#302f42",
          session = session
        )
      }
      
      # stop here if number of sites selected is less than 100
      validate(
        need(nrow(dataset_to_save) >= min_sites_to_approach,
             "Less than 100 sites are currently selected")
      )
      
      # add input data to list of of dataframes 
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
                 '").style.color = "#c92626"')
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
  
  # render plots on data description page
  output$description_plots <- renderPlot({draw_histograms(population_dataset)})
  
  
  # exploration page --------------------------------------------------------
  
  # select which dataset to use on data exploration tab
  exploration_selected_data <- reactive({
    
    # if the send invitations button has been triggered then use the 
    # "stacked_results" dataframe instead of the user selected dataset
    if ("stacked_results" %in% datasets_available$data_names){
      data <- datasets_available$data[[match("stacked_results", datasets_available$data_names)]]
      
      # apply filters from checkmark boxes
      data <- data[data$site_group %in% input$exploration_checkboxes,]
      
      return(data)
      
    } else {
      # otherwise use the user selected dataframe on the dropdown
      datasets_available$data[[match(input$exploration_dataset, datasets_available$data_names)]]
    }
  })
  
  # site exploration plots
  output$exploration_plot <- renderPlot({
    
    # set which dataset to use
    plot_data <- exploration_selected_data()
    
    # add kmeans cluster to dataframe
    if(input$exploration_select_plot_type == 'Scatter' & input$exploration_variable_fill == "cluster"){
      
      # run kmeans algo
      km <- kmeans(x = plot_data[, c(input$exploration_variable_x, input$exploration_variable_y)],
                   centers = input$n_clusters, iter.max = 50, nstart = 5)
      
      # add cluster assignment to the dataframe
      plot_data$cluster <- as.factor(km$cluster)
      
      # hclust
      # dist_matrix <- dist(plot_data[, c(input$exploration_variable_x, input$exploration_variable_y)])
      # clust <- hclust(d = dist_matrix,  method = 'ward.D2')
      # plot_data$cluster <- as.factor(cutree(clust, input$n_clusters))
      
    }
    
    # create base ggplot object
    p <- plot_data %>% 
      ggplot(aes_string(x = input$exploration_variable_x))
    
    # scatter
    if (input$exploration_select_plot_type == 'Scatter'){
      p <- p +
        geom_point(aes_string(y = input$exploration_variable_y,
                              fill = input$exploration_variable_fill,
                              size = input$exploration_variable_size,
                              color = input$exploration_variable_fill),
                   alpha = input$alpha_variable)
      
      # regression line
      if(input$exploration_variable_regression == 'include'){
        p <- p + geom_smooth(
          aes(y = get(input$exploration_variable_y)),
          method = "lm",
          formula = 'y ~ x',
          color = "grey20"
        )
      } 
      
      # cluster centers
      if(input$exploration_variable_fill == "cluster"){
        p <- p +
          geom_point(data = as_tibble(km$centers),
                     aes_string(x = colnames(km$centers)[1],
                                y = colnames(km$centers)[2]),
                     color = violet_col,
                     shape = 4, size = 8, stroke = 1.5)
      }
      
    }
    
    # histogram
    if (input$exploration_select_plot_type == 'Histogram'){
      p <- p + geom_histogram(color = 'white', bins = input$n_bins, 
                              fill = violet_col, alpha = 0.9) +
        labs(y = NULL)
    }
    
    # density
    if (input$exploration_select_plot_type == 'Density'){
      p <- p + geom_density(fill = violet_col, alpha = 0.5) +
        labs(y = NULL)
    }
    
    # boxplot
    if (input$exploration_select_plot_type == 'Boxplot'){
      p <- p + 
        geom_boxplot(fill = violet_col, alpha = 0.5,
                     if(input$group_variable != 'none') aes_string(y = input$group_variable)
        ) +
        coord_flip() +
        scale_y_discrete()
    }
    
    # add faceting
    if (input$exploration_variable_facet != "none"){
      
      if (input$exploration_variable_facet_second == "none"){
        p <- p + facet_grid(input$exploration_variable_facet, labeller = label_both)
      } else{
        p <- p + facet_grid(list(input$exploration_variable_facet, input$exploration_variable_facet_second),
                            labeller = label_both)
      }
    } 
    
    # show plot
    return(p)
  })
  
  
  # text above the brush table
  output$brush_text <- renderText({
    
    if (input$exploration_variable_facet == "none" & input$exploration_select_plot_type == 'Scatter') {
      txt <- "<h4>Highlight data points on the above plot to view their information below</h4>"
    } else {
      txt <- NULL
    }
    
    return(txt)
    
  })
  
  # table of brushed data points from plot
  output$brush_info <- DT::renderDataTable(
    
    # show only if there isn't faceting
    if (input$exploration_variable_facet == "none" & input$exploration_select_plot_type == 'Scatter') {
      
      custom_datatable(
        brushedPoints(population_dataset, input$plot1_brush),
        selection = "none"
      ) %>%
        formatRound(5:8, 2) %>%
        formatRound(9, 0)
    })
  
  
  # filtering page ----------------------------------------------------------
  
  # text at top of page
  output$filtering_html_n_sites_selected <- renderText(
    paste0('<h4>',
           'Apply filters to the data using the inputs below. ',
           scales::comma_format()(nrow(filtering_data())),' 
             sites are currently selected</h4>'
    )
  )
  
  # select which dataset to use on filtering tab
  filter_selected_data <- reactive({
    datasets_available$data[[match(input$filtering_dataset, datasets_available$data_names)]]
  })

  # generate select inputs for each categorical variable
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
  filtering_data <- reactive({
    
    # apply user's inputted filters
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
  
  # initiate a popup notifying the user if at least 100 sites have been selected
  observeEvent(nrow(filtering_data()), {

    # if the user is on the filtering page and the selected dataset has less
    # than 100 sites then show the popup
    if(input$nav == HTML("&nbsp &nbsp Filtering") &
       nrow(filtering_data()) < min_sites_to_approach) {

      # launch popup confirmation
      show_alert(
        title = "Less than 100 sites are currently selected",
        text = "You must approach at least 100 sites",
        type = "warning",
        btn_colors = "#302f42",
        session = session
      )
    }
  })
  
  # histograms and bar plots on 'plots' tab
  output$filtering_plot_hist <- renderPlot({draw_histograms(filtering_data())})
  
  # display the table in the 'table of selected sites' tab
  output$filtering_table_selected <- DT::renderDataTable(
    custom_datatable(
      filtering_data(),
      selection = 'none'
    ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0))
  
  # display the table in the 'table of excluded sites' tab
  output$filtering_table_excluded <- DT::renderDataTable(
    custom_datatable(
      anti_join(population_dataset, filtering_data()),
      selection = 'none'
    ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0))
  
  
  # sampling page -----------------------------------------------------------
  
  # select which dataset to use on sampling tab
  sampling_selected_data <- reactive({
    datasets_available$data[[match(input$sampling_dataset, datasets_available$data_names)]]
  })
  
  # update sampling_slider_simple_n slider max so it's not larger than the dataset
  observeEvent(nrow(sampling_selected_data()), {
    updateSliderInput(session = session, 
                      inputId = 'sampling_slider_simple_n', 
                      value = ceiling(nrow(sampling_selected_data()) / 2),
                      max = nrow(sampling_selected_data()))
  })
  
  # table of strata combinations that exist for the selected dataset
  strata_combos <- reactive({
    
    # create vector of the pairwise variable names
    # this method works even if one or two input$strata_variables are entered
    name_combos <- Reduce(
      x = sampling_selected_data()[, input$strata_variables],
      f = function(var1, var2) paste(var1, var2, sep = "_")
    )
    
    # tally the names
    strata_combos <- as.data.frame(table(name_combos))
    
    # clean up names and variable types
    colnames(strata_combos) <- c("name", "n")
    strata_combos$name <- as.character(strata_combos$name)
    
    return(strata_combos)
    
  })
  
  # generate sliders for each strata combinations
  output$sampling_strata_sliders <- renderUI({
    tagList(
      map2(.x = strata_combos()$name,
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
    slider_ids <- strata_combos()$name
    
    # update the position of the sliders to the maximum amount that still
    # allows equality across the sliders
    lapply(slider_ids, function(slider){
      updateSliderInput(session = session, inputId = slider, value = min(strata_combos()$n))
    })
    
  })
  
  # random sampling
  sampling_data <- eventReactive(input$sampling_button_run_sampling, {
    
    data <- sampling_selected_data()
    
    # if simple sampling
    if (input$sampling_select_simple_or_stratified == "simple"){
      
      # sample the data
      indices <- sample(1:nrow(data), size = input$sampling_slider_simple_n, replace = FALSE)
      sampled_data <- data[indices,]
      
      return(sampled_data)
      
    } else {
      # stratified sampling
      
      # split the data into groups corresponding to
        # combinations of input$strata_variables 
      split_groups <- split(x = data,
                            f = data[, input$strata_variables],
                            sep = "_")
      
      # reorder list so it matches strata_combos() order
      split_groups <- split_groups[strata_combos()$name]
      
      # list of current slider input values
      sample_size_per_group <- reactiveValuesToList(input)[strata_combos()$name]
      
      # sample n rows per strata
      sampled_data <- map2_dfr(.x = split_groups,
                               .y = sample_size_per_group,
                               .f = function(strata, strata_size){
                                 
                                 # sample the data
                                 indices <- sample(1:nrow(strata), size = strata_size, replace = FALSE)
                                 sampled_strata <- strata[indices,]
                                 return(sampled_strata)
                               })
      
      return(sampled_data)
      
    }
  })
  
  # show text below sample size slider indicating total sample size
  output$n_strata <- renderText({
    slider_sum <- sum(unlist(reactiveValuesToList(input)[strata_combos()$name]))
    paste0("The total selected sample size is ", slider_sum)
  })  
  
  # initiate a popup notifying the user if at least 100 sites have been selected
  observeEvent(nrow(sampling_data()), {
    
    # if the user is on the filtering page and the selected dataset has less
    # than 100 sites then show the popup
    if(input$nav == HTML("&nbsp &nbsp Sampling") &
       nrow(sampling_data()) < min_sites_to_approach) {
      
      # launch popup confirmation
      show_alert(
        title = "Less than 100 sites are currently selected",
        text = "You must approach at least 100 sites",
        type = "warning",
        btn_colors = "#302f42",
        session = session
      )
    }
  })
  
  # the plots for sampling page
  output$sampling_plots <- renderPlot({draw_histograms(sampling_data())})
  
  # display the table in the sampling tab    
  output$sampling_table_selected <- DT::renderDataTable(
    custom_datatable(
      sampling_data(),
      selection = 'none'
    ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0)
  )
  
  # display excluded table in the sampling tab
  output$sampling_table_excluded <- DT::renderDataTable(
    custom_datatable(
      anti_join(population_dataset, 
                sampling_data()),
      selection = 'none'
    ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0)
  )
  
  
  # weighting page ----------------------------------------------------------
  
  # select which dataset to use on weighting page
  # weighting_selected_data <- reactive({
  #   datasets_available$data[[match(input$weighting_dataset, datasets_available$data_names)]]
  # })
  # 
  # # update weighting_slider_n slider max so it's not larger than the dataset
  # observeEvent(nrow(weighting_selected_data()), {
  #   updateSliderInput(session, "weighting_slider_n",
  #                     value = nrow(weighting_selected_data()),
  #                     max = nrow(weighting_selected_data())
  #   )
  # })
  # 
  # # generate sliders for each variable
  # output$weighting_sliders <- renderUI({
  #   tagList(
  #     lapply(numeric_vars, function(variable) {
  #       sliderInput(
  #         inputId = paste0("weighting_slider_", variable),
  #         label = paste0(variable, ": "),
  #         min = 1,
  #         max = 100,
  #         value = 50
  #       )
  #     }
  #     ))
  # })
  # 
  # # current weighted dataset based on inputs
  # weighting_data <- reactive({
  #   
  #   data <- weighting_selected_data()
  #   
  #   # scale the variables
  #   numeric_vars_scaled <- data[, numeric_vars]
  #   numeric_vars_scaled <- apply(numeric_vars_scaled, MARGIN = 2, scale_01)
  #   
  #   # vector of weights (ordering must match the numeric_vars_scaled column order)
  #   weights <- c(input$weighting_slider_comfort, input$weighting_slider_cost, 
  #                input$weighting_slider_income, input$weighting_slider_pct_hs,
  #                input$weighting_slider_unemp)
  #   
  #   # calculate score per each row
  #   data$site_score <- apply(numeric_vars_scaled, MARGIN = 1, function(row) sum(row * weights))
  #   
  #   # filter to just the top rows selected by the user based on site_score
  #   data <- data[order(-data$site_score), ][1:input$weighting_slider_n,]
  #   
  #   # make site_score as first column in dataframe
  #   data <- data[, c('site_score', setdiff(colnames(data), "site_score"))]
  #   
  #   # round the score
  #   data$site_score <- round(data$site_score, 1)
  #   
  #   return(data)
  # })
  # 
  # # display the table in the 'table of selected sites' tab within the weighting page
  # output$weighting_selected_table <- DT::renderDataTable(
  #   DT::datatable(
  #     weighting_data(),
  #     selection = 'none',
  #     rownames = FALSE,
  #     options = list(
  #       # sets n observations shown
  #       pageLength = 20,
  #       # removes option to change n observations shown
  #       lengthChange = FALSE,
  #       # removes the search bar
  #       sDom  = '<"top">lrt<"bottom">ip',
  #       # default sort by site score
  #       order = list(0, 'desc'),
  #       # enable side scroll so table doesn't overflow
  #       scrollX = TRUE
  #     )
  #   ) %>%
  #     formatRound(6:9, 2) %>%
  #     formatRound(10, 0)
  # )
  
  
  # manual exclusions page --------------------------------------------------
  
  # select which dataset to use on manual exclusion page
  manual_selected_data <- reactive({
    selected_data <- datasets_available$data[[match(input$manual_dataset, datasets_available$data_names)]]
    
    # exclude rows from manual input
    selected_data <- selected_data[!selected_data$site_id %in% input$manual_select_sites_excl,]
    
    return(selected_data)
  })
  
  # save row selections when button is clicked
  dd <- reactiveValues(select = NULL)
  observeEvent(input$manual_button_save_row_selections, {
    
    # get the site ids from the current selected rows
    dd$current_row_selections <- manual_selected_data()$site_id[input$manual_table_selected_rows_selected]
    
    # append selected list in the values in the user input field
    updateSelectInput(session = session, inputId = "manual_select_sites_excl",
                      selected = unique(append(input$manual_select_sites_excl, dd$current_row_selections)))
    
  })
  
  # destroy the user input saved IDs after the dataset is saved 
  observeEvent(input$manual_data_save_button, {
    updateSelectInput(session = session,
                      inputId = "manual_select_sites_excl",
                      selected = NA)
  })
  
  # initiate a popup notifying the user if at least 100 sites have been selected
  observeEvent(nrow(manual_selected_data()), {
    
    # if the user is on the filtering page and the selected dataset has less
    # than 100 sites then show the popup
    if(input$nav == HTML("&nbsp &nbsp Manual exclusions") &
       nrow(manual_selected_data()) < min_sites_to_approach) {
      
      # launch popup confirmation
      show_alert(
        title = "Less than 100 sites are currently selected",
        text = "You must approach at least 100 sites",
        type = "warning",
        btn_colors = "#302f42",
        session = session
      )
    }
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
      anti_join(population_dataset, manual_selected_data()),
      selection = 'none'
    ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0))
  
  
  # send invitations page ---------------------------------------------------
  
  # select which dataset to use on send invitations tab
  sent_invitations_data <- reactive({
    datasets_available$data[[match(input$invitations_dataset, datasets_available$data_names)]]
  })
  
  # text for top of send invitations page
  output$invitations_table_summary <- renderText({
    
    data <- sent_invitations_data()
    
    n_sites <- nrow(data)
    mean_acceptance <- mean(data$comfort)
    expected_cost <- sum(data$comfort * data$cost)
    
    # return a red error message if n sites is less than 100
    if (n_sites < min_sites_to_approach){
      error_message <- paste0(
        '<h4 style="color:#c92626">',
        'You must approach at least 100 sites. Your selected dataset contains only ',
        scales::comma_format()(n_sites),
        ' sites.',
        '</h4>'
      )
      return(error_message)
    }
    
    final_HTML <- paste0(
      '<h4>',
      scales::comma_format()(n_sites),
      ' sites are currently selected to be approached. Of these, ',
      floor(n_sites * mean_acceptance), ' sites are expected to accept the invitation, and have a total expected cost of ',
      scales::label_dollar(accuracy = 1)(expected_cost), '.</h4>'
    )
    
    return(final_HTML)
  })
  
  # render invitations_table_scores and invitations_button_send
    # only if selected dataset contains >=100 sites
  output$invitations_table_button <- renderUI({
    
    # only render the table and send invitations button if the selected
    # dataset contains 100 or more sites
    validate(
      need(nrow(sent_invitations_data()) >= min_sites_to_approach,
           "")
    )
    
    tagList(
      tableOutput("invitations_table_scores"),
      br(),
      actionButton(inputId = "invitations_button_send",
                   label = HTML(invitations_HTML_send)
      )
    )
  })
  
  # table of key metrics for the send invitations page
  output$invitations_table_scores <- renderTable(
    score_attributes({
      sent_invitations_data()
    }), 
    rownames = TRUE, align = 'r'
  )
  
  # take action when the submit invitations button is clicked
  observeEvent(input$invitations_button_send, {
    
    # do not take action dataset contains less than 100 sites
    validate(
      need(nrow(sent_invitations_data()) >= min_sites_to_approach,
           "Selected dataset contains less than 100 sites")
    )
    
    # launch popup confirmation
    ask_confirmation(inputId = "invitations_popup_confirm",
                     title = "Are you sure you would like to send the invitations?",
                     text = "Site selection will no longer be available",
                     btn_colors = c("#c7c7c7", "#302f42"),
                     html = TRUE
    )
  })
    
  # if the user confirms on the popup, then do the rest of these actions
  # otherwise stop here
  observeEvent(input$invitations_popup_confirm, {
    if (isTRUE(input$invitations_popup_confirm)) {
      # move user to the final tab
      updateNavlistPanel(session = session,
                         inputId = "nav",
                         selected = "4. Results")
      showTab(inputId = "nav",
              target = "4. Results",
              session = session)
      
      # change the tab name from 'data exploration' to 'results exploration' so
      # user knows it's new
      output$exploration_tab_name = renderText({
        HTML("&nbsp &nbsp Results exploration")
      })
      
      # hide old tabs
      hide(selector = "li.navbar-brand") # this hides the HTML(2. Site selection) text
      hideTab(
        inputId = "nav",
        target = HTML("&nbsp &nbsp Data description"),
        session = session
      )
      hideTab(
        inputId = "nav",
        target = HTML("&nbsp &nbsp Filtering"),
        session = session
      )
      # hideTab(inputId = "nav", target = HTML("&nbsp &nbsp Weighting"), session = session)
      hideTab(
        inputId = "nav",
        target = HTML("&nbsp &nbsp Sampling"),
        session = session
      )
      hideTab(
        inputId = "nav",
        target = HTML("&nbsp &nbsp Manual exclusions"),
        session = session
      )
      hideTab(inputId = "nav",
              target = "3. Send invitations",
              session = session)
      
      # save which dataset was used to send invitations
      sent_invitations_data <<- sent_invitations_data()
      
      # flip a coin with prob = comfort to see which sites accepted
      accepted_boolean <- rbinom(
        n = nrow(sent_invitations_data),
        size = 1,
        prob = sent_invitations_data$comfort
      ) == 1
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
      tmp3 <-
        population_dataset[, setdiff(colnames(population_dataset),
                                     c("sent_invitation", "accepted"))]
      tmp3$site_group <- "Population"
      stacked_results <- rbind(tmp1, tmp2, tmp3)
      rm(tmp1, tmp2, tmp3)
      
      # factor the site_group variable so its in the right order
      stacked_results$site_group <- factor(
        stacked_results$site_group,
        levels = c("Accepted_invitation",
                   "Sent_invitation",
                   "Population")
      )
      
      # add stacked_results to list of dataframes available
      datasets_available$data <- c(datasets_available$data,
                                   list(stacked_results))
      datasets_available$data_names <-
        c(datasets_available$data_names,
          "stacked_results")
      
      # on the data exploration page, add a grouping variable that represents
      # population, sent invitations, and accepted invitations sites
      updateSelectInput(session = session,
                        inputId = "exploration_dataset",
                        selected = "stacked_results")
      updateSelectInput(
        session = session,
        inputId = "exploration_variable_facet",
        choices = c("none", "site_group", categorical_vars),
        selected = "site_group"
      )
      updateSelectInput(
        session = session,
        inputId = "exploration_variable_facet_second",
        choices = c("none", "site_group", categorical_vars),
        selected = "none"
      )
      updateSelectInput(
        session = session,
        inputId = "group_variable",
        choices = c("none", "site_group", categorical_vars)
      )
      
      # create the three checkmark boxes on the data exploration page
      output$exploration_selection_data_spawn <- renderUI({
        tagList(
          checkboxGroupInput(
            inputId = "exploration_checkboxes",
            label = "Sites to include:",
            choices = list(
              "Population" = "Population",
              "Sent invitation" = "Sent_invitation",
              "Accepted invitation" = "Accepted_invitation"
            ),
            selected = c("Population", "Sent_invitation", "Accepted_invitation")
          ),
          # add "Information" expansion below the checkmark boxes
          HTML('<details><summary>Information</summary>'),
          "These datasets are nested within each other. Including all three without faceting or grouping will cause duplicates to appear on the plot.",
          HTML('</details><br>'),
          br()
        )
      })
      
      # remove dataset selector element from the data exploration page
      removeUI(selector = "#exploration_dataset_div")
      
    }
  })
  
  # plots on send invitations page
  output$invitations_plots <- renderPlot({draw_histograms({sent_invitations_data()})})
  
  
  # results page ------------------------------------------------------------
  
  # text for Results page page
  output$results_text_summary <- renderText({
    
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
  output$results_table_scores <- renderTable(
    score_attributes(sites_that_accepted), 
    rownames = TRUE, align = 'r'
  )
  
  # insert tab after running simulation
  observeEvent(input$results_button_run_simulation, {
    
    # insert the tab
    appendTab(inputId = "results_tabs",
              tabPanel("Actual vs. expected", 
                       plotOutput("results_plot_actual_v_expected", height = 650)),
              select = TRUE
    )
    
    # add text
    insertUI(selector = "#results_button_run_simulation",
             where = "afterEnd",
             ui = h4("One moment ... Simulation results will appear on the 'Actual vs. expected' tab"))
    
    # remove button
    removeUI(selector = "#results_button_run_simulation")
    
  })
  
  # summary table for the Results page
  output$results_table_summary <- renderTable({
    
    data <- final_results
    
    # create list of dataframes representing the three groups
    list_of_tables <- list(sites_that_accepted, sent_invitations_data, data)
    
    # calculate mean numeric stats per group
    numeric_means <- sapply(list_of_tables, function(df){
      apply(df[, numeric_vars], MARGIN = 2, FUN = mean)
    })
    
    # convert to data frame and move rownames to a column
    numeric_means <- as.data.frame(numeric_means)
    numeric_means$row_name <- rownames(numeric_means)
    rownames(numeric_means) <- NULL
    colnames(numeric_means) <- c('Accepted', 'Sent invitation', 'Population', 'row_name')
    
    # calculate proportions for categorical per group
    categorical_proportions <- sapply(list_of_tables, simplify = FALSE, function(df) {
      
      # calculate percent other prog / urbanicity
      prog_urban <- apply(df[, c("other_prog", "urban")], MARGIN = 2, function(col) {
        table(col)["TRUE"] / length(col)
      })
      
      # calculate proportion per region
      regions <- apply(df[, "region"], MARGIN = 2, function(col) {
        table(col) / length(col)
      })
      
      # bind data into a matrix
      all_categoricals <- as.data.frame(rbind(as.matrix(prog_urban), regions))
      
      # move row names to column for later merging
      all_categoricals$row_name <- rownames(all_categoricals)
      
      return(all_categoricals)
    })
    
    # merge all the categorical data frames into one using full joins
    categorical_proportions <- Reduce(
      x = categorical_proportions,
      f = function(df1, df2) merge(df1, df2, by = 'row_name', all.x = TRUE, all.y = TRUE)
    )
    
    # set column names
    colnames(categorical_proportions) <- c('row_name', 'Accepted', 'Sent invitation', 'Population')
    
    # merge with numeric means
    summary_table <- rbind(numeric_means, categorical_proportions)
    
    # create shell of the summary table
    # this ensures that any NAs in the previous calculations (e.g. a region is missing)
    # won't affect the structure of the table
    # 'row_name' matches the natual output of the above apply functions
    # 'clean_row_name' is what we want the table to eventually display
    shell_table <- data.frame(
      'row_name' = c(
        "comfort",
        "cost",
        "income",
        "pct_hs",
        "unemp",
        "other_prog",
        "urban",
        "Northcentral",
        "Northeast",
        "South",
        "West"
      ),
      'clean_row_name' = c(
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
      ), stringsAsFactors = FALSE)
    
    # join the tables
    summary_table <- merge(shell_table, summary_table, by = 'row_name', all.x = TRUE)
    
    # replace NAs with 0s
    summary_table[is.na(summary_table)] <- 0
    
    # sort the rows so the labels match shell_table
    summary_table <- summary_table[match(shell_table$clean_row_name, summary_table$clean_row_name),]
    
    # change back to matrix
    final_table <- as.matrix(summary_table[, c('Accepted', 'Sent invitation', 'Population')])
    rownames(final_table) <- summary_table$clean_row_name
    
    return(final_table)
    
  }, rownames = TRUE
  )
  
  # the histograms
  output$results_plot_hist <- renderPlot({draw_histograms(sites_that_accepted)})
  
  # sample vs population plots
  output$results_plot_samp_v_pop_ <- renderPlot({
    
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
      mutate(prop = n / sum(n),
             name = factor(name, levels = categorical_vars)) %>% 
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
  output$results_table_accepted <- DT::renderDataTable(
    
    custom_datatable(
      sites_that_accepted,
      selection = 'none'
    ) %>%
      formatRound(5:8, 2) %>%
      formatRound(9, 0)
    
  )
  
  
  # actual vs expected plots
  output$results_plot_actual_v_expected <- renderPlot({
    
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
  output$results_button_download_data <- downloadHandler(
    filename <- "DS4SI_sites.csv",
    content <- function(file) {
      write.csv(final_results, file, row.names = FALSE)
    }
  )
  
  # restart the session based on button click
  observeEvent(input$results_button_restart, {
    session$reload()
  } )
  
}
