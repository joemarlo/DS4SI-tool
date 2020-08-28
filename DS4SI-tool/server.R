
# Define server logic
server <- function(input, output, session) {
  
  # hide the Results tab on start
  hideTab(inputId = "nav", target = "&nbsp &nbsp Upload dataset", session = session)
  hideTab(inputId = "nav", target = "&nbsp &nbsp Summary results", session = session)
  
  # name the data exploration tab panel (this name is later changed to 
    # 'sites exploration' once the send invitations button is clicked)
  output$exploration_tab_name <- renderText("&nbsp &nbsp Data exploration")
  
  # saving datasets ---------------------------------------------------------
  
  # initialize list of saved datasets and the selections made to create the 
    # dataset
  datasets_available <- reactiveValues(data = NULL, data_names = NULL, selections = NULL)
  datasets_available$data <- list(population_dataset$`Site ID`)
  datasets_available$data_names <- c("Population")
  
  # update list of dataframes in the dataset dropdowns
    # this applies to every page that has a dataset dropdown
  observeEvent(datasets_available$data, {
    
    # update the dropdown options
    lapply(dataset_selector_ids, function(id){
      updateSelectInput(session = session, inputId = id,
                        choices = datasets_available$data_names)  
    })
  })
  
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
      
      # add popover to the dataset selector so user knows where to find their saved dataset
      dropdown_id <- paste0(str_extract(id, "^[a-z]*"), "_dataset")
      addPopover(
        session = session,
        id = dropdown_id,
        title = "Look here!",
        content = "You can access the dataset you just saved here!",
        placement = 'top'
      )
      # force the popover to show itself
      runjs(paste0("$('#", dropdown_id,"').popover('show')"))

    })
    
    # save dataset on button click
    observeEvent(input[[save_button]], {
      
      # clean up name for logic checks
      clean_save_name <- str_to_lower(str_trim(input[[save_name]]))
      
      # make sure inputted dataset name is not already used and is not length 0
        # this prevents the saving of datasets if the name already exists in
        # datasets_available$data_names
      if (str_to_lower(str_trim(input[[save_name]])) %in%
          str_to_lower(c(datasets_available$data_names, "stacked_results"))) {
        show_alert(
          title = "Dataset name already taken",
          text = "Please choose another name",
          type = "error",
          btn_colors = "#302f42",
          session = session
        )
      }
      
      # stop here if dataset name is taken or if no text is entered in the field
      validate(
        need(!(clean_save_name %in% str_to_lower(c(datasets_available$data_names, "stacked_results"))) &
               nchar(clean_save_name) > 0,
             "Dataset name already taken")
      )
      
      # get the dataset for this page
      dataset_to_save <- switch(
        id,
        "filtering_data_save" = filtering_data(),
        "sampling_data_save" = sampling_data(),
        "manual_data_save" = manual_selected_data()
      )
      
      # launch popup alert if sites are less than 100
      if (nrow(dataset_to_save) < min_sites_to_approach) {
        show_alert_min_sites(session)
      }
      
      # stop here if number of sites selected is less than 100
      validate(
        need(nrow(dataset_to_save) >= min_sites_to_approach,
             paste0("Less than ", min_sites_to_approach, " sites are currently selected"))
      )

      # append the selected sites as a new sublist to the master list of saved sites
      datasets_available$data <- c(datasets_available$data, list(dataset_to_save$`Site ID`))
      
      # add input string to list of dataset names
      datasets_available$data_names <- c(datasets_available$data_names,
                                         str_trim(input[[save_name]]))
      
      # add input selections to list
      selections <- reactiveValuesToList(input)
      datasets_available$selections <- c(datasets_available$selections,
                                         list(selections))
      
      # destroy the user entered text
      updateTextInput(session = session,
                      inputId = save_name,
                      value = NA)
      
      # if the user is saving the filtering dataset then reset the sliders
        # on button click, reset the sliders to the starting position
      if (id == 'filtering_data_save') {
        
        # reset the categorical inputs to default position
        pmap(.l = list(categorical_vars, categorical_choices),
             .f = function(variable, var_choices) {
               updateSelectInput(session = session,
                                 inputId = paste0("filtering_select_", variable),
                                 selected = var_choices)
               })
        
        # reset the numeric inputs
        pmap(.l = list(numeric_vars, min_max_df$min, min_max_df$max),
             .f = function(variable, var_min, var_max) {
               updateSliderInput(session = session,
                                 inputId = paste0("filtering_slider_", variable),
                                 value = c(var_min, var_max))
              }
            )
      }
      
      # if the user is saving the manual exclusions dataset then
        # destroy the user input saved IDs after the dataset is saved and
        # insert text below the save button reminding the user of implications
      if (id == 'manual_data_save') {
        updateSelectInput(session = session,
                          inputId = "manual_select_sites_excl",
                          selected = NA)
      
        output$manual_text_reminder <- renderUI(HTML('<p style="font-style: italic;">Don’t forget to justify your decision to exclude certain sites within your paper.</p>'))
      }
        
    })
    
    # make save_name text red if its a duplicate of a name
      # already in datasets_available$data_names
    observe({
      
      # make names lower case and remove white space so 
        # save_name is 1:1 with data_names
      clean_save_name <- str_to_lower(str_trim(input[[save_name]]))
      dataset_names <- str_to_lower(c(datasets_available$data_names, "stacked_results"))
      
      # make text red
      if(clean_save_name %in% dataset_names) {
        runjs(
          paste0('document.getElementById("',
                 save_name,
                 '").style.color = "#c92626"')
        )
        runjs(
          paste0('document.getElementById("',
                 save_name,
                 '").style.fontWeight = "bold"')
        )
      } else {
        # javascript permanently changes text color so it needs to change it back
         # to black when the input text is valid
        runjs(
          paste0('document.getElementById("',
                 save_name,
                 '").style.color = "#363636"')
        )
        runjs(
          paste0('document.getElementById("',
                 save_name,
                 '").style.fontWeight = 350')
        )
      }
    })
    
  })
  
  
  # description page --------------------------------------------------------
  
  # render plots on data description page
  output$description_plots <- renderPlot(draw_histograms(population_dataset))
  
  # add popovers
  map2(.x = c(categorical_vars, numeric_vars),
       .y = c(categorical_popover_messages, numeric_popover_messages),
       .f = function(variable, message){
        addPopover(
          session = session,
          id = paste0("description_text_", tolower(str_replace_all(variable, " ", "_"))),
          title = "Hint!",
          content = message,
          placement = 'top'
        )
      })
  
  
  # exploration page --------------------------------------------------------
  
  # select which dataset to use on data exploration tab
  exploration_selected_data <- reactive({
    
    # if the send invitations button has been triggered then use the 
     # "stacked_results" dataframe instead of the user selected dataset
    if ("stacked_results" %in% datasets_available$data_names){
      data <- get_dataset("stacked_results", datasets_available)
      
      # apply filters from checkmark boxes
      data <- data[data$`Site group` %in% input$exploration_checkboxes,]
      
      return(data)
      
    } else {
      # otherwise use the user selected dataframe on the dropdown
      get_dataset(input$exploration_dataset, datasets_available)
    }
  })
  
  # update second facet options so user cannot double facet on the same variable
    # b/c that causes an error
  observeEvent(input$exploration_variable_facet, {
    if (input$exploration_variable_facet != "None") {
      updateSelectInput(
        session = session,
        inputId = "exploration_variable_facet_second",
        choices = setdiff(c("None", categorical_vars), input$exploration_variable_facet)
      )
    }
  })
  
  # if user clicks download plot then trigger popup with additional inputs
  observeEvent(input$exploration_button_download, {
    show_alert(
      title = "Download your plot",
      text =
        list(
          br(),
          tags$div(id = "inline",
            textInput(inputId = "exploration_text_plot_title",
                      label = "Give your plot a title",
                      value = "Figure 1: My plot"),
            numericInput(inputId = "exploration_numeric_width",
                            label = "Set width (cm)",
                            value = 16,
                            min = 1),
            numericInput(inputId = "exploration_numeric_height",
                         label = "Set height (cm)",
                         value = 12,
                         min = 1)
          ), 
          br(),
          downloadButton(outputId = "exploration_button_download_plot", 
                         label = "Download")
        ),
      type = NULL,
      btn_labels = "Cancel",
      btn_colors = "#c7c7c7",
      session = session,
      html = TRUE
    )
  })
  
  # download the plot on the popup
  output$exploration_button_download_plot <- downloadHandler(
    
    # use plot title as file name but only retain alpha-numeric characters
    filename <- function() {paste0(
      str_replace_all(
        input$exploration_text_plot_title, 
        "[^[:alnum:]]", ""),
      ".png")
      }, 
    
    # plot to save
    content <- function(file) {
      ggsave(file,
             plot = exploration_plot() + labs(title = input$exploration_text_plot_title),
             device = "png",
             width = input$exploration_numeric_width,
             height = input$exploration_numeric_height,
             units = "cm")
    }
  )
  
  # build the exploration plots
  exploration_plot <- reactive({
    
    # set which dataset to use
    plot_data <- exploration_selected_data()
    
    # add kmeans cluster to data dataframe
    if(input$exploration_select_plot_type == 'Scatter' & input$exploration_variable_fill == "Cluster"){
      
      # run kmeans algo
      km <- kmeans(x = plot_data[, c(input$exploration_variable_x, input$exploration_variable_y)],
                   centers = input$n_clusters, iter.max = 50, nstart = 5)
      
      # add cluster assignment to the dataframe
      plot_data$Cluster <- as.factor(km$cluster)
      
      # run hclust algo instead
      # dist_matrix <- dist(plot_data[, c(input$exploration_variable_x, input$exploration_variable_y)])
      # clust <- hclust(d = dist_matrix,  method = 'ward.D2')
      # plot_data$cluster <- as.factor(cutree(clust, input$n_clusters))
      
    }
    
    # create base ggplot object
    p <- ggplot(plot_data, aes_string(x = sym(input$exploration_variable_x)))
    
    # scatter
    if (input$exploration_select_plot_type == 'Scatter'){
      p <- p +
        geom_point(aes_string(y = sym(input$exploration_variable_y),
                              fill = sym(input$exploration_variable_fill),
                              size = sym(input$exploration_variable_size),
                              color = sym(input$exploration_variable_fill)),
                   alpha = input$exploration_variable_alpha)
      
      # regression line
      if(input$exploration_variable_regression == 'Include'){
        p <- p + geom_smooth(
          aes_string(y = sym(input$exploration_variable_y)),
          method = "lm",
          formula = 'y ~ x',
          color = "grey20"
        )
      } 
      
      # cluster centers
      if(input$exploration_variable_fill == "Cluster"){
        p <- p +
          geom_point(data = as_tibble(km$centers),
                     aes_string(x = sym(colnames(km$centers)[1]),
                                y = sym(colnames(km$centers)[2])),
                     color = violet_col,
                     shape = 4, size = 8, stroke = 1.5)
      }
      
    }
    
    # histogram
    if (input$exploration_select_plot_type == 'Histogram'){
      p <- p + geom_histogram(color = 'white', bins = input$exploration_variable_n_bins, 
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
                     if(input$exploration_variable_group != 'None') aes_string(y = sym(input$exploration_variable_group))
        ) +
        coord_flip() +
        scale_y_discrete()
    }
    
    # add faceting
    if (input$exploration_variable_facet != "None"){
      
      if (input$exploration_variable_facet_second == "None"){
        p <- p + facet_grid(sym(input$exploration_variable_facet), labeller = label_both)
      } else {
        p <- p + facet_grid(list(sym(input$exploration_variable_facet), sym(input$exploration_variable_facet_second)),
                            labeller = label_both)
      }
    } 
    
    # show plot
    return(p)
  })
  
  # render site exploration plots
  output$exploration_plot <- renderPlot(exploration_plot())
  
  # text above the brush table
  output$brush_text <- renderText({
    
    if (input$exploration_variable_facet == "None" & input$exploration_select_plot_type == 'Scatter') {
      txt <- "<h4>Highlight data points on the above plot to view their information below</h4>"
    } else {
      txt <- NULL
    }
    
    return(txt)
    
  })
  
  # table of brushed data points from plot
  output$brush_info <- DT::renderDataTable(
    
    # show only if there isn't faceting
    if (input$exploration_variable_facet == "None" & input$exploration_select_plot_type == 'Scatter') {
      
      custom_datatable(
        brushedPoints(population_dataset, input$plot1_brush),
        selection = "none"
      ) %>%
        formatRound(5:10, 2)
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
  filtering_selected_data <- reactive({
    get_dataset(input$filtering_dataset, datasets_available)
  })
  
  # generate checkmark box inputs for each categorical variable
  output$filtering_select_categorical <- renderUI({
    tagList(
      pmap(
        .l = list(categorical_vars, categorical_choices, categorical_popover_messages),
        .f = function(variable, var_choices, popover_message) {
          prettyCheckboxGroup(
            inputId = paste0("filtering_select_", tolower(str_replace_all(variable, " ", "_"))),
            label = paste0(variable, ": "),
            inline = TRUE,
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
        .l = list(numeric_vars, min_max_df$min, min_max_df$max, numeric_popover_messages),
        .f = function(variable, var_min, var_max, popover_message) {
          sliderInput(
            inputId = paste0("filtering_slider_", tolower(str_replace_all(variable, " ", "_"))),
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
    data <- filtering_selected_data()
    data <- data[data$Region %in% input$filtering_select_region,]
    data <- data[data$Urban %in% input$filtering_select_urban,]
    data <- data[data$`Other program at site` %in% input$filtering_select_other_program_at_site,]
    data <- data[data$`Unemployment rate` >= input$filtering_slider_unemployment_rate[1] &
                   data$`Unemployment rate` <= input$filtering_slider_unemployment_rate[2],]
    data <- data[data$`High school degree rate` >= input$filtering_slider_high_school_degree_rate[1] &
                   data$`High school degree rate` <= input$filtering_slider_high_school_degree_rate[2],]
    data <- data[data$`Mean income` >= input$filtering_slider_mean_income[1] &
                   data$`Mean income` <= input$filtering_slider_mean_income[2],]
    data <- data[data$`Comfort` >= input$filtering_slider_comfort[1] &
                   data$`Comfort` <= input$filtering_slider_comfort[2],]
    # data <- data[data$cost >= input$filtering_slider_cost[1] &
    #                data$cost <= input$filtering_slider_cost[2],]
    data <- data[data$`Cost to approach site` >= input$filtering_slider_cost_to_approach_site[1] &
                   data$`Cost to approach site` <= input$filtering_slider_cost_to_approach_site[2],]
    data <- data[data$`Cost to approach site` >= input$filtering_slider_cost_to_approach_site[1] &
                   data$`Cost to approach site` <= input$filtering_slider_cost_to_approach_site[2],]
    data <- data[data$`Cost to run RCT` >= input$filtering_slider_cost_to_run_rct[1] &
                   data$`Cost to run RCT` <= input$filtering_slider_cost_to_run_rct[2],]
    
    return(data)
  })
  
  # initiate a popup notifying the user if at least 100 sites have been selected
  observeEvent(nrow(filtering_data()), {
    
    # if the user is on the filtering page and the selected dataset has less
    # than 100 sites then show the popup
    if(input$nav == HTML("&nbsp &nbsp Filtering") &
       nrow(filtering_data()) < min_sites_to_approach) {
      
      # launch popup alert
      show_alert_min_sites(session)
    }
  })
  
  # histograms and bar plots on 'plots' tab
  output$filtering_plot_hist <- renderPlot(draw_histograms(filtering_data()))
  
  # display the table in the 'table of selected sites' tab
  output$filtering_table_selected <- DT::renderDataTable(
    custom_datatable(
      filtering_data(),
      selection = 'none'
    ) %>%
      formatRound(5:10, 2))
  
  # display the table in the 'table of excluded sites' tab
  output$filtering_table_excluded <- DT::renderDataTable(
    custom_datatable(
      anti_join(population_dataset, filtering_data()),
      selection = 'none'
    ) %>%
      formatRound(5:10, 2))
  
  
  # sampling page -----------------------------------------------------------
  
  # select which dataset to use on sampling tab
  sampling_selected_data <- reactive({
    get_dataset(input$sampling_dataset, datasets_available)
  })
  
  # capture the user selection in the sampling_select_simple_or_stratified dropdown
  user_selected <- reactive({
    if(is.null(input$sampling_select_simple_or_stratified)){
      return("Simple")
    } else {
      return(input$sampling_select_simple_or_stratified)
    }
  })
  
  # determine which popover message to display above sampling_select_simple_or_stratified
  sampling_popover_message <- reactive({
    if (is.null(input$sampling_select_simple_or_stratified)) {
      return("Error in message rendering")
    } else {
      if (input$sampling_select_simple_or_stratified == "Simple") {
        return(sampling_simple_message)
      } else {
        return(sampling_stratified_message)
      }
    }
  })
  
  # render sampling_select_simple_or_stratified dropdown and its popover 
    # with the appropriate message
  # this method is necessary b/c the popover needs to be regenerated when the user 
    # changes sampling_select_simple_or_stratified
  output$sampling_select_simple_or_stratified <- renderUI({
    selectInput(inputId = "sampling_select_simple_or_stratified",
                label = "Simple or stratified sample: ",
                multiple = FALSE,
                selected = user_selected(),
                choices = c("Simple", "Stratified")) %>%
      # add the popover element
      popify(
        el = .,
        title = "Sampling method",
        content = sampling_popover_message(),
        placement = 'top'
      )
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
    
    # add id column for use with sliders
    strata_combos$slider_id <- paste0("sampling_slider_stratified_n_",
                                      paste0(input$strata_variables, collapse = "_"),
                                      "_",
                                      strata_combos$name)
    strata_combos$slider_id <- str_replace_all(strata_combos$slider_id, " ", "_")
    
    return(strata_combos)
    
  })
  
  # generate sliders for each strata combinations
  output$sampling_strata_sliders <- renderUI({
    tagList(
      pmap(.l = list(strata_combos()$name,
                     strata_combos()$n,
                     strata_combos()$slider_id),
           .f = function(combo, max_n, id) {
             sliderInput(
               inputId = id, 
               label = str_replace_all(combo, "_", ":"), 
               value = min(strata_combos()$n), 
               min = 0, max = max_n, step = 1) 
           })
    )
  })
  
  # on button click, reset the sliders to the starting position
  observeEvent(input$sampling_reset_sliders, {
    
    # get current list of sliders
    slider_ids <- strata_combos()$slider_id
    
    # update the position of the sliders to the maximum amount that still
      # allows equality across the sliders
    lapply(slider_ids, function(id){
      updateSliderInput(session = session, 
                        inputId = id, 
                        value = min(strata_combos()$n))
    })
    
  })
  
  # random sampling
  sampling_data <- eventReactive(input$sampling_button_run_sampling, {
    
    data <- sampling_selected_data()
    
    # if simple sampling
    if (input$sampling_select_simple_or_stratified == "Simple"){
      
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
      sample_size_per_group <- reactiveValuesToList(input)[strata_combos()$slider_id]
      
      # sample n rows per strata
      sampled_data <- map2_dfr(
        .x = split_groups,
        .y = sample_size_per_group,
        .f = function(strata, strata_size) {
          # sample the data
          indices <- sample(1:nrow(strata), size = strata_size, replace = FALSE)
          sampled_strata <- strata[indices, ]
          return(sampled_strata)
        }
      )
      
      return(sampled_data)
      
    }
  })
  
  # show text below sample size slider indicating total sample size
  output$n_strata <- renderText({
    slider_sum <- sum(unlist(reactiveValuesToList(input)[strata_combos()$slider_id]))
    paste0("The total selected sample size is ", slider_sum)
  })  
  
  # initiate a popup notifying the user if at least 100 sites have been selected
  observeEvent(nrow(sampling_data()), {
    
    # if the user is on the filtering page and the selected dataset has less
    # than 100 sites then show the popup
    if(input$nav == HTML("&nbsp &nbsp Sampling") &
       nrow(sampling_data()) < min_sites_to_approach) {
      
      # launch popup alert
      show_alert_min_sites(session)
    }
  })
  
  # the plots for sampling page
  output$sampling_plots <- renderPlot(draw_histograms(sampling_data()))
  
  # display the table in the sampling tab    
  output$sampling_table_selected <- DT::renderDataTable(
    custom_datatable(
      sampling_data(),
      selection = 'none'
    ) %>%
      formatRound(5:10, 2)
  )
  
  # display excluded table in the sampling tab
  output$sampling_table_excluded <- DT::renderDataTable(
    custom_datatable(
      anti_join(population_dataset, 
                sampling_data()),
      selection = 'none'
    ) %>%
      formatRound(5:10, 2)
  )

  
  # manual exclusions page --------------------------------------------------
  
  # select which dataset to use on manual exclusion page
  manual_selected_data <- reactive({
    selected_data <- get_dataset(input$manual_dataset, datasets_available)
    
    # exclude rows from manual input
    selected_data <- selected_data[!selected_data$`Site ID` %in% input$manual_select_sites_excl,]
    
    return(selected_data)
  })
  
  # save row selections when button is clicked
  selected_rows <- reactiveValues(select = NULL)
  observeEvent(input$manual_button_save_row_selections, {
    
    # get the site ids from the current selected rows
    # placing "_rows_selected" after the table name returns the selected rows 
    selected_rows$current_row_selections <- manual_selected_data()$`Site ID`[input$manual_table_selected_rows_selected]
    
    # append selected list in the values in the user input field
    updateSelectInput(session = session, 
                      inputId = "manual_select_sites_excl",
                      selected = unique(append(input$manual_select_sites_excl, selected_rows$current_row_selections)))
    
  })
  
  # initiate a popup notifying the user if at least 100 sites have been selected
  observeEvent(nrow(manual_selected_data()), {
    
    # if the user is on the filtering page and the selected dataset has less
    # than 100 sites then show the popup
    if(input$nav == HTML("&nbsp &nbsp Manual exclusions") &
       nrow(manual_selected_data()) < min_sites_to_approach) {
      
      # launch popup alert
      show_alert_min_sites(session)
    }
  })
  
  # display the table in the 'table of selected sites' tab within the final selection page
  output$manual_table_selected <- DT::renderDataTable(
    custom_datatable(
      manual_selected_data()
    ) %>%
      formatRound(5:10, 2))
  
  # display the table in the 'table of excluded sites' tab: Final selection tab
  output$manual_table_excluded <- DT::renderDataTable(
    custom_datatable(
      anti_join(population_dataset, manual_selected_data()),
      selection = 'none'
    ) %>%
      formatRound(5:10, 2))
  
  
  # send invitations page ---------------------------------------------------
  
  # select which dataset to use on send invitations tab
  sent_invitations_data <- reactive({
    get_dataset(input$invitations_dataset, datasets_available)
  })

  # table of key metrics for the send invitations page
  output$invitations_table_scores <- renderTable({
    
    # simulate outcomes to get 
    sim_scores <- sim_results()[[2]]
    sim_scores <- as.matrix(apply(sim_scores, 2, function(col) round(mean(col), 2)))
    sim_scores[1,] <- floor(sim_scores[1,])
    sim_scores[2,] <- scales::dollar(round(sim_scores[2,]))
    
    # calculate the metrics for the population
    pop_scores <- score_attributes(sent_invitations_data())
    
    # combine scores into one matrix
    scores <- cbind(sim_scores, pop_scores)
    
    # set col and row names
    colnames(scores) <- c("Expected", "Total")
    rownames(scores) <- rownames(pop_scores)
    
    # remove total cost as it now longer makes sense when cost is split across
      # costs to approach and cost to execute
    scores["Total cost", "Total"] <- NA
    
    # return the matrix
    scores
    
  }, rownames = TRUE, align = 'lrr')
  
  # take action when the submit invitations button is clicked
  observeEvent(input$invitations_button_send, {
    
    # do not take action dataset contains less than 100 sites
    validate(
      need(nrow(sent_invitations_data()) >= min_sites_to_approach,
           paste0("Selected dataset contains less than ", min_sites_to_approach, " sites"))
    )
    
    # launch popup confirmation
    ask_confirmation(inputId = "invitations_popup_confirm",
                     title = "Are you sure you would like to send the invitations?",
                     text = "Site selection will no longer be available. You will, however, be able to restart from the beginning if you wish.",
                     btn_colors = c("#c7c7c7", "#302f42"),
                     html = TRUE
    )
  })
  

  # if the user confirms on the popup or clicks on fromt the welcome page, then do the following actions
    # otherwise stop here
  button_or_confirm <- reactive({
    list(input$invitations_popup_confirm, input$welcome_button_jump_to_Results)
  })
  button_clicked <- FALSE
  # create switch indicating if user clicked the button
  observeEvent(input$welcome_button_jump_to_Results, {
    button_clicked <<- TRUE
  })
  observeEvent(button_or_confirm(), {
    if (isTRUE(input$invitations_popup_confirm) | button_clicked) {
      
      if (isTRUE(input$invitations_popup_confirm)){
      # launch popup to download the data but only if the user came through Site Selection
      show_alert(
        title = "Invitations (metaphorically) sent!",
        text =
          list(
            br(),
            "This is as far as you need to go from Assignments One and Two. Be sure to download the dataset of sites you've selected. Without this you'll be unable to do Assignment Three so save it somewhere safe.",
            br(), br(),
            div(
              downloadButton(outputId = "invitations_button_download_data",
                             label = "Download your data"),
              actionButton(inputId = "invitations_button_restart",
                           label = 'Erase and restart')
            )
          ),
        type = 'success',
        btn_labels = "Move to Assignment Three",
        btn_colors = "#c7c7c7",
        session = session,
        html = TRUE
      )
        # since the user came through site selection then make that the default option
          # on the upload dataset page
        updateMaterialSwitch(session = session,
                             inputId = 'upload_switch_use_site_selection_data',
                             value = TRUE)
      }
      
      # remove the button on the welcome page as this causes errors if clicked again
      removeUI(selector = '#welcome_button_jump_to_Results')
      
      # show and move user to the final tab
      showTab(inputId = "nav",
              target = "&nbsp &nbsp Upload dataset",
              session = session)
      updateNavlistPanel(session = session,
                         inputId = "nav",
                         selected = "&nbsp &nbsp Upload dataset")

      # hide old tabs
      hide(selector = "li.navbar-brand") # this hides the "1. ..." and "2. ..." text in the nav
      hideTab(
        inputId = "nav",
        target = HTML("&nbsp &nbsp Data exploration"),
        session = session
      )
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
      # insert fake tab header
      insertTab(inputId = "nav",
                tab = HTML('<div><h5>4. Results</h5></div>'),
                target = "&nbsp &nbsp Upload dataset",
                position = 'before')
      insertTab(inputId = "nav",
                tab = HTML('<div><h5>&nbsp &nbspSummary Results</h5></div>'),
                target = "&nbsp &nbsp Upload dataset",
                position = 'after')
      # insertTab(inputId = "nav",
      #           tab = HTML('<div><h5>&nbsp &nbsp Data exploration</h5></div>'),
      #           target = '<div><h5>&nbsp &nbspSummary Results</h5></div>',
      #           position = 'after')
      
      # save which dataset was used to send the invitations
      sent_invitations_data <<- sent_invitations_data()
    }
  })
  
  # download the data
  output$invitations_button_download_data <- downloadHandler(
    
    # use plot title as file name but only retain alpha-numeric characters
    filename <- function() "DS4SI_site_selection_invitations.zip", 
    
    # plot to save
    content <- function(file) {
      
      # go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      # write out the dataframe containing only the sites the user sent invitations to
      write.csv(sent_invitations_data, "sites_sent_invitations.csv", row.names = FALSE)
      files <- "sites_sent_invitations.csv"
      
      # save an .RData of the selctions made for all datasets
      # necessary b/c if user e.g. filtered then sampled then we need the history
      # first get the list of datasets which contains the data, the dataset names, and selections
      selection_history <- reactiveValuesToList(datasets_available)
      
      # trim off the population dataframe that is also stored here
      selection_history$data_names <- selection_history$data_names[-1]
      selection_history$data <- selection_history$data[-1]
      n_datasets <- length(selection_history$data)
      if (n_datasets == 0) selection_history <- "Student did not make any selections"
      
      # save the object
      save(selection_history, file = "Selection_history.RData")
      files <- c("Selection_history.RData", files)
      
      # create the zip file
      zip(file, files)
    }
  )
  
  # restart the session based on button click
  observeEvent(input$invitations_button_restart, {
    session$reload()
    gc()
  })
  
  # plots on send invitations page
  output$invitations_plots <- renderPlot(draw_histograms(sent_invitations_data()))
  
  # sites to send invitations vs population plots
  output$invitations_plot_sites_v_pop <- renderPlot({
    
    # get stacked dataset from list 
    selected_sites <- sent_invitations_data()
    selected_sites$`Site group` <- "Sites to send invitations"
    pop <- population_dataset
    pop$`Site group` <- "Population"
    data_for_plot <- rbind(selected_sites, pop)
    data_for_plot$`Site group` <- factor(data_for_plot$`Site group`,
                                       levels = c("Sites to send invitations",
                                                  "Population"))
    
    # barplots
    p1 <- data_for_plot %>% 
      select(`Site group`, all_of(categorical_vars)) %>% 
      mutate_at(categorical_vars, as.character) %>% 
      pivot_longer(cols = -c("Site group")) %>%
      group_by(`Site group`, name, value) %>%
      tally() %>%
      group_by(`Site group`, name) %>% 
      mutate(prop = n / sum(n),
             name = factor(name, levels = categorical_vars)) %>% 
      ggplot(aes(x = value, y = prop, group = `Site group`, fill = `Site group`)) +
      geom_col(position = 'dodge', color = 'white', alpha = 0.6) +
      scale_fill_viridis_d() +
      facet_wrap(~name, scales = 'free_x') +
      labs(x = NULL,
           y = NULL) +
      theme(legend.position = 'none',
            axis.text.x = element_text(angle = 40, hjust = 1))
    
    # density plots
    p2 <- data_for_plot %>% 
      select(`Site group`, all_of(numeric_vars)) %>% 
      pivot_longer(cols = -c("Site group")) %>% 
      ggplot(aes(x = value, group = `Site group`, fill = `Site group`)) +
      geom_density(alpha = 0.24) +
      facet_wrap(~name, scales = 'free') +
      scale_fill_viridis_d() +
      labs(x = NULL,
           y = NULL) +
      theme(legend.title = element_blank(),
            legend.position = 'bottom')
    
    # render both plots vertically
    grid.arrange(p1, p2, ncol = 1, heights = c(1, 2))
    
  })
  
  # table of sites to send invitations to
  output$invitations_table_send <- DT::renderDataTable({
    custom_datatable(sent_invitations_data(), 
                     selection = 'none') %>%
      formatRound(5:10, 2)
  })
  
  
  sim_results <- reactive({
    # function re-simulates the acceptance of the invitations
      # and returns a list of dataframes, each representing one simulation and containing
      # the sites that accepted the invitation
    # it also returns a list of the scores (n, cost, generalizbility and causality scores) 
      # per each simulation
    
    data <- sent_invitations_data() 
    
    list_of_accepted_dataframes <- list()
    nsims <- 200
    scores <- data.frame("Sample size" = NA, "Total cost" = NA, "Generalizability index" = NA, "Causality index" = NA,
                         check.names = FALSE)
    for (i in 1:nsims){
      # sample the data and return T/F for indices that accepted
      accepted_boolean <- rbinom(n = nrow(data),
                                 size = 1,
                                 prob = data$Comfort) == 1
      
      # subset the data based on the indices
      accepted_data <- data[accepted_boolean,]
      
      # add identifier for use in plotting
      accepted_data$sim <- i
      
      # add dataframe to list of total dataframes
      list_of_accepted_dataframes[[i]] <- accepted_data
      
      # create dataframe of scores
      scores[i, 'Sample size'] <- sum(accepted_boolean)
      scores[i, 'Total cost'] <- sum(data$`Cost to approach site`, accepted_data$`Cost to run RCT`)
      scores[i, 'Generalizability index'] <- score_generalizability(accepted_data)
      scores[i, 'Causality index'] <- score_causality(accepted_data)
    }
    
    return(list(list_of_accepted_dataframes, scores))
    
  })
  
  # expected attributes plots
  output$invitations_plot_expected_attributes <- renderPlot({
    
    # get the sim results
    list_of_accepted_dataframes <- sim_results()[[1]]
    
    # plot categoricals only
    p1 <- bind_rows(list_of_accepted_dataframes) %>%
      select(sim, all_of(categorical_vars)) %>%
      mutate_all(as.character) %>%
      pivot_longer(cols = -c("sim")) %>%
      group_by(sim, name, value) %>%
      tally() %>%
      mutate(prop = n / sum(n),
             x_pos = determine_x_pos(value),
             name = factor(name, categorical_vars)) %>% 
      ggplot(aes(x = value, y = prop)) +
      geom_jitter(alpha = 0) + # this for some reason allows us to retain the x labels
      geom_segment(aes(x = x_pos - 0.25, xend = x_pos + 0.25,
                       y = prop, yend = prop, group = sim), alpha = 0.025) +
      # this ensures the x limits do not change by building an invisible
      # plot of the full population
      geom_col(data = pop_data_for_categorical_limits %>% mutate(prop = 0),
               alpha = 0) +
      facet_wrap(~name, scales = 'free_x') +
      labs(x = NULL,
           y = NULL) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1))

    # plot numerics only
    p2 <- bind_rows(list_of_accepted_dataframes) %>%
      select(sim, all_of(numeric_vars)) %>%
      pivot_longer(cols = -c("sim")) %>%
      ggplot(aes(x = value)) +
      geom_line(stat = "density", aes(group = sim), alpha = 0.025, color = 'black') +
      # this ensures the x limits do not change by building an invisible
      # plot of the full population
      geom_line(data = pop_data_for_numeric_limits,
                stat = "density", alpha = 0) +
      facet_wrap(~name, scales = 'free') +
      labs(x = NULL,
           y = NULL)
    
    # render both plots vertically
    grid.arrange(p1, p2, ncol = 1, heights = c(1, 2))
  })
  
  # expected metrics plots  
  output$invitations_plot_expected_attributes_metrics <- renderPlot({
    
    # get the sim results
    scores <- sim_results()[[2]]
    
    # plot it
    scores %>%
      pivot_longer(cols = everything()) %>%
      mutate(name = factor(name, levels = metrics_order)) %>% 
      ggplot(aes(x = value)) +
      geom_density(fill = violet_col, alpha = 0.5) +
      facet_wrap(~name, scales = 'free', ncol = 3) +
      labs(x = NULL,
           y = NULL)
  })
  
  # on button click, remove the floating dialog box on the plots
  observeEvent(input$invitations_button_exit_att_box, {
    removeUI(selector = "#invitations_plot_att_box")
  })
  observeEvent(input$invitations_button_exit_metrics_box, {
    removeUI(selector = "#invitations_plot_metrics_box")
  })
  

  # upload dataset page -----------------------------------------------------
  
  # read in the CSV that was uploaded
  upload_dataset_csv <- reactive({
    
    req(input$upload_file)
    tryCatch({
      upload_csv <- read_csv(
        file = input$upload_file$datapath,
        col_types = cols(
          `Site ID` = col_integer(),
          Region = col_character(),
          Urban = col_logical(),
          `Other program at site` = col_logical(),
          `Unemployment rate` = col_double(),
          `High school degree rate` = col_double(),
          `Mean income` = col_double(),
          Comfort = col_double(),
          `Cost to approach site` = col_double(),
          `Cost to run RCT` = col_double()
        )
      )
    },
    error = function(e) {
      # return a safeError if a parsing error occurs or if dataset isn't yet uploaded
      stop(safeError(e))
    })
    
    # ensure dataframe is valid by comparing it to the population_dataset
    # cols_match <- base::setequal(colnames(upload_csv), colnames(population_dataset))
    # classes_match <- all(apply(upload_csv, 2, class) == apply(population_dataset, 2, class))
    # 
    # if (!isTRUE(cols_match) | !isTRUE(classes_match)) {
    #   # if there is an error then remove #upload_panel_message b/c it obfuscates the error message
    #   removeUI(selector = "#upload_panel_message")
    # }
    # 
    # validate(
    #   need(isTRUE(cols_match) & isTRUE(classes_match),
    #        "Uploaded data frame is not valid: columns and/or classes do not match"
    #   ))
    
    return(upload_csv)
  })

  # generate the text input for site IDs if there is an issue with the uploaded dataframe
  observeEvent(upload_dataset_csv(), {
    
    # ensure dataframe is valid by comparing it to the population_dataset
    cols_match <- base::setequal(colnames(upload_dataset_csv()), colnames(population_dataset))
    classes_match <- all(apply(upload_dataset_csv(), 2, class) == apply(population_dataset, 2, class))
    
    df_validated <- !isTRUE(cols_match) | !isTRUE(classes_match)
    
    if (df_validated){
      
      show_alert(session = session,
                 title = "Hmmm, that dataset doesn't look quite right",
                 text = list(HTML("Try another file or, if you continue to have issues, copy/paste your Site IDs into the text field. <br><br> Here's how your CSV should be structured:"),
                             DT::dataTableOutput("upload_dataset_example")),
                 type = "error",
                 btn_colors = "#302f42",
                 html = TRUE)
      
      output$upload_selection_siteid <- renderUI({
        tagList(
          textInput(inputId = "upload_selection_input",
                    label = "Looks like you're having some issues with uploading the CSV. You can manually enter your Site IDs — each separated by a single space — here instead:"),
          materialSwitch(
            inputId = "upload_switch_use_site_ids",
            label = strong("Use site IDs"),
            value = FALSE,
            status = "danger"
          ),
          br()
        )
      })
    }
  })
  
  # dataset example to show in popup
  output$upload_dataset_example <- DT::renderDataTable({
    DT::datatable(head(population_dataset, n = 5), 
                  rownames = FALSE, 
                  selection = "none",
                  options = list(
                    paging = FALSE,
                    # sets n observations shown
                    pageLength = 20,
                    # removes option to change n observations shown
                    lengthChange = FALSE,
                    # removes the search bar
                    sDom  = '<"top">lrt<"bottom">ip',
                    # enable side scroll so table doesn't overflow
                    scrollX = TRUE
                  )) %>%
                    formatRound(5:10, 2)
  })
  
  # create a dataset from the manually inputted site IDs
  upload_dataset_manual <- reactive({
    parsed_IDs <- unlist(str_split(input$upload_selection_input, " "))
    siteID_df <- population_dataset[population_dataset$`Site ID` %in% parsed_IDs,]
    return(siteID_df)
  })
  
  # if both switches are flipped then flip the other one
  observeEvent(input$upload_switch_use_site_ids, {
    if (isTRUE(input$upload_switch_use_site_ids) &
        isTRUE(input$upload_switch_use_site_selection_data)) {
      updateMaterialSwitch(session = session,
                           inputId = 'upload_switch_use_site_selection_data',
                           value = FALSE)
    }
  })
  observeEvent(input$upload_switch_use_site_selection_data, {
    if (isTRUE(input$upload_switch_use_site_ids) &
        isTRUE(input$upload_switch_use_site_selection_data)) {
      updateMaterialSwitch(session = session,
                           inputId = 'upload_switch_use_site_ids',
                           value = FALSE)
    }
  })
  
  
  # choose which dataset to use based on the switches postions
  upload_dataset <- reactive({

    # use the sent invitations data if the switch is TRUE
    if (isTRUE(input$upload_switch_use_site_selection_data)) {
      return(sent_invitations_data)
    }
    
    if (isTRUE(input$upload_switch_use_site_ids)){
      return(upload_dataset_manual())
    }
    
    return(upload_dataset_csv())
  })

  # trigger all these events once the user clicks "get results"
  observeEvent(input$upload_button_get_results, {
    
    # make sure dataset is selected
    # TODO: this doesn't work; possible an issue with is.data.frame()
    if (!isTRUE(is.data.frame(upload_dataset())))  {
      show_alert(
        title = "Dataset not selected",
        text = "Please upload a dataset or flip the switch to use the one from Site selection",
        type = "error",
        btn_colors = "#302f42",
        session = session
      )
    }
    
    # make sure score is between [1, 100]
    if (input$upload_numeric_persuasion < 1 | input$upload_numeric_persuasion > 100)  {
      show_alert(
        title = "Persuasion score must be between [1, 100]",
        text = "Please update the score",
        type = "error",
        btn_colors = "#302f42",
        session = session
      )
    }
    
    validate(
      need(is.data.frame(upload_dataset()),
           "Dataset not yet uploaded or selected"),
      need(input$upload_numeric_persuasion >= 1 & input$upload_numeric_persuasion <= 100,
           "Persuasion score not between [1, 100]")
    )
    
    # flip a coin with prob = comfort to see which sites accepted
    accepted_boolean <- rbinom(
      n = nrow(upload_dataset()),
      size = 1,
      prob = pmin(1, upload_dataset()$Comfort * scale_persuasion(input$upload_numeric_persuasion))
    ) == 1
    sites_that_accepted <- upload_dataset()[accepted_boolean,]
    
    # create a stacked dataframe with observation per population, sent invitations, and accepted invitations
    # i.e. nrow(...) should be nrow(population) + nrow(sent_invitations) + nrow(accepted)
    tmp1 <- upload_dataset()
    tmp1$`Site group` <- "Sent invitation"
    tmp2 <- sites_that_accepted
    tmp2$`Site group` <- "Accepted invitation"
    tmp3 <- population_dataset[, setdiff(colnames(population_dataset),
                                         c("sent_invitation", "accepted"))]
    tmp3$`Site group` <- "Population"
    stacked_results <- rbind(tmp1, tmp2, tmp3)
    rm(tmp1, tmp2, tmp3)
    
    # factor the site_group variable so its in the right order
    stacked_results$`Site group` <- factor(
      stacked_results$`Site group`,
      levels = c("Accepted invitation",
                 "Sent invitation",
                 "Population")
    )
    
    # add stacked_results to list of dataframes available
    datasets_available$data <- c(datasets_available$data,
                                 list(stacked_results))
    datasets_available$data_names <- c(datasets_available$data_names,
                                       "stacked_results")
    
    # show Summary results tab and replace Upload dataset tab with unclickable version
    hide(selector = "li.dropdown-header") # this hides the "1. ..." and "2. ..." text in the nav
    insertTab(inputId = "nav",
              tab = HTML('<div><h5>4. Results</h5></div>'),
              target = "&nbsp &nbsp Upload dataset",
              position = 'before')
    hideTab(inputId = "nav",
            target = "&nbsp &nbsp Upload dataset",
            session = session)
    insertTab(inputId = "nav",
              tab = HTML('<div><h5>&nbsp &nbsp Upload dataset</h5></div>'),
              target = "&nbsp &nbsp Summary results",
              position = 'before')
    showTab(inputId = "nav",
            target = "&nbsp &nbsp Summary results",
            session = session)
    showTab(inputId = "nav",
            target = HTML("&nbsp &nbsp Data exploration"),
            session = session)
    
    # move the user to the Summary results page
    updateNavlistPanel(session = session,
                       inputId = "nav",
                       selected = "&nbsp &nbsp Summary results")
    
    # change the tab name from 'data exploration' to 'results exploration' so
    # user knows it's a new tab
    # output$exploration_tab_name = renderText({
    #   HTML("&nbsp &nbsp Results exploration")
    # })
    # showTab(inputId = "nav",
    #         target = "&nbsp &nbsp Results exploration",
    #         session = session)

    # add popover elements
    # addPopover(
    #   session = session,
    #   id = 'exploration_tab_name',
    #   title = "Explore your data further",
    #   content = 'Create custom plots to understand how your sites compare',
    #   placement = 'bottom'
    # )
    addPopover(
      session = session,
      id = 'results_button_download',
      title = "Download your data",
      content = 'Be sure to download your data for future assignments',
      placement = 'bottom'
    )
    # force the popover to show itself on load
    # runjs("$('#exploration_tab_name').popover('show')")
    
    # on the data exploration page, add a grouping variable that represents
    # population, sent invitations, and accepted invitations sites
    updateSelectInput(session = session,
                      inputId = "exploration_dataset",
                      selected = "stacked_results")
    updateSelectInput(
      session = session,
      inputId = "exploration_variable_facet",
      choices = c("None", "Site group", categorical_vars),
      selected = "Site group"
    )
    updateSelectInput(
      session = session,
      inputId = "exploration_variable_group",
      choices = c("None", "Site group", categorical_vars)
    )
    
    # create the three checkmark boxes on the data exploration page
    output$exploration_selection_data_spawn <- renderUI({
      tagList(
        prettyCheckboxGroup(
          inputId = "exploration_checkboxes",
          label = "Sites to include:",
          choices = list(
            "Population" = "Population",
            "Sent invitation" = "Sent invitation",
            "Accepted invitation" = "Accepted invitation"
          ),
          selected = c("Population", "Sent invitation", "Accepted invitation")
        ),
        # add "Information" expansion below the checkmark boxes
        HTML('<details><summary>Information</summary>'),
        "These datasets are nested within each other. Including multiple without faceting or grouping will cause duplicates to appear on the plot.",
        HTML('</details><br>'),
        br()
      )
    })
    
    # remove dataset selector element from the data exploration page
    removeUI(selector = "#exploration_dataset_div")
    
    # add popup indicating to user dangers of not faceting on site_group if there are
    # multiple populations selected
    # this is triggered if the user checks multiple groups while facet != site_group or
    # if the user unselects facet == site_group while multiples are checked
    events_to_listen <- reactive({
      list(input$exploration_variable_facet, input$exploration_checkboxes)
    })
    observeEvent(events_to_listen(), {
      if (input$exploration_variable_facet != "Site group" &
          input$exploration_variable_group != "Site group" &
          length(input$exploration_checkboxes) > 1) {
        
        show_alert(
          title = "Multiple groups of sites are being shown without faceting",
          text = "Including multiple groups of sites without faceting on `Site group` will cause duplicate data to be shown on the plot(s). Either facet on `Site group` or check only one group under 'Sites to include'",
          type = "warning",
          btn_colors = "#302f42",
          session = session
        )
        
      }
    })
  })
  
  # numeric input does not respect min max limits so need to inform user when theres an error
  observe({
    
    boolean <- input$upload_numeric_persuasion < 1 | input$upload_numeric_persuasion > 100
    
    # make text red
    if(isTRUE(boolean)) {
      runjs(
        paste0('document.getElementById("',
               "upload_numeric_persuasion",
               '").style.color = "#c92626"')
      )
      runjs(
        paste0('document.getElementById("',
               "upload_numeric_persuasion",
               '").style.fontWeight = "bold"')
      )
    } else {
      # javascript permanently changes text color so it needs to change it back
      # to black when the input text is valid
      runjs(
        paste0('document.getElementById("',
               "upload_numeric_persuasion",
               '").style.color = "#363636"')
      )
      runjs(
        paste0('document.getElementById("',
               "upload_numeric_persuasion",
               '").style.fontWeight = "350"')
      )
    }
  })
  
  # table of selected dataset
  output$upload_table <- DT::renderDataTable({
      custom_datatable(
        upload_dataset(),
        selection = "none"
      ) %>%
        formatRound(5:10, 2)
  })
  
  # render the histograms
  output$upload_plot_hist <- renderPlot(draw_histograms(upload_dataset()))
  
  
  # results page ------------------------------------------------------------
  
  # text for Results page 
  output$results_text_summary <- renderText({
    
    # get the dataset
    data <- get_dataset("stacked_results", datasets_available)
    data <- data[data$`Site group` == "Accepted invitation",]
    
    # calculate summary stats
    n_sites <- nrow(data)
    
    # paste together the sentence
    sentence <- paste0(
      '<h2>Congrats! ', scales::comma_format()(n_sites), ' sites accepted the invitation</h2>'
    )
    
    return(sentence)
  })
  
  # function to return summary table for the Results page
  results_table <- reactive({
    
    # get the dataset
    data <- get_dataset("stacked_results", datasets_available)
    
    # create list of dataframes representing the three groups
    list_of_tables <- split(x = data,
                            f = data$`Site group`)
    
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
      prog_urban <- apply(df[, c("Other program at site", "Urban")], MARGIN = 2, function(col) {
        table(col)["TRUE"] / length(col)
      })
      
      # calculate proportion per region
      regions <- apply(df[, "Region"], MARGIN = 2, function(col) {
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
    # 'row_name' matches the natural output of the above apply functions
    # 'clean_row_name' is what we want the table to eventually display
    shell_table <- data.frame(
      'row_name' = c(
        "Comfort",
        "Cost to approach site",
        "Cost to run RCT",
        "Mean income",
        "High school degree rate",
        "Unemployment rate",
        "Other program at site",
        "Urban",
        "Northcentral",
        "Northeast",
        "South",
        "West"
      ),
      'clean_row_name' = c(
        "Mean comfort",
        "Mean cost to approach site",
        "Mean cost to run RCT",
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
    
    # add in scores to the top of the matrix
    scores <- Reduce(
      x = lapply(list_of_tables, score_attributes),
      f = function(df1, df2) cbind(df1, df2)
    )
    scores_char <- apply(scores, 2, as.character)
    rownames(scores_char) <- rownames(scores)
    colnames(scores_char) <- colnames(final_table)
    final_table <- rbind(scores_char, round(final_table, 2))
    
    # replace cost estimates so cost to approach comes from all sites the user approached
      # and cost to execute comes from just the sites that accepted
    accepted_df <- data[data$`Site group` == 'Accepted invitation',]
    sent_invitation_df <- data[data$`Site group` == 'Sent invitation',]
    final_table['Total cost', 1] <- scales::dollar_format()(round(sum(sent_invitation_df$`Cost to approach site`, accepted_df$`Cost to run RCT`), 0))
    final_table['Total cost', 2:3] <- NA
    
    
    return(final_table)
  })
  
  # render the summary table for the Results page
  output$results_table_summary <- renderTable(results_table(), rownames = TRUE, align = 'lrrr')
  
  # function to return the histograms
  results_hist <- reactive({
    
    # get the data
    data <- get_dataset("stacked_results", datasets_available)
    data <- data[data$`Site group` == 'Accepted invitation',]
    
    draw_histograms(data)
  })
  
  # render the histograms
  output$results_plot_hist <- renderPlot(results_hist())
  
  # function to return sample vs population plots
  results_samp_v_pop <- reactive({
    
    # get stacked dataset from list 
    data_for_plot <- get_dataset("stacked_results", datasets_available)
    
    # barplots
    p1 <- data_for_plot %>% 
      select(`Site group`, all_of(categorical_vars)) %>% 
      mutate_at(categorical_vars, as.character) %>% 
      pivot_longer(cols = -c("Site group")) %>%
      group_by(`Site group`, name, value) %>%
      tally() %>%
      group_by(`Site group`, name) %>% 
      mutate(prop = n / sum(n),
             name = factor(name, levels = categorical_vars)) %>% 
      ggplot(aes(x = value, y = prop, group = `Site group`, fill = `Site group`)) +
      geom_col(position = 'dodge', color = 'white', alpha = 0.6) +
      scale_fill_viridis_d() +
      facet_wrap(~name, scales = 'free_x') +
      labs(x = NULL,
           y = NULL) +
      theme(legend.position = 'none',
            axis.text.x = element_text(angle = 40, hjust = 1))
    
    # density plots
    p2 <- data_for_plot %>% 
      select(`Site group`, all_of(numeric_vars)) %>% 
      pivot_longer(cols = -c("Site group")) %>% 
      ggplot(aes(x = value, group = `Site group`, fill = `Site group`)) +
      geom_density(alpha = 0.24) +
      facet_wrap(~name, scales = 'free') +
      scale_fill_viridis_d() +
      labs(x = NULL,
           y = NULL) +
      theme(legend.title = element_blank(),
            legend.position = 'bottom')
    
    # render both plots vertically
    grid.arrange(p1, p2, ncol = 1, heights = c(1, 2))
  })
  
  # render the plot
  output$results_plot_samp_v_pop <- renderPlot(results_samp_v_pop())

  # table of sites that accepted for the Results table
  output$results_table_accepted <- DT::renderDataTable({
    
    # get the data
    data <- get_dataset("stacked_results", datasets_available)
    data <- data[data$`Site group` == 'Accepted invitation',]
    
    # remove site_group column
    data <- data[, setdiff(colnames(data), 'Site group')]
    
    # build the table
    custom_datatable(data, selection = 'none') %>%
      formatRound(5:10, 2)
  })
  
  # download the table, plots, and sites dataframe
  output$results_button_download <- downloadHandler(
    filename = function() "DS4SI_site_selection_results.zip",
    content = function(file){
      
      # go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      # create data frame of all sites indicator columns 
       # if site was sent invitation and if accepted
      data <- get_dataset("stacked_results", datasets_available)
      population_dataset$`Sent invitation` <-
        population_dataset$`Site ID` %in% data$`Site ID`[data$`Site group` == "Sent invitation"]
      population_dataset$Accepted <-
        population_dataset$`Site ID` %in% data$`Site ID`[data$`Site group` == "Accepted invitation"]
      
      # write out the dataframe containing every site and its status
      write.csv(population_dataset, "sites.csv", row.names = FALSE)
      files <- "sites.csv"
      
      # write out the summary metrics and attributes table
      write.csv(results_table(), "summary.csv", row.names = TRUE)
      files <- c("summary.csv", files)
      
      # save the histograms
      ggsave("sampled_plots.png",
             plot = results_hist(),
             device = "png",
             width = 25,
             height = 25,
             units = "cm")
      files <- c("sampled_plots.png", files)
      
      # save the sampled vs. population
      ggsave("sampled_v_population_plots.png",
             plot = results_samp_v_pop(),
             device = "png",
             width = 25,
             height = 25,
             units = "cm")
      files <- c("sampled_v_population_plots.png", files)
      
      # create the zip file
      zip(file, files)
    }
  )
  
  # restart the session based on button click
  observeEvent(input$results_button_restart, {
    session$reload()
    gc()
  } )
  
}
