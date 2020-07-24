# DS4SI-tool
Pedagogical tool for selecting where to conduct randomized control trials. Built in Shiny.

Live version of the current draft can be found here: [jmarlo.shinyapps.io/DS4SI-tool/](https://jmarlo.shinyapps.io/DS4SI-tool/)

<br>

![](screenshot.png)

<br>

## Documentation

- /DS4SI-Tool:
  - /R:
    - functions.R: various functions that are used mulitple times by app.R
    - ggplot_settings.R: color and theme settings for ggplot to be loaded by app.R
    - score_generalizability.RData: pre-defined score_generalizability() function to be loaded by app.R. This is an .RData file becuase it contains an underlying empirical distribution
    - variables.R: pre-defined variables to be loaded by app.R
  - /data/jpta.csv: dataset of the population of sites
  - /Markdowns:
    - data_description.md: text on the data description page
    - tool_instructions.md: text on the tool instructions tab of the landing page
    - welcome_text.md: text on the landing page
  - /www/custom_css.css: custom CSS that gives the tool its look and feel
  - app.R: the core program
- /R/generate_site_selection_data.R: script generates the jpta.csv dataset

<br>

### Naming conventions

|                                                                                                                     | Convention                | Example                      |
|---------------------------------------------------------------------------------------------------------------------|---------------------------|------------------------------|
| UI slider id                                                                                                        | [page]_slider\_[variable] | filtering_slider_comfort     |
| UI select input id                                                                                                  | [page]_select\_[variable] | filtering_select_region      |
| UI button id                                                                                                        | [page]_button\_[label]    | sampling_button_run_sampling |
| UI table id                                                                                                         | [page]_table\_[name]      | sampling_table_excluded      |
| UI plot id                                                                                                          | [page]_plot\_[name]       | filtering_plot_hist          |
| The element id containing the user's selection for the dataset                                                      | [page]_dataset            | sampling_dataset             |
| The reactive function that returns the dataset object resulting from the user's selection                           | [page]_selected\_data()   | sampling_selected_data()     |
| The reactive function that receives the [page]_selected_data and returns the modified dataset based on user's input | [page]_data()             | sampling_data()              |
| ...                                                                                                                 |                           |                              |