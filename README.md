# DS4SI-tool
Pedagogical tool for selecting where to conduct randomized control trials. Built in Shiny.

Current draft can be found here: [jmarlo.shinyapps.io/DS4SI-tool/](https://jmarlo.shinyapps.io/DS4SI-tool/)

<br>

![](screenshot.png)

<br>

### Naming conventions

|                                                                                                                     | Convention                | Example                      |
|---------------------------------------------------------------------------------------------------------------------|---------------------------|------------------------------|
| UI slider id                                                                                                        | [page]_slider\_[variable] | filtering_slider_comfort     |
| UI select input id                                                                                                  | [page]_select\_[variable] | filtering_select_region      |
| UI button id                                                                                                        | [page]_button\_[label]    | sampling_button_run_sampling |
| Table id                                                                                                            | [page]_table\_[name]      | sampling_table_excluded      |
| Plot id                                                                                                             | [page]_plot\_[name]       | filtering_plot_hist          |
| The element id containing the user's selection for the dataset                                                      | [page]_dataset            | [page]_data()                |
| The dataset object resulting from the user's selection                                                              | [page]_selected\_data     | sampling_selected_data       |
| The reactive function that receives the [page]_selected_data and returns the modified dataset based on user's input | [page]_data()             | sampling_data()              |
| ...                                                                                                                 |                           |                              |