# DS4SI-tool
Pedagogical tool for selecting where to conduct randomized control trials. Built in Shiny.

Current draft can be found here: [jmarlo.shinyapps.io/DS4SI-tool/](https://jmarlo.shinyapps.io/DS4SI-tool/)

<br>

![](screenshot.png)

<br>

### Variable naming convention

| Element                                                                        | Convention               | Example                      |
|--------------------------------------------------------------------------------|--------------------------|------------------------------|
| The element containing the user's selection for the datasete                   | [page]_dataset           | sampling_dataset             |
| The dataset object resulting from the user's selection                         | [page]_selected_data     | sampling_selected_data       |
| The reactive function that contains the modified dataset based on user's input | [page]_data()            | sampling_data()              |
| UI sliders                                                                     | [page]_slider_[variable] | filtering_slider_comfort     |
| UI select inputs                                                               | [page]_select_[variable] | filtering_select_region      |
| UI buttons                                                                     | [page]_button_[label]    | sampling_button_run_sampling |
| ...                                                                            |                          |                              |