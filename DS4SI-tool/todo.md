# To-do

### Welcome
  - Rewrite welcome tab
  - Write tool instructions

<br>
### Filtering page
  - Need to fix quantile slider

<br>
### Weighting page
  - Need to implement scoring mechanism

<br>
### Sampling page
  - Should implement button to intiate sampling 
  - The stratified sampling techinique needs to be verified
  - Can/should probabilities be incorporated for each strata?

<br>
### Manual exclusions page
  - Need a method to add sites back to the selected list

<br>    
### Send invitations page
  - What other information should be included here?

<br>  
### Results page
  - Visuals need some more thought
  - Figure out why this crashed when deployed to server (memory limit?)

<br>    
### Backend
  - There needs to be a way for users to save a dataframe and then reuse on other tabs
    - Right now, the only data frames to "start with" are the original 400 and then the results of the filtering
    - Users should have the ability to do their adjustments and then "save" the dataset for use on other tabs
    - On the backend, there needs to be a dynamic list of the possible dataframes for selection that can be reused across pages
  - Implement variable naming conventions that references which page it's used on 
  - Separate files into UI and server side
  - Should increase the size of the original dataset
  