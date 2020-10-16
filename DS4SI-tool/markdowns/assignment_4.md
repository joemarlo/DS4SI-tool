# Assignment Four: Reproducibility

You work for a program evaluation firm (like MDRC). Your goal is to create an instructional manual for teaching junior researchers at your organization commands in R. Your submission for this assignment should be an R Markdown file or Stata file (+pdf) that includes code, descriptions, documentation of your process, and any other materials or information that could help teach others how to complete the following actions. The results of the code included in the manual should be easily reproduced when run by another researcher (classmate, instructor, etc.). 

### Assignment details

The assignment requires a mix of (reproducible) programming and communication skills. **You are being asked to both present key descriptives and findings from the PNGD impact evaluation for which you selected and recruited sites in assignments one and two.** These results should reflect the effect(s) of the program for the sample of sites who consented to participate in the study. You are also, as part of this project, in charge of training junior members of your team in some basic data cleaning, analysis, and visualization methods in either R or Stata. Toward this end, the study results will be presented in the form of an instructional manual that can be used to teach a junior member of your team how to perform the following tasks:  

1. Load the csv dataset into R or Stata.  
2. Create a summary table or figure with descriptive statistics for all variables in the dataset. Make sure that the summaries are appropriate for the type of data being described (e.g. do not use a 5-number summary for a binary variable!).  
3. Provide a brief (one paragraph) description of what the table/figure shows you.  
4. Assess the impact of PNGD on individuals’ annual income. Valid methods include (difference in mean outcomes between treatment groups, regression on the treatment variable, regression on the treatment variable and pre-treatment covariates, a multilevel model, etc). Run at least two models for this and explain your choices (e.g., what covariates you are including). Write up a brief description of your findings.  
5. Create a visualization that will be useful in understanding your sample population or the findings. [Note: you can use any command that you want as long as (a) it incorporates at least two variables and (b) you have never used it before.] Describe why you chose that command (how it uses data to inform the mission of the organization) and how you implemented it. Examples of types of commands: visualizations (graphs, charts, plots), data sorting, missing data summaries, predictive models. Pie charts of any sort are not allowed! Use the Data exploration page for ideas if you are struggling with brainstorming but the final visualization should be created in your original code.   
6. Create a title for your manual that is appropriate given what you did, for instance “Manual for estimating treatment effects of a job training program”  
7. R users should bundle the packages used as demonstrated by Vicky Steeves.  

### What to turn in

*For R users:*
- PDF created by knitting R Markdown file
- R Markdown file and everything needed for me to run it to produce your PDF (including the csv file)
- The zip file downloaded from the tool on the ‘Summary results’ page

*For Stata users:*
- A clean and readable do-file containing all of your code
- A 1-2 page PDF write-up of your process containing your final tables and figures. Your write-up should also describe the results of each table in a clearly written paragraph
- An excel file with your table shell of your regression results (using putexcel)
- The zip file downloaded from the tool on the ‘Summary results’ page


<br>