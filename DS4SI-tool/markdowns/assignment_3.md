# Assignment Three: Reproducibility
The goal of this assignment is to create an instructional manual for teaching other researchers commands in R. Your submission for this assignment should be an R Markdown file that includes code, descriptions, documentation of your process, and any other materials or information that could help teach others how to complete the following actions. The results of the code included in the manual should be easily reproduced when run by another researcher (classmate, instructor, etc.). You will use data from the Center for Disease Control's National Health and Nutrition Examination Survey II (NHANES II) for this assignment. A Stata version of this dataset can be found at http://www.stata-press.com/data/r14/nhanes2.dta (here). [For the purposes of this assignment, you are not required to use the survey weights.]

### Assignment details
The assignment requires a mix of (reproducible) programming and communication skills. Suppose you are working as part of a team of researchers at a health equity institute. Part of your job entails performing data analyses to help inform health policy. Your job also entails training junior members of your team in some basic data cleaning, analysis, and visualization methods in R. Produce an instructional manual to teach a junior member of your team how to perform the following tasks:   

1. Load the Stata dataset into R (hint: package readstata13 will be helpful).  
2. Create a summary table or figure with descriptive statistics for at least 5 variables. Make sure that the summaries are appropriate for the type of data being described (e.g. do not use a 5-number summary for a binary variable!).  
3. Provide a brief description of what the table/figure shows you.  
4. Perform an analysis or create a visualization that will be useful to your organization. [Note to student: you can use any command that you want as long as (a) it incorporates at least two variables and (b) you have never used it before.] Describe why you chose that command (how it
uses data to inform the mission of the organization) and how you implemented it. Examples of types of commands: visualizations (graphs, charts, plots), data sorting, missing data summaries, predictive models. Pie charts of any sort are not allowed!  
5. Bundle this using packrat as demonstrated by Vicky Steeves.  
6. Create a title for your manual that is appropriate given what you did, for instance “Manual for visualizing how the relationship between blood pressure and heart attacks varies by race”

### What to turn in:  
1. PDF created by knitting R Markdown file  
2. R Markdown file and everything needed for me to run it to produce your PDF (Note that your
code should assume that the user can connect to the internet. So assume the user will access the
data via the link.)

<br>