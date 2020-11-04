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
<!--
Used this tool to generate an html table 
https://www.tablesgenerator.com/html_tables
-->

*For R users:*
- PDF created by knitting R Markdown file
- R Markdown file and everything needed for me to run it to produce your PDF (including the csv file within the zip file)
- The zip file downloaded from the tool on the ‘Summary results’ page

*For Stata users:*
- A clean and readable do-file containing all of your code
- A 1-2 page PDF write-up of your process containing your final tables and figures. Your write-up should also describe the results of each table in a clearly written paragraph
- An excel file with your table shell of your regression results (using putexcel)
- The zip file downloaded from the tool on the ‘Summary results’ page

### Rubric
<!--
Used this tool to generate an html table 
https://www.tablesgenerator.com/html_tables
-->

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-0pky{border-color:inherit;text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-0pky"></th>
    <th class="tg-0pky"><span style="font-weight:bold">Great</span><br><span style="font-style:italic">90-100% of possible points</span></th>
    <th class="tg-0pky"><span style="font-weight:bold">Needs work</span><br><span style="font-style:italic">&lt;90% of possible points</span><br></th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-0pky"><span style="font-weight:bold">Load the data</span> <span style="font-style:italic">5pts</span></td>
    <td class="tg-0pky">- <span style="font-weight:400;font-style:normal;text-decoration:none">Includes code that loads the data into R or Stata</span></td>
    <td class="tg-0pky"><span style="font-weight:400;font-style:normal;text-decoration:none">- Does not include code that loads the data</span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:bold">Summary table or figure </span><span style="font-style:italic">25pts total</span><br>Table or figure - 15pts<br>Description - 10pts<br></td>
    <td class="tg-0pky"><span style="font-weight:400;font-style:normal;text-decoration:none">- Includes a table or figure that appropriately summarizes the data for the given data type (e.g. do not use a 5-number summary for a binary variable)</span><br><span style="font-weight:400;font-style:normal;text-decoration:none">- Includes a brief description of what the table or figure is showing</span></td>
    <td class="tg-0pky"><span style="font-weight:400;font-style:normal;text-decoration:none">- Includes inappropriate descriptive summaries for variable types</span><br><span style="font-weight:400;font-style:normal;text-decoration:none">- Inaccurate or unclear description</span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:bold">Analysis</span> <span style="font-style:italic">40pts total</span><br>Analysis - 15pts<br>Description of how you implemented it - 15pts<br>Description of why it is important to your organization - 10pts</td>
    <td class="tg-0pky"><span style="font-weight:400;font-style:normal;text-decoration:none">- Analysis reflects the effect(s) of the program for the sample of sites</span><br><span style="font-weight:400;font-style:normal;text-decoration:none">- Describes why you chose the command</span><br><span style="font-weight:400;font-style:normal;text-decoration:none">- Describes how results of the analysis and why it is useful to your organization</span></td>
    <td class="tg-0pky"><span style="font-weight:400;font-style:normal;text-decoration:none">- Analysis does not address the effect(s) of the program for the sample of sites</span><br><span style="font-weight:400;font-style:normal;text-decoration:none">- Unclear how to implement the analysis</span><br><span style="font-weight:400;font-style:normal;text-decoration:none">- Includes a pie chart</span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:bold">Title for your manual </span><span style="font-style:italic">5pts</span></td>
    <td class="tg-0pky">- <span style="font-weight:400;font-style:normal;text-decoration:none">Title is clear and accurately summarizes the manual</span></td>
    <td class="tg-0pky">- <span style="font-weight:400;font-style:normal;text-decoration:none">Title is not included, not clear, or not descriptive of the manual</span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:bold">Reproducible</span> <span style="font-style:italic">10pts</span></td>
    <td class="tg-0pky"><span style="font-weight:400;font-style:normal;text-decoration:none">- Reproducible as demonstrated by Vicky Steeves using renv (for R users)</span><br><span style="font-weight:400;font-style:normal;text-decoration:none">- Stata code is well documents and uses the required commands; output is identical to contents of the PDF (for Stata users)</span></td>
    <td class="tg-0pky"><span style="font-weight:400;font-style:normal;text-decoration:none">- Is not reproducible (i.e. code does not run for R users)</span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:bold">Submission files</span> <span style="font-style:italic">15pts</span></td>
    <td class="tg-0pky"><span style="font-weight:400;font-style:normal;text-decoration:none">- Includes all the files mentioned above in 'What to turn in'</span></td>
    <td class="tg-0pky">- <span style="font-weight:400;font-style:normal;text-decoration:none">Missing files</span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:bold">Extra credit</span> <span style="font-style:italic">5pts</span></td>
    <td class="tg-0pky" colspan="2">- <span style="font-weight:400;font-style:normal;text-decoration:none">Exceptional plots, tables, or insights. Or exceptionally well organized</span></td>
  </tr>
</tbody>
</table>


<br>
<br>
<br>