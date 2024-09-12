# BioStimulantWebTool


## The shiny app uses the following components


1. app.R - R code that contains the UI and Server code
2. The 4 csv lookup files stored in a Data folder-
    - Questions.csv - List of questions that will be asked and the available options for each question.
    - Recommendations.csv - Question specific recommendations.
    - FinalRollup.csv - Any additional recommendations based on all the user choices for previous questions.
    - Counties.csv - Lookup csv table for county/map based selection specific recommendations.
3. www folder containing any images to be displayed

## How to create the web app
####  Note: There are several ways to set up a github repository. This is one way that this can be done
1. Add the above components to a github directory
2. Open RStudio and set up a new project (File -> New Project)
3. Select Version Control then Git and add your github URL and a project name
4. Click Create Project
5. 
