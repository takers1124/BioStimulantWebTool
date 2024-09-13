# BioStimulantWebTool


## The shiny app uses the following components


1. app.R - R code that contains the UI and Server code
2. The 4 csv lookup files stored in a Data folder-
    - Questions.csv - List of questions that will be asked and the available options for each question.
    - Recommendations.csv - Question specific recommendations.
    - FinalRollup.csv - Any additional recommendations based on all the user choices for previous questions.
    - Counties.csv - Lookup csv table for county/map based selection specific recommendations.
3. www folder containing any images to be displayed

## How to create the web app - you will need a github account, a shinyapp account, and RStudio

### First create a github repo and clone it to your local drive. 
####  Note: There are several ways to clone a github repository. This is one way that this can be done
1. Create or open a github account at https://github.com/
2. Add the above components to a github directory
3. Open RStudio and set up a new project (File -> New Project)
4. Select Version Control then Git and add your github URL and a project name. **Remember that the project name will be part of the URL of the web app you publish.
5. Select a location on your local drive where the repo will be cloned
6. Click Create Project. This will clone all the data from your github repo (except those listed in .gitignore file) then it will open your new RStudio Project

### Now test the app.R file then publish it online

1. Open your app.R code file inside this new RStudio project you created above. Make any edits to the github URL it is pointing to.
2. To test the code file click on the Run App button and this will open a test version of the web app
3. Create a shinyapps account and configure it using the rsconnect command as described here https://shiny.posit.co/r/articles/share/shinyapps/ **Remember the URL of your shinyapps account will be part of the web app you publish.
5. In RStudio click on the Publish icon to the right of the Run App button. This will take you through the needed steps and checks to publish your app


