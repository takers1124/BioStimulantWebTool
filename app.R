library(shiny)
library(readr)
library(dplyr)

#  https://www.jdtrat.com/blog/connect-shiny-google/
library(googlesheets4)

#setwd("C:/Users/djongsomjit/OneDrive - Point Blue/BioStimulantWeb/BioStimulantWebTool")

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "C:/Users/djongsomjit/OneDrive - Point Blue/BioStimulantWeb/BioStimulantWebTool/.secrets"
)
#googledrive::drive_auth() 
#googlesheets4::gs4_auth()

sheet_id <- googledrive::drive_get("testsheet")$id



# Load data
questions <- read_csv("https://raw.githubusercontent.com/pointblue/BioStimulantWebTool/main/Data/Questions.csv")
recommendations <- read_csv("https://raw.githubusercontent.com/pointblue/BioStimulantWebTool/main/Data/Recommendations.csv")
final_recommendations <- read_csv("https://raw.githubusercontent.com/pointblue/BioStimulantWebTool/main/Data/FinalRollup.csv")

###################################
### UI code
###################################
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      * { font-family: 'Roboto', sans-serif; }
      
      body { background-color: #90AEAD; margin-bottom: 50px; }
      
      .title-container { position: fixed; width: 100%; z-index: 1000; background-image: url(https://ksqd.org/wp-content/uploads/2020/11/Screen-Shot-2020-11-04-at-6.45.59-PM.png); background-size: cover; border-bottom: 1px solid #ccc;  }
      
      .title-container .row { margin: 0; }
      
      .title-container h2 { color: #E64833; font-weight: bold; }
      
      .questions-container h4 { font-weight: bold; }
      
      .recommendation-text { color: #244855; font-size: 18px; font-weight: bold; margin-top: 10px; }
      
      .questions-container { margin-top: 25px; }
      
      .additional-recommendations-btn { margin-top: 20px; background-color: #244855; color: white; }
      
      .final-recommendation { color: #141619; font-size: 18px; font-weight: bold; margin-top: 10px; }
      
      .intro-text { color: #141619; font-size: 20px; font-weight: 500; margin-top: 130px; text-align: center; padding: 20px; background-color: #90AEAD; }
    "))
  ),
  
  #########################
  div(
    class = "title-container",
    titlePanel(
      fluidRow(
        column(width = 6, tags$img(src = "https://github.com/pointblue/BioStimulantWebTool/blob/main/www/CoRenewal-Header-Logo-less-text.jpg?raw=true", height = "110px")),
        column(width = 6, h2("BioStimulant Exploration Tool", style = "margin-top: 10px; font-size: 40px;"))
      )
    )
  ),
  
  #########################
  div(class = "intro-text", "Welcome to the biostimulant web tool. Please select from the following options below to see general recommendations based on your fire. Once all selections are made, click on 'Additional Recommendations' to see biostimulant general guidelines based on all your choices as a whole. Hit the 'Refresh' button to start over."),
  
  #########################
  div(
    class = "questions-container",
    actionButton("refreshButton", "Refresh"),
    uiOutput("questionsUI"),
    uiOutput("buttonUI"),
    div(class = "final-recommendation", textOutput("finalRecommendation"))
  )
)

###################################
## Server code
###################################
server <- function(input, output, session) {
  # Initialize reactive values to store user responses
  user_responses <- reactiveValues(data = list())
  
  #########################
  output$questionsUI <- renderUI({
    lapply(1:nrow(questions), function(i) {
      question_code <- questions$QuestionCode[i]
      question_title <- questions$QuestionTitle[i]
      options <- c("", questions$Option1[i], questions$Option2[i])
      
      div(
        h4(question_title),
        selectInput(paste0("question_", question_code), label = NULL, choices = options),
        div(class = "recommendation-text", textOutput(paste0("recommendation_", question_code)))
      )
    })
  })
  
  #########################
  # Create additional recommendations button
  output$buttonUI <- renderUI({
    req(questions)
    
    if (all(sapply(questions$QuestionCode, function(q) input[[paste0("question_", q)]] != ""))) {
      actionButton("showRecommendations", "Additional Recommendations", class = "additional-recommendations-btn")
    }
  })
  
  #########################
  # Action for "Additional recommendations" button click
  observeEvent(input$showRecommendations, {
    req(questions, input$showRecommendations)
    
    if (input$showRecommendations > 0) {
      selected_options <- sapply(questions$QuestionCode, function(q) input[[paste0("question_", q)]])
      
      # Store user responses in the reactive values object
      user_responses$data <- as.list(selected_options)
      
      
      # Prepare the data frame with appropriate column names
      response_df <- as.data.frame(t(user_responses$data), stringsAsFactors = FALSE)
      colnames(response_df) <- questions$QuestionCode
      rownames(response_df) <- NULL
      response_df[] <- lapply(response_df, as.character) 
      
      # Check if sheet exists and has data
      sheet_exists <- tryCatch({
        sheet_names(sheet_id)
        TRUE
      }, error = function(e) { FALSE })
      
      if (!sheet_exists || nrow(read_sheet(sheet_id, sheet = "main")) == 0) {
        # If sheet doesn't exist or is empty, write with headers
        sheet_write(response_df, ss = sheet_id, sheet = "main")
      } else {
        # If sheet exists and has data, append without headers
        sheet_append(response_df, ss = sheet_id, sheet = "main")
      }
      

      
      filtered_recs <- final_recommendations
      for (i in 1:nrow(questions)) {
        question_col <- LETTERS[i]
        if (!is.na(final_recommendations[[question_col]][1]) && selected_options[i] != "") {
          filtered_recs <- filter(filtered_recs, !!sym(question_col) == selected_options[i])
        }
      }
      
      final_rec <- filtered_recs %>% select(FinalRollup) %>% pull()
      
      output$finalRecommendation <- renderText({
        if (length(final_rec) > 0) {
          return(final_rec[1])
        } else {
          return("No specific recommendation found for this combination of choices.")
        }
      })
    }
  })
  
  
  
  # Action if user hits "refresh" button
  # Clears selected inputs
  # Clears recommendation text
  observeEvent(input$refreshButton, {
    lapply(1:nrow(questions), function(i) {
      question_code <- questions$QuestionCode[i]
      updateSelectInput(session, paste0("question_", question_code), selected = "")
      output[[paste0("recommendation_", question_code)]] <- renderText({ NULL })
    })
  })
  
  
  
  # Dynamic display of recommendations based on dropdown questions
  observeEvent(questions$QuestionCode, {
    lapply(1:nrow(questions), function(i) {
      question_code <- questions$QuestionCode[i]
      
      # Monitors value of each question dropdown value
      # Retrieve corresponding recommendation based on question code and option
      observeEvent(input[[paste0("question_", question_code)]], {
        selected_option <- input[[paste0("question_", question_code)]]
        
        if (selected_option != "") {
          recommendation <- recommendations %>%
            filter(QuestionCode == question_code & Option == selected_option) %>%
            select(RecommendationDisplay) %>%
            pull()
          
          # Update text
          output[[paste0("recommendation_", question_code)]] <- renderText({ recommendation })
        } else {
          output[[paste0("recommendation_", question_code)]] <- renderText({ NULL })
        }
      }, ignoreInit = TRUE)
    })
  }, once = TRUE)
}

shinyApp(ui, server)
