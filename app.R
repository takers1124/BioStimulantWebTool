library(shiny)
library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(urbnmapr)
library(shinyjs)

#URL of github repo
githuburl<-"https://raw.githubusercontent.com/pointblue/BioStimulantWebTool/main/Data/"

# Load csv files with questions and recommendations. 
# These csv's are saved in github and should be edited and uploaded as needed within the same github folder using the same file name
questions <- read_csv(paste0(githuburl,"Questions.csv"))
recommendations <- read_csv(paste0(githuburl,"Recommendations.csv"))
final_recommendations <- read_csv(paste0(githuburl,"FinalRollup.csv"))
# Lookup csv table for county specific recommendations
lookup_table <- read.csv(paste0(githuburl,"Counties.csv"))
# Adjust county name to match urbnmapr names geospatial layer
lookup_table$County <- paste(lookup_table$County, "County", sep = " ")



# Get California county boundaries for mapping
counties_sf <- get_urbn_map("counties", sf = TRUE)
sf::st_crs(counties_sf) <- 2163
counties_wgs84 <- sf::st_transform(counties_sf, 4326)
ca_counties <- counties_wgs84 %>% filter(state_name == "California")



###################################
### UI code
### Set section and page wide parameters such as text color, spacing, background color, etc 
###################################
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      * { font-family: 'Roboto', sans-serif; }
      
      body { background-color: #90AEAD; margin-bottom: 50px; }
      
      .title-container { position: fixed; width: 100%; z-index: 1000; background-image: url(https://ksqd.org/wp-content/uploads/2020/11/Screen-Shot-2020-11-04-at-6.45.59-PM.png); background-size: cover; border-bottom: 1px solid #ccc;  }
      
      .title-container .row { margin: 0; }
      
      .title-container h2 { color: #E64833; font-weight: bold; }

      .intro-text { color: #141619; font-size: 20px; font-weight: 500; margin-top: 130px; text-align: center; padding: 20px; background-color: #90AEAD; }
      
      .questions-container h4 { font-weight: bold; }
      
      .questions-container { margin-top: 25px; }
      
      .info-text { font-size: 18px; font-weight: bold; margin-top: 20px; }
      

      .map-container { margin-top: 0px; margin-bottom: 5px; margin-right: 10px; width: 100%; height: 400px; float: left;}
    
      .recommendation-text { color: #244855; font-size: 18px; font-weight: bold; margin-top: 50px; }  
      
       .final-recommendation { color: #141619; font-size: 18px; font-weight: bold; margin-top: 20px;}
      
 .additional-recommendations-container {  margin-top: 20px; display: inline-block; margin-left: 10px;  }
      
           
      .additional-recommendations-btn { margin-top: 1px; background-color: #244855; color: white; }
      
  .email-container { margin-top: 20px; display: inline-block; margin-left: 10px; }
  
   .email-prompt { font-size: 18px; font-weight: bold; margin-top: 10px; }
   
    .radio label {
    font-size: 18px; font-weight: bold;
    }
    
      
  "))
  ),
  
  #########################
  div(
    class = "title-container",
    titlePanel(
      fluidRow(
        column(width = 6, tags$img(src = "https://github.com/pointblue/BioStimulantWebTool/blob/main/www/CoRenewal-Header-Logo-less-text.jpg?raw=true", height = "110px")),
        column(width = 6, h2("BioStimulant Exploration Tool", style = "margin-top: 10px; font-size: 40px;"))
      ),windowTitle = "BioStim"
    )
  ),
  
  #########################
  div(class = "intro-text", "Welcome to the biostimulant web tool. Please select from the following options below to see general recommendations based on your fire. Once all selections are made, click on 'Additional Recommendations' to see biostimulant general guidelines based on all your choices as a whole. Hit the 'Refresh' button to start over."),
  
  #########################
  div(
    class = "questions-container",
    actionButton("refreshButton", "Refresh"),
    fluidRow(
      column(6, uiOutput("questionsUI_col1")),
      column(6, uiOutput("questionsUI_col2"))
    ),
    
    # Create a new row with two columns
    fluidRow(
      column(
        6,
        div(class = "info-text", "Choose the area you are working in by selecting it in the map below:"),
        div(class = "map-container", leafletOutput("map"))
      ),
      
      column(
        6,
        # Add region recommendation and buttonUI to the right column
        div(class = "recommendation-text", textOutput("regionRecommendation")),
        uiOutput("buttonUI")
      )
    ),
    
    div(class = "final-recommendation", textOutput("finalRecommendation"))
  )
)

###################################
## Server code
###################################
server <- function(input, output, session) {
  # Initialize reactive values to store user responses
  user_responses <- reactiveValues(data = list())
  clicked_county <- reactiveVal(NULL)
  
  options(shiny.sanitize.errors = FALSE)
  options(shiny.maxRequestSize = 30*1024^2) # 30MB max request size
  
  ######################### Set rows/columns for questions
  
  #Question column 1
  output$questionsUI_col1 <- renderUI({
    question_ids <- seq(1, nrow(questions), 2)
    lapply(question_ids, function(i) {
      if (i <= nrow(questions)) {
        question_code <- questions$QuestionCode[i]
        question_title <- questions$QuestionTitle[i]
        options <- c("", questions$Option1[i], questions$Option2[i])
        
        div(
          h4(question_title),
          selectInput(paste0("question_", question_code), label = NULL, choices = options),
          div(class = "recommendation-text", textOutput(paste0("recommendation_", question_code)))
        )
      }
    })
  })
  
  
  # Questions column 2
  output$questionsUI_col2 <- renderUI({
    question_ids <- seq(2, nrow(questions), 2)
    lapply(question_ids, function(i) {
      if (i <= nrow(questions)) {
        question_code <- questions$QuestionCode[i]
        question_title <- questions$QuestionTitle[i]
        options <- c("", questions$Option1[i], questions$Option2[i])
        
        div(
          h4(question_title),
          selectInput(paste0("question_", question_code), label = NULL, choices = options),
          div(class = "recommendation-text", textOutput(paste0("recommendation_", question_code)))
        )
      }
    })
  })
  
  
  #########################
  # Render map
  output$map <- renderLeaflet({
    leaflet(ca_counties) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ifelse(county_name == clicked_county(), "red", "lightblue"),
        weight = 1, 
        color = "black",
        highlightOptions = highlightOptions(weight = 3, color = "red"),
        label = ~county_name,
        layerId = ~county_name, 
        popup = ~county_name
      ) %>%
      setView(lng = -119.5, lat = 37.5, zoom = 5.5)
  })
  
  
  
  # Observe map clicks
  observeEvent(input$map_shape_click, {
    clicked_county(input$map_shape_click$id)
  })
  
  
  
  # Render region recommendation based on clicked county
  observeEvent(clicked_county(), {
    req(clicked_county())
    
    region <- lookup_table %>%
      filter(County == clicked_county()) %>%
      pull(Region)
    
    recommendation <- lookup_table %>%
      filter(County == clicked_county()) %>%
      pull(Recommendation)
    
    output$regionRecommendation <- renderText({
      if (length(region) > 0 && length(recommendation) > 0) {
        recommendation
      } else {
        "No recommendation currently available for the selected location"
      }
    })
  })
  
  
  
  #########################
  # Create additional recommendations button and email input field
  output$buttonUI <- renderUI({
    req(questions, clicked_county())
    
    all_selected <- all(sapply(questions$QuestionCode, function(q) {
      val <- input[[paste0("question_", q)]]
      !is.null(val) && val != ""
    }))
    
    if (all_selected) {
      tagList(
        # Wrap the consultation prompt and radio buttons in the same container div
        div(
          style = "margin-top: 20px;",
          div(class = "email-prompt", "Would you like to be contacted by a member of our team for a free 15 minute consultation?"),
          
          
          # Place radio buttons directly below the text with matching styles
           radioButtons("consultation_choice", label = NULL, 
                       choices = list(
                         "No thanks" = "No thanks", 
                         "Yes, please email me" = "Yes, please email me"
                       ), 
                       selected = "No thanks",
                       inline = FALSE, # Ensure they appear vertically aligned, not inline
                       width = "100%"  # Ensure they take up the full width of the container
          ),
          
          # Conditional UI for email and name fields based on user choice
          conditionalPanel(
            condition = "input.consultation_choice == 'Yes, please email me'",
            textInput("name", "Name"),
            textInput("email", "Email address", value = "")
          )
        ),
        

        # Additional recommendations button
        actionButton("showRecommendations", "Additional Recommendations", class = "additional-recommendations-btn", disabled = TRUE)
      )
    }
  })
  
  
  # Helper function to validate email format
  isValidEmail <- function(email) {
    # Regular expression to match basic email pattern
    grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", email)
  }
  
  # Observer to enable/disable the button based on conditions
  observe({
    req(input$consultation_choice)  # Ensure consultation_choice is available
    
    if (input$consultation_choice == "Yes, please email me") {
      # Ensure both name and email are filled and the email is valid
      shinyjs::toggleState("showRecommendations", 
                           condition = !is.null(input$name) && input$name != "" && 
                             !is.null(input$email) && input$email != "" && 
                             isValidEmail(input$email))  # Validate email format
    } else {
      # If "No thanks" is selected, enable the button
      shinyjs::enable("showRecommendations")
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
      
      # Conditionally store name and email if the user selected the consultation
      if (input$consultation_choice == "Yes, please email me") {
        user_responses$name <- input$name
        user_responses$email <- input$email
      } else {
        user_responses$name <- NULL
        user_responses$email <- NULL
      }
      
      # Prepare the data frame with appropriate column names
      response_df <- as.data.frame(t(user_responses$data), stringsAsFactors = FALSE)
      colnames(response_df) <- questions$QuestionCode
      rownames(response_df) <- NULL
      response_df[] <- lapply(response_df, as.character) 
      
      # Add the name and email columns if they exist
      if (!is.null(user_responses$name)) {
        response_df$name <- user_responses$name
        response_df$email <- user_responses$email
      }
      
      # Write to or append to the CSV file
      file_path <- "www/UserInput.csv"
      if (!file.exists(file_path)) {
        write.csv(response_df, file = file_path, row.names = FALSE)
      } else {
        write.table(response_df, file = file_path, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
      }
      
      # Process recommendations logic
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
  
  
  
  #########################
  # Action if user hits "refresh" button
  # Clears selected inputs
  # Clears recommendation text
  observeEvent(input$refreshButton, {
    lapply(1:nrow(questions), function(i) {
      question_code <- questions$QuestionCode[i]
      
      updateSelectInput(session, paste0("question_", question_code), selected = "")
      output[[paste0("recommendation_", question_code)]] <- renderText({ NULL })
    })
    
    output$finalRecommendation <- renderText({ NULL })  # Clear the final recommendation
    
    clicked_county(NULL)  # Reset the map selection
    output$regionRecommendation <- renderText({ NULL })  # Clear the region recommendation
  })
  
  
  
  
  #########################
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
