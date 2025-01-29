packages <- c("shiny", "DT", "dplyr")
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}
sapply(packages, install_if_missing)

library(shiny)
library(DT)
library(dplyr)
Sys.setlocale("LC_ALL", "en_US.UTF-8")

ui <- fluidPage(
  titlePanel("Lunch Notebook 🍔🍛🌯📓"),
  
  fluidRow(
    column(
      width = 12,
      div(style = "background-color: #f0f0f0; height: 10px; margin: 20px 0;")
    )
  ),
  fluidRow(
    column(
      width = 12,
      h3("Rating Search"),
      textAreaInput("bulk_input", h5(""),rows = 2,
                    placeholder = "Paste restaurant list from email or start typing a name")
    )
  ),
  fluidRow(
    column(
      width = 12,
      h4("Restaurant Ratings"),
      h5("Click a row to see all of your rankings for that restaurant"),
      DTOutput("data_table")
    )
  ),
  fluidRow(
    column(
      width = 12,
      DTOutput("selected_restaurant_table")
    )
  ),
  fluidRow(
    column(
      width = 12,
      div(style = "background-color: #f0f0f0; height: 10px; margin: 20px 0;")
    )
  ),
  
  # Create a rating button
  fluidRow(
    column(
      width = 12,
      actionButton("create_rating", "Create a Rating", class = "btn-primary")
    )
  )
)

server <- function(input, output, session) {
  file_path <-  "~/lt_ratings__.csv"
  
  # Load existing data from CSV if it exists, or initialize an empty dataframe
  if (file.exists(file_path)) {
    rankings_data <- read.csv(file_path, stringsAsFactors = FALSE)
  } else {
    rankings_data <- data.frame(
      Date = as.Date(character()),
      Restaurant = character(),
      Dish = character(),
      Ranking = numeric(),
      Notes = character(),
      stringsAsFactors = FALSE
    )
    write.csv(rankings_data, file_path, row.names = FALSE)
  }
  
  # Reactive value to store the dataset
  data <- reactiveVal(rankings_data)
  
  # Reactive expression to calculate the average ranking for each restaurant
  avg_rankings <- reactive({
    data() %>%
      group_by(Restaurant) %>%
      summarise(Average_Rating = round(mean(Ranking, na.rm = TRUE), 2)) %>%
      ungroup()
  })
  
  # Reactive expression to filter the restaurant ratings based on search input
  # Reactive expression to filter the restaurant ratings based on search input
  # Reactive expression to filter the restaurant ratings based on search input
  filtered_rankings <- reactive({
    req(avg_rankings()) # Ensure data exists
    
    # Get input text and trim spaces
    search_string <- trimws(input$bulk_input)
    
    if (search_string == "") {
      return(avg_rankings()) # Show all data if no search input
    } else {
      # Split input by comma or newline
      restaurant_list <- unlist(strsplit(search_string, "[,\n]"))
      
      # Trim whitespace and remove empty values
      restaurant_list <- trimws(restaurant_list)
      restaurant_list <- restaurant_list[restaurant_list != ""]
      
      # Convert restaurant names to lowercase for case-insensitive matching
      avg_rankings() %>%
        filter(
          sapply(Restaurant, function(resto) {
            any(sapply(restaurant_list, function(query) {
              grepl(query, resto, ignore.case = TRUE)  # Case-insensitive partial match
            }))
          })
        )
    }
  })
  
  
  
  # Reactive value to store the selected restaurant's rankings
  selected_restaurant_data <- reactiveVal(data.frame())
  
  # Observe row selection on the main table
  observe({
    selected_row <- input$data_table_rows_selected
    if (!is.null(selected_row) && length(selected_row) > 0) {
      selected_restaurant <- filtered_rankings()$Restaurant[selected_row]
      all_rankings <- data() %>%
        filter(Restaurant == selected_restaurant)
      selected_restaurant_data(all_rankings)
    } else {
      selected_restaurant_data(data.frame())
    }
  })
  
  # Render the filtered main table using DT
  output$data_table <- renderDT({
    datatable(
      filtered_rankings(),
      selection = "single",
      options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE)
    )
  })
  
  # Render the table with all rankings for the selected restaurant using DT
  output$selected_restaurant_table <- renderDT({
    datatable(
      selected_restaurant_data(),
      options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE)
    )
  })
  
  default_restaurants <- c("CuCu The Organic Bird","Mendy's Kosher Restaurant","Handsome Rice","Jerusalem Cafe","Sergimmo Salumeria - 9th Ave","Fridges Market","Teriyaki R Us","Greek Xpress - W40th St","Mendy's Kosher Restaurant","Ben's Fast Food - Times Square","Tortazo - Times Square","Pita Grill Kosher - 2nd Ave","Green Blend","Jerusalem Cafe","Fridges Market","Blue Maiz - 8th Ave","Bareburger - Hell's Kitchen","Happy Tuna Sushi & Crispy Rice","Zuckers Bagels - Flatiron","Mendy's Kosher Restaurant","Terra - Eat Better Food","Jerusalem Cafe","Pastrami Queen - Moynihan Food Hall","Chopt Creative Salad Co - Penn Station","Fridges Market","Frame Gourmet Eatery","Mendy's Kosher Restaurant","The Kati Roll Company - Midtown West","Pokeworks - Hudson Yards","Pita Grill Kosher - 2nd Ave","Sophie's Cuban Cuisine - Bryant Park","Lenwich - Flatiron","YONO Sushi by BondSt","Pita Grill Kosher - 2nd Ave","Just Salad - Flatiron","Jerusalem Cafe","Naya - Nomad","Fridges Market","Handsome Rice","INDAY Express","Pita Grill Kosher - 2nd Ave","Red Poke - 9th Ave","Honeybrains - Flatiron","Jerusalem Cafe","Fridges Market","Little Biggs - Chelsea","Takumi Taco - Chelsea","Extravert - Chelsea")
  
  # Show modal when "Create a Rating" is clicked
  observeEvent(input$create_rating, {
    showModal(
      modalDialog(
        title = "Add a New Rating - try pasting from email if adding new restaurant for consistency",
        selectizeInput("new_restaurant_suggestions", "Select or Type a Restaurant", 
                       choices = unique(c(default_restaurants, data()$Restaurant)),  
                       options = list(create = TRUE)),  # Allow new entries
        textInput("new_dish", "Dish"),
        sliderInput("new_ranking", "Ranking (1-5)", min = 1, max = 5, value = 3, step = 0.5),
        textAreaInput("new_notes", "Notes", ""),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_rating", "Save", class = "btn-success")
        )
      )
    )
    
    # **Force reset after the modal opens**
    updateSelectizeInput(session, "new_restaurant_suggestions", selected = "")
  })
  
  # Auto-suggest restaurant names based on input
  
  # output$restaurant_suggestions <- renderUI({
  #   selectizeInput("new_restaurant_suggestions", "Select or Type a Restaurant", 
  #                  choices = unique(c(default_restaurants, data()$Restaurant)),  # Combine defaults + dataset
  #                  selected ="",
  #                  options = list(create = TRUE))
  # })
  
  # Save new rating
  observeEvent(input$save_rating, {
    selected_restaurant <- as.character(input$new_restaurant_suggestions)  # Ensure it's a character
    
    if (is.null(selected_restaurant) || selected_restaurant == "") {
      selected_restaurant <- "Unknown"  # Default value if empty
    }
    
    new_entry <- data.frame(
      Date = Sys.Date(),
      Restaurant = as.character(selected_restaurant),  
      Dish = as.character(input$new_dish),
      Ranking = input$new_ranking,
      Notes = as.character(input$new_notes),
      stringsAsFactors = FALSE
    )
    
    # Ensure the existing dataset is not empty and has the correct types
    existing_data <- data()
    if (nrow(existing_data) == 0) {
      existing_data <- data.frame(
        Date = as.Date(character()),
        Restaurant = character(),
        Dish = character(),
        Ranking = numeric(),
        Notes = character(),
        stringsAsFactors = FALSE
      )
    }
    
    # Convert existing data's Restaurant column to character before binding
    existing_data <- existing_data %>%
      mutate(Restaurant = as.character(Restaurant))
    
    # Ensure Date is formatted correctly before merging
    updated_data <- bind_rows(
      existing_data %>% mutate(Date = as.Date(Date)),  # Convert existing Date column
      new_entry
    )
    
    data(updated_data)  # Update reactive dataset
    
    # Save to CSV
    write.csv(updated_data, file_path, row.names = FALSE)
    
    # Close modal
    removeModal()
  })
  session$onSessionEnded(function() {
    stopApp()  # Stop the app when the browser tab is closed
  })
  
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
