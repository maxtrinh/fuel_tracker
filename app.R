library(shiny)
library(tidyverse)
library(lubridate)

# Constants
CSV_FILE <- "fuel.csv"
COL_TYPES <- cols(
    date = col_date(),
    .default = col_double()
)

# Function to initialize the CSV file
initialize_csv <- function() {
    if (!file.exists(CSV_FILE)) {
        df <- data.frame(matrix(ncol = 6, nrow = 0))
        colnames(df) <- c("date", "price_per_gallon", "gallons", "total", "mileage_in", "mpg")
        write_csv(df, CSV_FILE)
    }
}

# Function to add an entry to the CSV file
add_entry <- function(date, price_per_gallon, gallons, mileage_in) {
    df <- read_csv(CSV_FILE, col_types = COL_TYPES)
    
    # Calculate the total cost
    total <- round(as.numeric(price_per_gallon) * as.numeric(gallons), 2)
    
    # Calculate MPG
    if (nrow(df) > 0) {
        last_entry <- tail(df, 1)
        mileage_diff <- mileage_in - last_entry$mileage_in
        mpg <- ifelse(gallons != 0, round(mileage_diff / gallons, 3), 0)
    } else {
        mpg <- 0
    }
    
    new_entry <- data.frame(
        date = date,
        price_per_gallon = as.numeric(price_per_gallon),
        gallons = as.numeric(gallons),
        total = total,
        mileage_in = as.numeric(mileage_in),
        mpg = mpg
    )
    
    df <- bind_rows(df, new_entry)
    write_csv(df, CSV_FILE)
}

# Function to plot MPG and Price per Gallon over time
plot_mpg_chart <- function() {
    if (!file.exists(CSV_FILE)) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available (CSV file missing)", size = 6, color = "red") + theme_void())
    }
    
    df <- tryCatch({
        read_csv(CSV_FILE, col_types = COL_TYPES)
    }, error = function(e) {
        return(NULL)
    })
    
    if (is.null(df)) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available (CSV file empty)", size = 6, color = "red") + theme_void())
    }
    
    df <- df %>%
        arrange(date)
    
    if (all(is.na(df$mpg)) || all(is.na(df$price_per_gallon))) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data to plot", size = 6, color = "red") + theme_void())
    }
    
    p <- ggplot() +
        geom_line(data = df, aes(x = date, y = mpg, color = "MPG"), linewidth = 1) +
        geom_line(data = df, aes(x = date, y = price_per_gallon * 10, color = "Price per Gallon"), linetype = "dashed", linewidth = 1) +
        scale_y_continuous(
            name = "MPG",
            sec.axis = sec_axis(~./10, name = "Price per Gallon ($)")
        ) +
        labs(title = "MPG and Price per Gallon Over Time", x = "Date") +
        scale_color_manual(name = "Legend", values = c("MPG" = "blue", "Price per Gallon" = "green")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(p)
}

# Shiny UI
ui <- fluidPage(
    titlePanel("Fuel Data Tracker"),
    
    sidebarLayout(
        sidebarPanel(
            dateInput("date", "Date:", value = Sys.Date()),
            numericInput("price_per_gallon", "Price per Gallon ($):", value = 3.00, min = 0, step = 0.01),
            numericInput("gallons", "Gallons:", value = 10, min = 0, step = 0.1),
            numericInput("mileage_in", "Mileage In:", value = 100000, min = 0, step = 1),
            actionButton("add_entry", "Add Entry")
        ),
        
        mainPanel(
            plotOutput("mpgPlot")
        )
    )
)

# Shiny Server
server <- function(input, output, session) {
    # Initialize the CSV when the app starts
    initialize_csv()
    
    # Reactive value to track when data is updated
    reactive_data <- reactive({
        invalidateLater(5000, session)  # Refresh every 5 seconds
        read_csv(CSV_FILE, col_types = COL_TYPES)
    })
    
    observeEvent(input$add_entry, {
        # Add the entry when the button is clicked
        add_entry(input$date, input$price_per_gallon, input$gallons, input$mileage_in)
        showNotification("Entry added successfully", type = "message")
    })
    
    output$mpgPlot <- renderPlot({
        # Render the MPG and Price per Gallon chart
        df <- reactive_data()
        plot_mpg_chart()
    })
}

# Run the app
shinyApp(ui = ui, server = server)
