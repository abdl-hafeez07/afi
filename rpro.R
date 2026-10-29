# Install packages only if missing
packages <- c("shiny", "ggplot2", "plotly")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

# Load packages
library(shiny)
library(ggplot2)
library(plotly)
------------------------------------------------------------------------------------------
#2Student Analytics Dashboard
#Using a student dataset containing Student_ID, Name, Gender, Department,
#Semester, Mark, Attendance, create an interactive R Shiny dashboard with:
#1. Department and Semester filters
#2. Minimum Mark slider
#3. Total Students, Average Mark, and Average Attendance KPIs
#4. Filtered student data table
#5. Marks distribution chart
#6. Department-wise average marks chart
#7. Use reactive() to ensure that all KPIs, tables, and charts update automatically.


# ==========================================
# STUDENT ANALYTICS DASHBOARD
# ==========================================

# 1. Install required package
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

library(shiny)

# 2. Create sample student dataset
set.seed(123)

students <- data.frame(
  Student_ID = 1:100,
  Name = paste("Student", 1:100),
  Gender = sample(c("Male", "Female"), 100, replace = TRUE),
  Department = sample(
    c("Computer Science", "Data Analytics",
      "Commerce", "Mathematics"),
    100, replace = TRUE
  ),
  Semester = sample(1:6, 100, replace = TRUE),
  Mark = sample(35:100, 100, replace = TRUE),
  Attendance = sample(60:100, 100, replace = TRUE)
)

# 3. User Interface
ui <- fluidPage(
  
  titlePanel("Student Analytics Dashboard"),
  
  sidebarLayout(
    
    # Sidebar filters
    sidebarPanel(
      
      selectInput(
        "department",
        "Select Department:",
        choices = c("All", sort(unique(students$Department))),
        selected = "All"
      ),
      
      selectInput(
        "semester",
        "Select Semester:",
        choices = c("All", sort(unique(students$Semester))),
        selected = "All"
      ),
      
      sliderInput(
        "min_mark",
        "Minimum Mark:",
        min = 0,
        max = 100,
        value = 0
      )
    ),
    
    # Main dashboard
    mainPanel(
      
      h3("Key Performance Indicators"),
      
      fluidRow(
        column(
          4,
          wellPanel(
            h4("Total Students"),
            textOutput("total_students")
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("Average Mark"),
            textOutput("average_mark")
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("Average Attendance"),
            textOutput("average_attendance")
          )
        )
      ),
      
      hr(),
      
      h3("Filtered Student Data"),
      tableOutput("student_table"),
      
      hr(),
      
      h3("Marks Distribution"),
      plotOutput("marks_plot"),
      
      hr(),
      
      h3("Department-wise Average Marks"),
      plotOutput("department_plot")
    )
  )
)

# 4. Server Logic
server <- function(input, output) {
  
  # Reactive filtered dataset
  filtered_students <- reactive({
    
    data <- students
    
    if (input$department != "All") {
      data <- data[
        data$Department == input$department, ]
    }
    
    if (input$semester != "All") {
      data <- data[
        data$Semester == as.numeric(input$semester), ]
    }
    
    data <- data[
      data$Mark >= input$min_mark, ]
    
    data
  })
  
  # KPI 1: Total Students
  output$total_students <- renderText({
    nrow(filtered_students())
  })
  
  # KPI 2: Average Mark
  output$average_mark <- renderText({
    data <- filtered_students()
    
    if (nrow(data) == 0) {
      "No data"
    } else {
      round(mean(data$Mark, na.rm = TRUE), 2)
    }
  })
  
  # KPI 3: Average Attendance
  output$average_attendance <- renderText({
    data <- filtered_students()
    
    if (nrow(data) == 0) {
      "No data"
    } else {
      paste0(
        round(mean(data$Attendance, na.rm = TRUE), 2),
        "%"
      )
    }
  })
  
  # Filtered student table
  output$student_table <- renderTable({
    filtered_students()
  })
  
  # Marks distribution histogram
  output$marks_plot <- renderPlot({
    data <- filtered_students()
    
    validate(
      need(nrow(data) > 0, "No students match these filters.")
    )
    
    hist(
      data$Mark,
      breaks = 10,
      col = "skyblue",
      border = "white",
      main = "Distribution of Student Marks",
      xlab = "Marks",
      ylab = "Number of Students"
    )
  })
  
  # Department-wise average marks bar chart
  output$department_plot <- renderPlot({
    data <- filtered_students()
    
    validate(
      need(nrow(data) > 0, "No students match these filters.")
    )
    
    averages <- aggregate(
      Mark ~ Department,
      data = data,
      FUN = mean
    )
    
    barplot(
      averages$Mark,
      names.arg = averages$Department,
      col = "orange",
      border = "white",
      main = "Average Marks by Department",
      xlab = "Department",
      ylab = "Average Marks",
      ylim = c(0, 100),
      las = 2
    )
  })
  
}

# 5. Run Shiny Application
shinyApp(ui = ui, server = server)

------------------------------------------------------------------------------------------------------------------------------------------------------
  2. Car Analytics Dashboard using mtcars
Using the built-in R dataset mtcars, create an interactive Shiny dashboard that
provides:
  1. Cylinder selection filter
2. Minimum MPG slider
3. Number of Cars, Average MPG, and Average Horsepower KPIs
4. Filtered data table
5. MPG distribution histogram
6. Cylinder-wise average MPG bar chart
7. Use reactive() for filtering and ensure that all outputs update dynamically.


# ==========================================
# CAR ANALYTICS DASHBOARD USING MTCARS
# ==========================================

# 1. Install packages if missing
packages <- c("shiny", "ggplot2", "plotly")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

# 2. Load packages
library(shiny)
library(ggplot2)
library(plotly)

# 3. Load built-in dataset
data(mtcars)

# Add car names as a column
cars <- data.frame(
  Car = rownames(mtcars),
  mtcars,
  row.names = NULL
)

# 4. User Interface
ui <- fluidPage(
  
  titlePanel("Car Analytics Dashboard"),
  
  sidebarLayout(
    
    # Sidebar filters
    sidebarPanel(
      
      selectInput(
        "cyl",
        "Select Cylinders:",
        choices = c("All", sort(unique(cars$cyl))),
        selected = "All"
      ),
      
      sliderInput(
        "min_mpg",
        "Minimum MPG:",
        min = floor(min(cars$mpg)),
        max = ceiling(max(cars$mpg)),
        value = floor(min(cars$mpg)),
        step = 1
      )
    ),
    
    # Dashboard outputs
    mainPanel(
      
      h3("Key Performance Indicators"),
      
      fluidRow(
        column(
          4,
          wellPanel(
            h4("Number of Cars"),
            textOutput("total_cars")
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("Average MPG"),
            textOutput("avg_mpg")
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("Average Horsepower"),
            textOutput("avg_hp")
          )
        )
      ),
      
      hr(),
      
      h3("Filtered Car Data"),
      tableOutput("car_table"),
      
      hr(),
      
      h3("MPG Distribution Histogram"),
      plotOutput("mpg_hist"),
      
      hr(),
      
      h3("Cylinder-wise Average MPG"),
      plotlyOutput("cylinder_chart")
    )
  )
)

# 5. Server Logic
server <- function(input, output) {
  
  # Reactive filtering
  filtered_cars <- reactive({
    
    data <- cars
    
    # Filter by cylinder
    if (input$cyl != "All") {
      data <- data[
        data$cyl == as.numeric(input$cyl), ]
    }
    
    # Filter by minimum MPG
    data <- data[
      data$mpg >= input$min_mpg, ]
    
    data
  })
  
  # KPI 1: Number of Cars
  output$total_cars <- renderText({
    nrow(filtered_cars())
  })
  
  # KPI 2: Average MPG
  output$avg_mpg <- renderText({
    data <- filtered_cars()
    
    if (nrow(data) == 0) {
      "No data"
    } else {
      round(mean(data$mpg), 2)
    }
  })
  
  # KPI 3: Average Horsepower
  output$avg_hp <- renderText({
    data <- filtered_cars()
    
    if (nrow(data) == 0) {
      "No data"
    } else {
      round(mean(data$hp), 2)
    }
  })
  
  # Filtered data table
  output$car_table <- renderTable({
    filtered_cars()
  }, rownames = FALSE)
  
  # MPG distribution histogram using ggplot2
  output$mpg_hist <- renderPlot({
    
    data <- filtered_cars()
    
    validate(
      need(nrow(data) > 0, "No cars match the selected filters.")
    )
    
    ggplot(data, aes(x = mpg)) +
      geom_histogram(
        bins = 10,
        fill = "skyblue",
        color = "white"
      ) +
      labs(
        title = "Distribution of Miles Per Gallon",
        x = "Miles Per Gallon (MPG)",
        y = "Number of Cars"
      ) +
      theme_minimal()
  })
  
  # Cylinder-wise average MPG using ggplot2 and plotly
  output$cylinder_chart <- renderPlotly({
    
    data <- filtered_cars()
    
    validate(
      need(nrow(data) > 0, "No cars match the selected filters.")
    )
    
    averages <- aggregate(
      mpg ~ cyl,
      data = data,
      FUN = mean
    )
    
    p <- ggplot(
      averages,
      aes(
        x = factor(cyl),
        y = mpg,
        text = paste(
          "Cylinders:", cyl,
          "<br>Average MPG:", round(mpg, 2)
        )
      )
    ) +
      geom_col(fill = "orange") +
      labs(
        title = "Average MPG by Cylinder Count",
        x = "Number of Cylinders",
        y = "Average MPG"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
}

# 6. Run application
shinyApp(ui = ui, server = server)
----------------------------------------------------------------------------------------------------------------------------------------
  2. Iris Species Analytics Dashboard
Using the built-in R dataset iris, create an interactive Shiny application that
allows the user to select an iris species and displays:
  1. Number of observations
2. Average Sepal Length
3. Average Petal Length
4. Also provide a species-wise comparison chart and a filtered data table.
5. Ensure that the displayed values and visualizations update reactively when
the selected species changes.


# Install required packages if missing
packages <- c("shiny", "ggplot2")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(shiny)
library(ggplot2)

# Load built-in Iris dataset
data(iris)

# UI
ui <- fluidPage(
  
  titlePanel("Iris Species Analytics Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "species",
        "Select Iris Species:",
        choices = c("All", levels(iris$Species)),
        selected = "All"
      )
    ),
    
    mainPanel(
      h3("Key Performance Indicators"),
      
      fluidRow(
        column(
          4,
          wellPanel(
            h4("Number of Observations"),
            textOutput("total_obs")
          )
        ),
        column(
          4,
          wellPanel(
            h4("Average Sepal Length"),
            textOutput("avg_sepal")
          )
        ),
        column(
          4,
          wellPanel(
            h4("Average Petal Length"),
            textOutput("avg_petal")
          )
        )
      ),
      
      hr(),
      
      h3("Species-wise Comparison Chart"),
      plotOutput("species_chart"),
      
      hr(),
      
      h3("Filtered Iris Data"),
      tableOutput("iris_table")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtering
  filtered_iris <- reactive({
    if (input$species == "All") {
      iris
    } else {
      iris[iris$Species == input$species, ]
    }
  })
  
  # KPI 1: Number of observations
  output$total_obs <- renderText({
    nrow(filtered_iris())
  })
  
  # KPI 2: Average sepal length
  output$avg_sepal <- renderText({
    round(mean(filtered_iris()$Sepal.Length), 2)
  })
  
  # KPI 3: Average petal length
  output$avg_petal <- renderText({
    round(mean(filtered_iris()$Petal.Length), 2)
  })
  
  # Species-wise comparison chart
  output$species_chart <- renderPlot({
    
    # Calculate average measurements for each species
    avg_data <- aggregate(
      cbind(Sepal.Length, Petal.Length) ~ Species,
      data = filtered_iris(),
      FUN = mean
    )
    
    # Convert data to long format for plotting
    chart_data <- data.frame(
      Species = rep(avg_data$Species, 2),
      Measurement = c(
        avg_data$Sepal.Length,
        avg_data$Petal.Length
      ),
      Type = rep(
        c("Average Sepal Length", "Average Petal Length"),
        each = nrow(avg_data)
      )
    )
    
    ggplot(
      chart_data,
      aes(x = Species, y = Measurement, fill = Type)
    ) +
      geom_col(position = "dodge") +
      labs(
        title = "Average Sepal and Petal Length by Species",
        x = "Iris Species",
        y = "Average Length (cm)",
        fill = "Measurement"
      ) +
      theme_minimal()
  })
  
  # Filtered data table
  output$iris_table <- renderTable({
    filtered_iris()
  }, rownames = FALSE)
}

# Run the Shiny application
shinyApp(ui = ui, server = server)

-----------------------------------------------------------------------------------------------------------------------------------------------------------
  2. Product Sales Analytics Dashboard
Using a product sales dataset containing Product_ID, Product, Category, Region,
Units_Sold, Revenue, Profit, create an interactive Shiny dashboard with:
  1. Category and Region filters
2. Minimum Revenue slider
3. Total Products, Total Revenue, and Average Profit KPIs
4. Filtered product table
5. Revenue distribution chart
6. Category-wise average revenue chart
7. Implement the filtering using reactive() and ensure all dashboard
components update automatically.


# Install required packages if missing
packages <- c("shiny", "ggplot2")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(shiny)
library(ggplot2)

# Create sample product sales dataset
sales <- data.frame(
  Product_ID = 1:15,
  Product = c(
    "Laptop", "Phone", "Tablet", "Headphones", "Keyboard",
    "Monitor", "Mouse", "Printer", "Camera", "Speaker",
    "Smartwatch", "Router", "Webcam", "SSD", "Microphone"
  ),
  Category = c(
    "Electronics", "Electronics", "Electronics",
    "Accessories", "Accessories", "Electronics",
    "Accessories", "Electronics", "Electronics",
    "Accessories", "Electronics", "Electronics",
    "Accessories", "Electronics", "Accessories"
  ),
  Region = c(
    "North", "South", "East", "West", "North",
    "South", "East", "West", "North", "South",
    "East", "West", "North", "South", "East"
  ),
  Units_Sold = c(
    20, 50, 35, 80, 60, 25, 100, 15, 10, 45,
    30, 20, 70, 40, 55
  ),
  Revenue = c(
    800000, 600000, 350000, 120000, 90000,
    250000, 50000, 180000, 400000, 135000,
    300000, 100000, 140000, 220000, 110000
  ),
  Profit = c(
    120000, 90000, 52500, 24000, 18000,
    50000, 10000, 27000, 80000, 27000,
    60000, 20000, 28000, 44000, 22000
  )
)

# UI
ui <- fluidPage(
  
  titlePanel("Product Sales Analytics Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "category",
        "Select Category:",
        choices = c("All", sort(unique(sales$Category))),
        selected = "All"
      ),
      
      selectInput(
        "region",
        "Select Region:",
        choices = c("All", sort(unique(sales$Region))),
        selected = "All"
      ),
      
      sliderInput(
        "min_revenue",
        "Minimum Revenue:",
        min = 0,
        max = max(sales$Revenue),
        value = 0,
        step = 10000
      )
    ),
    
    mainPanel(
      h3("Key Performance Indicators"),
      
      fluidRow(
        column(
          4,
          wellPanel(
            h4("Total Products"),
            textOutput("total_products")
          )
        ),
        column(
          4,
          wellPanel(
            h4("Total Revenue"),
            textOutput("total_revenue")
          )
        ),
        column(
          4,
          wellPanel(
            h4("Average Profit"),
            textOutput("avg_profit")
          )
        )
      ),
      
      hr(),
      
      h3("Filtered Product Table"),
      tableOutput("product_table"),
      
      hr(),
      
      h3("Revenue Distribution"),
      plotOutput("revenue_chart"),
      
      hr(),
      
      h3("Category-wise Average Revenue"),
      plotOutput("category_chart")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtering
  filtered_sales <- reactive({
    data <- sales
    
    if (input$category != "All") {
      data <- data[data$Category == input$category, ]
    }
    
    if (input$region != "All") {
      data <- data[data$Region == input$region, ]
    }
    
    data <- data[data$Revenue >= input$min_revenue, ]
    
    data
  })
  
  # KPI 1: Total products
  output$total_products <- renderText({
    nrow(filtered_sales())
  })
  
  # KPI 2: Total revenue
  output$total_revenue <- renderText({
    data <- filtered_sales()
    if (nrow(data) == 0) {
      "₹0"
    } else {
      paste0("₹", format(
        sum(data$Revenue),
        big.mark = ",",
        scientific = FALSE
      ))
    }
  })
  
  # KPI 3: Average profit
  output$avg_profit <- renderText({
    data <- filtered_sales()
    if (nrow(data) == 0) {
      "₹0"
    } else {
      paste0("₹", format(
        round(mean(data$Profit), 2),
        big.mark = ",",
        scientific = FALSE
      ))
    }
  })
  
  # Filtered product table
  output$product_table <- renderTable({
    filtered_sales()
  }, rownames = FALSE)
  
  # Revenue distribution chart
  output$revenue_chart <- renderPlot({
    data <- filtered_sales()
    
    validate(need(
      nrow(data) > 0,
      "No products match the selected filters."
    ))
    
    ggplot(data, aes(x = Revenue)) +
      geom_histogram(
        bins = 10,
        fill = "steelblue",
        color = "white"
      ) +
      labs(
        title = "Distribution of Product Revenue",
        x = "Revenue (₹)",
        y = "Number of Products"
      ) +
      theme_minimal()
  })
  
  # Category-wise average revenue chart
  output$category_chart <- renderPlot({
    data <- filtered_sales()
    
    validate(need(
      nrow(data) > 0,
      "No products match the selected filters."
    ))
    
    avg_revenue <- aggregate(
      Revenue ~ Category,
      data = data,
      FUN = mean
    )
    
    ggplot(
      avg_revenue,
      aes(x = Category, y = Revenue)
    ) +
      geom_col(fill = "orange") +
      labs(
        title = "Average Revenue by Category",
        x = "Category",
        y = "Average Revenue (₹)"
      ) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

------------------------------------------------------------------------------------------------------------------------------------------
  1. Iris Dataset Dashboard
High priority
- Select species using selectInput()
- Display average sepal and petal measurements
- Create a scatter plot or bar chart
- Display a filtered table


# Install packages if missing
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(shiny)
library(ggplot2)

# Load built-in Iris dataset
data(iris)

# UI
ui <- fluidPage(
  
  titlePanel("Iris Dataset Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "species",
        "Select Species:",
        choices = c("All", as.character(unique(iris$Species))),
        selected = "All"
      )
    ),
    
    mainPanel(
      
      h3("Average Measurements"),
      
      wellPanel(
        textOutput("avg_sepal"),
        textOutput("avg_petal")
      ),
      
      hr(),
      
      h3("Sepal Length vs Petal Length"),
      plotOutput("scatter_plot"),
      
      hr(),
      
      h3("Filtered Iris Data"),
      tableOutput("iris_table")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtering
  filtered_data <- reactive({
    
    if (input$species == "All") {
      iris
    } else {
      iris[iris$Species == input$species, ]
    }
    
  })
  
  # Average Sepal Length
  output$avg_sepal <- renderText({
    
    paste(
      "Average Sepal Length:",
      round(mean(filtered_data()$Sepal.Length), 2),
      "cm"
    )
    
  })
  
  # Average Petal Length
  output$avg_petal <- renderText({
    
    paste(
      "Average Petal Length:",
      round(mean(filtered_data()$Petal.Length), 2),
      "cm"
    )
    
  })
  
  # Scatter Plot
  output$scatter_plot <- renderPlot({
    
    ggplot(
      filtered_data(),
      aes(
        x = Sepal.Length,
        y = Petal.Length,
        color = Species
      )
    ) +
      geom_point(size = 3) +
      labs(
        title = "Sepal Length vs Petal Length",
        x = "Sepal Length (cm)",
        y = "Petal Length (cm)",
        color = "Species"
      ) +
      theme_minimal()
    
  })
  
  # Filtered Data Table
  output$iris_table <- renderTable({
    
    filtered_data()
    
  }, rownames = FALSE)
  
}

# Run Shiny Application
shinyApp(ui = ui, server = server)

-----------------------------------------------------------------------------------------------------------------
  2. Car Performance Dashboard
High priority
- Use mtcars
- Filter by cylinders or transmission
- Display average MPG and horsepower
- Create a histogram and scatter plot


# Install packages if missing
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(shiny)
library(ggplot2)

# Load built-in dataset
data(mtcars)

# Add car names as a column
cars <- data.frame(
  Car = rownames(mtcars),
  mtcars,
  row.names = NULL
)

# Convert transmission values into readable labels
cars$Transmission <- ifelse(
  cars$am == 0, "Automatic", "Manual"
)

# UI
ui <- fluidPage(
  
  titlePanel("Car Performance Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "cyl",
        "Select Cylinders:",
        choices = c("All", sort(unique(cars$cyl))),
        selected = "All"
      ),
      
      selectInput(
        "trans",
        "Select Transmission:",
        choices = c("All", "Automatic", "Manual"),
        selected = "All"
      )
      
    ),
    
    mainPanel(
      
      h3("Performance Summary"),
      
      wellPanel(
        textOutput("avg_mpg"),
        textOutput("avg_hp")
      ),
      
      hr(),
      
      h3("MPG Distribution Histogram"),
      plotOutput("mpg_hist"),
      
      hr(),
      
      h3("MPG vs Horsepower Scatter Plot"),
      plotOutput("scatter_plot"),
      
      hr(),
      
      h3("Filtered Car Data"),
      tableOutput("car_table")
      
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtering
  filtered_cars <- reactive({
    
    data <- cars
    
    if (input$cyl != "All") {
      data <- data[data$cyl == as.numeric(input$cyl), ]
    }
    
    if (input$trans != "All") {
      data <- data[data$Transmission == input$trans, ]
    }
    
    data
  })
  
  # Average MPG
  output$avg_mpg <- renderText({
    
    data <- filtered_cars()
    
    if (nrow(data) == 0) {
      "Average MPG: No data"
    } else {
      paste(
        "Average MPG:",
        round(mean(data$mpg), 2)
      )
    }
    
  })
  
  # Average Horsepower
  output$avg_hp <- renderText({
    
    data <- filtered_cars()
    
    if (nrow(data) == 0) {
      "Average Horsepower: No data"
    } else {
      paste(
        "Average Horsepower:",
        round(mean(data$hp), 2)
      )
    }
    
  })
  
  # Histogram
  output$mpg_hist <- renderPlot({
    
    data <- filtered_cars()
    
    validate(need(
      nrow(data) > 0,
      "No cars match the selected filters."
    ))
    
    ggplot(data, aes(x = mpg)) +
      geom_histogram(
        bins = 10,
        fill = "skyblue",
        color = "white"
      ) +
      labs(
        title = "Distribution of Miles Per Gallon",
        x = "Miles Per Gallon (MPG)",
        y = "Number of Cars"
      ) +
      theme_minimal()
    
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlot({
    
    data <- filtered_cars()
    
    validate(need(
      nrow(data) > 0,
      "No cars match the selected filters."
    ))
    
    ggplot(
      data,
      aes(x = hp, y = mpg, color = Transmission)
    ) +
      geom_point(size = 3) +
      labs(
        title = "MPG vs Horsepower",
        x = "Horsepower",
        y = "Miles Per Gallon (MPG)",
        color = "Transmission"
      ) +
      theme_minimal()
    
  })
  
  # Filtered data table
  output$car_table <- renderTable({
    
    filtered_cars()
    
  }, rownames = FALSE)
  
}

# Run the application
shinyApp(ui = ui, server = server)

-------------------------------------------------------------------------------------------------------------------------------
  3. Weather Analytics Dashboard
- Use airquality
- Filter by month
- Display average temperature and ozone
- Create a line chart or histogram


# Install packages if missing
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(shiny)
library(ggplot2)

# Load built-in dataset
data(airquality)

# UI
ui <- fluidPage(
  
  titlePanel("Weather Analytics Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "month",
        "Select Month:",
        choices = c("All", sort(unique(airquality$Month))),
        selected = "All"
      )
    ),
    
    mainPanel(
      
      h3("Weather Summary"),
      
      wellPanel(
        textOutput("avg_temp"),
        textOutput("avg_ozone")
      ),
      
      hr(),
      
      h3("Temperature Line Chart"),
      plotOutput("temp_chart"),
      
      hr(),
      
      h3("Ozone Distribution Histogram"),
      plotOutput("ozone_hist"),
      
      hr(),
      
      h3("Filtered Weather Data"),
      tableOutput("weather_table")
      
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtering
  filtered_weather <- reactive({
    
    if (input$month == "All") {
      airquality
    } else {
      airquality[airquality$Month == as.numeric(input$month), ]
    }
    
  })
  
  # Average Temperature
  output$avg_temp <- renderText({
    
    data <- filtered_weather()
    
    paste(
      "Average Temperature:",
      round(mean(data$Temp, na.rm = TRUE), 2),
      "°F"
    )
    
  })
  
  # Average Ozone
  output$avg_ozone <- renderText({
    
    data <- filtered_weather()
    
    if (all(is.na(data$Ozone))) {
      "Average Ozone: No data available"
    } else {
      paste(
        "Average Ozone:",
        round(mean(data$Ozone, na.rm = TRUE), 2)
      )
    }
    
  })
  
  # Temperature Line Chart
  output$temp_chart <- renderPlot({
    
    data <- filtered_weather()
    
    validate(need(
      nrow(data) > 0,
      "No weather data available."
    ))
    
    ggplot(
      data,
      aes(x = Day, y = Temp)
    ) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(color = "red", size = 2) +
      labs(
        title = "Daily Temperature",
        x = "Day of Month",
        y = "Temperature (°F)"
      ) +
      theme_minimal()
    
  })
  
  # Ozone Histogram
  output$ozone_hist <- renderPlot({
    
    data <- filtered_weather()
    
    validate(need(
      any(!is.na(data$Ozone)),
      "No ozone measurements available for this selection."
    ))
    
    ggplot(
      data,
      aes(x = Ozone)
    ) +
      geom_histogram(
        bins = 10,
        fill = "skyblue",
        color = "white",
        na.rm = TRUE
      ) +
      labs(
        title = "Ozone Distribution",
        x = "Ozone Level",
        y = "Frequency"
      ) +
      theme_minimal()
    
  })
  
  # Filtered Data Table
  output$weather_table <- renderTable({
    
    filtered_weather()
    
  }, rownames = FALSE)
  
}

# Run the application
shinyApp(ui = ui, server = server)

-----------------------------------------------------------------------------------------------------------------------------
  5. Plant Growth Dashboard
- Use PlantGrowth
- Filter by treatment group
- Display average weight
- Create a box plot and summary table


# Install Shiny if missing
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(shiny)
library(ggplot2)

# Load built-in dataset
data(PlantGrowth)

# UI
ui <- fluidPage(
  
  titlePanel("Plant Growth Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "group",
        "Select Treatment Group:",
        choices = c("All", as.character(unique(PlantGrowth$group))),
        selected = "All"
      )
    ),
    
    mainPanel(
      
      h3("Average Plant Weight"),
      wellPanel(
        textOutput("avg_weight")
      ),
      
      hr(),
      
      h3("Plant Weight Box Plot"),
      plotOutput("weight_plot"),
      
      hr(),
      
      h3("Summary Table"),
      tableOutput("summary_table"),
      
      hr(),
      
      h3("Filtered Plant Growth Data"),
      tableOutput("plant_table")
      
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtering
  filtered_plants <- reactive({
    
    if (input$group == "All") {
      PlantGrowth
    } else {
      PlantGrowth[PlantGrowth$group == input$group, ]
    }
    
  })
  
  # Average plant weight
  output$avg_weight <- renderText({
    
    paste(
      "Average Weight:",
      round(mean(filtered_plants()$weight), 2)
    )
    
  })
  
  # Box plot
  output$weight_plot <- renderPlot({
    
    ggplot(
      filtered_plants(),
      aes(x = group, y = weight, fill = group)
    ) +
      geom_boxplot() +
      labs(
        title = "Plant Weight by Treatment Group",
        x = "Treatment Group",
        y = "Plant Weight"
      ) +
      theme_minimal()
    
  })
  
  # Summary table
  output$summary_table <- renderTable({
    
    data <- filtered_plants()
    
    data.frame(
      Group = unique(data$group),
      Observations = nrow(data),
      Average_Weight = round(mean(data$weight), 2),
      Minimum_Weight = min(data$weight),
      Maximum_Weight = max(data$weight),
      Standard_Deviation = round(sd(data$weight), 2)
    )
    
  }, rownames = FALSE)
  
  # Filtered data table
  output$plant_table <- renderTable({
    
    filtered_plants()
    
  }, rownames = FALSE)
  
}

# Run application
shinyApp(ui = ui, server = server)

-------------------------------------------------------------------------------------------------------------------------------------------
  Other Questions

Using the built-in mtcars dataset in R, perform an exploratory data analysis by creating the following interactive visualizations:
  
  a. Create an interactive scatter plot showing the relationship between wt and mpg, using cyl to distinguish the data points.

b. Create an interactive histogram to visualize the distribution of mpg.

c. Create an interactive bubble chart with:
  - X-axis: hp
- Y-axis: mpg
- Bubble size: disp
- Bubble colour: gear
d. Generate an interactive correlation heatmap for all numerical variables.

e. Write a brief interpretation of each visualization.

install.packages("plotly")
install.packages("dplyr")


# ============================================================
# QUESTION 1: EXPLORATORY DATA ANALYSIS USING MTCARS
# ============================================================

# 1. Load libraries and dataset
library(plotly)
library(dplyr)

data(mtcars)

cars <- mtcars %>%
  mutate(Car = rownames(mtcars))

# 2. Inspect the dataset
cat("First six records:\n")
print(head(cars))

cat("\nDataset structure:\n")
str(cars)

cat("\nSummary statistics:\n")
print(summary(cars))

cat("\nMissing values in each column:\n")
print(colSums(is.na(cars)))

cat("\nNumber of duplicate records:\n")
print(sum(duplicated(cars)))

# 3. Interactive scatter plot: Weight vs MPG
p1 <- plot_ly(
  data = cars,
  x = ~wt,
  y = ~mpg,
  color = ~factor(cyl),
  colors = c("blue", "red", "green"),
  type = "scatter",
  mode = "markers",
  text = ~Car,
  hovertemplate = paste(
    "Car: %{text}<br>",
    "Weight: %{x}<br>",
    "MPG: %{y}<br>",
    "<extra></extra>"
  )
) %>%
  layout(
    title = "Car Weight vs Fuel Efficiency",
    xaxis = list(title = "Weight (1000 lbs)"),
    yaxis = list(title = "Miles per Gallon (MPG)")
  )

print(p1)

# 4. Interactive histogram: MPG distribution
p2 <- plot_ly(
  data = cars,
  x = ~mpg,
  type = "histogram",
  nbinsx = 10,
  marker = list(color = "steelblue")
) %>%
  layout(
    title = "Distribution of MPG",
    xaxis = list(title = "Miles per Gallon"),
    yaxis = list(title = "Frequency")
  )

print(p2)

# 5. Interactive bubble chart
# X-axis = Horsepower
# Y-axis = MPG
# Bubble size = Displacement
# Bubble colour = Number of gears

p3 <- plot_ly(
  data = cars,
  x = ~hp,
  y = ~mpg,
  size = ~disp,
  color = ~factor(gear),
  colors = c("orange", "blue", "green"),
  type = "scatter",
  mode = "markers",
  sizes = c(8, 45),
  text = ~Car,
  hovertemplate = paste(
    "Car: %{text}<br>",
    "Horsepower: %{x}<br>",
    "MPG: %{y}<br>",
    "<extra></extra>"
  )
) %>%
  layout(
    title = "Horsepower vs MPG",
    xaxis = list(title = "Horsepower"),
    yaxis = list(title = "Miles per Gallon")
  )

print(p3)

# 6. Correlation heatmap for numerical variables
cor_matrix <- cor(mtcars, use = "complete.obs")

p4 <- plot_ly(
  x = colnames(cor_matrix),
  y = rownames(cor_matrix),
  z = cor_matrix,
  type = "heatmap",
  colorscale = list(
    list(0, "blue"),
    list(0.5, "white"),
    list(1, "red")
  ),
  zmin = -1,
  zmax = 1,
  hovertemplate = paste(
    "Variable 1: %{y}<br>",
    "Variable 2: %{x}<br>",
    "Correlation: %{z:.2f}",
    "<extra></extra>"
  )
) %>%
  layout(
    title = "Correlation Heatmap",
    xaxis = list(title = "Numerical Variables"),
    yaxis = list(title = "Numerical Variables")
  )

print(p4)

# 7. Calculate important statistics
cat("\nKEY STATISTICS\n")

cat("Total number of cars:", nrow(mtcars), "\n")
cat("Average MPG:", round(mean(mtcars$mpg), 2), "\n")
cat("Average weight:", round(mean(mtcars$wt), 2), "\n")
cat("Average horsepower:", round(mean(mtcars$hp), 2), "\n")

cat(
  "Correlation between weight and MPG:",
  round(cor(mtcars$wt, mtcars$mpg), 3),
  "\n"
)

# 8. Final interpretation
cat("\nINTERPRETATION\n")
cat("1. Scatter plot: Heavier cars generally have lower MPG.\n")
cat("2. Histogram: Shows the distribution and frequency of MPG values.\n")
cat("3. Bubble chart: Compares horsepower, MPG, displacement and gears.\n")
cat("4. Heatmap: Shows positive and negative correlations between variables.\n")

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
  Question 2: Employee Data Management Using R (30 marks)
Question: Create a dataset containing at least 30 employees with the attributes Employee_ID, Employee_Name, Department, Experience, Basic_Salary, HRA, Bonus, and Tax.

Perform the following tasks:
  1. Create the dataset in R.
2. Save it as EmployeeData.rds.
3. Read the dataset using readRDS().
4. Calculate Gross Salary and Net Salary.
5. Categorize employees into Low, Medium, and High Salary groups.
6. Display the top 10 highest-paid employees.
7. Export the final dataset as CSV and Excel files.


# ============================================================
# QUESTION 2: EMPLOYEE DATA MANAGEMENT
# ============================================================

library(writexl)

# 1. Create a dataset containing 30 employees
set.seed(123)

n <- 30

EmployeeData <- data.frame(
  Employee_ID = 1001:1030,
  
  Employee_Name = paste0("Employee_", 1:n),
  
  Department = sample(
    c("IT", "HR", "Finance", "Marketing", "Sales"),
    n, replace = TRUE
  ),
  
  Experience = sample(1:20, n, replace = TRUE),
  
  Basic_Salary = sample(
    seq(20000, 80000, by = 1000),
    n, replace = TRUE
  ),
  
  HRA = sample(
    seq(3000, 15000, by = 500),
    n, replace = TRUE
  ),
  
  Bonus = sample(
    seq(1000, 10000, by = 500),
    n, replace = TRUE
  ),
  
  Tax = sample(
    seq(1000, 8000, by = 500),
    n, replace = TRUE
  )
)

# Display the dataset
cat("EMPLOYEE DATASET\n")
print(EmployeeData)

# 2. Save the dataset as an RDS file
saveRDS(EmployeeData, "EmployeeData.rds")

cat("\nDataset saved as EmployeeData.rds\n")

# 3. Read the dataset using readRDS()
EmployeeData <- readRDS("EmployeeData.rds")

cat("\nDATASET LOADED FROM RDS\n")
print(head(EmployeeData))

# 4. Calculate Gross Salary and Net Salary
# Gross Salary = Basic Salary + HRA + Bonus
# Net Salary = Gross Salary - Tax

EmployeeData$Gross_Salary <-
  EmployeeData$Basic_Salary +
  EmployeeData$HRA +
  EmployeeData$Bonus

EmployeeData$Net_Salary <-
  EmployeeData$Gross_Salary -
  EmployeeData$Tax

# 5. Categorize employees by Net Salary
EmployeeData$Salary_Category <- cut(
  EmployeeData$Net_Salary,
  breaks = c(-Inf, 40000, 70000, Inf),
  labels = c("Low", "Medium", "High"),
  right = TRUE
)

cat("\nEMPLOYEE DATA WITH SALARY CALCULATIONS\n")
print(EmployeeData)

# 6. Display the top 10 highest-paid employees
Top_10_Employees <- EmployeeData[
  order(-EmployeeData$Net_Salary),
  c(
    "Employee_ID",
    "Employee_Name",
    "Department",
    "Gross_Salary",
    "Net_Salary",
    "Salary_Category"
  )
]

Top_10_Employees <- head(Top_10_Employees, 10)

cat("\nTOP 10 HIGHEST-PAID EMPLOYEES\n")
print(Top_10_Employees)

# 7. Export the final dataset as CSV and Excel
write.csv(
  EmployeeData,
  "EmployeeData_Final.csv",
  row.names = FALSE
)

write_xlsx(
  EmployeeData,
  "EmployeeData_Final.xlsx"
)

cat("\nFiles exported successfully:\n")
cat("1. EmployeeData.rds\n")
cat("2. EmployeeData_Final.csv\n")
cat("3. EmployeeData_Final.xlsx\n")

# 8. Validate the final dataset
cat("\nDATA VALIDATION\n")

cat("Number of employees:", nrow(EmployeeData), "\n")
cat("Number of columns:", ncol(EmployeeData), "\n")
cat("Missing values:", sum(is.na(EmployeeData)), "\n")
cat(
  "Duplicate employee IDs:",
  sum(duplicated(EmployeeData$Employee_ID)),
  "\n"
)

cat("\nAverage Net Salary:",
    round(mean(EmployeeData$Net_Salary), 2), "\n")

cat("\nSalary Category Counts:\n")
print(table(EmployeeData$Salary_Category))

------------------------------------------------------------------------------------------------------------------------------------
  Question 3: Complete Data Preprocessing Using the Titanic Dataset (30 marks)
Question from your CAE1 paper — Set 1, Section B

Perform a complete data preprocessing workflow on the Titanic dataset. Your solution must include:
  1. Import and inspect the dataset.
2. Identify and correct data quality issues.
3. Handle missing values, duplicate records, inconsistent values and incorrect data types.
4. Clean text and categorical variables.
5. Detect and treat outliers.
6. Scale numerical variables.
7. Create at least four meaningful derived features.
8. Select the final variables required for analysis.
9. Validate the cleaned dataset.
10. Export the processed dataset as a CSV file.


# ============================================================
# QUESTION 3: TITANIC DATA PREPROCESSING
# ============================================================

library(dplyr)
library(stringr)

# 1. IMPORT THE DATASET
# Select the Titanic CSV file when prompted

Titanic <- read.csv(
  file.choose(),
  stringsAsFactors = FALSE
)

# 2. INSPECT THE DATASET

cat("First six records:\n")
print(head(Titanic))

cat("\nDataset structure:\n")
str(Titanic)

cat("\nSummary statistics:\n")
print(summary(Titanic))

cat("\nDataset dimensions:\n")
print(dim(Titanic))

cat("\nMissing values by column:\n")
print(colSums(is.na(Titanic)))

cat("\nDuplicate records:\n")
print(sum(duplicated(Titanic)))

# 3. REMOVE DUPLICATE RECORDS

Titanic <- distinct(Titanic)

# Remove duplicate PassengerId values if that column exists
if ("PassengerId" %in% names(Titanic)) {
  Titanic <- Titanic %>%
    distinct(PassengerId, .keep_all = TRUE)
}

# 4. CLEAN COLUMN NAMES

names(Titanic) <- make.names(names(Titanic))

# 5. CLEAN TEXT AND CATEGORICAL VARIABLES

text_columns <- names(Titanic)[
  sapply(Titanic, is.character)
]

for (col in text_columns) {
  Titanic[[col]] <- str_squish(Titanic[[col]])
  Titanic[[col]][Titanic[[col]] == ""] <- NA
}

# Standardize common Titanic categorical values
if ("Sex" %in% names(Titanic)) {
  Titanic$Sex <- tolower(Titanic$Sex)
  
  Titanic$Sex[Titanic$Sex %in% c("m", "male")] <- "male"
  Titanic$Sex[Titanic$Sex %in% c("f", "female")] <- "female"
}

if ("Embarked" %in% names(Titanic)) {
  Titanic$Embarked <- toupper(Titanic$Embarked)
}

# 6. CORRECT NUMERICAL DATA TYPES

numeric_columns <- intersect(
  c("Survived", "Pclass", "Age", "SibSp",
    "Parch", "Fare", "PassengerId"),
  names(Titanic)
)

for (col in numeric_columns) {
  Titanic[[col]] <- suppressWarnings(
    as.numeric(Titanic[[col]])
  )
}

# 7. HANDLE MISSING NUMERICAL VALUES
# Median imputation is less sensitive to extreme values

median_columns <- intersect(
  c("Age", "Fare", "SibSp", "Parch"),
  names(Titanic)
)

for (col in median_columns) {
  med <- median(Titanic[[col]], na.rm = TRUE)
  
  if (is.finite(med)) {
    Titanic[[col]][is.na(Titanic[[col]])] <- med
  }
}

# 8. HANDLE MISSING CATEGORICAL VALUES

impute_mode <- function(x) {
  observed <- x[!is.na(x)]
  
  if (length(observed) == 0) {
    return(x)
  }
  
  mode_value <- names(which.max(table(observed)))
  x[is.na(x)] <- mode_value
  x
}

categorical_columns <- intersect(
  c("Sex", "Embarked"),
  names(Titanic)
)

for (col in categorical_columns) {
  Titanic[[col]] <- impute_mode(Titanic[[col]])
}

# Cabin has many missing values in common Titanic datasets.
# Preserve missingness as an explicit category.
if ("Cabin" %in% names(Titanic)) {
  Titanic$Cabin[is.na(Titanic$Cabin)] <- "Unknown"
}

# Remove rows where the target variable is missing
if ("Survived" %in% names(Titanic)) {
  Titanic <- Titanic[!is.na(Titanic$Survived), ]
}

# 9. DETECT AND TREAT OUTLIERS USING IQR CAPPING

cap_outliers <- function(x) {
  if (!is.numeric(x) || all(is.na(x))) {
    return(x)
  }
  
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value
  
  x[x < lower] <- lower
  x[x > upper] <- upper
  
  x
}

outlier_columns <- intersect(
  c("Age", "Fare"),
  names(Titanic)
)

for (col in outlier_columns) {
  Titanic[[col]] <- cap_outliers(Titanic[[col]])
}

# 10. CREATE DERIVED FEATURES

# Feature 1: Family size
if (all(c("SibSp", "Parch") %in% names(Titanic))) {
  Titanic$Family_Size <-
    Titanic$SibSp + Titanic$Parch + 1
}

# Feature 2: Whether the passenger travelled alone
if ("Family_Size" %in% names(Titanic)) {
  Titanic$Is_Alone <- ifelse(
    Titanic$Family_Size == 1, "Yes", "No"
  )
}

# Feature 3: Age category
if ("Age" %in% names(Titanic)) {
  Titanic$Age_Group <- cut(
    Titanic$Age,
    breaks = c(-Inf, 12, 18, 35, 60, Inf),
    labels = c(
      "Child", "Teenager", "Young Adult",
      "Adult", "Senior"
    )
  )
}

# Feature 4: Extract passenger title from name
if ("Name" %in% names(Titanic)) {
  Titanic$Title <- str_extract(
    Titanic$Name,
    "(?<=, )[^.]+"
  )
  
  Titanic$Title <- str_squish(Titanic$Title)
  Titanic$Title[is.na(Titanic$Title)] <- "Unknown"
}

# Feature 5: Cabin availability
if ("Cabin" %in% names(Titanic)) {
  Titanic$Has_Cabin <- ifelse(
    Titanic$Cabin == "Unknown", "No", "Yes"
  )
}

# Feature 6: Fare per family member
if (all(c("Fare", "Family_Size") %in% names(Titanic))) {
  Titanic$Fare_Per_Person <-
    Titanic$Fare / Titanic$Family_Size
}

# Feature 7: Survival status
if ("Survived" %in% names(Titanic)) {
  Titanic$Survival_Status <- ifelse(
    Titanic$Survived == 1,
    "Survived",
    "Did Not Survive"
  )
}

# 11. SCALE NUMERICAL VARIABLES USING Z-SCORES

scale_columns <- intersect(
  c("Age", "Fare", "Family_Size"),
  names(Titanic)
)

for (col in scale_columns) {
  x <- Titanic[[col]]
  s <- sd(x, na.rm = TRUE)
  
  if (is.finite(s) && s > 0) {
    Titanic[[paste0(col, "_Scaled")]] <-
      as.numeric(scale(x))
  }
}

# 12. CONVERT CATEGORICAL VARIABLES TO FACTORS

factor_columns <- intersect(
  c("Sex", "Embarked", "Pclass", "Is_Alone",
    "Age_Group", "Title", "Survival_Status"),
  names(Titanic)
)

for (col in factor_columns) {
  Titanic[[col]] <- as.factor(Titanic[[col]])
}

# 13. SELECT FINAL VARIABLES

desired_columns <- c(
  "PassengerId", "Survived", "Survival_Status",
  "Pclass", "Name", "Sex", "Age", "Age_Group",
  "SibSp", "Parch", "Family_Size", "Is_Alone",
  "Fare", "Fare_Per_Person", "Embarked",
  "Cabin", "Has_Cabin", "Title",
  "Age_Scaled", "Fare_Scaled", "Family_Size_Scaled"
)

final_columns <- intersect(
  desired_columns,
  names(Titanic)
)

Titanic_Cleaned <- Titanic[, final_columns, drop = FALSE]

# 14. VALIDATE THE CLEANED DATASET

cat("\nCLEANED DATASET:\n")
print(head(Titanic_Cleaned))

cat("\nFinal dataset dimensions:\n")
print(dim(Titanic_Cleaned))

cat("\nMissing values after preprocessing:\n")
print(colSums(is.na(Titanic_Cleaned)))

cat("\nDuplicate records after preprocessing:\n")
print(sum(duplicated(Titanic_Cleaned)))

cat("\nFinal data structure:\n")
str(Titanic_Cleaned)

# 15. EXPORT THE CLEANED DATASET

write.csv(
  Titanic_Cleaned,
  "Titanic_Cleaned.csv",
  row.names = FALSE,
  na = ""
)

cat("\nPreprocessing completed successfully!\n")
cat("Output file: Titanic_Cleaned.csv\n")
--------------------------------------------------------------------------------------------------------------------------------------
  Question 4: Complete Data Preprocessing Using the Google Play Store Dataset (30 marks)
Question from your CAE1 paper — Set 1, Section B

Perform a complete data preprocessing workflow on the Google Play Store dataset. Your solution must include:
  1. Import and inspect the dataset.
2. Identify and correct data quality issues.
3. Handle missing values, duplicate records, inconsistent values and incorrect data types.
4. Clean text and categorical variables.
5. Detect and treat outliers.
6. Scale numerical variables.
7. Create at least four meaningful derived features.
8. Select the final variables required for analysis.
9. Validate the cleaned dataset.
10. Export the processed dataset as a CSV file.


# ============================================================
# QUESTION 4: GOOGLE PLAY STORE DATA PREPROCESSING
# ============================================================

library(dplyr)
library(stringr)

# 1. IMPORT DATASET
PlayStore <- read.csv(
  file.choose(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# 2. INSPECT DATASET
cat("First six records:\n")
print(head(PlayStore))

cat("\nStructure:\n")
str(PlayStore)

cat("\nSummary:\n")
print(summary(PlayStore))

cat("\nDimensions:\n")
print(dim(PlayStore))

cat("\nMissing values:\n")
print(colSums(is.na(PlayStore)))

cat("\nDuplicate records:\n")
print(sum(duplicated(PlayStore)))

# 3. STANDARDIZE COLUMN NAMES
names(PlayStore) <- make.names(
  names(PlayStore),
  unique = TRUE
)

# 4. REMOVE DUPLICATES
PlayStore <- distinct(PlayStore)

# Remove duplicate app names, keeping the first record
if ("App" %in% names(PlayStore)) {
  PlayStore$App <- str_squish(PlayStore$App)
  PlayStore <- PlayStore[
    !duplicated(tolower(PlayStore$App)),
  ]
}

# 5. CLEAN TEXT AND CATEGORICAL VARIABLES
text_columns <- names(PlayStore)[
  sapply(PlayStore, is.character)
]

for (col in text_columns) {
  PlayStore[[col]] <- str_squish(PlayStore[[col]])
  PlayStore[[col]][PlayStore[[col]] == ""] <- NA
}

# Standardize categorical text
category_columns <- intersect(
  c("Category", "Type", "Content.Rating", "Genres"),
  names(PlayStore)
)

for (col in category_columns) {
  PlayStore[[col]] <- str_to_title(
    PlayStore[[col]]
  )
}

# 6. CLEAN NUMERICAL VARIABLES

# Rating
if ("Rating" %in% names(PlayStore)) {
  PlayStore$Rating <- suppressWarnings(
    as.numeric(PlayStore$Rating)
  )
  
  # Ratings should range from 1 to 5
  PlayStore$Rating[
    PlayStore$Rating < 1 |
      PlayStore$Rating > 5
  ] <- NA
}

# Reviews: convert character counts to numeric
if ("Reviews" %in% names(PlayStore)) {
  PlayStore$Reviews <- suppressWarnings(
    as.numeric(gsub(",", "", PlayStore$Reviews))
  )
}

# Size: convert K and M values into megabytes
if ("Size" %in% names(PlayStore)) {
  size_text <- toupper(
    str_squish(PlayStore$Size)
  )
  
  size_text[size_text %in% c("VARIES WITH DEVICE", "NA")] <- NA
  
  size_number <- suppressWarnings(
    as.numeric(str_extract(size_text, "[0-9.]+"))
  )
  
  is_kb <- str_detect(size_text, "K")
  is_mb <- str_detect(size_text, "M")
  
  size_number[is_kb] <- size_number[is_kb] / 1024
  size_number[!is_kb & !is_mb] <- NA
  
  PlayStore$Size_MB <- size_number
}

# Installs: remove plus signs and commas
if ("Installs" %in% names(PlayStore)) {
  PlayStore$Installs <- suppressWarnings(
    as.numeric(
      gsub("[,+]", "", PlayStore$Installs)
    )
  )
}

# Price: remove currency symbols
if ("Price" %in% names(PlayStore)) {
  PlayStore$Price <- suppressWarnings(
    as.numeric(
      gsub("[^0-9.]", "", PlayStore$Price)
    )
  )
}

# 7. HANDLE MISSING VALUES

# Median imputation for selected numerical variables
numeric_columns <- intersect(
  c("Rating", "Reviews", "Installs", "Price", "Size_MB"),
  names(PlayStore)
)

for (col in numeric_columns) {
  med <- median(PlayStore[[col]], na.rm = TRUE)
  
  if (is.finite(med)) {
    PlayStore[[col]][is.na(PlayStore[[col]])] <- med
  }
}

# Mode imputation for categorical variables
impute_mode <- function(x) {
  observed <- x[!is.na(x)]
  
  if (length(observed) == 0) {
    return(x)
  }
  
  most_common <- names(which.max(table(observed)))
  x[is.na(x)] <- most_common
  x
}

categorical_columns <- intersect(
  c("Category", "Type", "Content.Rating", "Genres"),
  names(PlayStore)
)

for (col in categorical_columns) {
  PlayStore[[col]] <- impute_mode(PlayStore[[col]])
}

# 8. DETECT AND TREAT OUTLIERS USING IQR CAPPING
cap_outliers <- function(x) {
  if (!is.numeric(x) || all(is.na(x))) {
    return(x)
  }
  
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value
  
  x[x < lower] <- lower
  x[x > upper] <- upper
  
  x
}

outlier_columns <- intersect(
  c("Reviews", "Size_MB", "Installs", "Price"),
  names(PlayStore)
)

for (col in outlier_columns) {
  PlayStore[[col]] <- cap_outliers(PlayStore[[col]])
}

# 9. CREATE DERIVED FEATURES

# Feature 1: Free or paid app
if ("Price" %in% names(PlayStore)) {
  PlayStore$App_Type <- ifelse(
    PlayStore$Price == 0, "Free", "Paid"
  )
}

# Feature 2: Popularity category based on installs
if ("Installs" %in% names(PlayStore)) {
  PlayStore$Popularity <- cut(
    PlayStore$Installs,
    breaks = c(-Inf, 10000, 1000000, Inf),
    labels = c("Low", "Medium", "High")
  )
}

# Feature 3: Rating category
if ("Rating" %in% names(PlayStore)) {
  PlayStore$Rating_Category <- cut(
    PlayStore$Rating,
    breaks = c(-Inf, 2.5, 3.5, 4.2, Inf),
    labels = c("Poor", "Average", "Good", "Excellent")
  )
}

# Feature 4: Log-transformed reviews
if ("Reviews" %in% names(PlayStore)) {
  PlayStore$Log_Reviews <- log1p(PlayStore$Reviews)
}

# Feature 5: Log-transformed installs
if ("Installs" %in% names(PlayStore)) {
  PlayStore$Log_Installs <- log1p(PlayStore$Installs)
}

# Feature 6: Reviews per installation
if (all(c("Reviews", "Installs") %in% names(PlayStore))) {
  PlayStore$Review_Install_Ratio <- ifelse(
    PlayStore$Installs > 0,
    PlayStore$Reviews / PlayStore$Installs,
    0
  )
}

# 10. SCALE NUMERICAL VARIABLES
scale_columns <- intersect(
  c("Rating", "Reviews", "Installs", "Price", "Size_MB"),
  names(PlayStore)
)

for (col in scale_columns) {
  x <- PlayStore[[col]]
  s <- sd(x, na.rm = TRUE)
  
  if (is.finite(s) && s > 0) {
    PlayStore[[paste0(col, "_Scaled")]] <-
      as.numeric(scale(x))
  }
}

# 11. CONVERT CATEGORICAL VARIABLES TO FACTORS
factor_columns <- intersect(
  c("Category", "Type", "Content.Rating", "Genres",
    "App_Type", "Popularity", "Rating_Category"),
  names(PlayStore)
)

for (col in factor_columns) {
  PlayStore[[col]] <- as.factor(PlayStore[[col]])
}

# 12. SELECT FINAL VARIABLES
desired_columns <- c(
  "App", "Category", "Rating", "Reviews",
  "Size_MB", "Installs", "Type", "Price",
  "Content.Rating", "Genres", "App_Type",
  "Popularity", "Rating_Category", "Log_Reviews",
  "Log_Installs", "Review_Install_Ratio",
  "Rating_Scaled", "Reviews_Scaled",
  "Installs_Scaled", "Price_Scaled", "Size_MB_Scaled"
)

final_columns <- intersect(
  desired_columns,
  names(PlayStore)
)

PlayStore_Cleaned <- PlayStore[
  , final_columns, drop = FALSE
]

# 13. VALIDATE CLEANED DATA
cat("\nCLEANED DATASET:\n")
print(head(PlayStore_Cleaned))

cat("\nFinal dimensions:\n")
print(dim(PlayStore_Cleaned))

cat("\nMissing values:\n")
print(colSums(is.na(PlayStore_Cleaned)))

cat("\nDuplicate rows:\n")
print(sum(duplicated(PlayStore_Cleaned)))

cat("\nFinal data structure:\n")
str(PlayStore_Cleaned)

# 14. EXPORT TO CSV
write.csv(
  PlayStore_Cleaned,
  "GooglePlayStore_Cleaned.csv",
  row.names = FALSE,
  na = ""
)

cat("\nPreprocessing completed successfully!\n")
cat("Output: GooglePlayStore_Cleaned.csv\n")

------------------------------------------------------------------------------------------------------------------------
  Question 5: Employee Attrition Data Preprocessing (30 marks)
Question from your CAE1 paper — Set 2, Section B

Perform a complete data preprocessing workflow on the Employee_Attrition dataset. Include importing and inspecting data, handling missing values and duplicates, correcting data types, cleaning categorical variables, detecting and treating outliers, scaling numerical variables, creating at least four derived features, selecting final variables, validating the dataset, and exporting the cleaned data as CSV.


# ============================================================
# QUESTION 5: EMPLOYEE ATTRITION DATA PREPROCESSING
# ============================================================

library(dplyr)
library(stringr)

# 1. IMPORT DATASET
Employee <- read.csv(
  file.choose(),
  stringsAsFactors = FALSE
)

# 2. INSPECT DATA
cat("First six records:\n")
print(head(Employee))

cat("\nStructure:\n")
str(Employee)

cat("\nSummary:\n")
print(summary(Employee))

cat("\nDimensions:\n")
print(dim(Employee))

cat("\nMissing values:\n")
print(colSums(is.na(Employee)))

cat("\nDuplicate rows:\n")
print(sum(duplicated(Employee)))

# 3. STANDARDIZE COLUMN NAMES
names(Employee) <- make.names(
  names(Employee),
  unique = TRUE
)

# 4. REMOVE DUPLICATE RECORDS
Employee <- distinct(Employee)

# 5. CLEAN TEXT AND CATEGORICAL VALUES
text_columns <- names(Employee)[
  sapply(Employee, is.character)
]

for (col in text_columns) {
  Employee[[col]] <- str_squish(Employee[[col]])
  Employee[[col]][Employee[[col]] == ""] <- NA
}

# Standardize categorical text
categorical_columns <- intersect(
  c("Attrition", "BusinessTravel", "Department",
    "EducationField", "Gender", "JobRole",
    "MaritalStatus", "OverTime"),
  names(Employee)
)

for (col in categorical_columns) {
  Employee[[col]] <- str_to_title(Employee[[col]])
}

# 6. CONVERT NUMERICAL COLUMNS
numeric_columns <- intersect(
  c("Age", "DailyRate", "DistanceFromHome",
    "Education", "EnvironmentSatisfaction",
    "HourlyRate", "JobInvolvement", "JobLevel",
    "JobSatisfaction", "MonthlyIncome",
    "MonthlyRate", "NumCompaniesWorked",
    "PercentSalaryHike", "PerformanceRating",
    "RelationshipSatisfaction", "StockOptionLevel",
    "TotalWorkingYears", "TrainingTimesLastYear",
    "WorkLifeBalance", "YearsAtCompany",
    "YearsInCurrentRole", "YearsSinceLastPromotion",
    "YearsWithCurrManager"),
  names(Employee)
)

for (col in numeric_columns) {
  Employee[[col]] <- suppressWarnings(
    as.numeric(Employee[[col]])
  )
}

# 7. HANDLE MISSING NUMERICAL VALUES
for (col in numeric_columns) {
  med <- median(Employee[[col]], na.rm = TRUE)
  
  if (is.finite(med)) {
    Employee[[col]][is.na(Employee[[col]])] <- med
  }
}

# 8. HANDLE MISSING CATEGORICAL VALUES
impute_mode <- function(x) {
  observed <- x[!is.na(x)]
  
  if (length(observed) == 0) {
    return(x)
  }
  
  mode_value <- names(which.max(table(observed)))
  x[is.na(x)] <- mode_value
  x
}

for (col in categorical_columns) {
  Employee[[col]] <- impute_mode(Employee[[col]])
}

# 9. DETECT AND TREAT OUTLIERS USING IQR CAPPING
cap_outliers <- function(x) {
  if (!is.numeric(x) || all(is.na(x))) {
    return(x)
  }
  
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value
  
  x[x < lower] <- lower
  x[x > upper] <- upper
  
  x
}

outlier_columns <- intersect(
  c("Age", "MonthlyIncome", "DistanceFromHome",
    "TotalWorkingYears", "YearsAtCompany"),
  names(Employee)
)

for (col in outlier_columns) {
  Employee[[col]] <- cap_outliers(Employee[[col]])
}

# 10. CREATE DERIVED FEATURES

# Feature 1: Age group
if ("Age" %in% names(Employee)) {
  Employee$Age_Group <- cut(
    Employee$Age,
    breaks = c(-Inf, 25, 35, 45, 55, Inf),
    labels = c(
      "Young", "Early Career", "Mid Career",
      "Senior", "Late Career"
    )
  )
}

# Feature 2: Income category
if ("MonthlyIncome" %in% names(Employee)) {
  Employee$Income_Category <- cut(
    Employee$MonthlyIncome,
    breaks = c(-Inf, 3000, 7000, Inf),
    labels = c("Low", "Medium", "High")
  )
}

# Feature 3: Years at company compared with total experience
if (all(c("YearsAtCompany", "TotalWorkingYears") %in%
        names(Employee))) {
  Employee$Company_Experience_Ratio <- ifelse(
    Employee$TotalWorkingYears > 0,
    Employee$YearsAtCompany /
      Employee$TotalWorkingYears,
    0
  )
}

# Feature 4: Number of years in current role
if ("YearsInCurrentRole" %in% names(Employee)) {
  Employee$Role_Tenure_Group <- cut(
    Employee$YearsInCurrentRole,
    breaks = c(-Inf, 2, 5, Inf),
    labels = c("Short", "Medium", "Long")
  )
}

# Feature 5: Overtime indicator
if ("OverTime" %in% names(Employee)) {
  Employee$Overtime_Flag <- ifelse(
    Employee$OverTime == "Yes", 1, 0
  )
}

# Feature 6: Attrition indicator
if ("Attrition" %in% names(Employee)) {
  Employee$Attrition_Flag <- ifelse(
    Employee$Attrition == "Yes", 1, 0
  )
}

# 11. SCALE SELECTED NUMERICAL VARIABLES
scale_columns <- intersect(
  c("Age", "MonthlyIncome", "DistanceFromHome",
    "YearsAtCompany", "TotalWorkingYears"),
  names(Employee)
)

for (col in scale_columns) {
  x <- Employee[[col]]
  s <- sd(x, na.rm = TRUE)
  
  if (is.finite(s) && s > 0) {
    Employee[[paste0(col, "_Scaled")]] <-
      as.numeric(scale(x))
  }
}

# 12. CONVERT CATEGORICAL VARIABLES TO FACTORS
factor_columns <- intersect(
  c("Attrition", "Department", "Gender", "JobRole",
    "MaritalStatus", "Age_Group", "Income_Category",
    "Role_Tenure_Group"),
  names(Employee)
)

for (col in factor_columns) {
  Employee[[col]] <- as.factor(Employee[[col]])
}

# 13. SELECT FINAL VARIABLES
desired_columns <- c(
  "Age", "Gender", "Attrition", "Attrition_Flag",
  "Department", "JobRole", "MaritalStatus",
  "MonthlyIncome", "Income_Category",
  "DistanceFromHome", "TotalWorkingYears",
  "YearsAtCompany", "YearsInCurrentRole",
  "Age_Group", "Company_Experience_Ratio",
  "Role_Tenure_Group", "OverTime", "Overtime_Flag",
  "Age_Scaled", "MonthlyIncome_Scaled",
  "DistanceFromHome_Scaled",
  "YearsAtCompany_Scaled",
  "TotalWorkingYears_Scaled"
)

final_columns <- intersect(
  desired_columns,
  names(Employee)
)

Employee_Cleaned <- Employee[
  , final_columns, drop = FALSE
]

# 14. VALIDATE THE CLEANED DATA
cat("\nCLEANED DATASET:\n")
print(head(Employee_Cleaned))

cat("\nFinal dimensions:\n")
print(dim(Employee_Cleaned))

cat("\nMissing values:\n")
print(colSums(is.na(Employee_Cleaned)))

cat("\nDuplicate rows:\n")
print(sum(duplicated(Employee_Cleaned)))

cat("\nFinal structure:\n")
str(Employee_Cleaned)

if ("Attrition" %in% names(Employee_Cleaned)) {
  cat("\nAttrition distribution:\n")
  print(table(Employee_Cleaned$Attrition))
}

# 15. EXPORT THE CLEANED DATASET
write.csv(
  Employee_Cleaned,
  "Employee_Attrition_Cleaned.csv",
  row.names = FALSE,
  na = ""
)

cat("\nPreprocessing completed successfully!\n")
cat("Output: Employee_Attrition_Cleaned.csv\n")
---------------------------------------------------------------------------------------------------------------------------------------
  Question 6: Complete Data Preprocessing Using the Pokémon Dataset (30 marks)
Question from your CAE1 paper — Set 2, Section B

Perform a complete data preprocessing workflow on the Pokémon dataset. Your solution must include importing and inspecting data, correcting quality issues, handling missing values and duplicates, cleaning categorical variables, treating outliers, scaling numerical variables, creating at least four derived features, selecting final variables, validating the data and exporting it as CSV.


# ============================================================
# QUESTION 6: POKEMON DATA PREPROCESSING
# ============================================================

library(dplyr)
library(stringr)

# 1. IMPORT DATASET
Pokemon <- read.csv(
  file.choose(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# 2. INSPECT DATASET
cat("First six records:\n")
print(head(Pokemon))

cat("\nStructure:\n")
str(Pokemon)

cat("\nSummary:\n")
print(summary(Pokemon))

cat("\nDataset dimensions:\n")
print(dim(Pokemon))

cat("\nMissing values:\n")
print(colSums(is.na(Pokemon)))

cat("\nDuplicate records:\n")
print(sum(duplicated(Pokemon)))

# 3. STANDARDIZE COLUMN NAMES
names(Pokemon) <- make.names(
  names(Pokemon),
  unique = TRUE
)

# 4. REMOVE DUPLICATE RECORDS
Pokemon <- distinct(Pokemon)

# Remove duplicate Pokemon names if a Name column exists
if ("Name" %in% names(Pokemon)) {
  Pokemon$Name <- str_squish(Pokemon$Name)
  
  Pokemon <- Pokemon[
    !duplicated(tolower(Pokemon$Name)),
  ]
}

# 5. CLEAN TEXT AND CATEGORICAL VARIABLES
text_columns <- names(Pokemon)[
  sapply(Pokemon, is.character)
]

for (col in text_columns) {
  Pokemon[[col]] <- str_squish(Pokemon[[col]])
  Pokemon[[col]][Pokemon[[col]] == ""] <- NA
}

# Standardize type names
type_columns <- intersect(
  c("Type.1", "Type.2"),
  names(Pokemon)
)

for (col in type_columns) {
  Pokemon[[col]] <- str_to_title(Pokemon[[col]])
}

# 6. CONVERT NUMERICAL VARIABLES
numeric_columns <- intersect(
  c("Total", "HP", "Attack", "Defense",
    "Sp..Atk", "Sp..Def", "Speed",
    "Generation", "Legendary"),
  names(Pokemon)
)

for (col in numeric_columns) {
  Pokemon[[col]] <- suppressWarnings(
    as.numeric(Pokemon[[col]])
  )
}

# Convert Legendary text values to 0/1 if necessary
if ("Legendary" %in% names(Pokemon)) {
  if (is.character(Pokemon$Legendary)) {
    Pokemon$Legendary <- ifelse(
      tolower(Pokemon$Legendary) == "true", 1,
      ifelse(
        tolower(Pokemon$Legendary) == "false", 0,
        NA
      )
    )
  }
}

# 7. HANDLE MISSING NUMERICAL VALUES
for (col in numeric_columns) {
  med <- median(Pokemon[[col]], na.rm = TRUE)
  
  if (is.finite(med)) {
    Pokemon[[col]][is.na(Pokemon[[col]])] <- med
  }
}

# 8. HANDLE MISSING CATEGORICAL VALUES
impute_mode <- function(x) {
  observed <- x[!is.na(x)]
  
  if (length(observed) == 0) {
    return(x)
  }
  
  mode_value <- names(which.max(table(observed)))
  x[is.na(x)] <- mode_value
  x
}

for (col in type_columns) {
  Pokemon[[col]] <- impute_mode(Pokemon[[col]])
}

# Keep missing secondary types as an explicit category
if ("Type.2" %in% names(Pokemon)) {
  Pokemon$Type.2[is.na(Pokemon$Type.2)] <- "None"
}

# 9. DETECT AND TREAT OUTLIERS USING IQR CAPPING
cap_outliers <- function(x) {
  if (!is.numeric(x) || all(is.na(x))) {
    return(x)
  }
  
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value
  
  x[x < lower] <- lower
  x[x > upper] <- upper
  
  x
}

# Apply capping to selected battle statistics
outlier_columns <- intersect(
  c("HP", "Attack", "Defense", "Speed"),
  names(Pokemon)
)

for (col in outlier_columns) {
  Pokemon[[col]] <- cap_outliers(Pokemon[[col]])
}

# 10. CREATE DERIVED FEATURES

# Feature 1: Physical strength
if (all(c("Attack", "Defense") %in% names(Pokemon))) {
  Pokemon$Physical_Strength <-
    Pokemon$Attack + Pokemon$Defense
}

# Feature 2: Special strength
if (all(c("Sp..Atk", "Sp..Def") %in% names(Pokemon))) {
  Pokemon$Special_Strength <-
    Pokemon$Sp..Atk + Pokemon$Sp..Def
}

# Feature 3: Overall power category
if ("Total" %in% names(Pokemon)) {
  Pokemon$Power_Category <- cut(
    Pokemon$Total,
    breaks = c(-Inf, 300, 450, 600, Inf),
    labels = c("Low", "Medium", "High", "Elite")
  )
}

# Feature 4: Speed category
if ("Speed" %in% names(Pokemon)) {
  Pokemon$Speed_Category <- cut(
    Pokemon$Speed,
    breaks = c(-Inf, 40, 80, Inf),
    labels = c("Slow", "Moderate", "Fast")
  )
}

# Feature 5: HP-to-attack ratio
if (all(c("HP", "Attack") %in% names(Pokemon))) {
  Pokemon$HP_Attack_Ratio <- ifelse(
    Pokemon$Attack > 0,
    Pokemon$HP / Pokemon$Attack,
    0
  )
}

# Feature 6: Legendary flag
if ("Legendary" %in% names(Pokemon)) {
  Pokemon$Legendary_Status <- ifelse(
    Pokemon$Legendary == 1,
    "Legendary",
    "Non-Legendary"
  )
}

# 11. SCALE NUMERICAL VARIABLES
scale_columns <- intersect(
  c("HP", "Attack", "Defense", "Speed", "Total"),
  names(Pokemon)
)

for (col in scale_columns) {
  x <- Pokemon[[col]]
  s <- sd(x, na.rm = TRUE)
  
  if (is.finite(s) && s > 0) {
    Pokemon[[paste0(col, "_Scaled")]] <-
      as.numeric(scale(x))
  }
}

# 12. CONVERT CATEGORICAL VARIABLES TO FACTORS
factor_columns <- intersect(
  c("Type.1", "Type.2", "Power_Category",
    "Speed_Category", "Legendary_Status"),
  names(Pokemon)
)

for (col in factor_columns) {
  Pokemon[[col]] <- as.factor(Pokemon[[col]])
}

# 13. SELECT FINAL VARIABLES
desired_columns <- c(
  "Name", "Type.1", "Type.2", "Total",
  "HP", "Attack", "Defense", "Sp..Atk",
  "Sp..Def", "Speed", "Generation",
  "Legendary", "Physical_Strength",
  "Special_Strength", "Power_Category",
  "Speed_Category", "HP_Attack_Ratio",
  "Legendary_Status", "HP_Scaled",
  "Attack_Scaled", "Defense_Scaled",
  "Speed_Scaled", "Total_Scaled"
)

final_columns <- intersect(
  desired_columns,
  names(Pokemon)
)

Pokemon_Cleaned <- Pokemon[
  , final_columns, drop = FALSE
]

# 14. VALIDATE CLEANED DATA
cat("\nCLEANED DATASET:\n")
print(head(Pokemon_Cleaned))

cat("\nFinal dimensions:\n")
print(dim(Pokemon_Cleaned))

cat("\nMissing values:\n")
print(colSums(is.na(Pokemon_Cleaned)))

cat("\nDuplicate rows:\n")
print(sum(duplicated(Pokemon_Cleaned)))

cat("\nFinal structure:\n")
str(Pokemon_Cleaned)

# 15. EXPORT CLEANED DATASET
write.csv(
  Pokemon_Cleaned,
  "Pokemon_Cleaned.csv",
  row.names = FALSE,
  na = ""
)

cat("\nPreprocessing completed successfully!\n")
cat("Output: Pokemon_Cleaned.csv\n")

---------------------------------------------------------------------------------------------------------------------------
  Question 7: Time Series Analysis and Forecasting using AirPassengers
Source: CAE2, Set 1, Question 1

Objective: Analyze monthly airline passenger data, identify trend and seasonality, decompose the series, split it into training and testing data, compare forecasting models, evaluate their accuracy, and forecast the next 12 months with prediction intervals.

# ============================================================
# QUESTION 7: TIME SERIES ANALYSIS AND FORECASTING
# Dataset: AirPassengers
# ============================================================

# Install this package only once if needed
# install.packages("forecast")

library(forecast)

# ------------------------------------------------------------
# 1. LOAD AND EXPLORE THE DATA
# ------------------------------------------------------------

data("AirPassengers")

# Display dataset
print(AirPassengers)
summary(AirPassengers)

# Basic information
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
length(AirPassengers)

# Plot the original time series
plot(
  AirPassengers,
  main = "Monthly Airline Passengers (1949–1960)",
  xlab = "Year",
  ylab = "Number of Passengers (thousands)",
  col = "blue",
  lwd = 2
)

# ------------------------------------------------------------
# 2. IDENTIFY TREND AND SEASONALITY
# ------------------------------------------------------------

# Trend and seasonal patterns can be examined from the plot.
# AirPassengers generally shows an upward trend and increasing
# seasonal variation.

# Seasonal plot
monthplot(
  AirPassengers,
  main = "Seasonal Pattern of Airline Passengers",
  xlab = "Month",
  ylab = "Passengers (thousands)"
)

# Seasonal subseries plot
seasonplot(
  AirPassengers,
  year.labels = TRUE,
  main = "Seasonal Plot of AirPassengers",
  xlab = "Month",
  ylab = "Passengers (thousands)"
)

# ------------------------------------------------------------
# 3. DECOMPOSE THE TIME SERIES
# ------------------------------------------------------------

# Multiplicative decomposition is suitable when seasonal
# fluctuations increase as the series level increases.

decomp <- decompose(AirPassengers, type = "multiplicative")

plot(decomp)

# Display the estimated components
print(decomp)

# ------------------------------------------------------------
# 4. SPLIT INTO TRAINING AND TESTING DATA
# ------------------------------------------------------------

# Training data: January 1949 to December 1958
# Testing data: January 1959 to December 1960

train <- window(AirPassengers, end = c(1958, 12))
test <- window(AirPassengers, start = c(1959, 1))

cat("Training observations:", length(train), "\n")
cat("Testing observations:", length(test), "\n")

# Plot the training and testing periods
plot(
  train,
  xlim = c(1949, 1961),
  ylim = range(AirPassengers),
  main = "Training and Testing Periods",
  xlab = "Year",
  ylab = "Passengers (thousands)",
  col = "blue",
  lwd = 2
)

lines(test, col = "red", lwd = 2)

legend(
  "topleft",
  legend = c("Training Data", "Testing Data"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2
)

# ------------------------------------------------------------
# 5. MODEL 1: SEASONAL NAIVE FORECAST
# ------------------------------------------------------------

# Uses the value from the same season in the previous year.

model_snaive <- snaive(train, h = length(test))

plot(
  model_snaive,
  main = "Seasonal Naive Forecast"
)

# ------------------------------------------------------------
# 6. MODEL 2: HOLT-WINTERS EXPONENTIAL SMOOTHING
# ------------------------------------------------------------

# Multiplicative seasonality allows seasonal variation to grow
# as the level of the series increases.

model_hw <- hw(
  train,
  seasonal = "multiplicative",
  h = length(test)
)

plot(
  model_hw,
  main = "Holt-Winters Forecast"
)

# ------------------------------------------------------------
# 7. MODEL 3: SEASONAL ARIMA
# ------------------------------------------------------------

# Automatically selects an appropriate seasonal ARIMA model.

model_arima <- auto.arima(
  train,
  seasonal = TRUE
)

print(model_arima)

forecast_arima <- forecast(
  model_arima,
  h = length(test)
)

plot(
  forecast_arima,
  main = "Seasonal ARIMA Forecast"
)

# ------------------------------------------------------------
# 8. EVALUATE FORECAST ACCURACY
# ------------------------------------------------------------

# Error metrics:
# MAE  = Mean Absolute Error
# RMSE = Root Mean Squared Error
# MAPE = Mean Absolute Percentage Error

calculate_mae <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}

calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

calculate_mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100
}

# Predictions from each model
pred_snaive <- as.numeric(model_snaive$mean)
pred_hw <- as.numeric(model_hw$mean)
pred_arima <- as.numeric(forecast_arima$mean)

actual <- as.numeric(test)

# Build comparison table
comparison <- data.frame(
  Model = c(
    "Seasonal Naive",
    "Holt-Winters",
    "Seasonal ARIMA"
  ),
  MAE = c(
    calculate_mae(actual, pred_snaive),
    calculate_mae(actual, pred_hw),
    calculate_mae(actual, pred_arima)
  ),
  RMSE = c(
    calculate_rmse(actual, pred_snaive),
    calculate_rmse(actual, pred_hw),
    calculate_rmse(actual, pred_arima)
  ),
  MAPE = c(
    calculate_mape(actual, pred_snaive),
    calculate_mape(actual, pred_hw),
    calculate_mape(actual, pred_arima)
  )
)

print(comparison)

# Select the model with the lowest test RMSE
best_model_name <- comparison$Model[which.min(comparison$RMSE)]

cat("Best model based on test RMSE:", best_model_name, "\n")

# ------------------------------------------------------------
# 9. PLOT ACTUAL VS PREDICTED VALUES
# ------------------------------------------------------------

plot(
  test,
  main = "Actual vs Forecasted Passengers (1959–1960)",
  xlab = "Year",
  ylab = "Passengers (thousands)",
  col = "black",
  lwd = 2
)

lines(model_snaive$mean, col = "blue", lwd = 2)
lines(model_hw$mean, col = "green", lwd = 2)
lines(forecast_arima$mean, col = "red", lwd = 2)

legend(
  "topleft",
  legend = c(
    "Actual",
    "Seasonal Naive",
    "Holt-Winters",
    "Seasonal ARIMA"
  ),
  col = c("black", "blue", "green", "red"),
  lty = 1,
  lwd = 2,
  cex = 0.8
)

# ------------------------------------------------------------
# 10. FORECAST THE NEXT 12 MONTHS
# ------------------------------------------------------------

# Refit the selected model using the entire dataset.

if (best_model_name == "Seasonal Naive") {
  
  final_forecast <- snaive(AirPassengers, h = 12)
  
} else if (best_model_name == "Holt-Winters") {
  
  final_forecast <- hw(
    AirPassengers,
    seasonal = "multiplicative",
    h = 12
  )
  
} else {
  
  final_model <- auto.arima(
    AirPassengers,
    seasonal = TRUE
  )
  
  final_forecast <- forecast(
    final_model,
    h = 12
  )
}

# Display forecast with prediction intervals
print(final_forecast)

plot(
  final_forecast,
  main = paste("12-Month Forecast Using", best_model_name),
  xlab = "Year",
  ylab = "Passengers (thousands)"
)

# Display numerical forecasts and intervals
forecast_table <- data.frame(
  Period = time(final_forecast$mean),
  Forecast = as.numeric(final_forecast$mean),
  Lower_80 = as.numeric(final_forecast$lower[, 1]),
  Upper_80 = as.numeric(final_forecast$upper[, 1]),
  Lower_95 = as.numeric(final_forecast$lower[, 2]),
  Upper_95 = as.numeric(final_forecast$upper[, 2])
)

print(forecast_table)

# ------------------------------------------------------------
# 11. FINAL CONCLUSION
# ------------------------------------------------------------

cat("\nTIME SERIES ANALYSIS CONCLUSION\n")
cat("Best model based on test RMSE:", best_model_name, "\n")
cat("The series shows a long-term upward trend.\n")
cat("Passenger numbers also show recurring yearly seasonality.\n")
cat("The selected model was refitted on all observations.\n")
cat("The final forecast covers the next 12 months.\n")

----------------------------------------------------------------------------------------------------------------------------------------------
  Question 8: Text Mining and Sentiment Analysis using Amazon Fine Food Reviews
Source: CAE2, Set 2, Question 1

Objective: Perform text preprocessing, tokenize reviews, remove stop words, generate bigrams, calculate TF-IDF, analyze sentiment using Bing and NRC lexicons, identify positive and negative terms, visualize emotions, and draw conclusions.

This solution uses R with tidytext, dplyr, stringr, and related packages. Download the Amazon Fine Food Reviews dataset from Kaggle before running the code.

- Amazon Fine Food Reviews dataset on Kaggle

The dataset typically contains a Text column with the review text and a Score column with the rating. The code below assumes those columns are present.


# ============================================================
# QUESTION 8: TEXT MINING AND SENTIMENT ANALYSIS
# Dataset: Amazon Fine Food Reviews
# ============================================================

# Install packages only once if required
# install.packages(c("tidyverse", "tidytext", "textdata", "wordcloud"))

library(tidyverse)
library(tidytext)
library(textdata)
library(wordcloud)

# ------------------------------------------------------------
# 1. LOAD AND EXPLORE THE DATASET
# ------------------------------------------------------------

# Select Reviews.csv downloaded from Kaggle
reviews <- read.csv(
  file.choose(),
  stringsAsFactors = FALSE
)

head(reviews)
str(reviews)
summary(reviews$Score)
dim(reviews)

# Keep only necessary columns
reviews <- reviews %>%
  select(Score, Text) %>%
  filter(!is.na(Text), Text != "", !is.na(Score))

# Use a reproducible sample to reduce memory usage.
# Change 20000 to nrow(reviews) to process the entire dataset.
set.seed(123)
if (nrow(reviews) > 20000) {
  reviews <- reviews %>% slice_sample(n = 20000)
}

reviews <- reviews %>%
  mutate(
    review_id = row_number(),
    Score = as.numeric(Score),
    Sentiment_Rating = case_when(
      Score >= 4 ~ "Positive",
      Score <= 2 ~ "Negative",
      TRUE ~ "Neutral"
    )
  )

print(head(reviews))
print(table(reviews$Sentiment_Rating))

# Plot rating distribution
ggplot(reviews, aes(x = factor(Score))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Food Review Ratings",
    x = "Rating",
    y = "Number of Reviews"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 2. CLEAN REVIEW TEXT
# ------------------------------------------------------------

reviews <- reviews %>%
  mutate(
    Text = str_to_lower(Text),
    Text = str_replace_all(Text, "<[^>]+>", " "),
    Text = str_replace_all(Text, "http\\S+|www\\S+", " "),
    Text = str_replace_all(Text, "[^a-z\\s]", " "),
    Text = str_squish(Text)
  )

# ------------------------------------------------------------
# 3. TOKENIZATION AND STOPWORD REMOVAL
# ------------------------------------------------------------

# Convert reviews into one word per row
words <- reviews %>%
  select(review_id, Sentiment_Rating, Text) %>%
  unnest_tokens(word, Text)

# Remove common English stop words
data("stop_words")

clean_words <- words %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_length(word) > 1)

print(head(clean_words))

# Count most frequent words
top_words <- clean_words %>%
  count(word, sort = TRUE) %>%
  slice_max(order_by = n, n = 20)

print(top_words)

ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequent Words",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 4. BIGRAM ANALYSIS
# ------------------------------------------------------------

# A bigram is a sequence of two consecutive words.
# Generate bigrams from cleaned review text.

bigrams <- reviews %>%
  select(review_id, Text) %>%
  unnest_tokens(
    bigram,
    Text,
    token = "ngrams",
    n = 2
  ) %>%
  separate(
    bigram,
    into = c("word1", "word2"),
    sep = " "
  ) %>%
  filter(
    !is.na(word1),
    !is.na(word2),
    word1 != "",
    word2 != ""
  ) %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
  )

top_bigrams <- bigrams %>%
  count(word1, word2, sort = TRUE) %>%
  mutate(bigram = paste(word1, word2)) %>%
  slice_max(order_by = n, n = 20)

print(top_bigrams)

ggplot(top_bigrams, aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(
    title = "Top 20 Frequent Bigrams",
    x = "Two-word phrase",
    y = "Frequency"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 5. TF-IDF ANALYSIS
# ------------------------------------------------------------

# TF-IDF identifies words that are important in a document
# relative to the complete collection of documents.

tfidf_table <- clean_words %>%
  count(review_id, word, sort = FALSE) %>%
  bind_tf_idf(word, review_id, n) %>%
  arrange(desc(tf_idf))

print(head(tfidf_table, 20))

# Show the highest TF-IDF words across reviews
top_tfidf <- tfidf_table %>%
  group_by(word) %>%
  summarise(
    Total_TFIDF = sum(tf_idf),
    .groups = "drop"
  ) %>%
  slice_max(order_by = Total_TFIDF, n = 20)

print(top_tfidf)

ggplot(top_tfidf, aes(x = reorder(word, Total_TFIDF),
                      y = Total_TFIDF)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(
    title = "Top Words by Total TF-IDF",
    x = "Word",
    y = "Total TF-IDF"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 6. BING LEXICON SENTIMENT ANALYSIS
# ------------------------------------------------------------

# Bing lexicon classifies words as positive or negative.

bing_lexicon <- get_sentiments("bing")

bing_results <- clean_words %>%
  inner_join(bing_lexicon, by = "word")

# Count positive and negative words
bing_counts <- bing_results %>%
  count(sentiment, sort = TRUE)

print(bing_counts)

ggplot(bing_counts, aes(x = sentiment, y = n,
                        fill = sentiment)) +
  geom_col() +
  labs(
    title = "Positive and Negative Word Counts (Bing)",
    x = "Sentiment",
    y = "Word Count"
  ) +
  theme_minimal()

# Identify the most common positive and negative words
bing_top_words <- bing_results %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 10) %>%
  ungroup()

print(bing_top_words)

# ------------------------------------------------------------
# 7. REVIEW-LEVEL SENTIMENT SCORING
# ------------------------------------------------------------

review_sentiment <- bing_results %>%
  mutate(
    sentiment_score = if_else(
      sentiment == "positive", 1, -1
    )
  ) %>%
  group_by(review_id) %>%
  summarise(
    Bing_Score = sum(sentiment_score),
    Positive_Words = sum(sentiment == "positive"),
    Negative_Words = sum(sentiment == "negative"),
    .groups = "drop"
  ) %>%
  mutate(
    Bing_Category = case_when(
      Bing_Score > 0 ~ "Positive",
      Bing_Score < 0 ~ "Negative",
      TRUE ~ "Neutral"
    )
  )

review_sentiment <- reviews %>%
  select(review_id, Score, Sentiment_Rating) %>%
  left_join(review_sentiment, by = "review_id") %>%
  mutate(
    Bing_Score = replace_na(Bing_Score, 0),
    Positive_Words = replace_na(Positive_Words, 0L),
    Negative_Words = replace_na(Negative_Words, 0L),
    Bing_Category = replace_na(Bing_Category, "Neutral")
  )

print(head(review_sentiment))

# Compare rating-based sentiment with Bing sentiment
print(table(
  Rating_Sentiment = review_sentiment$Sentiment_Rating,
  Bing_Sentiment = review_sentiment$Bing_Category
))

# ------------------------------------------------------------
# 8. NRC EMOTION ANALYSIS
# ------------------------------------------------------------

# NRC identifies emotions such as joy, anger, fear,
# sadness, trust, anticipation, surprise and disgust.

nrc_lexicon <- get_sentiments("nrc")

nrc_results <- clean_words %>%
  inner_join(nrc_lexicon, by = "word")

# Count each emotion
emotion_counts <- nrc_results %>%
  count(sentiment, sort = TRUE)

print(emotion_counts)

ggplot(emotion_counts, aes(
  x = reorder(sentiment, n),
  y = n
)) +
  geom_col(fill = "coral") +
  coord_flip() +
  labs(
    title = "Emotion Distribution in Food Reviews (NRC)",
    x = "Emotion",
    y = "Word Count"
  ) +
  theme_minimal()

# Compare emotions for positive and negative rated reviews
emotion_by_rating <- clean_words %>%
  inner_join(
    reviews %>% select(review_id, Sentiment_Rating),
    by = "review_id"
  ) %>%
  inner_join(nrc_lexicon, by = "word") %>%
  count(Sentiment_Rating, sentiment, sort = TRUE)

print(emotion_by_rating)

ggplot(
  emotion_by_rating,
  aes(x = sentiment, y = n, fill = Sentiment_Rating)
) +
  geom_col(position = "dodge") +
  labs(
    title = "NRC Emotions by Rating-based Sentiment",
    x = "Emotion",
    y = "Word Count",
    fill = "Rating group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------------------------------------
# 9. WORD CLOUD
# ------------------------------------------------------------

wordcloud(
  words = top_words$word,
  freq = top_words$n,
  max.words = 100,
  random.order = FALSE,
  scale = c(3, 0.5)
)

# ------------------------------------------------------------
# 10. SENTIMENT COMPARISON BY STAR RATING
# ------------------------------------------------------------

sentiment_comparison <- review_sentiment %>%
  group_by(Sentiment_Rating, Bing_Category) %>%
  summarise(
    Reviews = n(),
    .groups = "drop"
  )

print(sentiment_comparison)

# Mean Bing score for each star rating
rating_bing_summary <- review_sentiment %>%
  group_by(Score) %>%
  summarise(
    Mean_Bing_Score = mean(Bing_Score),
    Reviews = n(),
    .groups = "drop"
  )

print(rating_bing_summary)

ggplot(
  rating_bing_summary,
  aes(x = Score, y = Mean_Bing_Score)
) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Mean Bing Sentiment Score by Star Rating",
    x = "Star Rating",
    y = "Mean Bing Sentiment Score"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 11. EXPORT RESULTS
# ------------------------------------------------------------

write.csv(
  review_sentiment,
  "Amazon_Review_Sentiment.csv",
  row.names = FALSE
)

write.csv(
  top_words,
  "Amazon_Top_Words.csv",
  row.names = FALSE
)

write.csv(
  emotion_counts,
  "Amazon_NRC_Emotion_Counts.csv",
  row.names = FALSE
)

cat("Text mining and sentiment analysis completed.\n")
cat("Results exported to CSV files.\n")

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Question 9: Twitter US Airline Sentiment Analysis
Source: CAE2, Set 3, Question 1

Objective: Perform text mining and sentiment analysis on airline-related tweets. Clean and tokenize tweets, remove stop words, extract bigrams, calculate TF-IDF, apply Bing and NRC sentiment lexicons, compare airline sentiment, visualize the results, and draw conclusions.

Download the dataset here: Twitter US Airline Sentiment on Kaggle.

The commonly used dataset contains text, airline, and airline_sentiment columns. The code below standardizes column names so it can also handle common capitalization differences.


# ============================================================
# QUESTION 9: TWITTER US AIRLINE SENTIMENT ANALYSIS
# ============================================================

# INSTALL PACKAGES ONCE IF REQUIRED
# install.packages(c("tidyverse", "tidytext", "textdata", "wordcloud"))

library(tidyverse)
library(tidytext)
library(textdata)
library(wordcloud)

# ============================================================
# 1. LOAD AND EXPLORE DATASET
# ============================================================

# Select the downloaded Tweets.csv file
tweets <- read.csv(
  file.choose(),
  stringsAsFactors = FALSE
)

# Standardize column names
names(tweets) <- tolower(names(tweets))

# Verify required columns
print(names(tweets))

if (!all(c("text", "airline", "airline_sentiment") %in%
         names(tweets))) {
  stop("Dataset must contain text, airline, and airline_sentiment.")
}

# Select required columns and remove missing records
tweets <- tweets %>%
  select(text, airline, airline_sentiment) %>%
  filter(
    !is.na(text),
    text != "",
    !is.na(airline),
    !is.na(airline_sentiment)
  )

# Use a reproducible sample for faster processing
set.seed(123)

if (nrow(tweets) > 20000) {
  tweets <- tweets %>% slice_sample(n = 20000)
}

tweets <- tweets %>%
  mutate(
    tweet_id = row_number(),
    airline = as.character(airline),
    airline_sentiment = str_to_lower(
      as.character(airline_sentiment)
    )
  )

# Inspect data
head(tweets)
str(tweets)
summary(tweets)
table(tweets$airline_sentiment)

# ============================================================
# 2. VISUALIZE ORIGINAL SENTIMENT DISTRIBUTION
# ============================================================

sentiment_counts <- tweets %>%
  count(airline_sentiment, sort = TRUE)

print(sentiment_counts)

ggplot(
  sentiment_counts,
  aes(
    x = airline_sentiment,
    y = n,
    fill = airline_sentiment
  )
) +
  geom_col() +
  labs(
    title = "Overall Twitter Airline Sentiment",
    x = "Sentiment",
    y = "Number of Tweets"
  ) +
  theme_minimal()

# ============================================================
# 3. COMPARE SENTIMENT ACROSS AIRLINES
# ============================================================

airline_sentiment_counts <- tweets %>%
  count(airline, airline_sentiment)

print(airline_sentiment_counts)

ggplot(
  airline_sentiment_counts,
  aes(
    x = airline,
    y = n,
    fill = airline_sentiment
  )
) +
  geom_col(position = "fill") +
  labs(
    title = "Sentiment Proportion by Airline",
    x = "Airline",
    y = "Proportion of Tweets",
    fill = "Sentiment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================
# 4. CLEAN TWEET TEXT
# ============================================================

tweets <- tweets %>%
  mutate(
    text_clean = str_to_lower(text),
    text_clean = str_replace_all(
      text_clean,
      "https?://\\S+|www\\.\\S+",
      " "
    ),
    text_clean = str_replace_all(
      text_clean,
      "@[a-z0-9_]+",
      " "
    ),
    text_clean = str_replace_all(
      text_clean,
      "&amp;",
      " and "
    ),
    text_clean = str_replace_all(
      text_clean,
      "#",
      ""
    ),
    text_clean = str_replace_all(
      text_clean,
      "[^a-z\\s]",
      " "
    ),
    text_clean = str_squish(text_clean)
  )

print(head(tweets[, c("text", "text_clean")]))

# ============================================================
# 5. TOKENIZATION AND STOP-WORD REMOVAL
# ============================================================

# Convert tweets into individual words
tweet_words <- tweets %>%
  select(
    tweet_id,
    airline,
    airline_sentiment,
    text_clean
  ) %>%
  unnest_tokens(word, text_clean)

# Load English stop words
data("stop_words")

# Remove stop words and very short tokens
clean_words <- tweet_words %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_length(word) > 1)

print(head(clean_words))

# Find the top 20 frequent words
top_words <- clean_words %>%
  count(word, sort = TRUE) %>%
  slice_max(order_by = n, n = 20)

print(top_words)

ggplot(
  top_words,
  aes(x = reorder(word, n), y = n)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequent Words",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal()

# ============================================================
# 6. BIGRAM ANALYSIS
# ============================================================

# A bigram is a sequence of two consecutive words.
# Create bigrams from the cleaned tweet text.

tweet_bigrams <- tweets %>%
  select(tweet_id, text_clean) %>%
  unnest_tokens(
    bigram,
    text_clean,
    token = "ngrams",
    n = 2
  ) %>%
  separate(
    bigram,
    into = c("word1", "word2"),
    sep = " "
  ) %>%
  filter(
    !is.na(word1),
    !is.na(word2),
    word1 != "",
    word2 != ""
  ) %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
  )

top_bigrams <- tweet_bigrams %>%
  count(word1, word2, sort = TRUE) %>%
  mutate(bigram = paste(word1, word2)) %>%
  slice_max(order_by = n, n = 20)

print(top_bigrams)

ggplot(
  top_bigrams,
  aes(x = reorder(bigram, n), y = n)
) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(
    title = "Top 20 Frequent Bigrams",
    x = "Bigram",
    y = "Frequency"
  ) +
  theme_minimal()

# ============================================================
# 7. TF-IDF ANALYSIS
# ============================================================

# TF-IDF identifies words that are distinctive across tweets.

tfidf_results <- clean_words %>%
  count(tweet_id, word) %>%
  bind_tf_idf(word, tweet_id, n) %>%
  arrange(desc(tf_idf))

print(head(tfidf_results, 20))

top_tfidf <- tfidf_results %>%
  group_by(word) %>%
  summarise(
    Total_TFIDF = sum(tf_idf),
    .groups = "drop"
  ) %>%
  slice_max(order_by = Total_TFIDF, n = 20)

print(top_tfidf)

ggplot(
  top_tfidf,
  aes(
    x = reorder(word, Total_TFIDF),
    y = Total_TFIDF
  )
) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(
    title = "Top Words by Total TF-IDF",
    x = "Word",
    y = "Total TF-IDF"
  ) +
  theme_minimal()

# ============================================================
# 8. BING SENTIMENT ANALYSIS
# ============================================================

# Bing lexicon classifies words as positive or negative.

bing_lexicon <- get_sentiments("bing")

bing_results <- clean_words %>%
  inner_join(bing_lexicon, by = "word")

bing_counts <- bing_results %>%
  count(sentiment, sort = TRUE)

print(bing_counts)

ggplot(
  bing_counts,
  aes(x = sentiment, y = n, fill = sentiment)
) +
  geom_col() +
  labs(
    title = "Positive and Negative Words (Bing)",
    x = "Sentiment",
    y = "Word Count"
  ) +
  theme_minimal()

# Most frequent positive and negative words
bing_top_words <- bing_results %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 10) %>%
  ungroup()

print(bing_top_words)

# ============================================================
# 9. CALCULATE BING SCORE FOR EACH TWEET
# ============================================================

tweet_scores <- bing_results %>%
  mutate(
    word_score = if_else(
      sentiment == "positive",
      1,
      -1
    )
  ) %>%
  group_by(tweet_id) %>%
  summarise(
    Bing_Score = sum(word_score),
    Positive_Words = sum(sentiment == "positive"),
    Negative_Words = sum(sentiment == "negative"),
    .groups = "drop"
  )

# Bring original tweet labels back into the results.
# Tweets with no Bing-matched words receive a score of zero.

tweet_scores <- tweets %>%
  select(
    tweet_id,
    airline,
    airline_sentiment
  ) %>%
  left_join(tweet_scores, by = "tweet_id") %>%
  mutate(
    Bing_Score = replace_na(Bing_Score, 0),
    Positive_Words = replace_na(Positive_Words, 0L),
    Negative_Words = replace_na(Negative_Words, 0L),
    Bing_Category = case_when(
      Bing_Score > 0 ~ "positive",
      Bing_Score < 0 ~ "negative",
      TRUE ~ "neutral"
    )
  )

print(head(tweet_scores))

# Compare original labels with Bing predictions
sentiment_comparison <- table(
  Actual_Sentiment = tweet_scores$airline_sentiment,
  Bing_Sentiment = tweet_scores$Bing_Category
)

print(sentiment_comparison)

# ============================================================
# 10. NRC EMOTION ANALYSIS - CORRECTED
# ============================================================

# NRC assigns words to emotion categories.
# A word can belong to multiple NRC categories.

nrc_lexicon <- get_sentiments("nrc")

# Explicitly restore airline_sentiment and airline
# before joining to NRC, fixing the missing-column error.

nrc_results <- clean_words %>%
  inner_join(
    tweets %>%
      select(
        tweet_id,
        airline,
        airline_sentiment
      ),
    by = "tweet_id"
  ) %>%
  inner_join(
    nrc_lexicon,
    by = "word",
    relationship = "many-to-many"
  )

print(head(nrc_results))

# Exclude positive and negative labels to retain
# the eight emotion categories only.

nrc_emotions_only <- nrc_results %>%
  filter(
    !sentiment %in% c("positive", "negative")
  )

# Overall emotion counts
emotion_counts <- nrc_emotions_only %>%
  count(sentiment, sort = TRUE)

print(emotion_counts)

ggplot(
  emotion_counts,
  aes(x = reorder(sentiment, n), y = n)
) +
  geom_col(fill = "coral") +
  coord_flip() +
  labs(
    title = "Emotion Distribution in Airline Tweets",
    x = "Emotion",
    y = "Word Count"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 10A. EMOTIONS BY ORIGINAL TWEET SENTIMENT
# ------------------------------------------------------------

emotion_by_sentiment <- nrc_emotions_only %>%
  count(
    airline_sentiment,
    sentiment,
    sort = TRUE
  )

print(emotion_by_sentiment)

ggplot(
  emotion_by_sentiment,
  aes(
    x = sentiment,
    y = n,
    fill = airline_sentiment
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "NRC Emotions by Original Tweet Sentiment",
    x = "Emotion",
    y = "Word Count",
    fill = "Tweet Sentiment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ------------------------------------------------------------
# 10B. EMOTIONS BY AIRLINE
# ------------------------------------------------------------

emotion_by_airline <- nrc_emotions_only %>%
  count(airline, sentiment, sort = TRUE)

print(emotion_by_airline)

ggplot(
  emotion_by_airline,
  aes(
    x = sentiment,
    y = n,
    fill = airline
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "NRC Emotions Across Airlines",
    x = "Emotion",
    y = "Word Count",
    fill = "Airline"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================
# 11. COMPARE BING SENTIMENT ACROSS AIRLINES
# ============================================================

bing_airline_comparison <- tweet_scores %>%
  count(airline, Bing_Category) %>%
  group_by(airline) %>%
  mutate(Proportion = n / sum(n)) %>%
  ungroup()

print(bing_airline_comparison)

ggplot(
  bing_airline_comparison,
  aes(
    x = airline,
    y = Proportion,
    fill = Bing_Category
  )
) +
  geom_col(position = "fill") +
  labs(
    title = "Bing-Based Sentiment by Airline",
    x = "Airline",
    y = "Proportion",
    fill = "Bing Sentiment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================
# 12. WORD CLOUD
# ============================================================

wordcloud(
  words = top_words$word,
  freq = top_words$n,
  max.words = 100,
  random.order = FALSE,
  scale = c(3, 0.5)
)

# ============================================================
# 13. EXPORT RESULTS
# ============================================================

write.csv(
  tweet_scores,
  "Airline_Tweet_Sentiment.csv",
  row.names = FALSE
)

write.csv(
  top_words,
  "Airline_Top_Words.csv",
  row.names = FALSE
)

write.csv(
  emotion_counts,
  "Airline_NRC_Emotion_Counts.csv",
  row.names = FALSE
)

write.csv(
  bing_airline_comparison,
  "Airline_Bing_Comparison.csv",
  row.names = FALSE
)

cat("\nTwitter Airline Sentiment Analysis Completed!\n")
cat("CSV output files have been saved to your working directory.\n")

----------------------------------------------------------------------------------------------------------------------------------------------------
  Question 10: Time Series Analysis and Forecasting using co2
Source: CAE2, Set 4, Question 1

Objective: Analyze the atmospheric CO₂ concentration time series, visualize its trend and seasonal variation, decompose the series, split it into training and testing data, compare Seasonal Naive, Holt-Winters, and ARIMA models, evaluate forecasting accuracy using MAE, RMSE, and MAPE, and forecast the next 12 months with prediction intervals.

The built-in R dataset co2 contains weekly atmospheric CO₂ concentration measurements at Mauna Loa from 1959 to 1997. It does not require downloading a dataset.


# ============================================================
# QUESTION 10: TIME SERIES ANALYSIS AND FORECASTING
# Dataset: co2
# ============================================================

# Install the package once if required
# install.packages("forecast")

library(forecast)

# ------------------------------------------------------------
# 1. LOAD AND EXPLORE DATA
# ------------------------------------------------------------

data("co2")

print(co2)
summary(co2)

start(co2)
end(co2)
frequency(co2)
length(co2)

# Plot original time series
plot(
  co2,
  main = "Atmospheric CO2 Concentration",
  xlab = "Year",
  ylab = "CO2 Concentration (ppm)",
  col = "blue",
  lwd = 2
)

# ------------------------------------------------------------
# 2. TREND AND SEASONALITY
# ------------------------------------------------------------

# The series has a long-term increasing trend and an
# annual seasonal pattern.

# Plot seasonal pattern
monthplot(
  co2,
  main = "Seasonal Pattern of CO2",
  xlab = "Month",
  ylab = "CO2 Concentration (ppm)"
)

# Seasonal subseries plot
seasonplot(
  co2,
  year.labels = TRUE,
  main = "Seasonal Plot of CO2",
  xlab = "Season",
  ylab = "CO2 Concentration (ppm)"
)

# ------------------------------------------------------------
# 3. DECOMPOSE THE TIME SERIES
# ------------------------------------------------------------

# co2 is a weekly series with frequency 52.
# Classical decomposition is used to examine trend,
# seasonal variation, and random variation.

co2_decomp <- decompose(co2, type = "additive")

plot(co2_decomp)

print(co2_decomp)

# ------------------------------------------------------------
# 4. TRAINING AND TESTING SPLIT
# ------------------------------------------------------------

# Reserve the last 3 years for testing.
# This preserves the time order of the observations.

test_length <- 3 * frequency(co2)

train <- window(
  co2,
  end = time(co2)[length(co2) - test_length]
)

test <- window(
  co2,
  start = time(co2)[length(co2) - test_length + 1]
)

cat("Training observations:", length(train), "\n")
cat("Testing observations:", length(test), "\n")

# Plot training and testing data
plot(
  train,
  xlim = range(time(co2)),
  ylim = range(co2),
  main = "CO2 Training and Testing Data",
  xlab = "Year",
  ylab = "CO2 Concentration (ppm)",
  col = "blue",
  lwd = 2
)

lines(test, col = "red", lwd = 2)

legend(
  "topleft",
  legend = c("Training Data", "Testing Data"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2
)

# ------------------------------------------------------------
# 5. MODEL 1: SEASONAL NAIVE
# ------------------------------------------------------------

# Forecasts using the corresponding season from the previous year.

model_snaive <- snaive(
  train,
  h = length(test)
)

plot(
  model_snaive,
  main = "Seasonal Naive Forecast for CO2"
)

# ------------------------------------------------------------
# 6. MODEL 2: HOLT-WINTERS EXPONENTIAL SMOOTHING
# ------------------------------------------------------------

# Holt-Winters models level, trend, and seasonal variation.
# Additive seasonality is appropriate as a starting point
# because the seasonal fluctuations are relatively stable.

model_hw <- hw(
  train,
  seasonal = "additive",
  h = length(test)
)

plot(
  model_hw,
  main = "Holt-Winters Forecast for CO2"
)

# ------------------------------------------------------------
# 7. MODEL 3: SEASONAL ARIMA
# ------------------------------------------------------------

# Automatically select an ARIMA model.

model_arima <- auto.arima(
  train,
  seasonal = TRUE
)

print(model_arima)

forecast_arima <- forecast(
  model_arima,
  h = length(test)
)

plot(
  forecast_arima,
  main = "ARIMA Forecast for CO2"
)

# ------------------------------------------------------------
# 8. FORECAST ACCURACY METRICS
# ------------------------------------------------------------

calculate_mae <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}

calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

calculate_mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100
}

# Extract model predictions
pred_snaive <- as.numeric(model_snaive$mean)
pred_hw <- as.numeric(model_hw$mean)
pred_arima <- as.numeric(forecast_arima$mean)

actual <- as.numeric(test)

# Compare model accuracy
comparison <- data.frame(
  Model = c(
    "Seasonal Naive",
    "Holt-Winters",
    "Seasonal ARIMA"
  ),
  MAE = c(
    calculate_mae(actual, pred_snaive),
    calculate_mae(actual, pred_hw),
    calculate_mae(actual, pred_arima)
  ),
  RMSE = c(
    calculate_rmse(actual, pred_snaive),
    calculate_rmse(actual, pred_hw),
    calculate_rmse(actual, pred_arima)
  ),
  MAPE = c(
    calculate_mape(actual, pred_snaive),
    calculate_mape(actual, pred_hw),
    calculate_mape(actual, pred_arima)
  )
)

print(comparison)

# Select model with the lowest testing RMSE
best_model_name <- comparison$Model[
  which.min(comparison$RMSE)
]

cat("Best model based on test RMSE:", best_model_name, "\n")

# ------------------------------------------------------------
# 9. ACTUAL VS PREDICTED VALUES
# ------------------------------------------------------------

plot(
  test,
  main = "Actual vs Predicted CO2 Concentration",
  xlab = "Year",
  ylab = "CO2 Concentration (ppm)",
  col = "black",
  lwd = 2
)

lines(model_snaive$mean, col = "blue", lwd = 2)
lines(model_hw$mean, col = "green", lwd = 2)
lines(forecast_arima$mean, col = "red", lwd = 2)

legend(
  "topleft",
  legend = c(
    "Actual",
    "Seasonal Naive",
    "Holt-Winters",
    "Seasonal ARIMA"
  ),
  col = c("black", "blue", "green", "red"),
  lty = 1,
  lwd = 2,
  cex = 0.8
)

# ------------------------------------------------------------
# 10. FORECAST NEXT 12 MONTHS
# ------------------------------------------------------------

# The original co2 data is weekly, so 12 months corresponds
# approximately to 52 weeks, not 12 observations.

forecast_horizon <- 52

if (best_model_name == "Seasonal Naive") {
  
  final_forecast <- snaive(
    co2,
    h = forecast_horizon
  )
  
} else if (best_model_name == "Holt-Winters") {
  
  final_forecast <- hw(
    co2,
    seasonal = "additive",
    h = forecast_horizon
  )
  
} else {
  
  final_model <- auto.arima(
    co2,
    seasonal = TRUE
  )
  
  final_forecast <- forecast(
    final_model,
    h = forecast_horizon
  )
}

# Print and plot forecast with prediction intervals
print(final_forecast)

plot(
  final_forecast,
  main = paste(
    "Approximately 12-Month CO2 Forecast Using",
    best_model_name
  ),
  xlab = "Year",
  ylab = "CO2 Concentration (ppm)"
)

# Forecast table with 80% and 95% prediction intervals
forecast_table <- data.frame(
  Period = as.numeric(time(final_forecast$mean)),
  Forecast = as.numeric(final_forecast$mean),
  Lower_80 = as.numeric(final_forecast$lower[, 1]),
  Upper_80 = as.numeric(final_forecast$upper[, 1]),
  Lower_95 = as.numeric(final_forecast$lower[, 2]),
  Upper_95 = as.numeric(final_forecast$upper[, 2])
)

print(head(forecast_table, 12))

# ------------------------------------------------------------
# 11. CONCLUSION
# ------------------------------------------------------------

cat("\nTIME SERIES ANALYSIS CONCLUSION\n")
cat("The CO2 series shows a long-term increasing trend.\n")
cat("Seasonal variation occurs during the year.\n")
cat("Three forecasting models were compared.\n")
cat("Best model based on testing RMSE:", best_model_name, "\n")
cat("The selected model forecasts approximately 12 months ahead.\n")
cat("Prediction intervals represent forecast uncertainty.\n")

--------------------------------------------------------------------------------------------------------------------------------------------------
  Question 11: Air Quality Data Analysis Using R
This is the next alternative question from your CAE1 paper. You need to handle missing values, create interactive visualizations, generate a correlation heatmap, and interpret the results.


# QUESTION 11: AIR QUALITY DATA ANALYSIS

# Step 1: Install packages (run only once)
install.packages(c("plotly", "dplyr"))

# Step 2: Load libraries
library(plotly)
library(dplyr)

# Step 3: Load the built-in Air Quality dataset
data("airquality")

# View the original dataset
head(airquality)
str(airquality)
summary(airquality)

# Step 4: Check missing values
colSums(is.na(airquality))

# Step 5: Handle missing values using median imputation
air_clean <- airquality

air_clean$Ozone[is.na(air_clean$Ozone)] <-
  median(air_clean$Ozone, na.rm = TRUE)

air_clean$Solar.R[is.na(air_clean$Solar.R)] <-
  median(air_clean$Solar.R, na.rm = TRUE)

# Convert Month into a categorical variable
air_clean$Month <- factor(
  air_clean$Month,
  levels = 5:9,
  labels = c("May", "June", "July", "August", "September")
)

# Verify missing values after cleaning
colSums(is.na(air_clean))

# Step 6: Interactive scatter plot
# Temperature vs Ozone, colored by Month
p1 <- plot_ly(
  data = air_clean,
  x = ~Temp,
  y = ~Ozone,
  color = ~Month,
  type = "scatter",
  mode = "markers",
  marker = list(size = 9),
  text = ~paste(
    "Month:", Month,
    "<br>Temperature:", Temp,
    "<br>Ozone:", Ozone
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "Temperature vs Ozone by Month",
    xaxis = list(title = "Temperature (Fahrenheit)"),
    yaxis = list(title = "Ozone Level")
  )

p1

# Step 7: Interactive Wind histogram
p2 <- plot_ly(
  data = air_clean,
  x = ~Wind,
  type = "histogram",
  nbinsx = 15,
  marker = list(line = list(width = 1))
) %>%
  layout(
    title = "Distribution of Wind Speed",
    xaxis = list(title = "Wind Speed (mph)"),
    yaxis = list(title = "Frequency")
  )

p2

# Step 8: Interactive bubble chart
# Temperature vs Ozone
# Bubble size = Solar Radiation
# Bubble color = Month
p3 <- plot_ly(
  data = air_clean,
  x = ~Temp,
  y = ~Ozone,
  size = ~Solar.R,
  color = ~Month,
  type = "scatter",
  mode = "markers",
  sizes = c(5, 35),
  marker = list(opacity = 0.7),
  text = ~paste(
    "Month:", Month,
    "<br>Temperature:", Temp,
    "<br>Ozone:", Ozone,
    "<br>Solar Radiation:", Solar.R
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "Air Quality Bubble Chart",
    xaxis = list(title = "Temperature (Fahrenheit)"),
    yaxis = list(title = "Ozone Level")
  )

p3

# Step 9: Correlation heatmap
numeric_data <- air_clean %>%
  select(Ozone, Solar.R, Wind, Temp)

cor_matrix <- cor(numeric_data, use = "complete.obs")

print(round(cor_matrix, 2))

p4 <- plot_ly(
  x = colnames(cor_matrix),
  y = rownames(cor_matrix),
  z = cor_matrix,
  type = "heatmap",
  colorscale = "RdBu",
  reversescale = TRUE,
  zmin = -1,
  zmax = 1
) %>%
  layout(
    title = "Air Quality Correlation Heatmap",
    xaxis = list(title = ""),
    yaxis = list(title = "")
  )

p4

# Step 10: Generate basic statistical insights
cat("\nAverage Temperature:", mean(air_clean$Temp), "\n")
cat("Average Ozone:", mean(air_clean$Ozone), "\n")
cat("Average Wind Speed:", mean(air_clean$Wind), "\n")

cat("\nCorrelation between Temperature and Ozone:",
    cor(air_clean$Temp, air_clean$Ozone), "\n")

cat("Correlation between Wind and Ozone:",
    cor(air_clean$Wind, air_clean$Ozone), "\n")

# Step 11: Save cleaned dataset
write.csv(
  air_clean,
  "AirQuality_Cleaned.csv",
  row.names = FALSE
)

cat("\nAnalysis completed successfully!\n")

---------------------------------------------------------------------------------------------------------------------------------------------------
  Question 12: Patient Dataset Analysis Using R
This is the next alternative from your CAE1 paper. You need to create a dataset of 30 patients, save and read it using RDS, calculate final treatment bills, categorize expenses, identify the top 10 patients by treatment cost, and export the data.


# QUESTION 12: PATIENT DATA ANALYSIS

# Step 1: Create a dataset of 30 patients
set.seed(123)

n <- 30

Patient_Data <- data.frame(
  Patient_ID = 1:n,
  
  Patient_Name = paste0("Patient_", 1:n),
  
  Age = sample(18:80, n, replace = TRUE),
  
  Gender = sample(
    c("Male", "Female"),
    n,
    replace = TRUE
  ),
  
  Department = sample(
    c("Cardiology", "Neurology", "Orthopedics",
      "General Medicine", "Dermatology"),
    n,
    replace = TRUE
  ),
  
  Admission_Days = sample(1:20, n, replace = TRUE),
  
  Treatment_Cost = sample(
    5000:100000, n, replace = TRUE
  ),
  
  Insurance_Coverage = sample(
    c(0, 0.25, 0.50, 0.75),
    n,
    replace = TRUE
  )
)

# View the dataset
print(Patient_Data)
str(Patient_Data)
summary(Patient_Data)

# Step 2: Save the dataset as an RDS file
saveRDS(Patient_Data, "PatientData.rds")

# Step 3: Read the RDS file
Patient_Data <- readRDS("PatientData.rds")

# Verify the imported dataset
head(Patient_Data)
dim(Patient_Data)

# Step 4: Calculate final bill after insurance
Patient_Data$Insurance_Amount <-
  Patient_Data$Treatment_Cost *
  Patient_Data$Insurance_Coverage

Patient_Data$Final_Bill_Amount <-
  Patient_Data$Treatment_Cost -
  Patient_Data$Insurance_Amount

# Step 5: Categorize treatment expenses
Patient_Data$Expense_Category <- cut(
  Patient_Data$Final_Bill_Amount,
  breaks = c(-Inf, 20000, 50000, Inf),
  labels = c("Low", "Medium", "High")
)

# Step 6: Display the updated dataset
print(Patient_Data)

# Step 7: Find the top 10 patients by treatment cost
Top_10_Patients <- Patient_Data[
  order(-Patient_Data$Treatment_Cost),
]

Top_10_Patients <- head(Top_10_Patients, 10)

print(Top_10_Patients)

# Step 8: Export the full dataset to CSV
write.csv(
  Patient_Data,
  "PatientData_Analysis.csv",
  row.names = FALSE
)

# Step 9: Export the top 10 patients to CSV
write.csv(
  Top_10_Patients,
  "Top10_Patients.csv",
  row.names = FALSE
)

# Step 10: Export to Excel
install.packages("writexl")  # Run only once
library(writexl)

write_xlsx(
  list(
    All_Patients = Patient_Data,
    Top_10_Patients = Top_10_Patients
  ),
  "PatientData_Analysis.xlsx"
)

# Step 11: Basic analysis
cat("Total number of patients:",
    nrow(Patient_Data), "\n")

cat("Average treatment cost:",
    mean(Patient_Data$Treatment_Cost), "\n")

cat("Average final bill:",
    mean(Patient_Data$Final_Bill_Amount), "\n")

cat("\nExpense category counts:\n")
print(table(Patient_Data$Expense_Category))

cat("\nAnalysis completed successfully!\n")

--------------------------------------------------------------------------------------------------------------------------------------