
library(shiny)
library(png)

# Define UI for final project application

ui <- navbarPage(
    
    # Formatting for the about page
    
    "Investigating Demographics within Higher Education STEM Programs",
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This project's goal is to analyze students studying STEM at the university level 
             and to break down and compare racial and gender demographics between highly 
             ranked institutions within the United States. This data is collected from the 
             American Society for Engineering Education."),
             p("This investigation hopes to critique the current demographic breakdown of 
               STEM fields and encourage higher institutions to think more about how they
               can help encourage underrepresented minorities to enter and remain
               interested in STEM fields."),
             h3("About Me"),
             p("My name is Jessica Edwards and I study Computer Science and Education. 
             You can reach me at jedwards@college.harvard.edu.")),
    
    # Formatting for the school demographics page 
    
    tabPanel("School Demographics",
             fluidPage(
                 titlePanel("School Demographics"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("demographic", label = "Demographic categories", 
                                     choices = c("Race", "Gender"))
                     ),
                     mainPanel(
                         imageOutput("demographic_plot")
                     )
                 )
             )),
    
    # Formatting for the retention at Harvard page
    
    tabPanel("Retention at Harvard",
             fluidPage(
                 titlePanel("Retention at Harvard"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("retention", label = "Demographic categories", 
                                     choices = c("Black", "Hispanic", "White", "Asian", 
                                                 "Female", "Male"))
                     ),
                     mainPanel(
                         imageOutput("retention_plot")
                     )
                 )
             )),
    
    # Formatting for the regression page
    
    tabPanel("Effect of Percentage of Minority Students on School Size",
             fluidPage(
                 titlePanel("Effect of Percentage of Minority Students on School Size"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("regression", label = "Demographic categories", 
                                     choices = c("Race", "Gender"))
                     ),
                     mainPanel(
                         imageOutput("regression_plot")
                     )
                 )
             )))

# Define server logic

server <- function(input, output) {
    
    # Send a pre-rendered image, and don't delete the image after sending it
    # Render race or gender image depending on dropdown menu selection
    
    output$demographic_plot <- renderImage({
        if (input$demographic == "Race") {
            return(list(
                src = 'race_all_plot.png',
                width = 900,
                height = 600,
                alt = "Race"
            ))
        } else if (input$demographic == "Gender") {
            return(list(
                src = 'gender_all_plot.png',
                width = 900,
                height = 600,
                alt = "Gender"
            ))
        }
    }, deleteFile = FALSE)
    
    # Render different image depending on dropdown menu selection for retention
    # plot
    
    output$retention_plot <- renderImage({
        if (input$retention == "Black") {
            return(list(
                src = 'blk_plot.png',
                width = 900,
                height = 600,
                alt = "Black"
            ))
        } else if (input$retention == "Hispanic") {
            return(list(
                src = 'his_plot.png',
                width = 900,
                height = 600,
                alt = "Hispanic"
            ))
        } else if (input$retention == "White") {
            return(list(
                src = 'wht_plot.png',
                width = 900,
                height = 600,
                alt = "White"
            ))
        } else if (input$retention == "Asian") {
            return(list(
                src = 'asi_plot.png',
                width = 900,
                height = 600,
                alt = "Asian"
            ))
        } else if (input$retention == "Female") {
            return(list(
                src = 'female_plot.png',
                width = 900,
                height = 600,
                alt = "Female"
            ))
        } else if (input$retention == "Male") {
            return(list(
                src = 'male_plot.png',
                width = 900,
                height = 600,
                alt = "Male"
            ))
        }
    }, deleteFile = FALSE)
    
    # Render race or gender image depending on dropdown menu selection for
    # regression plot
    
    output$regression_plot <- renderImage({
        if (input$regression == "Race") {
            return(list(
                src = 'blk_his_reg_plot.png',
                width = 500,
                height = 500,
                alt = "Race"
            ))
        } else if (input$regression == "Gender") {
            return(list(
                src = 'female_reg_plot.png',
                width = 500,
                height = 500,
                alt = "Gender"
            ))
        }
    }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
