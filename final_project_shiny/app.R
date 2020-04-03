
library(shiny)

# Define UI for final project application

ui <- navbarPage(
    "Investigating Demographics within Higher Education STEM Programs ",
    tabPanel("Models",
             fluidPage(
                 titlePanel("Models"),
                 mainPanel(imageOutput("gender_plot"))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
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
             You can reach me at jedwards@college.harvard.edu.")))

# Define server logic

server <- function(input, output) {
    
    # Send a pre-rendered image, and don't delete the image after sending it
    
    output$gender_plot <- renderImage({
        
        # Return a list containing the filename and alt text
        
        list(src = 'gender_plot.png',
             width = 750,
             height = 600,
             alt = "This is alternate text")
        
    }, deleteFile = FALSE)
}
# Run the application 
shinyApp(ui = ui, server = server)
