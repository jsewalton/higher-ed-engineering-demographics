
library(shiny)
library(png)
library(plotly)
source("data.R")

# Define UI for final project application

ui <- navbarPage(
    
    # Formatting for the about page
    
    "Investigating Demographics within Higher Education Engineering Programs",
    
    # Formatting for the race demographics page 
    
    tabPanel("Race",
             fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("school", label = "Schools", 
                                     choices = c("All Schools",
                                                 "Ivy Leagues",
                                                 "Brown", "Columbia", 
                                                 "Cornell", "Dartmouth",
                                                 "Harvard", "UPenn", 
                                                 "Princeton", "Yale", 
                                                 "Boston University", 
                                                 "CalTech", "UC Berkeley", 
                                                 "Carnegie Mellon", 
                                                 "Duke", "Georgia Tech", 
                                                 "Harvey Mudd", "Johns Hopkins", 
                                                 "MIT", "Northwestern", 
                                                 "Tufts", "Vanderbilt", "UVA", 
                                                 "WashU"))
                     ),
                     mainPanel(
                         plotOutput("race_plot")
                     )
                 )
             )),
    
    # Formatting for the race demographics page 
    
    tabPanel("Gender",
             fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("school", label = "Schools", 
                                     choices = c("All Schools",
                                                 "Ivy Leagues",
                                                 "Brown", "Columbia", 
                                                 "Cornell", "Dartmouth",
                                                 "Harvard", "UPenn", 
                                                 "Princeton", "Yale", 
                                                 "Boston University", 
                                                 "CalTech", "UC Berkeley", 
                                                 "Carnegie Mellon", 
                                                 "Duke", "Georgia Tech", 
                                                 "Harvey Mudd", "Johns Hopkins", 
                                                 "MIT", "Northwestern", 
                                                 "Tufts", "Vanderbilt", "UVA", 
                                                 "WashU"))
                     ),
                     mainPanel(
                         plotOutput("gender_plot")
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
             )),
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
             You can reach me at jedwards@college.harvard.edu."))
    )


# Define server logic

server <- function(input, output) {
    
    output$race_plot <- renderPlot({
        case_when(
            input$school == "All Schools" ~ list(
                race_all_plot,
                width = "auto",
                height = "auto"),
            input$school == "Ivy Leagues" ~ list(
                race_ivy_plot,
                width = "auto",
                height = "auto"),
            input$school == "Brown" ~ list(
                race_school_plot(brown_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Columbia" ~ list(
                race_school_plot(columbia_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Cornell" ~ list(
                race_school_plot(cornell_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Dartmouth" ~ list(
                race_school_plot(dartmouth_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Harvard" ~ list(
                race_school_plot(harvard_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "UPenn" ~ list(
                race_school_plot(penn_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Princeton" ~ list(
                race_school_plot(princeton_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Yale" ~ list(
                race_school_plot(yale_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Boston University" ~ list(
                race_school_plot(boston_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "CalTech" ~ list(
                race_school_plot(caltech_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "UC Berkeley" ~ list(
                race_school_plot(berkeley_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Carnegie Mellon" ~ list(
                race_school_plot(cmu_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Duke" ~ list(
                race_school_plot(duke_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Georgia Tech" ~ list(
                race_school_plot(gt_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Harvey Mudd" ~ list(
                race_school_plot(mudd_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Johns Hopkins" ~ list(
                race_school_plot(jhu_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "MIT" ~ list(
                race_school_plot(mit_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Northwestern" ~ list(
                race_school_plot(nw_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Tufts" ~ list(
                race_school_plot(tufts_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Vanderbilt" ~ list(
                race_school_plot(vanderbilt_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "UVA" ~ list(
                race_school_plot(uva_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "WashU" ~ list(
                race_school_plot(washu_ugrad_degrees_2018),
                width = "auto",
                height = "auto")
        )
    })
    
    output$gender_plot <- renderPlot({
        case_when(
            input$school == "All Schools" ~ list(
                gender_all_plot,
                width = "auto",
                height = "auto"),
            input$school == "Ivy Leagues" ~ list(
                gender_ivy_plot,
                width = "auto",
                height = "auto"),
            input$school == "Brown" ~ list(
                gender_school_plot(brown_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Columbia" ~ list(
                gender_school_plot(columbia_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Cornell" ~ list(
                gender_school_plot(cornell_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Dartmouth" ~ list(
                gender_school_plot(dartmouth_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Harvard" ~ list(
                gender_school_plot(harvard_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "UPenn" ~ list(
                gender_school_plot(penn_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Princeton" ~ list(
                gender_school_plot(princeton_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Yale" ~ list(
                gender_school_plot(yale_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Boston University" ~ list(
                gender_school_plot(boston_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "CalTech" ~ list(
                gender_school_plot(caltech_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "UC Berkeley" ~ list(
                gender_school_plot(berkeley_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Carnegie Mellon" ~ list(
                gender_school_plot(cmu_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Duke" ~ list(
                gender_school_plot(duke_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Georgia Tech" ~ list(
                gender_school_plot(gt_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Harvey Mudd" ~ list(
                gender_school_plot(mudd_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Johns Hopkins" ~ list(
                gender_school_plot(jhu_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "MIT" ~ list(
                gender_school_plot(mit_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Northwestern" ~ list(
                gender_school_plot(nw_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Tufts" ~ list(
                gender_school_plot(tufts_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "Vanderbilt" ~ list(
                gender_school_plot(vanderbilt_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "UVA" ~ list(
                gender_school_plot(uva_ugrad_degrees_2018),
                width = "auto",
                height = "auto"),
            input$school == "WashU" ~ list(
                gender_school_plot(washu_ugrad_degrees_2018),
                width = "auto",
                height = "auto")
        )
    })
    
    
    
    # Send a pre-rendered image, and don't delete the image after sending it
    # Render race or gender image depending on dropdown menu selection
    
    # output$demographic_plot <- renderImage({
    #     if (input$demographic == "Race") {
    #         return(list(
    #             src = 'race_all_plot.png',
    #             width = 900,
    #             height = 600,
    #             alt = "Race"
    #         ))
    #     } else if (input$demographic == "Gender") {
    #         return(list(
    #             src = 'gender_all_plot.png',
    #             width = 900,
    #             height = 600,
    #             alt = "Gender"
    #         ))
    #     }
    # }, deleteFile = FALSE)
    
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
