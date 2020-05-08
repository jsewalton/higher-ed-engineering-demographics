library(shinythemes)
library(shiny)
library(png)
library(plotly)
source("data.R")

# Define UI for final project application

ui <- navbarPage(
    
    # Formatting for the about page
    
    "Investigating Demographics within Higher Education Engineering Programs",
    
    # Formatting for the demographics page 
    
    tabPanel("School Demographics",
             fluidPage(
                 theme = shinytheme("flatly"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("school", label = "School",
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
                                                 "WashU")),
                         radioButtons("demographic", label = "Demographic", c("Race", "Gender")),
                         p("Explore racial and gender demographics within undergraduate engineering programs. 
                           Select data from more than twenty colleges and universities on undergraduate
                           degrees awarded by program in 2018.")
                     ),
                     mainPanel(
                         plotOutput("demographic_plot", width = "100%")
                     )
                 )
             )),
    
    # Formatting for the retention at Harvard page
    
    tabPanel("Retention at Harvard",
             fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons("retention", label = "Demographic", 
                                     choices = c("Black", "Hispanic", "White", "Asian", 
                                                 "Female", "Male")),
                         p("Explore data from the ASEE College Profile for Harvard's School of 
                         Engineering and Applied Sciences (SEAS) by comparing SEAS sophomores in 2016 and
                         SEAS seniors who graduated with degrees in 2018.
                           Analyze the retention rate by major for various demographics.")),
                     mainPanel(
                         plotOutput("retention_plot")
                     )
                 )
             )),
    
    # Formatting for the regression page
    
    tabPanel("Effect of Percentage of Minority Students on School Size",
             fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons("regression", label = "Demographic", 
                                     choices = c("Race", "Gender")),
                         p("Is the percentage of underrepresented students in an engineering school associated with the 
                           total number of students enrolled in the program? For race, underrepresented students are defined
                           as Black and Hispanic undergraudates. For gender, underrepresented students are females."),
                         p("A positive correlation would suggest an association between larger engineering programs 
                           and a higher percentage of underrepresented students. A negative correlation
                           would suggest that there is an association between smaller programs and ")
                     ),
                     mainPanel(
                         plotOutput("regression_plot")
                     )
                 )
             )),
    tabPanel("About", 
             fluidPage(
                 h3("Project Background and Motivations"),
                 p("This project's goal is to analyze students studying STEM at the university level 
                 and to break down and compare racial and gender demographics between highly 
                 ranked institutions within the United States. This data is collected from the", 
                 a(href = "http://profiles.asee.org/", "American Society for Engineering Education's 
                   Online College Profiles.")),
                 p("This investigation hopes to critique the current demographic breakdown of 
                   STEM fields and encourage higher institutions to think more about how they
                   can help encourage underrepresented minorities to enter and remain
                   interested in STEM fields."),
                 tags$a(href="https://github.com/jessie9111/higher-ed-engineering-demographics", 
                        "Explore this project more on GitHub"),
                 h3("About Me"),
                 sidebarLayout(
                     sidebarPanel(
                         imageOutput("me")
                     ),
                     mainPanel(
                         p("My name is Jessica Edwards, and I'm a rising senior at Harvard studying
                            Computer Science and Education."),
                         p(tags$a(href="https://github.com/jessie9111", "Check out my GitHub Profile")),
                         p(tags$a(href="https://www.linkedin.com/in/jessicasedwards/", "Connect with me on LinkedIn")),
                         p(tags$a(href="mailto:jedwards@college.harvard.edu", "Contact me by email"))
                     )
                 )
             )
    )
)


# Define server logic

server <- function(input, output) {
    output$demographic_plot <- renderPlot({
        if (input$demographic == "Race") {
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
            )} else if (input$demographic == "Gender") {
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
        )}
    })
    
    # Render different image depending on selection for retention plot
    
    output$retention_plot <- renderPlot({
        case_when(
            input$retention == "Black" ~ list(
                blk_plot,
                width = "auto",
                height = "auto"),
            input$retention == "Hispanic" ~ list(
                his_plot,
                width = "auto",
                height = "auto"),
            input$retention == "White" ~ list(
                wht_plot,
                width = "auto",
                height = "auto"),
            input$retention == "Asian" ~ list(
                asi_plot,
                width = "auto",
                height = "auto"),
            input$retention == "Female" ~ list(
                female_plot,
                width = "auto",
                height = "auto"),
            input$retention == "Male" ~ list(
                male_plot,
                width = "auto",
                height = "auto")
            )
        })
    
    # Render race or gender image depending on selection for regression plot
    
    output$regression_plot <- renderPlot({
        case_when(
            input$regression == "Race" ~ list(
                blk_his_reg_plot,
                width = "auto",
                height = "auto"),
            input$regression == "Gender" ~ list(
                female_reg_plot,
                width = "auto",
                height = "auto"),
        )
    })
    
    output$me <- renderImage({
        list(src = "me.jpeg",
             width = 200,
             height = 200,
             alt = "Jessica")
        }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
