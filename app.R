#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Clonal Invasion Dynamics"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "s",
                        label = "Frequency of males in the sexual subpopulation:",
                        min = 0.5,
                        max = 0.8,
                        value = 0.5,
                        step = 0.05)
      
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput(outputId = "p")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$p <- renderPlot({
      # a is a constant that gives the sensitivity to total population density
      a <- 0.0001
      # d is the death rate.  Here I set d=1, meaning an annual species.
      d <- 1.0
      # b is the number of offspring prouduced by a single female (sexual or asexual)
      b <- 3.0
      # c is a constant that gives the sensitivity of the death rate to density.
      c <- 0
      # s is the frequency of males in the sexual subpopulation. (1-s) gives the freq of females in sexual pop.
      s <- input$s
      
      
      # anayltical solutions: carrying capacities for sexual and asexuals are set by the parameters given above,
      # following Lively (2009) J Evol Biol. doi: 10.1111/j.1420-9101.2009.01824.x
      
      #solution for carrying capacity of sexual population
      k_sex <- ((1 - s) * b - d) / ((1 - s) * a + c)
      #solution for carrying capacity of asexual population
      k_asex <- (b - d) / (a + c)
      
      # intitial conditions.  Sex initiated at Ksex.  Asex at 0.
      sex <- k_sex
      a_sex <- 0
      
      # generation at which a single asexual female is introduced
      ga_sex <- 1000
      
      # t is the number of time steps in addition to time step 0
      t <- 1200
      # sets up do loop for i = 1 to T
      time <- c(1:t)
      
      # outSex vector saving output for number of Aexuals
      out_sex <- vector()
      out_sex[1] <- sex[1]
      
      # outAsex vector saving output for number of Asexuals
      out_asex <- vector()
      out_asex[1] <- a_sex[1]
      
      for (i in 1:t){
        out_sex[i] <- sex[i]
        sex[i + 1] <- sex[i] - sex[i] * (d + c * (sex[i] +a_sex[i])) + sex[i] * (1 - s) * (b - a * (sex[i] + a_sex[i]))
        if(i == ga_sex)
        {a_sex[i] <- a_sex[i] + 1
        }
        out_asex[i] <- a_sex[i]
        a_sex[i + 1] <- a_sex[i] - a_sex[i] * d + a_sex[i] * (b - a * (sex[i] + a_sex[i]))
      }

      
      # plot the output
      p <- plot(time, out_asex, type = "l", col = "blue", xlim = c(980, 1080), ylim = c(0, k_asex), xlab = "", ylab = "")
      par(new = TRUE)
      plot(time,
           out_sex,
           type = "l",
           col = "red",
           xlim = c(980, 1080),
           ylim = c(0, k_asex),
           xlab = "Generation",
           ylab = "Number")
      legend(1060,
             17500,
             legend = c("Asexuals", "Sexuals"),
             col = c("blue", "red"),
             lty = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
