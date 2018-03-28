library(dplyr)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(scales)
library(shinydashboard)
library(tidyr)

load("sp_yearly.RData")
spps <- Hmisc::capitalize(sort(unique(dat$sp)))

ui <- dashboardPage(
  dashboardHeader(title = "Halias observations"),
  dashboardSidebar(),
  dashboardBody(
    fluidPage(
      fluidRow(
        column(4,
          box(
            width = 12,
            title = "Controls",
            selectInput("selector", "Select species", choices = spps),
            checkboxInput("fixy", "Fix y-axis")
          ),
          box(
            width = 12,
            textOutput("text1")
          )
        ),
        column(8,
          box(
            width = 12,
            plotOutput("plot1", height = 800))
          )
      )
    )
  )
)

server <- function(input, output) {
  
  output$text1 <- renderText({
    paste("You have selected", input$selector,
          ". Additional info goes here.")
  })
  
  output$plot1 <- renderPlot({
    
    sp_name <- tolower(input$selector)
    
    sp_dat <- dat %>% 
      dplyr::filter(sp == sp_name)  
    
    # Make a subselectiong of the data containing two different epochs:
    # 1979-1999 and 2009-
    epochs <- sp_dat %>% 
      dplyr::select(day, begin, end) %>% 
      tidyr::gather(variable, value, -day)
    
    p1 <- ggplot(sp_dat, aes(x = day, y = paik)) +
      geom_line() + xlab("Day of Year") + ylab("Yks./pvm - Ind./day") +
      ggtitle(paste0(sp_name, ", Paikalliset / Lokal / Locals")) + 
      scale_x_date(labels = date_format('%e %b')) + theme_bw()
    
    p2 <- ggplot(sp_dat, aes(x = day, y = muutto)) +
      geom_line() + xlab("Day of Year") + ylab("Yks./pvm - Ind./day") +
      ggtitle(paste0(sp_name, ", Muuttavat / Flyttande / Migrants")) + 
      scale_x_date(labels = date_format('%e %b')) + theme_bw()
    
    p3 <- ggplot(epochs, aes(x = day, y = value, color = variable)) +
      geom_line() + xlab("Day of Year") + ylab("Yks./pvm - Ind./day") +
      ggtitle("Muutos / Förändring / Change") + 
      scale_color_manual(values = c("red", "blue"),
                         labels = c("1979-1999", "2009-")) + 
      scale_x_date(labels = date_format('%e %b')) +
      theme_bw() + theme(legend.title = element_blank(), legend.position = c(0.9, 0.75))
    
    p4 <- grid.arrange(p1, p2, p3, nrow = 3, ncol = 1)
  })
}

shinyApp(ui, server)
