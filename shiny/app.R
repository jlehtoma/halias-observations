library(dplyr)
library(ggplot2)
library(gridExtra)
library(highcharter)
library(scales)
library(shinydashboard)
library(tidyr)

load("sp_yearly_1_2.RData")

# FIXME: distinct shoulnd't be needed
dat <- dat %>% 
  dplyr::distinct()

# Read species definition data
sp_data <- readr::read_csv("../data/1.1/Halias_sp_v1.2.csv") %>% 
  dplyr::arrange(Species_code)

spps <- sp_data$Sci_name

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
                 highchartOutput("migration", height = "300px"),
               box(
                 width = 12,
                 highchartOutput("local", height = "300px"))
          )
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
  
  output$migration <- renderHighchart({
    
    sp_current <- sp_data %>% 
      dplyr::filter(Sci_name == input$selector)
    
    sp_name <- sp_current$Sci_name
    
    obs_current <- dat %>% 
      dplyr::filter(sp == sp_current$Species_Abb) 
    
    # Make a subselectiong of the data containing two different epochs:
    # 1979-1999 and 2009-
    epochs <- obs_current %>% 
      dplyr::select(day, begin, end) %>% 
      tidyr::gather(variable, value, -day)
    #browser()
    hc <- obs_current %>% 
      hchart(type = "spline", 
             hcaes(x = day, y = muutto),
             color = c("#e5b13a", "#4bd5ee")) 
    
    return(hc)
  })
  
  output$local <- renderHighchart({
    
    sp_current <- sp_data %>% 
      dplyr::filter(Sci_name == input$selector)
    
    sp_name <- sp_current$Sci_name
    
    obs_current <- dat %>% 
      dplyr::filter(sp == sp_current$Species_Abb) 
    
    # Make a subselectiong of the data containing two different epochs:
    # 1979-1999 and 2009-
    epochs <- obs_current %>% 
      dplyr::select(day, begin, end) %>% 
      tidyr::gather(variable, value, -day)
    #browser()
    hc <- obs_current %>% 
      hchart(type = "spline", 
             hcaes(x = day, y = paik),
             color = c("#e5b13a", "#4bd5ee")) 
    
    return(hc)
  })
  
  output$plot1 <- renderPlot({
    
    sp_current <- sp_data %>% 
      dplyr::filter(Sci_name == input$selector)

    sp_name <- sp_current$Sci_name
    
    obs_current <- dat %>% 
      dplyr::filter(sp == sp_current$Species_Abb)
      
    # Make a subselectiong of the data containing two different epochs:
    # 1979-1999 and 2009-
    epochs <- obs_current %>% 
      dplyr::select(day, begin, end) %>% 
      tidyr::gather(variable, value, -day)
    
      
    p1 <- ggplot(obs_current, aes(x = day, y = paik)) +
      geom_line() + xlab("Day of Year") + ylab("Yks./pvm - Ind./day") +
      ggtitle(paste0(sp_name, ", Paikalliset / Lokal / Locals")) +
      scale_x_date(labels = date_format('%e %b')) + theme_bw()

    p2 <- ggplot(obs_current, aes(x = day, y = muutto)) +
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
