library(shiny)
library(shinythemes)
library(DT)
library(plotly)
library(shinyWidgets)
source("vis_cell_do.R")
choices <- read.delim("data/choices.txt",header = TRUE,sep=",")

ui <- fluidPage(

  navbarPage(theme = shinytheme("flatly"),title = h2("Cell-Phenotype"),
             tabPanel(h3("Phenotype-Disease"),fluidRow(
               column(1),
               column(11,h2("Explore the cell-phenotype-disease association ")),
               column(1),
               column(5, wellPanel(
                 selectizeInput('pheno', label = h4("Enter phenotype or disease or OMIM number:"), choices = choices$x,
                   selected = "progressive myoclonus epilepsy",options = list(create = TRUE)),
                 h5("Note: Only diseases connected with mammalian phenotypes are on the choice list"),
                 awesomeRadio(inputId = "radio",label = h6(""),
                   choices = list("Mammalian phenotype" = 1, "Disease ontology term" = 2, "OMIM number" = 3),
                   selected = 2,inline = TRUE, status = "success")))),
               fluidRow(
                 column(2),
                 column(10,h4("Cell-Phenotype-Disease Network:")),
                 visNetworkOutput("network_do", height = "680px"),
                 plotlyOutput("pheno_cell_plot",height='100%'),
                 fluidPage(DTOutput('table_do'))
               ))
  )
)

server <- function(input, output) {
  
  output$network_do <- renderVisNetwork({
    network_cell_do(pheno = input$pheno,type = input$radio)
  })
  output$pheno_cell_plot <- plotly::renderPlotly(plot_pheno_cell(pheno = input$pheno,type = input$radio))
  output$table_do <- renderDT(table_cell_do(pheno = input$pheno,type = input$radio), 
                               rownames = FALSE,extensions = 'Buttons', 
                               options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
}

# Run the application 
shinyApp(ui = ui, server = server)
