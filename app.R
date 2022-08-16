library(shiny)
library(shinythemes)
library(DT)
library(plotly)
library(shinyWidgets)
source("vis_cell_do.R")
signif_results <- read.delim("data/signif_results.txt",header = TRUE,sep=",")

ui <- fluidPage(

  navbarPage(theme = shinytheme("flatly"),title = h2("Cell-Phenotype"),
             tabPanel(h3("Phenotype-Disease"),fluidRow(
               column(1),
               column(11,h2("Explore the cell-phenotype-disease association ")),
               column(1),
               column(5, wellPanel(
                 selectizeInput('pheno', label = h4("Enter phenotype or disease or OMIM number:"), choices = c(unique(signif_results$Phenotype),unique(signif_results$DO.Disease),unique(signif_results$OMIM.ID)),
                   selected = "progressive myoclonus epilepsy",options = list(create = TRUE)),
                 awesomeRadio(inputId = "radio",label = h6(""),
                   choices = list("Mammalian phenotype" = 1, "Disease ontology term" = 2, "OMIM number" = 3),
                   selected = 2,inline = TRUE, status = "success")))),
               fluidRow(
                 column(2),
                 column(10,h4("Cell-Phenotype-Disease Network:")),
                 visNetworkOutput("network_do", height = "600px"),
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
