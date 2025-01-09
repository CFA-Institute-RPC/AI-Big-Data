library(ComplexHeatmap)
library(circlize)
library(shiny)
library(BiocManager)
library(IRanges)
library(BiocGenerics)
library(S4Vectors)

heatmap_list <- readRDS("heatmap_list.rds")

###Create RShiny application

#create user interface 
ui <- fluidPage(
  titlePanel("Percentage technology use across workflows"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,  # narrower sidebar
      selectInput(
        inputId = "jobCategory",
        label = "Select Job Category:",
        choices = names(heatmap_list),
        selected = "Advisory"
      )
    ),
    mainPanel(
      width = 10,
      plotOutput("heatmapPlot",width = "100%", height = "800px")
    )
  )
)


server <- function(input, output, session) { #server reactively creates the heatmap based on UI inputs
  output$heatmapPlot <- renderPlot({
    #retrieve user’s selected job category
    selected_category <- input$jobCategory
    
    #extract the appropriate heatmap from the list
    ht <- heatmap_list[[selected_category]]
    
    #ComplexHeatmap objects need draw() to be rendered
    #newpage = TRUE ensures the page is cleared before drawing the new heatmap
    draw(ht, newpage = TRUE)
  })
}

#force check for installation of generics R package (ComplexHeatMap dependency) 
if (FALSE) {
  library(generics)
}
#Deploy app
shinyApp(ui = ui, server = server)
