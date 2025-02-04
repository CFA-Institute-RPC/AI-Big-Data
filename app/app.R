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
  # Title
  titlePanel("Workflow heatmap"),
  # Sidebar layout - encodes selection panel 
  sidebarLayout(
    sidebarPanel(
      width = 2,  # narrower sidebar
      selectInput(
        inputId = "jobCategory",
        label = "Select Job Category:",
        choices = names(heatmap_list),
        selected = "Advisory"),
      
      # Adds additional information in sidebar
      helpText("Note: Some workflows may be missing in certain heatmaps as there is no data for them (n=0)."),
      
      
      tags$hr(),  # Horizontal line for separation
      h4("Further Information"),
      p("This heatmap visualizes how AI and big data technologies are used for different workflows across job categories.
        The data is derived from a survey conducted by The CFA Institute between February and April 2024.
         "),
      
      # Links Section
      tags$hr(),
      h4("Useful Links"),
      tags$ul(
        tags$li("For questions and discussion, please visit our ",
                tags$a(href = "https://github.com/CFA-Institute-RPC/AI-finance-workflow-heatmap/discussions", 
                       target = "_blank", "GitHub Discussion Page.")),
        
        tags$li("Interested in the source code? Check out our ",
                tags$a(href = "https://github.com/CFA-Institute-RPC/AI-finance-workflow-heatmap", 
                       target = "_blank", "GitHub repository.")),
        
        tags$li("For more details on the content, see the original article ",
                tags$a(href = "https://rpc.cfainstitute.org/research/reports/2025/creating-value-from-big-data-in-the-investment-management-process", 
                       target = "_blank", "here."))
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
