
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # App title ----
                titlePanel("WinDiddie",windowTitle="Fat Stats - DFS NBA"),
                
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "$$$",
                  tabPanel("MoneyBall",

                           mainPanel(
                             tabsetPanel(
                               tabPanel("Player Assessment",
                                        sidebarPanel(
                                          fileInput("file1", "Choose CSV File",
                                                    multiple = FALSE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")),
                                                      actionButton("optimise", label = "Optimise", 
                                                       style="color: #fff; background-color: #1b9127; border-color: #232222")
                                        ),
                                   
                                        dataTableOutput(outputId = 'playertable')
                               ),
                               tabPanel("Optimised Teams", 
                                        sidebarPanel(
                                                  actionButton("calculate", label = "Calculate Stats", 
                                                       style="color: #fff; background-color: #1b9127; border-color: #232222")
                                        ),
                                        dataTableOutput(outputId = 'opts')
                                        ),
                               
                               tabPanel("Slate Stats", 
                                 
                                        plotOutput("ExposurePlot",width = "100%")
                               )
                             )
                           )
                  ),
                  tabPanel("DraftKings", "Blank"),
                  tabPanel("DraftStars", "Blank")
                )
)