for (package in c('shiny', 
                  'shinyjs',
                  'shinyBS',
                  'ggplot2')) {
  library(package, character.only = TRUE)
}

ui <- fluidPage(
                # Include shinyjs
                useShinyjs(),
                theme = "flatly.bootstrap.min.css",
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "pca_plot.css")
                ),
                navbarPage(
                  "PCA plot",
                  tabPanel("PCA plot",
                           sidebarLayout(
                             sidebarPanel(
                               fileInput('pca_data_file', 'Load PCA data file'),
                               fileInput('sample_file', 'Load Sample file'),
                               
                               h4("Options"),
                               # checkbox for displaying sample names
                               checkboxInput("sample_names", 
                                             label = 'Sample Names',
                                             value = TRUE),
                               
                               # Fill colour buttons
                               h5("Fill colour"),
                               radioButtons(
                                 "fill_var_type",
                                 label = h6("Variable Type"),
                                 choices = list(
                                   "Categorical" = 'Categorical',
                                   "Continuous" = 'Continuous'
                                 ),
                                 selected = 'Categorical'
                               ),
                               radioButtons(
                                 "fill_var",
                                 label = h6("Fill colour Variable"),
                                 choices = list(
                                   "Gene" = 'Gene',
                                   "Genotype" = 'Genotype'
                                 ),
                                 selected = 'Gene'
                               ),
                               checkboxGroupInput(
                                 "fill_levels_checkgroup",
                                 label = h6("Fill levels"),
                                 choices = list(),
                                 selected = c()
                               ),
                               # Shape buttons
                               h5("Shape"),
                               radioButtons(
                                 "shape_var",
                                 label = h6("Shape Variable"),
                                 choices = list(
                                   "None" = 'None',
                                   "Gene" = 'Gene',
                                   "Genotype" = 'Genotype'
                                 ),
                                 selected = 'None'
                               ),
                               checkboxGroupInput(
                                 "shape_levels_checkgroup",
                                 label = h6("Shape levels"),
                                 choices = list(),
                                 selected = c()
                               ),
                               h5("Axes"),
                               # x axis buttons
                               radioButtons(
                                 "x_axis_pc",
                                 label = h6("X axis component"),
                                 choices = list(
                                   "PC1" = 1,
                                   "PC2" = 2
                                 ),
                                 selected = 1
                               ),
                               # y axis buttons
                               radioButtons(
                                 "y_axis_pc",
                                 label = h6("Y axis component"),
                                 choices = list(
                                   "PC1" = 1,
                                   "PC2" = 2
                                 ),
                                 selected = 2
                               ),
                               sliderInput('point_size', 'Point Size', min = 1, max = 5, value = 4, step = 1),
                               hr(),
                               h4('Set Limits'),
                               numericInput('min_x', 'Min X:', NULL, min = NA, max = NA, step = NA,
                                            width = NULL),
                               numericInput('max_x', 'Max X:', NULL, min = NA, max = NA, step = NA,
                                            width = NULL),
                               numericInput('min_y', 'Min Y:', NULL, min = NA, max = NA, step = NA,
                                            width = NULL),
                               numericInput('max_y', 'Max Y:', NULL, min = NA, max = NA, step = NA,
                                            width = NULL),
                               actionButton('apply_limits', 'Apply', icon = NULL, width = NULL),
                               # actionButton('reset_limits', 'Reset', icon = NULL, width = NULL),
                               hr(),
                               h4('Downloads'),
                               radioButtons(
                                 "plotFormat",
                                 label = h5("Plot File"),
                                 choices = list('pdf' = 'pdf', 
                                                'eps' = 'eps',
                                                'svg' = 'svg',
                                                'png' = 'png'),
                                 selected = 'pdf'
                               ),
                               downloadButton('download_current', 'Download Current Plot'),
                               hr(),
                               downloadButton('download_all', 'Download all (pdf)'),
                               hr(),
                               downloadButton('download_rda', 'Download rda file of plot'),
                               width = 3
                             ),
                             mainPanel(
                               fluidPage(
                                 fluidRow(
                                   column(width = 12,
                                          bsAlert("Alert")
                                   )
                                 ),
                                 plotOutput(
                                   "pca_plot",
                                   hover = hoverOpts(
                                     id = "plot_hover"
                                   ),
                                   height = "640px"
                                 )                     
                               ),
                               width = 9
                             )
                           )),
                  tabPanel("Help", includeMarkdown("README.md"))
))
