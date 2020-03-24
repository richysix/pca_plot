library('shiny')
library('shinyBS')
library('ggplot2')
library('reshape2')
library('scales')
library('svglite')
library('viridis')
library('markdown')

source('R/load_data.R')
source('R/pca_plots.R')

# set default shape and fill palettes
colour_blind_palette <- 
  c( 'blue' = rgb(0,0.45,0.7),
     'yellow' = rgb(0.95, 0.9, 0.25),
     'vermillion' = rgb(0.8, 0.4, 0),
     'purple' = rgb(0.8, 0.6, 0.7),
     'blue_green' = rgb(0, 0.6, 0.5),
     'sky_blue' = rgb(0.35, 0.7, 0.9),
     'black' = rgb(0, 0, 0),
     'orange' = rgb(0.9, 0.6, 0)
  )
shapes <- c(21:25,4,8)

shinyServer(function(input, output, session) {
  # set testing and debugging options
  session$userData[['debug']] <- TRUE
  session$userData[['testing']] <- FALSE

  limits <- reactiveValues(
    xmin = NA,
    xmax = NA,
    ymin = NA,
    ymax = NA
  )
  
  combined_data <- reactive({
    if (session$userData[['debug']]) {
      cat("Function: combined_data\n")
    }
    test_data <- input$test_data
    if (test_data){
      data_file_info <- list(datapath = 'inst/extdata/test-pca.tsv')
      sample_file_info <- list(datapath = 'inst/extdata/test-samples.txt')
      createAlert(
        session,
        "TestData",
        "TestDataAlert",
        title = "Using Test Data",
        content = 'Test data supplied with the pca_plot package is being used at the moment',
        append = FALSE,
        style = 'warning'
      )
    } else {
      closeAlert(session, "TestDataAlert")
      data_file_info <- input$pca_data_file
      sample_file_info <- input$sample_file
      if (session$userData[['debug']]) {
        cat(sprintf('DATA FILE: %s\nSAMPLE FILE: %s\n', 
                    data_file_info$datapath, 
                    sample_file_info$datapath))
      }
    }
    if (!is.null(data_file_info) &
        !is.null(sample_file_info)) {
        combined_data <- load_data(
          data_file_info$datapath,
          sample_file_info$datapath,
          session )
    } else {
      if (session$userData[['testing']]){
        load('R/sysdata.rda')
        createAlert(
          session,
          "TestData",
          "Test Data",
          title = "Using Test Data",
          content = 'Test data supplied with the pca_plot package is being used at the moment',
          append = FALSE,
          style = 'warning'
        )
      } else {
        return(NULL)
      }
    }
    if (session$userData[['debug']]) {
      print(head(combined_data))
    }

    return(combined_data)
  })
  
  pcs <- reactive({
    if (session$userData[['debug']]) {
      cat("Function: pcs\n")
    }
    combined_data <- combined_data()
    if(is.null(combined_data)) {
      return(NULL)
    } else {
      return(colnames(combined_data)[ grepl('PC', colnames(combined_data)) ])
    }
  })
  
  factors_in_data <- reactive({
    if (session$userData[['debug']]) {
      cat("Function: factors_in_data\n")
    }
    combined_data <- combined_data()
    if(!is.null(combined_data)) {
      factor_names <- vector('list', length = length(combined_data))
      i <- 1
      for (column_name in colnames(combined_data)) {
        if (class(combined_data[[column_name]]) != 'factor') {
          next
        }
        factor_names[[i]] <- column_name
        i <- i + 1
      }
      return(unlist(factor_names))
    }
  })
  
  continuous_variables_in_data <- reactive({
    if (session$userData[['debug']]) {
      cat("Function: continuous_variables_in_data\n")
    }
    combined_data <- combined_data()
    if(!is.null(combined_data)) {
      other_colnames <- colnames(combined_data)[ !grepl('PC', colnames(combined_data)) ]
      variable_names <- vector('list', length = length(other_colnames))
      i <- 1
      for (column_name in other_colnames) {
        if (class(combined_data[[column_name]]) == 'integer' |
            class(combined_data[[column_name]]) == 'numeric') {
          variable_names[[i]] <- column_name
        }
        i <- i + 1
      }
      return(unlist(variable_names))
    } else {
      return(NULL)
    }
  })
  
  shape_variables <- reactive({
    if (session$userData[['debug']]) {
      cat("Function: shape_variables\n")
    }
    factors <- factors_in_data()
    if (is.null(factors)) {
      return(NULL)
    } else {
      combined_data <- combined_data()
      over_threshold <- NULL
      shape_variables <- NULL
      for(var_name in factors) {
        if (nlevels(combined_data[[var_name]]) > 7) {
          over_threshold <- list(over_threshold, var_name)
        } else {
          shape_variables <- list(shape_variables, var_name)
        }
      }
      if (length(unlist(over_threshold)) != 0) {
        shapes_warning <-
          paste0(
            "Some of the shape variables have too many levels and will not be available for selecting as shape.<br>Variables removed: ",
            paste(unlist(over_threshold), collapse = ", ")
          )
        createAlert(
          session,
          "Alert",
          "TooManyShapesAlert",
          title = "Too many shape levels",
          content = shapes_warning,
          append = FALSE,
          style = 'warning'
        )
      }
      return(unlist(shape_variables))
    }
  })
  
  # palette for shapes
  shape_palette <- reactive({
    if (session$userData[['debug']]) {
      cat("Function: shape_palette\n")
    }
    combined_data <- combined_data()
    if (is.null(combined_data)) {
      return(NULL)
    }
    shape_var <- input$shape_var
    num_shapes <- nlevels(combined_data[[shape_var]])
    shape_palette <- shapes[seq_len(num_shapes)]
    names(shape_palette) <- levels(combined_data[[shape_var]])
    return(shape_palette)
  })
  
  # palette for colours
  colour_palette <- reactive({
    if (session$userData[['debug']]) {
      cat("Function: colour_palette\n")
    }
    combined_data <- combined_data()
    if (is.null(combined_data)) {
      return(NULL)
    }
    fill_var <- input$fill_var
    if (session$userData[['debug']]) {
      cat("Function: colour palette\n")
      cat(fill_var, '\n')
    }
    if (class(combined_data[[fill_var]]) == 'factor') {
      # check number of levels
      num_colours <- nlevels(combined_data[[fill_var]])
      if (num_colours > length(colour_blind_palette)) {
        ord1 <- seq(1,num_colours,2)
        ord2 <- seq(2,num_colours,2)
        colour_palette <- hue_pal()(num_colours)[ order(c(ord1,ord2)) ]
        names(colour_palette) <- levels(combined_data[[fill_var]])
      } else {
        colour_palette <- colour_blind_palette[seq_len(num_colours)]
        names(colour_palette) <- levels(combined_data[[fill_var]])
      }
    } else {
      # use viridis if all values are either less than or greater than 0
      # otherwise use diverging scale
      if (min(combined_data[[fill_var]], na.rm = TRUE) > 0 |
          max(combined_data[[fill_var]], na.rm = TRUE) < 0) {
        colour_palette <- 'viridis'
      } else {
        colour_palette <- 'diverging'
      }
    }
    return(colour_palette)
  })
  
  # Change UI options based on input data
  # Fill
  observe({
    if (session$userData[['debug']]) {
      cat("Function: UI observer - Fill\n")
    }
    
    if (input$fill_var_type == 'Categorical') {
      if (!is.null(factors_in_data())) {
        # fill options
        fill_options <- as.list(factors_in_data())
        names(fill_options) <- factors_in_data()
        updateRadioButtons(session, "fill_var",
                           choices = fill_options,
                           selected = fill_options[[1]]
        )
      }
    } else {
      if (!is.null(continuous_variables_in_data())) {
        # fill options
        fill_options <- as.list(continuous_variables_in_data())
        names(fill_options) <- continuous_variables_in_data()
        updateRadioButtons(session, "fill_var",
                           choices = fill_options,
                           selected = fill_options[[1]]
        )
      }
    }
    
  })
  
  # Change UI options based on input data
  # Shape
  observe({
    if (session$userData[['debug']]) {
      cat("Function: UI observer - Shape\n")
    }
    
    if (!is.null(shape_variables())) {
      shape_options <- as.list(shape_variables())
      names(shape_options) <- shape_variables()
      shape_options <- append(shape_options, 'None', after = 0)
      names(shape_options)[1] <- 'None'
      updateRadioButtons(session, "shape_var",
                         choices = shape_options,
                         selected = shape_options[[1]]
      )
    }
  })
  
  # Change UI options based on input data
  # Fill and Shape levels
  observe({
    if (session$userData[['debug']]) {
      cat("Function: UI observer - Fill and Shape levels\n")
    }
    # Fill levels
    combined_data <- combined_data()
    if (!is.null(combined_data)) {
      fill_var <- input$fill_var
      if (class(combined_data[[fill_var]]) == 'factor') {
        fill_levels <- levels(combined_data()[[fill_var]])
      } else {
        fill_levels <- list()
      }
      updateCheckboxGroupInput(session, "fill_levels_checkgroup", 
                               choices = fill_levels,
                               selected = fill_levels)
      
      # Shape levels
      shape_var <- input$shape_var
      if (shape_var == 'None') {
        shape_levels <- list()
      } else {
        shape_levels <- levels(combined_data()[[shape_var]])
        updateCheckboxGroupInput(session, "shape_levels_checkgroup", 
                                 choices = shape_levels,
                                 selected = shape_levels)
      }
    }
  })
  
  # Change UI options based on input data
  # Components
  observe({
    if (session$userData[['debug']]) {
      cat("Function: UI observer - Components\n")
    }
    PCs <- pcs()
    if(!is.null(PCs)) {
      r_options <- lapply(PCs, function(PC){ as.integer(sub('PC', '', PC)) })
      names(r_options) <- PCs
      
      # Set the label, choices, and selected item
      updateRadioButtons(session, "x_axis_pc",
                         choices = r_options,
                         selected = r_options[[1]]
      )
      updateRadioButtons(session, "y_axis_pc",
                         choices = r_options,
                         selected = r_options[[2]]
      )
    }
  })
  
  # Change UI options based on input data
  # Limits
  observe({
    if (session$userData[['debug']]) {
      cat("Function: UI observer - Min/Max X/Y\n")
    }
    button_val <- input$apply_limits
    plot_data <- isolate(combined_data())
    if (is.null(plot_data)) {
      return(NULL)
    }
    limits$xmin <- isolate(input$min_x)
    limits$xmax <- isolate(input$max_x)
    limits$ymin <- isolate(input$min_y)
    limits$ymax <- isolate(input$max_y)
  })

  observeEvent(input$reset_limits, {
    if (session$userData[['debug']]) {
      cat("Function: UI observer - Reset Min/Max X/Y\n")
    }
    updateNumericInput(session, 'min_x', value = NA)
    updateNumericInput(session, 'max_x', value = NA)
    updateNumericInput(session, 'min_y', value = NA)
    updateNumericInput(session, 'max_y', value = NA)
    limits$xmin <- NA
    limits$xmax <- NA
    limits$ymin <- NA
    limits$ymax <- NA
  })
  
  # keep track of which rows have been selected
  vals <- reactiveValues()
  observeEvent(combined_data(), {
    data <- combined_data()
    if (!is.null(data)) {
      vals$keeprows = rep(FALSE, nrow(isolate(combined_data())))
    }
  })
  observeEvent(input$plot_click, {
    plot_data <- combined_data()
    res <- nearPoints(plot_data, input$plot_click, threshold = 5, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # create plot object
  pca_plot_obj <- reactive({
    plot_data <- combined_data()
    plot_data$highlight <- vals$keeprows
    if (session$userData[['debug']]) {
      print('Function: pca_plot_obj')
      print(head(plot_data))
    }
    if (is.null(plot_data)) {
      return(NULL)
    } else {
      first <- paste0('PC', input$x_axis_pc)
      second <- paste0('PC', input$y_axis_pc)
      colour_palette <- colour_palette()
      shape_palette <- shape_palette()

      plot <- create_pca_plot(plot_data, x_component = first, y_component = second,
                              colour_palette, shape_palette, reactiveValuesToList(limits),
                              input, session)
      return(plot)
    }
  })
  
  # render heatmap plot
  output$pca_plot <- renderPlot({
    return(pca_plot_obj())
  })
  
  output$hover_info <- renderUI({
    plot_data <- combined_data()
    if (is.null(plot_data)) {
      return(NULL)
    }
    hover <- input$plot_hover
    point <- nearPoints(plot_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) {
      return(NULL)
    }
    if (session$userData[['debug']]) {
      print(hover)
      print(point)
    }
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    # create content for tooltip
    content <- paste0("<b> Sample Name: </b>", point$sample_name, "<br/>")
    if (!is.null(hover$mapping$fill)) {
      content <- paste0(content, "<b>", hover$mapping$fill, ": </b>", point[[hover$mapping$fill]], "<br/>")
    }
    if (!is.null(hover$mapping$shape)) {
      content <- paste0(content, "<b>", hover$mapping$shape, ": </b>", point[[hover$mapping$shape]], "<br/>")
    }
    content <- paste0(content, 
                      "<b>", hover$mapping$x, ": </b>", sprintf('%.2f', point[[hover$mapping$x]]), "<br/>",
                      "<b>", hover$mapping$y, ": </b>", sprintf('%.2f', point[[hover$mapping$y]]), "<br/>" )
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(content))
    )
  })
  
  # for downloading the plot as a pdf/png
  output$download_current <- downloadHandler(
    filename = function() {
      paste('pca_plot', Sys.Date(), input$plotFormat, sep = '.')
    },
    content = function(file) {
      if (input$plotFormat == "pdf") {
        pdf(file,
          paper = "special",
          height = 7,
          width = 10) # open the pdf device
      } else if (input$plotFormat == "eps") {
        postscript(file,
                   paper = "special",
                   height = 7,
                   width = 10) # open the postscript device
      } else if (input$plotFormat == "svg") {
        svglite(file,
                height = 7,
                width = 10) # open the svg device
      } else if (input$plotFormat == "png") {
        png(file,
            height = 960,
            width = 480,
            res = 100) # open the png device
      }
      print(pca_plot_obj())
      dev.off()  # close device
    },
    contentType = paste0('image', input$plotFormat)
  )

  # for downloading a pdf of each component plotted against the next one
  output$download_all <- downloadHandler(
    filename = function() {
      paste('pca_plot', Sys.Date(), 'pdf', sep = '.')
    },
    content = function(file) {
      pdf(file,
          paper = "special",
          height = 7,
          width = 10) # open the pdf device
      
      plot_data <- combined_data()
      components <- pcs()
      for (i in seq.int(length(components) - 1)) {
        first <- paste0('PC', i)
        second <- paste0('PC', i + 1)
        plot <- create_pca_plot(plot_data, x_component = first, y_component = second,
                                colour_palette(), shape_palette(), isolate(reactiveValuesToList(limits)),
                                input, session)
        print(plot)
      }
      dev.off()  # close device
    },
    contentType = 'image/pdf'
  )
  
  # download an rda file of the current plot
  output$download_rda <- downloadHandler(
    filename = function() {
      paste('pca_plot', Sys.Date(), 'rda', sep = '.')
    },
    content = function(file) {
      pca_plot <- pca_plot_obj()
      save(pca_plot, file = file)
    }
  )
})
