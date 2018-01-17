#' create_pca_plot
#'
#' \code{create_pca_plot} Takes a data.frame and creates an x-y PCA scatterplot
#'
#'        The plot data data.frame should contain the columns for the x, y and fill variables
#'        and optionally ones for the shape variable and text labels. 
#'        The text label column should be named sample_names.
#'    
#' @param plot_data data.frame - plot data
#' @param x_component character - name of the column to plot on the x axis
#' @param y_component character - name of the column to plot on the y axis
#' @param fill_palette character - a named character vectors of colours for the fill aesthetic
#' @param shape_palette character - a named character vectors of colours for the shape aesthetic
#' @param input shiny input list - contains fill_var, fill_levels, shape_var, shape_levels and sample_names
#' @param session Shiny session_object
#' 
#' @return ggplot2 object
#'
#' @examples
#' create_pca_plot( plot_data, 'PC1', 'PC2', fill_palette, shape_palette, input, session )
#'
#' @export
#'
create_pca_plot <- function(plot_data, x_component = 'PC1', y_component = 'PC2',
                            fill_palette, shape_palette, input, session) {
  fill_var <- input$fill_var
  shape_var <- input$shape_var
  fill_levels <- input$fill_levels_checkgroup
  shape_levels <- input$shape_levels_checkgroup
  if (!any(colnames(plot_data) == fill_var)) {
    return(NULL)
  } else if ( shape_var != 'None' & 
              !any(colnames(plot_data) == shape_var)) {
    return(NULL)
  }
  plot_data <- subset_plot_data(plot_data, fill_var, fill_levels,
                                shape_var, shape_levels)
  
  # create plot
  plot <- 
    scatterplot_two_components(plot_data, 
                               x_component, y_component,
                               fill_var, fill_palette, 
                               shape_var, shape_palette,
                               sample_names = input$sample_names)
  # set limits
  button_val <- input$apply_limits
  if (!is.null(button_val) & button_val > 0) {
    min_x = isolate(input$min_x)
    max_x = isolate(input$max_x)
    min_y = isolate(input$min_y)
    max_y = isolate(input$max_y)
    new_vals <- c(min_x, max_x, min_y, max_y)
    old_vals <- calculate_limits(plot_data, x_component, y_component, session)
    for (i in 1:4) {
      new_vals[i] <- ifelse(is.na(new_vals[i]), old_vals[i], new_vals[i])
    }
    plot <- plot + xlim(new_vals[1:2]) + ylim(new_vals[3:4])
  }
  
  return(plot)
}

#' calculate_limits
#'
#' \code{calculate_limits} Calculate X/Y limits of the current plot data
#'
#'        The plot data data.frame should contain the columns for the x, y and fill variables
#'    
#' @param plot_data data.frame - plot data
#' @param x_component character - name of the column to plot on the x axis
#' @param y_component character - name of the column to plot on the y axis
#' @param session Shiny session_object
#' 
#' @return ggplot2 object
#'
#' @examples
#' calculate_limits( plot_data, 'PC1', 'PC2', session )
#'
#' @export
#'

calculate_limits <- function(plot_data, x_component, y_component, session) {
  limits <- c(floor(min(plot_data[[x_component]])),
              ceiling(max(plot_data[[x_component]]) + 0.5),
              floor(min(plot_data[[y_component]])),
              ceiling(max(plot_data[[y_component]]) + 0.5))
  return(limits)
}

#' scatterplot_two_components
#'
#' \code{scatterplot_two_components} Takes a data.frame and creates an x-y scatterplot
#'
#'        The plot data data.frame should contain the columns for the x, y and fill variables
#'        and optionally ones for the shape variable and text labels. 
#'        The text label column should be named sample_names.
#'    
#' @param plot_data data.frame - plot data
#' @param x_component character - name of the column to plot on the x axis
#' @param y_component character - name of the column to plot on the y axis
#' @param fill_var character - name of the column to use as the fill aesthetic
#' @param fill_palette character - a named character vectors of colours for the fill aesthetic
#' @param shape_var character - name of the column to use as the shape aesthetic
#' @param shape_palette character - a named character vectors of colours for the shape aesthetic
#' @param samples_names logical - whether text labels should be added to label the points
#' 
#' @return ggplot2 object
#'
#' @examples
#' scatterplot_two_components( plot_data, 'PC1', 'PC2', 'Gene', fill_palette,
#'                              'Genotype', shape_palette, sample_names )
#'
#' @export
#'
scatterplot_two_components <- 
  function(plot_data, x_component, y_component,
            fill_var, fill_palette, 
            shape_var, shape_palette,
            sample_names = TRUE) {
  plot <- ggplot(data = plot_data,
                 aes_(x = as.name(x_component), y = as.name(y_component)))
  
  if (shape_var == 'None') {
    plot <- plot +
      geom_point(aes_(fill = as.name(fill_var)),
                 size = 4, shape = 21,
                 colour = 'black')
  } else {
    plot <- plot +
      geom_point(aes_(fill = as.name(fill_var),
                      shape = as.name(shape_var)),
                 size = 4,
                 colour = 'black') +
      scale_shape_manual(values = shape_palette,
                         guide = guide_legend(order = 2))
  }
  # add fill scale
  plot <- plot +
    scale_fill_manual(
      values = fill_palette,
      guide = guide_legend(override.aes = list(shape = 21),
                           order = 1)
    )
  
  # add text labels
  if (sample_names) {
    plot <- plot + geom_text(aes_string(label = 'sample_name'),
                             hjust = 0, vjust = 0,
                             nudge_x = 0.5, nudge_y = 0.5,
                             size=4, show.legend=FALSE)
  }
  # change theme
  plot <- plot + theme_minimal()
  
  return(plot)
}


#' subset_plot_data
#'
#' \code{subset_plot_data} Takes a data.frame and subsets it to the supplied levels of up to 2 variables
#'
#' @param plot_data data.frame - plot data
#' @param fill_var character - name of the column to use as the fill aesthetic
#' @param fill_levels character - a named character vectors of colours for the fill aesthetic
#' @param shape_var character - name of the column to use as the shape aesthetic
#' @param shape_levels character - a named character vectors of colours for the shape aesthetic
#' 
#' @return data.frame
#'
#' @examples
#' subset_plot_data( plot_data, 'Gene', c('Gene1', 'Gene2'),
#'                              'Genotype', c('wt', 'hom') )
#'
#' @export
#'
subset_plot_data <- function(plot_data, fill_var, fill_levels,
                             shape_var, shape_levels) {
  cat('Plot data:\n')
  print(head(plot_data))
  cat(sprintf('Fill variable: %s\n', fill_var))
  cat(sprintf('Fill levels: %s\n', fill_levels))
  cat(sprintf('Shape variable: %s\n', shape_var))
  cat(sprintf('Shape levels: %s\n', shape_levels))
  
  # return original data if both sets of leveles are NULL
  if (is.null(fill_levels) & is.null(shape_levels)) {
    cat('Both fill levels and shape levels are NULL. Returning original data\n')
    return(plot_data)
  }
  
  # only subset data if levels are not NULL
  if (is.null(fill_levels)) {
    plot_data_subset <- plot_data
  } else{
    plot_data_subset <- do.call(rbind, lapply(fill_levels,
                                              function(level){ plot_data[plot_data[[fill_var]] == level, ] }) )
  }
  if (shape_var != 'None') {
    if (!is.null(shape_levels)) {
      plot_data_subset <- do.call(rbind, lapply(shape_levels,
                                                function(level){ plot_data_subset[plot_data_subset[[shape_var]] == level, ] }) )
    }
  }
  
  return(plot_data_subset)
}