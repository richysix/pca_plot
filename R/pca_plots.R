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
                            fill_palette, shape_palette, current_limits, input, session, ...) {
  if (session$userData[['debug']]) {
    cat("Function: create_pca_plot\n")
  }
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
                                shape_var, shape_levels, session)
  if (session$userData[['debug']]) {
    cat('Plot data after subset:\n')
    print(head(plot_data))
    print(fill_palette)
    print(nrow(plot_data))
  }
  if(nrow(plot_data) == 0) {
    cat('Plot data is empty, returning NULL\n')
    return(NULL)
  }
  # create plot
  plot <- 
    scatterplot_two_components(plot_data, 
                               x_component, y_component,
                               fill_var, fill_palette, 
                               shape_var, shape_palette,
                               sample_names = input$sample_names,
                               point_size = input$point_size, ... )
  # set limits
  # button_val <- input$apply_limits
  limits <- get_limits(current_limits, plot_data, x_component, y_component, session)
  plot <- plot + xlim(c(limits[['xmin']], limits[['xmax']])) + 
    ylim(c(limits[['ymin']], limits[['ymax']])) +
    coord_cartesian(clip = "off")
  
  return(plot)
}

get_limits <- function(current_limits, plot_data, x_component, y_component, session){
  if (session$userData[['debug']]) {
    cat("Function: get_limits\n")
  }
  data_limits <- calculate_limits(plot_data, x_component, y_component, session)
  new_limits <- list()
  for (i in c('xmin', 'xmax', 'ymin', 'ymax')) {
    new_limits[[i]] <- ifelse(is.na(current_limits[[i]]), data_limits[[i]], current_limits[[i]])
  }
  if (session$userData[['debug']]) {
    print(current_limits)
    print(data_limits)
    print(new_limits)
  }
  return(new_limits)
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
  limits <- list( xmin = floor(min(plot_data[[x_component]])),
                  xmax = ceiling(max(plot_data[[x_component]]) + 0.5),
                  ymin = floor(min(plot_data[[y_component]])),
                  ymax = ceiling(max(plot_data[[y_component]]) + 0.5))
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
            sample_names = TRUE,
            point_size = 4, ...) {
  plot <- ggplot(data = plot_data,
                 aes(x = !!rlang::sym(x_component), 
                     y = !!rlang::sym(y_component),
                      colour = highlight))
  
  if (shape_var == 'None') {
    plot <- plot +
      geom_point(aes(fill = !!rlang::sym(fill_var), 
                      stroke = highlight),
                 size = point_size, shape = 21)
  } else {
    plot <- plot +
      geom_point(aes(fill = !!rlang::sym(fill_var),
                      shape = !!rlang::sym(shape_var), 
                      stroke = highlight),
                 size = point_size) +
      scale_shape_manual(values = shape_palette,
                         guide = guide_legend(order = 2),
                         na.translate = FALSE)
  }
  # add colour scale for highlighting points
  plot <- plot + 
    scale_colour_manual(values = c("FALSE" = 'black',  "TRUE" = 'firebrick3'),
                                     guide = "none") +
    scale_discrete_manual(
      aesthetics = "stroke",
      values = c(`FALSE` = 1, `TRUE` = 2),
      guide = "none"
    )
  
  if (class(plot_data[[fill_var]]) == 'factor') {
    # add fill scale
    plot <- plot +
      scale_fill_manual(
        values = fill_palette,
        guide = guide_legend(override.aes = list(shape = 21),
                             order = 1)
      )
  } else {
    # fill_palette should be either viridis or diverging
    if(fill_palette == 'viridis'){
      plot <- plot + scale_fill_viridis(...)
    } else if (fill_palette == 'diverging') {
      plot <- plot +
        scale_fill_gradient2(low = '#2166ac', mid = 'white', high = '#b2182b',
                             midpoint = 0)
    }
  }
  
  # add text labels
  if (sample_names) {
    plot <- plot + geom_text_repel(aes(label = sample_name),
                             hjust = 0, vjust = 0,
                             nudge_x = 0.5, nudge_y = 0.5,
                             size=4, show.legend=FALSE)
  } else if (sum(plot_data$highlight, na.rm = TRUE) > 0) {
    plot <- plot + geom_label_repel(aes(label = sample_label),
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
#' @param session Shiny session_object
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
                             shape_var, shape_levels, session) {
  if (session$userData[['debug']]) {
    cat("Function: subset_plot_data\n")
    cat('Plot data:\n')
    print(head(plot_data))
    cat(sprintf('Fill variable: %s\n', fill_var))
    cat(sprintf('Fill levels: %s\n', fill_levels))
    cat(sprintf('Shape variable: %s\n', shape_var))
    cat(sprintf('Shape levels: %s\n', shape_levels))
  }
  
  # return original data if both sets of levels are NULL
  if (is.null(fill_levels) & is.null(shape_levels)) {
    cat('Both fill levels and shape levels are NULL. Returning original data\n')
    return(plot_data)
  }
  
  # only subset data if levels are not NULL
  if (is.null(fill_levels)) {
    plot_data_subset <- plot_data
  } else{
    fill_variable <- rlang::sym(fill_var)
    plot_data_subset <- 
      do.call(rbind, lapply(fill_levels,
        function(level){ dplyr::filter(plot_data, !!fill_variable == level) } ) )
  }
  if (shape_var != 'None') {
    shape_variable <- rlang::sym(shape_var)
    if (!is.null(shape_levels)) {
      plot_data_subset <- 
        do.call(rbind, lapply(shape_levels,
                              function(level){ dplyr::filter(plot_data, !!shape_variable == level) } ) )
    }
  }
  
  return(plot_data_subset)
}