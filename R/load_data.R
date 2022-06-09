#' load_data
#'
#' \code{load_data} Reads in PCA and sample data and retuns a combined data.frame
#'
#'    The first column of each file must contain the same data (i.e. sample names).
#'    The two data.frames will be joined on common columns
#'    
#' @param pca_data_file character - name of the PCA data file
#' @param sample_file   character - name of the sample file
#' @param session Shiny session_object
#' 
#' @return data.frame
#'
#' @examples
#' load_data( 'pca_data.tsv', 'samples.txt', session_obj )
#'
#' @export
#'
load_data <- function(pca_data_file, sample_file, session) {
  pca_data <- load_pca_data(pca_data_file, session)
  sample_info <- load_sample_data(sample_file, session)
  # join data together on sample name
  combined_data <- dplyr::inner_join(sample_info, pca_data,
                                     by = c("sample_name"))
  return(combined_data)
}

#' load_pca_data
#'
#' \code{load_pca_data} Reads in the PCA data and retuns it
#'
#'    The first column is used as row names.
#'    The data is converted to long format with columns:
#'    sample_name, PC and value
#'    
#' @param pca_data_file character - name of the PCA data file
#' @param session session_object
#' 
#' @return data.frame
#'
#' @examples
#' load_pca_data( 'pca_data.tsv', session_obj )
#'
#' @export
#'
load_pca_data <- function(pca_data_file, session){
  pca_data <- readr::read_tsv(pca_data_file)
  names(pca_data)[1] <- 'sample_name'
  return(pca_data)
}

#' load_sample_data
#'
#' \code{load_sample_data} Reads in the sample data and retuns it
#'
#'    The first column is used as row names.
#'    A sample_name column is created using the row names so that 
#'    the sample data can be joined to the PCA data.
#'    The levels of the column variables are set to the order that 
#'    they appear in the file.
#'    
#' @param sample_file   character - name of the sample file
#' @param session session_object
#' 
#' @return data.frame
#'
#' @examples
#' load_sample_data( 'samples.txt', session_obj )
#'
#' @export
#'
load_sample_data <- function(sample_file, session){
  sample_info <- readr::read_tsv(sample_file)
  names(sample_info)[1] <- 'sample_name'
  for (column_name in colnames(sample_info)) {
    if (class(sample_info[[column_name]]) == 'factor') {
      next
    } else if (class(sample_info[[column_name]]) %in% c('character', 'logical')) {
      # set levels to the order in which they appear in the file
      sample_info[[column_name]] <-
        factor(sample_info[[column_name]],
               levels = unique(sample_info[[column_name]]))
      # unless levels are 'wt', 'het' and 'hom'
      if (all( Reduce('|', 
                      lapply(c('wt', 'het', 'hom'), 
                             function(gt){ levels(sample_info[[column_name]]) == gt }) ) ) ) {
        sample_info[[column_name]] <-
          factor(sample_info[[column_name]],
                 levels = c('wt', 'het', 'hom'))
      }
    }
  }
  
  if (session$userData[['debug']]) {
    print(head(sample_info))
  }
  
  return(sample_info)
}
