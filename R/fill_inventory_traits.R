#' Fills parameters from inventory data
#'
#' Extracts species parameter values from forest inventory data
#'
#' @param SpParams A species parameter data frame to be filled.
#' @param x A list of \code{\link{forest}} objects, or a data frame with a column called \code{forest}, whose elements are of class \code{\link{forest}}.
#' @param quantile_Hmed Quantile for Hmed
#' @param quantile_Hmax Quantile for Hmax
#' @param quantile_fHDmin Quantile for fHDmin
#' @param quantile_fHDmax Quantile for fHDmax
#' @param progress A boolean flag to prompt progress.
#' @param verbose A boolean flag to indicate extra console output.
#'
#' @return A modified data frame of medfate species parameters
#'
#' @details
#' This function fills information of the species parameter table from the data of the target forest inventory where simulations
#' are to be conducted. Matching is performed between `Species` of the forest inventory data and `Name` of the species parameter table.
#' The following information is extracted:
#' \itemize{
#' \item{\code{GrowthForm}: Growth form according to the usage in the forest inventory. For example, if the species is cited in \code{treeData} tables but not in \code{shrubData} tables, then growth form will be \code{"Tree"}.}
#' \item{\code{Hmax}: Maximum tree/shrub height (cm), according to the `Height` column in \code{treeData} or \code{shrubData} and \code{quantile_Hmax} parameter.}
#' \item{\code{Hmed}: Median tree/shrub height (cm), according to the `Height` column in \code{treeData} or \code{shrubData} and \code{quantile_Hmed} parameter.}
#' \item{\code{fHDmin, fHDmax}: Minimum or maximum height to diameter ratio for trees, according to the `Height` and `DBH` columns in \code{treeData} and \code{quantile_fHDmin} or \code{quantile_fHDmax} parameters, respectively.}
#' }
#'
#' @export
fill_medfate_inventory_traits<-function(SpParams,
                                        x,
                                        quantile_Hmed = 0.5,
                                        quantile_Hmax = 0.99,
                                        quantile_fHDmin = 0.05,
                                        quantile_fHDmax = 0.95,
                                        progress = TRUE, verbose = FALSE) {
  if(!inherits(SpParams, "data.frame")) cli::cli_abort("SpParams should be a species parameter data frame")
  if(!inherits(x, "data.frame") && !inherits(x, "list")) cli::cli_abort("x should be a data frame or a list")
  if(inherits(x, "data.frame")) {
    if(!("forest" %in% names(x))) cli::cli_abort("x does not contain a column called `forest`")
    x <- x[["forest"]]
  }

  if(progress) cli::cli_progress_step("Pooling tree/shrub data")
  shrub_list <- vector("list", length(x))
  tree_list <- vector("list", length(x))
  for(i in 1:length(x)) {
    if(inherits(x[[i]], "forest")) {
      tree_list[[i]] <- x[[i]]$treeData
      shrub_list[[i]] <- x[[i]]$shrubData
    }
  }
  tree_data <- dplyr::bind_rows(tree_list)
  shrub_data <- dplyr::bind_rows(shrub_list)

  height_values <- c(as.numeric(tree_data$Height),
                     as.numeric(shrub_data$Height))
  dbh_values <- c(as.numeric(tree_data$DBH),
                  rep(NA, nrow(shrub_data)))
  names_species <- c(as.character(tree_data$Species),
                        as.character(shrub_data$Species))
  is_tree <- c(rep(TRUE,nrow(tree_data)), rep(FALSE, nrow(shrub_data)))

  toRemove <- is.na(names_species)
  if(sum(toRemove)>0) {
    height_values <- height_values[!toRemove]
    dbh_values <- dbh_values[!toRemove]
    names_species <- names_species[!toRemove]
    is_tree <- is_tree[!toRemove]
    if(progress) cli::cli_progress_message(paste0(sum(toRemove), " species missing values removed from inventory records."))
  }

  if(progress) cli::cli_progress_step("Extracting growth form")
  for(i in 1:nrow(SpParams)) {
    is_t <- is_tree[names_species == SpParams$Name[i]]
    if(length(is_t)>0) {
      if(all(is_t)) SpParams$GrowthForm[i] <- "Tree"
      else if(all(!is_t)) SpParams$GrowthForm[i] <- "Shrub"
      else SpParams$GrowthForm[i] <- "Tree/Shrub"
    }
  }
  if(progress) cli::cli_progress_step("Extracting plant size")
  for(i in 1:nrow(SpParams)) {
    heights <- height_values[names_species == SpParams$Name[i]]
    dbhs <- dbh_values[names_species == SpParams$Name[i]]
    if(length(heights)>0) {
      plant_heights <- heights[!is.na(heights)]
      if(length(plant_heights)>0) {
        SpParams$Hmed[i] <- round(as.numeric(quantile(plant_heights, probs=quantile_Hmed, na.rm=FALSE)))
        SpParams$Hmax[i] <- round(as.numeric(quantile(plant_heights, probs=quantile_Hmax, na.rm=FALSE)))
      }
      HD_values <- heights/dbhs
      HD_values <- HD_values[!is.na(heights)]
      HD_values <- HD_values[!is.na(dbhs)]
      HD_values <- HD_values[dbhs>=5]
      HD_values <- HD_values[!is.na(HD_values)]
      if(length(HD_values)>0) {
        SpParams$fHDmin[i] <- round(as.numeric(quantile(HD_values, probs=quantile_fHDmin, na.rm=FALSE)))
        SpParams$fHDmax[i] <- round(as.numeric(quantile(HD_values, probs=quantile_fHDmax, na.rm=FALSE)))
      }
    }
  }
  if(progress) cli::cli_progress_done()
  return(SpParams)
}
