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
#' @return
#' @export
#'
#' @examples
fill_inventory_traits<-function(SpParams,
                                x,
                                quantile_Hmed = 0.5,
                                quantile_Hmax = 0.99,
                                quantile_fHDmin = 0.05,
                                quantile_fHDmax = 0.95,
                                progress = TRUE, verbose = FALSE) {
  if(!inherits(SpParams, "data.frame")) cli::cli_abort("SpParams should be a data frame")
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

  if(progress) cli::cli_progress_step("Translating to correct taxa names")
  tree_data_accepted <- tree_data |>
    dplyr::left_join(SpParams[,c("Name", "AcceptedName")], by = c("Species" = "Name"))
  shrub_data_accepted <- shrub_data |>
    dplyr::left_join(SpParams[,c("Name", "AcceptedName")], by = c("Species" = "Name"))

  height_values <- c(as.numeric(tree_data_accepted$Height),
                     as.numeric(shrub_data_accepted$Height))
  dbh_values <- c(as.numeric(tree_data_accepted$DBH),
                  rep(NA, nrow(shrub_data_accepted)))
  accepted_species <- c(as.character(tree_data_accepted$AcceptedName),
                        as.character(shrub_data_accepted$AcceptedName))
  is_tree <- c(rep(TRUE,nrow(tree_data_accepted)), rep(FALSE, nrow(shrub_data_accepted)))


  toRemove <- is.na(height_values) | is.na(accepted_species)
  if(sum(toRemove)>0) {
    height_values <- height_values[!toRemove]
    dbh_values <- dbh_values[!toRemove]
    accepted_species <- accepted_species[!toRemove]
    is_tree <- is_tree[!toRemove]
    if(progress) cli::cli_progress_message(paste0(sum(toRemove), " species/height missing values removed from input."))
  }

  if(progress) cli::cli_progress_bar("Species", total = nrow(SpParams))
  for(i in 1:nrow(SpParams)) {
    if(progress) cli::cli_progress_update()
    heights <- height_values[accepted_species == SpParams$AcceptedName[i]]
    dbhs <- dbh_values[accepted_species == SpParams$AcceptedName[i]]
    is_t <- is_tree[accepted_species == SpParams$AcceptedName[i]]
    if(length(heights)>0) {
      if(all(is_t)) SpParams$GrowthForm[i] <- "Tree"
      else if(all(!is_t)) SpParams$GrowthForm[i] <- "Shrub"
      else SpParams$GrowthForm[i] <- "Tree/Shrub"
      plant_heights <- heights[!is.na(heights)]
      if(length(plant_heights)>0) {
        SpParams$Hmed[i] <- round(as.numeric(quantile(plant_heights, probs=quantile_Hmed, na.rm=FALSE)))
        SpParams$Hmax[i] <- round(as.numeric(quantile(plant_heights, probs=quantile_Hmax, na.rm=FALSE)))
      }
      HD_values <- heights/dbhs
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
