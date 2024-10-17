# Functions in For-loop --------------------------------------------------------

## 2.1 distance_df -------------------------------------------------------------
#' Title Distance calculation
#'
#' @param lb_train the prediction/imputations from the two stage models (brokenstick model and linear model) dataset for the training data
#' @param lb_test_ind the prediction/imputations from the two stage models (brokenstick model and linear model) dataset for the testing data, this is the prediction from the test individual
#' @param match_methods the distance calculation methods, either "euclidean", "mahalanobis", or "single"
#' @param match_time the critical time point for single-time matching, the time must be included in the anchor time set; do not need to specify the time for "euclidean" and "mahalanobis" methods
#' @param id_var the id variable in the dataset
#' @param outcome_var the outcome variable in the dataset
#' @param time_var the time variable in the dataset
#' @param ...
#'
#' @return a distance dataframe with the distance and p-value arranged
#' from smallest to largest
#' @export
#'
#' @examples \dontrun{}
distance_df <- function(lb_train,
                        lb_test_ind,
                        match_methods = c("euclidean", "mahalanobis", "single"),
                        match_time = NULL,
                        id_var,
                        outcome_var,
                        time_var,
                        ...) {
  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)

  ## the matching subset
  lb_sub1 <- lb_train %>%
    pivot_wider(names_from = {{ id_var }},
                values_from = {{ outcome_var }}) %>%
    column_to_rownames(var = as.character({{ time_var }})) %>%
    mutate_all(as.numeric)

  center = as.numeric(unlist(lb_test_ind[, 3]))

  if (match_methods == "euclidean") {
    dist_df <<- euclidean_df(Dmatrix = lb_sub1,
                             center = center)
    # cat("\n using euclidean distance\n")
  }

  if (match_methods == "mahalanobis") {
      dist_df <<- mahalanobis_df(Dmatrix = lb_sub1,
                                 center = center)
    #  cat("\n using mahalanobis distance\n")
  }

  if (match_methods == "single") {
    if (is.null(match_time)) {
      stop("provide matching time points for single-time PLM methods")
    }
    dist_df <<- single_df(Dmatrix = lb_sub1,
                          match_time = match_time,
                          center = center)
    # cat("\n using single critical time point matching \n")
  }

  return(distance = dist_df)
}


## 2.2 matching ----------------------------------------------------------------

#' Title Finding the matches subset
#'
#' @param distance_df the distance dataframe from the `distance_df()` function contains the distance and p-value noramlly will be arranged from smallest to largest
#' @param train the training dataset because we need to subset the training dataset to get the matched individuals
#' @param test_one the testing individual data
#' @param id_var the id variable in the dataset
#' @param outcome_var the outcome variable in the dataset
#' @param time_var the time variable in the dataset
#' @param match_alpha the p-value threshold for the matching (specifically for the Mahalanobis distance matching PLM)
#' @param match_number the number of matches you want to get from the distance dataframe (specifically for the Euclidean and Mahalanobis distance matching PLM)
#' @param match_plot whether you want to plot the matching individual trajectories
#'
#' @return a list with the matching subset, the plot of the matching individual trajectories, the id of the target individual, the alpha threshold for the Mahalanobis distance matching, and the number of matches for the Euclidean and Mahalanobis distance matching
#' @export
#'
#' @examples \dontrun{}
match <- function(distance_df = ddd,
                  train = train,
                  test_one,
                  id_var,
                  outcome_var,
                  time_var,
                  match_alpha = NULL,
                  match_number = NULL,
                  match_plot = FALSE) {

  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)


  if (is.null(match_alpha)) {
    data <- distance_df %>%
      slice(1:match_number) %>%
      inner_join(train, by = as.character({{ id_var }}))
  }

  if (is.null(match_number)) {
    data <- distance_df %>%
      filter(pvalue >= match_alpha) %>%
      inner_join(train, by = as.character({{ id_var }}))
  }

  if (match_plot == TRUE) {

    matching_plot <- ggplot() +
      geom_line(data = data, aes(x = {{ time_var }}, y = {{ outcome_var }},
                    group = {{ id_var }}),
                color = "grey",
                linetype = "dashed") +
      geom_line(data = test_one,
                aes(x = {{time_var}}, y = {{outcome_var}}),
                color = "darkblue",
                linewidth = 1) +
      theme_bw()

    cat("\n plotting matching paired individual trajectories \n")
  } else {
    matching_plot = NULL
  }

  return(list(subset = data,
              plot = matching_plot,
              id = unique(test_one[[as_label(enquo(id_var))]]),
              alpha = match_alpha,
              number = match_number))
}

#' Title The distance calculation function for the PLM
#'
#' @param lb_train the prediction/imputations from the two stage models (brokenstick model and linear model) dataset for the training data
#' @param lb_test_ind the prediction/imputations from the two stage models (brokenstick model and linear model) dataset for the testing data, this is the prediction from the test individual
#' @param train the training dataset because we need to subset the training dataset to get the matched individuals
#' @param match_methods the distance calculation methods, either "euclidean", "mahalanobis", or "single"
#' @param id_var the id variable in the dataset
#' @param outcome_var the outcome variable in the dataset
#' @param time_var the time variable in the dataset
#' @param match_alpha the p-value threshold for the matching (specifically for the Mahalanobis distance matching PLM)
#' @param match_number the number of matches you want to get from the distance dataframe (specifically for the Euclidean and Mahalanobis distance matching PLM)
#' @param match_time the critical time point for single-time matching, the time must be included in the anchor time set; do not need to specify the time for "euclidean" and "mahalanobis" methods
#' @param match_plot whether you want to plot the matching individual trajectories
#' @param ... other arguments for the function
#'
#' @return a list with the matching subset, the plot of the matching individual trajectories, the id of the target individual, the alpha threshold for the Mahalanobis distance matching, and the number of matches for the Euclidean and Mahalanobis distance matching
#' @export
#'
#' @examples \dontrun{}
#'
dis_match <- function(lb_train,
                      lb_test_ind,
                      train = train,
                      match_methods = c("euclidean", "mahalanobis", "single"),
                      id_var,
                      outcome_var,
                      time_var,
                      match_alpha = NULL,
                      match_number = NULL,
                      match_time = NULL,
                      match_plot,
                      ...) {
  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)
  ## the matching subset
  lb_sub1 <- lb_train %>%
    pivot_wider(names_from = {{ id_var }},
                values_from = {{ outcome_var }}) %>%
    column_to_rownames(var = as.character({{ time_var }})) %>%
    mutate_all(as.numeric)

  center = as.numeric(unlist(lb_test_ind[, outcome_var]))

  if (match_methods == "euclidean") {
    # dist_df <<- lb_sub1 %>%
    #   apply(2, norm, type = "2") %>%
    #   as.data.frame() %>%
    #   dplyr::select(diff = 1) %>%
    #   rownames_to_column("id") %>%
    #   arrange(diff) %>%
    #   slice(1:match_number)

    dist_df <<- euclidean_df(Dmatrix = lb_sub1,
                             center = center)
    # cat("\n using euclidean distance\n")
  }

  if (match_methods == "mahalanobis") {
    dist_df <<- mahalanobis_df(Dmatrix = lb_sub1,
                               center = center)
    # cat("\n using mahalanobis distance\n")
  }

  if (match_methods == "single") {
    if (is.null(match_time)) {
      stop("provide matching time points for single-time PLM methods")
    }
    dist_df <<- single_df(Dmatrix = lb_sub1,
                          match_time = match_time,
                          center = center)
    # cat("\n using single critical time point matching \n")
  }

  if (is.null(match_alpha)) {
    data1 <- dist_df %>%
      as.data.frame() %>%
      mutate(ww = 1 / diff) %>%
      slice(1:match_number) %>%
      inner_join(train, by = as.character({{ id_var }}))
  }

  if (is.null(match_number)) {
    data1 <- dist_df %>%
      as.data.frame() %>%
      mutate(ww = 1 / diff) %>%
      filter(pvalue >= match_alpha) %>%
      inner_join(train, by = as.character({{ id_var }}))
  }

  if (match_plot == TRUE) {

    matching_plot <- ggplot() +
      geom_line(data = data1, aes(x = {{ time_var }}, y = {{ outcome_var }},
                                 group = {{ id_var }}),
                color = "grey",
                linetype = "dashed") +
      geom_line(data = lb_test_ind,
                aes(x = {{time_var}}, y = {{outcome_var}}),
                color = "darkblue",
                linewidth = 1) +
      theme_bw()

    cat("\n plotting matching paired individual trajectories \n")
  } else {
    matching_plot = NULL
  }

  return(list(subset = data1,
              target = lb_test_ind,
              plot = matching_plot))
}



## 2.3 single_df --------------------------------------------------------------
#' Title The dynamic matching function for the PLM
#'
#' @param lb_train the prediction/imputations from the two stage models (brokenstick model and linear model) dataset for the training data
#' @param lb_test_ind the prediction/imputations from the two stage models (brokenstick model and linear model) dataset for the testing data, this is the prediction from the test individual
#' @param train the training dataset because we need to subset the training dataset to get the matched individuals
#' @param match_methods the distance calculation methods, either "euclidean", "mahalanobis", or "single"
#' @param id_var the id variable in the dataset
#' @param outcome_var the outcome variable in the dataset
#' @param time_var the time variable in the dataset
#' @param match_alpha the p-value threshold for the matching (specifically for the Mahalanobis distance matching PLM)
#' @param match_number the number of matches you want to get from the distance dataframe (specifically for the Euclidean and Mahalanobis distance matching PLM)
#' @param match_time the critical time point for single-time matching, the time must be included in the anchor time set; do not need to specify the time for "euclidean" and "mahalanobis" methods
#' @param match_plot whether you want to plot the matching individual trajectories
#' @param ... other arguments for the function
#'
#' @return a list with the matching subset, the plot of the matching individual trajectories, the id of the target individual, the alpha threshold for the Mahalanobis distance matching, and the number of matches for the Euclidean and Mahalanobis distance matching
#' @export
#'
#' @examples \dontrun{}
dyn_match <- function(lb_train,
                      lb_test_ind,
                      train = train,
                      match_methods = c("euclidean", "mahalanobis", "single"),
                      id_var,
                      outcome_var,
                      time_var,
                      match_alpha = NULL,
                      match_number = NULL,
                      match_time = NULL,
                      match_plot,
                      ...) {

  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)

  ## the matching subset
  # lb_sub1 <- lb_train %>%
  #   pivot_wider(names_from = {{ id_var }},
  #               values_from = {{ outcome_var }}) %>%
  #   column_to_rownames(var = as.character({{ time_var }})) %>%
  #   mutate_all(as.numeric)
  # center = as.numeric(unlist(lb_test_ind[, outcome_var]))

  center = as.numeric(unlist(lb_test_ind))

  if (match_methods == "euclidean") {
    dist_df <<- lb_train %>%
      apply(2, norm, type = "2") %>%
      as.data.frame() %>%
      dplyr::select(diff = 1) %>%
      rownames_to_column("id") %>%
      arrange(diff) %>%
      slice(1:match_number)
    # cat("\n using euclidean distance\n")
  }


  # Wed Feb 28 22:52:05 2024 ------------------------------
  ## need to change the function to work
  if (match_methods == "mahalanobis") {
    def <- nrow(lb_train)
    df <- lb_train %>%
      as.matrix() %>%
      t()

    x <- sweep(df, 2L, center)
    invcov <- MASS::ginv(cov(df))

    value <- rowSums(x %*% invcov * x)
    pvalue <- pchisq(value, df = def, lower.tail = FALSE)
    dist_df <<- data.frame(diff = value,
                            pvalue = pvalue) %>%
      arrange(desc(pvalue)) %>%
      rownames_to_column("id")

    # dist_df <<- mahalanobis_df(Dmatrix = lb_sub1,
    #                            center = center)
    # cat("\n using mahalanobis distance\n")
  }

  if (match_methods == "single") {
    if (is.null(match_time)) {
      stop("provide matching time points for single-time PLM methods")
    }
    dist_df <<- single_df(Dmatrix = lb_train,
                          match_time = match_time,
                          center = center)
    # cat("\n using single critical time point matching \n")
  }

  if (is.null(match_alpha)) {
    data1 <- dist_df %>%
      as.data.frame() %>%
      mutate(ww = 1 / diff) %>%
      slice(1:match_number) %>%
      inner_join(train, by = as.character({{ id_var }}))
  }

  if (is.null(match_number)) {
    data1 <- dist_df %>%
      as.data.frame() %>%
      mutate(ww = 1 / diff) %>%
      filter(pvalue >= match_alpha) %>%
      inner_join(train, by = as.character({{ id_var }}))
  }

  if (match_plot == TRUE) {

    matching_plot <- ggplot() +
      geom_line(data = data1, aes(x = {{ time_var }}, y = {{ outcome_var }},
                                  group = {{ id_var }}),
                color = "grey",
                linetype = "dashed") +
      # geom_line(data = lb_test_ind,
      #           aes(x = {{time_var}}, y = {{outcome_var}}),
      #           color = "darkblue",
      #           linewidth = 1) +
      theme_bw()

    # cat("\n plotting matching paired individual trajectories \n")
  } else {
    matching_plot = NULL
  }

  return(list(subset = data1, plot = matching_plot))
}








