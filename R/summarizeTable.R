#' Summarize A Data Frame
#'
#' @description
#' A minimum alternative to \code{summarytools::dfSummary} to avoid package dependency.
#'
#' @param data a data frame.
#' @param exclude_vars columns to be excluded from summary.
#' @param tte_vars character. Vector of time-to-event variables.
#' @param event_vars character. Vector of event indicators. Every time-to-event
#' variable should be corresponding to an event indicator.
#' @param categorical_vars character. Vector of categorical variables. This can
#' be used to specify variables with limited distinct values as categorical
#' variables in summary.
#' @param title title of the summary report.
#' @param sub_title sub-title.
#'
#' @returns a data frame of summary
#' @export
#'
#' @importFrom base64enc base64encode
#' @importFrom htmltools HTML
#' @importFrom grDevices dev.off png
#' @importFrom graphics axis barplot hist par
#' @importFrom stats IQR complete.cases median sd setNames
#' @importFrom rstudioapi isAvailable viewer
#' @examples
#'
#' set.seed(123)
#'
#' n <- 1000
#' data <- data.frame(
#'   age = rnorm(n, 65, 10),
#'   gender = sample(c('M', 'F', NA), n, replace = TRUE, prob = c(.4, .4, .2)),
#'   time_to_death = rexp(n, .01),
#'   death = rbinom(n, 1, .6),
#'   type = sample(LETTERS[1:8], n, replace = TRUE)
#' )
#'
#' summarizeDataFrame(data, tte_vars = 'time_to_death', event_vars = 'death')
#'
summarizeDataFrame <- function(data,
                               exclude_vars = NULL,
                               tte_vars = NULL,
                               event_vars = NULL,
                               categorical_vars = NULL,
                               title = 'Summary',
                               sub_title = '') {

  if(length(tte_vars) != length(event_vars)){
    stop('tte_vars should be of same length as of event_vars. ')
  }

  var_summaries <- list()

  for(var_name in setdiff(names(data), exclude_vars)) {

    var_data <- data[[var_name]]

    is_tte <- var_name %in% tte_vars
    is_event <- var_name %in% event_vars
    is_cate <- var_name %in% categorical_vars

    if(is_tte) {
      event_var <- event_vars[which(tte_vars == var_name)]
      event_data <- data[[event_var]]

      valid_idx <- complete.cases(var_data, event_data)
      n_valid <- sum(valid_idx)
      n_missing <- sum(!valid_idx)

      surv_obj <- Surv(var_data[valid_idx], event_data[valid_idx])
      km_fit <- survfit(surv_obj ~ 1)

      median_surv <- summary(km_fit)$table["median"]
      median_surv <- ifelse(is.na(median_surv), "Not reached", round(median_surv, 2))

      n_events <- sum(event_data[valid_idx])

      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = 120, height = 80, res = 96, bg = "white")
      par(mar = c(1.5, 1.5, 0.5, 0.5), cex = 0.5)
      plot(km_fit,
           conf.int = FALSE,
           col = "black",
           lwd = 1,
           xlab = "",
           ylab = "",
           main = "",
           axes = FALSE,
           ylim = c(0, 1))
      axis(1, cex.axis = 0.4, tck = -0.03)
      axis(2, cex.axis = 0.4, tck = -0.03)
      dev.off()

      img_raw <- readBin(temp_file, "raw", file.info(temp_file)$size)
      img_base64 <- base64encode(img_raw)
      unlink(temp_file)

      graph_html <- paste0('<img src="data:image/png;base64,', img_base64, '" alt="KM curve">')

      stats_text <- paste0(
        "Median time: ", median_surv, "<br>",
        "Events: ", n_events, "<br>",
        "Missing: ", n_missing, " (", round(n_missing/nrow(data)*100, 1), "%)"
      )

      var_summaries[[var_name]] <- list(
        no = length(var_summaries) + 1,
        variable = paste0(var_name, "<br>[time-to-event]"),
        stats = stats_text,
        graph = graph_html
      )


    } else if(is_event || is_cate || is.factor(var_data) || is.character(var_data)) {
      cnt <- .count(as.character(var_data))
      inv_cnt <- rev(cnt)
      freq_text <- paste0(names(inv_cnt), ": ", inv_cnt, " (",
                          round(inv_cnt/sum(inv_cnt) * 100, 1), "%)", collapse = "<br>")

      n_text_lines <- length(cnt)

      line_height_px <- 20
      total_height <- n_text_lines * line_height_px

      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = 120, height = total_height, res = 96, bg = "white")

      par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))

      barplot(cnt, horiz = TRUE,
              main = "", xlab = "", ylab = "",
              axes = FALSE, names.arg = rep("", length(cnt)),
              col = "lightgray", border = "gray60", lwd = 0.5,
              space = 0.2)

      dev.off()

      img_raw <- readBin(temp_file, "raw", file.info(temp_file)$size)
      img_base64 <- base64encode(img_raw)
      unlink(temp_file)

      graph_html <- paste0('<img src="data:image/png;base64,', img_base64,
                           '" alt="barplot" style="height: auto; width: 120px;">')

      var_summaries[[var_name]] <- list(
        no = length(var_summaries) + 1,
        variable = paste0(var_name, "<br>", ifelse(is_event, "[event indicator]", "[categorical]")),
        stats = freq_text,
        graph = graph_html
      )

    } else {
      stopifnot(is.numeric(var_data))

      n_valid <- sum(!is.na(var_data))
      n_missing <- sum(is.na(var_data))

      mean_val <- round(mean(var_data, na.rm = TRUE), 2)
      sd_val <- round(sd(var_data, na.rm = TRUE), 2)
      min_val <- round(min(var_data, na.rm = TRUE), 2)
      max_val <- round(max(var_data, na.rm = TRUE), 2)

      stats_text <- paste0(
        "Mean (sd): ", mean_val, " (", sd_val, ")<br>",
        "Min < median < max:<br>",
        min_val, " < ", round(median(var_data, na.rm = TRUE), 2), " < ", max_val, "<br>",
        "IQR (CV): ", round(IQR(var_data, na.rm = TRUE), 2), " (", round(sd_val/abs(mean_val), 2), ")<br>",
        "Missing: ", n_missing, " (", round(n_missing/nrow(data)*100, 1), "%)"
      )

      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = 120, height = 80, res = 96, bg = "white")
      par(mar = c(0, 0, 0, 0), cex = 0.5)
      hist(var_data, main = "", xlab = "", ylab = "", axes = FALSE,
           col = "lightgray", border = "gray60", breaks = 10, lwd = 0.5)
      dev.off()

      img_raw <- readBin(temp_file, "raw", file.info(temp_file)$size)
      img_base64 <- base64encode(img_raw)
      unlink(temp_file)

      graph_html <- paste0('<img src="data:image/png;base64,', img_base64, '" alt="histogram">')

      var_summaries[[var_name]] <- list(
        no = length(var_summaries) + 1,
        variable = paste0(var_name, "<br>[numeric]"),
        stats = stats_text,
        graph = graph_html
      )

    }
  }

  html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>', title, '</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 20px;
            background-color: white;
            display: flex;
            flex-direction: column;
            align-items: center;
        }
        h1 {
            color: black;
            text-align: center;
            margin-bottom: 20px;
            font-size: 20px;
        }
        .subtitle {
            text-align: center;
            color: #666;
            margin-bottom: 20px;
            font-size: 16px;
        }
        table {
            border-collapse: collapse;
            font-size: 14px;
            border: 1px solid #999;
            width: auto;
            margin: 0 auto;
        }
        th {
            background-color: #f0f0f0;
            color: black;
            padding: 10px;
            text-align: left;
            font-weight: normal;
            border: 1px solid #999;
            white-space: nowrap;
            font-size: 14px;
        }
        td {
            padding: 10px;
            border: 1px solid #999;
            vertical-align: top;
            line-height: 1.4;
            font-size: 14px;
        }
        .no-col {
            text-align: center;
            white-space: nowrap;
        }
        .variable-col {
            white-space: nowrap;
        }
        .stats-col {
        }
        .freqs-col {
            line-height: 20px;
        }
        .graph-col {
            text-align: center;
            white-space: nowrap;
            vertical-align: top;
        }
        img {
            display: block;
            margin: 0 auto;
            vertical-align: top;
        }
    </style>
</head>
<body>
    <h1>', title, '</h1>
    <div class="subtitle" style="text-align: left;">
        ', sub_title, '<br>
    </div>

    <table>
        <thead>
            <tr>
                <th class="no-col">No</th>
                <th class="variable-col">Variable</th>
                <th class="stats-col">Stats / Freqs</th>
                <th class="graph-col">Graph</th>
            </tr>
        </thead>
        <tbody>')

  for(var_summary in var_summaries) {
    html_content <- paste0(html_content, '
            <tr>
                <td class="no-col">', var_summary$no, '</td>
                <td class="variable-col">', var_summary$variable, '</td>
                <td class="stats-col">', var_summary$stats, '</td>
                <td class="graph-col">', var_summary$graph, '</td>
            </tr>')
  }

  html_content <- paste0(html_content, '
        </tbody>
    </table>
</body>
</html>')

  if(requireNamespace("knitr", quietly = TRUE) &&
     isTRUE(getOption('knitr.in.progress'))) {
    return(invisible(html_content))
  } else if(requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    temp_html <- tempfile(fileext = ".html")
    writeLines(html_content, temp_html, useBytes = TRUE)
    rstudioapi::viewer(temp_html)
    return(invisible(html_content))
  } else {
    cat("Summary generated.\n")
    return(invisible(html_content))
  }
}

.count <- function(x, top_k = 5) {

  x_char <- as.character(x)

  # Frequency of non-NA values
  tbl <- table(x_char, useNA = "no")
  tbl_sorted <- sort(tbl, decreasing = TRUE)

  # Count missing
  n_missing <- sum(is.na(x))

  # Identify top-k values
  if (length(tbl_sorted) <= top_k) {
    main_items <- tbl_sorted
    n_others <- 0
    others_count <- 0
    others_label <- NULL
  } else {
    main_items <- tbl_sorted[1:top_k]
    other_items <- tbl_sorted[(top_k + 1):length(tbl_sorted)]
    n_others <- length(other_items)
    others_count <- sum(other_items)
    others_label <- sprintf("%d others", n_others)
  }

  # Build result in correct order for horiz = TRUE
  # Order: [most freq -> least freq] + [others] + [missing]
  result <- main_items  # least to most frequent

  if (n_others > 0) {
    result <- c(result, setNames(others_count, others_label))
  }

  result <- c(result, Missing = n_missing) |> rev()

  return(result)
}



