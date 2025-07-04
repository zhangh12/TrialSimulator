library(survival)
library(base64enc)

# 创建示例数据
set.seed(123)
sample_data <- data.frame(
  patient_id = 1:100,
  age = rnorm(100, 65, 10),
  gender = sample(c("M", "F"), 100, replace = TRUE),
  time_to_death = rexp(100, 0.01),
  time_to_recurrence = rexp(100, 0.02),
  event_death = rbinom(100, 1, 0.7),
  event_recurrence = rbinom(100, 1, 0.5),
  treatment = sample(c("A", "B"), 100, replace = TRUE)
)

# 修改版本
dfSummary_with_km <- function(data, tte_vars, event_vars) {

  # 生成每个变量的摘要信息
  var_summaries <- list()

  for(var_name in names(data)) {
    var_data <- data[[var_name]]

    # 检查是否是TTE变量
    is_tte <- var_name %in% tte_vars
    # 检查是否是事件变量
    is_event <- var_name %in% event_vars

    if(is_tte) {
      # 处理Time-to-Event变量
      event_var <- event_vars[which(tte_vars == var_name)]
      event_data <- data[[event_var]]

      n_missing <- sum(is.na(var_data) | is.na(event_data))

        surv_obj <- Surv(var_data, event_data)
        km_fit <- survfit(surv_obj ~ 1)

        median_surv <- summary(km_fit)$table['median']
        median_surv <- ifelse(is.na(median_surv), 'Not reached', round(median_surv, 2))

        n_events <- sum(event_data, na.rm = TRUE)

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

        # 转换图片为base64
        img_raw <- readBin(temp_file, "raw", file.info(temp_file)$size)
        img_base64 <- base64encode(img_raw)
        unlink(temp_file)

        graph_html <- paste0('<img src="data:image/png;base64,', img_base64, '" alt="KM curve">')

        # 生成统计信息
        stats_text <- paste0(
          "Median time: ", median_surv, "<br>",
          "Events: ", n_events, " (", round(n_events/(nrow(data)-n_missing)*100, 1), "%)<br>",
          "Missing: ", n_missing, " (", round(n_missing/nrow(data)*100, 1), "%)"
        )

        var_summaries[[var_name]] <- list(
          no = length(var_summaries) + 1,
          variable = paste0(var_name, "<br>[time-to-event]"),
          stats = stats_text,
          freqs = '',
          graph = graph_html
        )

    } else if(is_event) {
      n_missing <- sum(is.na(var_data))

      stats_text <- paste0(
        "Event rate: ", round(mean(var_data, na.rm = TRUE)*100, 1), "%<br>",
        "Missing: ", n_missing, " (", round(n_missing/nrow(data)*100, 1), "%)"
      )

      freq_table <- table(var_data, useNA = "no")
      freq_table_ordered <- freq_table[order(names(freq_table))]
      names(freq_table_ordered) <- case_when(names(freq_table_ordered) == '0' ~ 'censored',
                                             names(freq_table_ordered) == '1' ~ '___event',
                                             .default = NA)

      freq_text <- paste0(names(freq_table_ordered), ": ", freq_table_ordered, " (",
                          round(freq_table_ordered/sum(freq_table_ordered)*100, 1), "%)", collapse = "<br>")

      freq_table_ordered <- freq_table[order(names(freq_table), decreasing = TRUE)]
      names(freq_table_ordered) <- case_when(names(freq_table_ordered) == '0' ~ 'censored',
                                             names(freq_table_ordered) == '1' ~ '___event',
                                             .default = NA)
      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = 120, height = 80, res = 96, bg = "white")
      par(mar = c(0, 1.5, 0, 0), cex = 0.5)

      barplot(freq_table_ordered, horiz = TRUE, main = "", xlab = "", ylab = "",
              axes = FALSE, col = "lightgray", border = "gray60",
              names.arg = NA, las = 1, lwd = 0.5)
      dev.off()

      img_raw <- readBin(temp_file, "raw", file.info(temp_file)$size)
      img_base64 <- base64encode(img_raw)
      unlink(temp_file)

      graph_html <- paste0('<img src="data:image/png;base64,', img_base64, '" alt="barplot">')

      var_summaries[[var_name]] <- list(
        no = length(var_summaries) + 1,
        variable = paste0(var_name, "<br>[event indicator]"),
        stats = stats_text,
        freqs = freq_text,
        graph = graph_html
      )

    } else {
      # 处理普通变量
      n_valid <- sum(!is.na(var_data))
      n_missing <- sum(is.na(var_data))

      if(is.numeric(var_data)) {
        # 数值型变量
        mean_val <- round(mean(var_data, na.rm = TRUE), 2)
        sd_val <- round(sd(var_data, na.rm = TRUE), 2)
        min_val <- round(min(var_data, na.rm = TRUE), 2)
        max_val <- round(max(var_data, na.rm = TRUE), 2)

        stats_text <- paste0(
          "Mean (sd): ", mean_val, " (", sd_val, ")<br>",
          "Min < med < max:<br>",
          min_val, " < ", round(median(var_data, na.rm = TRUE), 2), " < ", max_val, "<br>",
          "IQR (CV): ", round(IQR(var_data, na.rm = TRUE), 2), " (", round(sd_val/abs(mean_val), 2), ")<br>",
          "Missing: ", n_missing, " (", round(n_missing/nrow(data)*100, 1), "%)"
        )

        # 创建直方图 - 细边框
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
          freqs = paste0(n_valid, " distinct values"),
          graph = graph_html
        )

      } else if(is.factor(var_data) || is.character(var_data)) {
        # 分类变量
        freq_table <- table(var_data, useNA = "no")
        n_levels <- length(freq_table)

        # 统一排序策略：按频率从大到小排序
        freq_table_ordered <- sort(freq_table, decreasing = TRUE)

        if(n_levels <= 10) {
          # 频率显示和图形都使用相同的排序
          freq_text <- paste0(names(freq_table_ordered), ": ", freq_table_ordered, " (",
                              round(freq_table_ordered/n_valid*100, 1), "%)", collapse = "<br>")
          plot_data <- freq_table_ordered
        } else {
          # 只显示前5个，保持排序一致
          top_5 <- head(freq_table_ordered, 5)
          freq_text <- paste0(
            paste0(names(top_5), ": ", top_5, " (", round(top_5/n_valid*100, 1), "%)", collapse = "<br>"),
            "<br>[ ", n_levels - 5, " others ]"
          )
          plot_data <- top_5
        }

        stats_text <- paste0(
          "1. ", names(freq_table_ordered)[1], "<br>",
          "2. ", if(length(freq_table_ordered) > 1) names(freq_table_ordered)[2] else "---", "<br>",
          "3. ", if(length(freq_table_ordered) > 2) names(freq_table_ordered)[3] else "---", "<br>",
          "Missing: ", n_missing, " (", round(n_missing/nrow(data)*100, 1), "%)"
        )

        # 创建水平条形图 - 使用相同的排序
        temp_file <- tempfile(fileext = ".png")
        png(temp_file, width = 120, height = 80, res = 96, bg = "white")
        par(mar = c(0, 1.5, 0, 0), cex = 0.5)

        barplot(plot_data, horiz = TRUE, main = "", xlab = "", ylab = "",
                axes = FALSE, col = "lightgray", border = "gray60", las = 1, lwd = 0.5)
        dev.off()

        img_raw <- readBin(temp_file, "raw", file.info(temp_file)$size)
        img_base64 <- base64encode(img_raw)
        unlink(temp_file)

        graph_html <- paste0('<img src="data:image/png;base64,', img_base64, '" alt="barplot">')

        var_summaries[[var_name]] <- list(
          no = length(var_summaries) + 1,
          variable = paste0(var_name, "<br>[", class(var_data)[1], "]"),
          stats = stats_text,
          freqs = freq_text,
          graph = graph_html
        )
      }
    }
  }

  # 生成HTML - 表格居中，字体增大
  html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Data Frame Summary</title>
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
            /* 让统计信息列可以换行 */
        }
        .freqs-col {
            /* 让频率列可以换行 */
        }
        .graph-col {
            text-align: center;
            white-space: nowrap;
        }
        img {
            display: block;
            margin: 0 auto;
        }
    </style>
</head>
<body>
    <h1>Data Frame Summary</h1>
    <div class="subtitle">
        ', deparse(substitute(data)), '<br>
        Dimensions: ', nrow(data), ' x ', ncol(data), '<br>
        Duplicates: ', sum(duplicated(data)), '
    </div>

    <table>
        <thead>
            <tr>
                <th class="no-col">No</th>
                <th class="variable-col">Variable</th>
                <th class="stats-col">Stats / Values</th>
                <th class="freqs-col">Freqs (% of Valid)</th>
                <th class="graph-col">Graph</th>
            </tr>
        </thead>
        <tbody>')

  # 添加每个变量的行
  for(var_summary in var_summaries) {
    html_content <- paste0(html_content, '
            <tr>
                <td class="no-col">', var_summary$no, '</td>
                <td class="variable-col">', var_summary$variable, '</td>
                <td class="stats-col">', var_summary$stats, '</td>
                <td class="freqs-col">', var_summary$freqs, '</td>
                <td class="graph-col">', var_summary$graph, '</td>
            </tr>')
  }

  html_content <- paste0(html_content, '
        </tbody>
    </table>
</body>
</html>')

  # 直接在RStudio Viewer中显示
  if(requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    temp_html <- tempfile(fileext = ".html")
    writeLines(html_content, temp_html, useBytes = TRUE)
    rstudioapi::viewer(temp_html)
    message("✅ Summary displayed in RStudio Viewer")
  } else {
    cat(html_content)
  }

  return(invisible(var_summaries))
}

# 使用函数
tte_variables <- c("time_to_death", "time_to_recurrence")
event_variables <- c("event_death", "event_recurrence")

result <- dfSummary_with_km(
  data = sample_data,
  tte_vars = tte_variables,
  event_vars = event_variables
)
