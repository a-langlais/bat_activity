#' Read a TeensyRecorder microphone test
#'
#' Reads the CSV output from an extended TeensyRecorder microphone test and
#' returns a tidy object suitable for plotting.
#'
#' @param file_path Path to the microphone test CSV file.
#'
#' @return A list with `title`, `quality`, and `data` elements.
#' @export
read_microphone_test <- function(file_path) {
  if (missing(file_path) || is.null(file_path) || !nzchar(file_path)) {
    stop("file_path must be provided.", call. = FALSE)
  }
  if (!file.exists(file_path)) {
    stop("file_path does not exist.", call. = FALSE)
  }

  raw_data <- tryCatch(
    utils::read.csv(file_path, header = FALSE),
    error = function(error) {
      stop("Unable to read file_path as CSV: ", conditionMessage(error), call. = FALSE)
    }
  )
  if (nrow(raw_data) < 2) {
    stop("file_path does not contain enough rows for a microphone test.", call. = FALSE)
  }

  if (nrow(raw_data) > 6) {
    raw_data <- raw_data[(nrow(raw_data) - 5):nrow(raw_data), , drop = FALSE]
  }

  title <- paste(raw_data[1, 1], "from", raw_data[1, 2], "at", raw_data[1, 3])
  quality <- paste("Quality:", raw_data[1, 4])
  values <- raw_data[-1, , drop = FALSE]
  values <- as.data.frame(t(values), stringsAsFactors = FALSE)
  rownames(values) <- NULL
  names(values) <- c("channel", "signal", "template_min", "template_max", "silence")
  values <- values[-1, , drop = FALSE]
  values[] <- lapply(values, function(column) suppressWarnings(as.numeric(column)))

  list(
    title = title,
    quality = quality,
    data = values
  )
}

#' Plot a TeensyRecorder microphone test
#'
#' @param test Object returned by `read_microphone_test()`.
#'
#' @return A ggplot object.
#' @export
plot_microphone_test <- function(test) {
  if (!is.list(test) || !is.data.frame(test$data)) {
    stop("test must be the result of read_microphone_test().", call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required to plot microphone tests.", call. = FALSE)
  }

  plot_data <- stats::reshape(
    test$data,
    varying = c("signal", "template_min", "template_max", "silence"),
    v.names = "level",
    timevar = "curve",
    times = c("Signal", "Template min", "Template max", "Silence"),
    direction = "long"
  )
  rownames(plot_data) <- NULL

  ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "channel", y = "level", color = "curve")) +
    ggplot2::geom_line() +
    ggplot2::ggtitle(test$title, test$quality) +
    ggplot2::labs(x = "Channels (kHz)", y = "Level (dB)", color = "Curve") +
    ggplot2::scale_color_manual(
      values = c(
        "Signal" = "blue",
        "Template min" = "red",
        "Template max" = "green",
        "Silence" = "black"
      )
    ) +
    ggplot2::theme_classic()
}
