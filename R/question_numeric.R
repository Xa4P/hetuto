question_numeric <- function(
  text,
  ...,
  correct = "Correct!",
  incorrect = "Incorrect",
  try_again = incorrect,
  allow_retry = FALSE,
  value = NULL,
  min = NA,
  max = NA,
  step = NA,
  options = list()
) {
  min  <- min  %||% NA_real_
  max  <- max  %||% NA_real_
  step <- step %||% NA_real_

  checkmate::assert_numeric(value, len = 1, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_numeric(min, len = 1, null.ok = FALSE)
  checkmate::assert_numeric(max, len = 1, null.ok = FALSE)
  checkmate::assert_numeric(step, len = 1, null.ok = FALSE, lower = 0, finite = TRUE)

  learnr::question(
    text = text,
    ...,
    type = "learnr_numeric",
    correct = correct,
    incorrect = incorrect,
    allow_retry = allow_retry,
    random_answer_order = FALSE,
    options = modifyList(
      options,
      list(
        value = value,
        min = min,
        max = max,
        step = step
      )
    )
  )
}



#' @export
question_ui_initialize.learnr_numeric <- function(question, value, ...) {
  numericInput(
    question$ids$answer,
    label = question$question,
    value = value,
    min = question$options$min,
    max = question$options$max,
    step = question$options$step
  )
}

#' @export
question_is_valid.learnr_numeric <- function(question, value, ...) {
  if (is.null(value)) {
    return(FALSE)
  }
  value <- suppressWarnings(as.numeric(value))
  !is.na(value)
}

#' @export
question_is_correct.learnr_numeric <- function(question, value, ...) {
  value <- suppressWarnings(as.numeric(value))

  if (length(value) == 0 || is.na(value)) {
    showNotification("Please enter a number before submitting", type = "error")
    req(value)
  }

  for (ans in question$answers) {
    ans_val <- ans$value
    if (isTRUE(all.equal(ans_val, value, tolerance = 1e-10))) {
      return(mark_as(
        ans$correct,
        ans$message
      ))
    }
  }

  if (!is.na(question$options$min) && value < question$options$min) {
    return(mark_as(FALSE, paste0("The number is at least ", question$options$min, ".")))
  }
  if (!is.na(question$options$max) && value > question$options$max) {
    return(mark_as(FALSE, paste0("The number is at most ", question$options$max, ".")))
  }

  mark_as(FALSE, NULL)
}
