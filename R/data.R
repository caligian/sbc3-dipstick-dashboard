library(data.table)
library(stringr)
library(glue)
library(purrr)
library(R6)
library(magrittr)

options(error = NULL)

source("R/district.R")
source("R/config.R")

root <- rlang::env()
root$data <- NULL
root$files <- glue('{config$raw_dir}/*.csv') |>
  Sys.glob() |>
  keep(\(.file) !str_detect(.file, 'master'))
root$variables <- NULL
root$columns <- NULL
root$districts <- list()

root$merge <- function(force = F) {
  .do <- \(i) {
    .file <- root$files[[i]]
    .name <- str_replace(basename(.file), "\\.csv", '')
    .type <- if (str_detect(.name, 'post')) 'post' else 'pre'
    .name <- if (.type == 'post') str_replace(.name, '-post', '') else .name
    .root <- District$new(.name, .type)
    .root$read(force = force)
    .root$data
  }

  if (force || !file.exists(config$input_file)) {
    root$data <- root$files |>
      seq_along() |>
      lapply(.do) |>
      do.call(rbind, args = _)
    config$write(root$data, config$input_file)
  }

  if (!is.null(root$data)) {
    root$data
  } else {
    root$data <- fread(config$input_file)
    root$data
  }
}

root$parse <- function() {
  .id_cols <- c('gender', 'age', 'grade', 'age_group', 'district', 'type')
  .measure_cols <- keep(config$columns, \(.col) !(.col %in% .id_cols))
  root$data <- melt(
    root$data,
    id.vars = .id_cols,
    measure.vars = .measure_cols,
    variable.name = 'column',
    value.name = 'code'
  )
  root$data[!str_detect(code, '^[0-9]+$'), code := NA]
  root$data[, code := as.integer(code)]
  root$data <- root$data[!is.na(code) & code <= 9 & !is.na(age) & age > 0 & !is.na(grade) & grade > 0 & !is.na(gender)]
  root$data[, `:=`(
    type = factor(type, c('pre', 'post')),
    variable = str_replace(column, '_[0-9]+[a-zA-Z]$', ''),
    gender = factor(gender, c('Boy', 'Girl')),
    value = rep(NA_character_, nrow(root$data)),
    age_group = factor(age_group, c('10-13 years', '14-17 years'))
  )]
  root$columns <- unique(root$data$column)
  root$variables <- unique(root$data$variable)
  root$data <- config$limits_filter(root$data)
  root$data <- config$unabbrev(root$data)
  root$data <- config$decode(root$data)

  config$write(root$data, config$parsed_input_file)
  root$data
}

root$read <- function(force = F) {
  .raw_master <- file.exists(config$input_file)
  .master <- file.exists(config$parsed_input_file)

  if (force) {
    root$merge(T)
    root$parse()
  } else if (.master) {
    root$data <- fread(config$parsed_input_file)
  } else if (.raw_master) {
    root$data <- fread(config$input_file)
    root$parse()
  } else {
    root$read(force = T)
  }

  root$data
}

root$delete <- function() {
  message(paste0('deleting: ', config$input_file))
  if (file.exists(config$input_file)) file.remove(config$input_file)

  message(paste0('deleting: ', config$parsed_input_file))
  if (file.exists(config$parsed_input_file)) file.remove(config$parsed_input_file)
}

root$write <- function(dest) {
  config$write(root$data, dest)
}

root$grep <- function(...) {
  if (!is.null(root$data)) {
    .pats <- c(...)
    .pattern <- paste0('^(', paste(c(...), collapse = '|'), ')')
    root$data[str_detect(column, .pattern)]
  }
}

root$select <- function(...) {
  if (!is.null(root$data)) {
    cols <- c(...)
    root$data[column %in% cols]
  }
}

root$exists <- function(raw = F) {
  if (raw) {
    file.exists(config$input_file)
  } else {
    file.exists(config$parsed_input_file)
  }
}

root$read()
