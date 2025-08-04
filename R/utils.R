library(purrr)
library(ggplot2)
library(stringr)
library(glue)
library(data.table)

utils <- rlang::env()

utils$total <- function(data, by, index.by) {
  stopifnot(!is.na(match('count', names(data))))
  data <- data[, .(count = sum(count)), by = by]
  setkeyv(data, index.by)
  data
}

utils$count <- function(data, by, age_groups = c(), districts = c()) {
  data <- if (length(age_groups) > 0) data[age_group %in% age_groups] else data
  data <- if (length(districts) > 0) data[district %in% districts] else data
  data[, .(count = .N), by = by]
}

utils$calc_pct <- function(data, total, index_by) {
  .keys <- lapply(index_by, \(.col) data[[.col]])
  total <- total[.keys]
  data$N <- round(100 * (data$count / total$count), 2)
  data
}

utils$pct <- function(data,
                      re.variable = '.+',
                      re.column = '.+',
                      by = NULL,
                      by_total = NULL,
                      by_index = NULL,
                      value.var = 'count',
                      age_groups = c(),
                      districts = c(),
                      .split = NULL,
                      diff = F) {
  data <- if (!is.null(re.variable)) data[str_detect(variable, re.variable)] else data
  data <- if (!is.null(re.column)) data[str_detect(column, re.column)] else data
  data <- utils$count(data, by, age_groups, districts)
  by_total <- if (is.null(by_total)) by[1:length(by)-1] else by_total
  total <- utils$total(data, by_total, by_index)
  data <- utils$calc_pct(data, total, by_total)
  data <- if (diff) utils$diff(data, value.var) else data
  if (!is.null(.split)) split(data, data[[.split]]) else data
}

utils$diff <- function(data, value.var = 'count') {
  .cols <- names(data)
  .formula <- grep('^(count|N|type)$', .cols, invert = T, value = T)
  .formula <- paste0(paste0(.formula, collapse = ' + '), ' ~ type')
  data <- dcast(data, .formula, value.var = value.var)
  data$diff <- round(data$post - data$pre, 2) 
  data
}
