library(data.table)
library(ggplot2)

message('setting config')

config <- rlang::env(
  datatypes = c(Pre = 'pre', post = 'post'),
  age_groups = c('10-13 years', '14-17 years'),
  colors = c('pre' = 'lightblue', 'post' = 'salmon'),
  input_dir = 'csv', lookup_dir = 'csv/lookup',
  parsed_dir = 'csv/parsed', raw_dir = 'csv/raw',
  input_file = 'csv/master.csv', lookup_file = 'csv/lookup/master.csv',
  parsed_input_file = 'csv/master-parsed.csv',
)

## path should be a vector of strings from root dir
config$path <- function(type, path, district = NULL) {
  .dir <- config[[glue::glue("{type}_dir")]]
  stopifnot(!is.null(.dir))

  if (!is.null(district)) {
    paste(c(.dir, path), collapse = '/')
  } else {
    paste(c(.dir, district, path), collapse = '/')
  }
}

config$variables_lookup <- config$path('lookup', 'variables.csv')
config$variables_lookup <- data.table::fread(config$variables_lookup, na.strings = c('', 'NA'))

config$codes_lookup <- config$path('lookup', 'codes.csv')
config$codes_lookup <- data.table::fread(config$codes_lookup, na.strings = c('', 'NA'))
data.table::setkeyv(config$codes_lookup, c('variable', 'code'))

config$variables <- unique(config$variables_lookup$variable)
config$columns <- unique(config$variables_lookup$column)

config$mkdir <- function(type, path, district = NULL) {
  .dir <- config$dirname(type, path, district = district)
  dir.create(.dir, showWarnings = T, recursive = T)
}

config$cache <- hash::hash(
  title = hash::hash(),
  tasks = hash::hash(),
  variables = hash::hash(),
  limits = hash::hash()
)

config$cache_get <- function(type, key) {
  config$cache[[type]][[key]]
}

config$cache_set <- function(type, key, value) {
  config$cache[[type]][[key]] <- value
}

config$variables_lookup_get <- function(pat, startswith = F, endswith = F) {
  pat <- if (startswith) paste0('^', pat) else pat
  pat <- if (endswith) paste0(pat, '$') else pat
  .value <- config$cache_get('variables', pat)

  if (is.null(.value)) {
    .value <- config$variables_lookup[stringr::str_detect(column, pat)]
    config$cache_set('variables', pat, .value)
    .value
  } else {
    .value
  }
}

config$vlookup <- config$variables_lookup_get

config$title <- function(pat) {
  .value <- config$cache_get('title', pat)
  if (!is.null(.value)) {
    .value
  } else {
    .value <- unique(config$variables_lookup_get(pat)$title)
    config$cache_set('title', pat, .value)
    .value
  }
}

config$unabbrev <- function(data) {
  if (is.null(data$description)) {
    message('unabbreviating columns')
    .index <- match(data$column, config$variables_lookup$column)
    .desc <- config$variables_lookup$desc[.index]
    .tasks <- config$variables_lookup$task[.index]
    .tasks <- sapply(seq_along(.tasks), \(.ind) {
      .task <- .tasks[[.ind]]
      if (is.na(.task)) {
        .desc[[.ind]]
      } else {
        .task
      }
    })
    data$description <- .tasks
    data$desc <- .tasks
    data
  } else {
    data
  }
}

config$limits <- function(.variable) {
  .lookup <- config$variables_lookup_get(.variable, startswith = T)
  .start <- unique(.lookup$start_code)
  .end <- unique(.lookup$end_code)
  c(.start, .end)
}

config$decode <- function(data) {
  message('decoding responses')
  if (all(is.na(unique(data$value)))) {
    .codes <- config$codes_lookup[variable %in% unique(data$variable)]
    .values <- list(data$variable, data$code)
    data$value <- config$codes_lookup[.values]$value
    data
  } else {
    data
  }
}

config$limits_filter <- function(data) {
  do.call(
    rbind,
    lapply(config$variables, \(.variable) {
      limits <- config$cache_get('limits', .variable)
      if (is.null(limits)) limits <- config$limits(.variable)
      config$cache_set('limits', .variables, limits)
      data[variable == .variable & code >= limits[[1]] & code <= limits[[2]]]
    })
  )
}

config$plotters <- list(
  '^(like_games|aspirations|early_pregnancy)$' = function(data, variable = NULL, roc = F) {
    data[code == 1] |>
      ggplot(mapping = aes(N, value, fill = type, label = N)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_label(position = position_dodge(0.9), size = 6) +
      ylab("Response: Yes") +
      xlab("% of respondents") +
      scale_color_manual(values = config$colors) +
      config$theme()
  },
  '^(freedom|cons)$' = function(data, variable, roc = F) {
    .ylab <- switch(variable,
                    freedom = 'Freedom of?',
                    cons = 'Consequence')
    if (roc) {
      data[code == 1] |>
        ggplot(mapping = aes(y = description, label = diff)) +
        geom_point(mapping = aes(x = pre, color = 'pre'), size = 10) +
        geom_point(mapping = aes(x = post, color = 'post'), size = 10) +
        geom_segment(
          mapping = aes(
            x = 0, xend = post,
            y = description, yend = description,
            color = 'post'
          ),
          linewidth = 2
        ) +
        geom_label(mapping = aes(x = (pre + post) / 2), size = 5) +
        scale_color_manual(values = config$colors) +
        ylab(.ylab) +
        xlab("% of respondents") +
        config$theme()
    } else {
      data[code == 1] |>
        ggplot(mapping = aes(N, description, fill = type, label = N)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_label(position = position_dodge(0.9), size = 6) +
        ylab(.ylab) +
        xlab("% of respondents") +
        scale_fill_manual(values = config$colors) +
        config$theme()
    }
  },
  '(child_helpline|legal_marriage_age|financial_decisions)' = function(data, variable = NULL, roc = F) {

    .ylab <- switch(
      variable,
      child_helpline = 'Which number?',
      legal_marriage_age = 'Appropriate age (group)',
      financial_decisions = 'Who should make the decision?'
    )

    if (roc) {
      data |>
        ggplot(mapping = aes(y = value, label = diff)) +
        geom_point(mapping = aes(x = pre, color = 'pre'), size = 10) +
        geom_point(mapping = aes(x = post, color = 'post'), size = 10) +
        geom_segment(
          mapping = aes(
            x = 0, xend = post,
            y = value, yend = value,
            color = 'post'
          ),
          linewidth = 2
        ) +
        geom_label(mapping = aes(x = (pre + post) / 2), size = 5) +
        scale_color_manual(values = config$colors) +
        ylab(.ylab) +
        xlab("% of respondents") +
        config$theme()
    } else {
      data |>
        ggplot(mapping = aes(N, value, fill = type, label = N)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_label(position = position_dodge(0.9), size = 6) +
        ylab(.ylab) +
        xlab("% of respondents") +
        config$theme()
    }
  },
  'serving_guests' = function(data, variable = NULL, roc = F) {
    if (roc) {
      data |>
        ggplot(mapping = aes(y = value, label = diff)) +
        geom_point(mapping = aes(x = pre, color = 'pre'), size = 10) +
        geom_point(mapping = aes(x = post, color = 'post'), size = 10) +
        geom_segment(
          mapping = aes(
            x = 0, xend = post,
            y = value, yend = value,
            color = 'post'
          ),
          linewidth = 2
        ) +
        geom_label(mapping = aes(x = (pre + post) / 2), size = 5) +
        scale_color_manual(values = config$colors) +
        ylab("Who should serve the guests?") +
        xlab("% of respondents") +
        config$theme()
    } else {
      data |>
        ggplot(mapping = aes(N, value, fill = type, label = paste0(N, '%'))) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_label(position = position_dodge(0.9), size = 6) +
        scale_fill_manual(values = config$colors) +
        ylab("Who should serve the guests?") +
        xlab("% of respondents") +
        config$theme()
    }
  },
  'chores' = function(data, variable = NULL, roc = F) {
    if (roc) {
      data |>
        ggplot(mapping = aes(y = description, label = paste0(diff, '%'))) +
        geom_point(mapping = aes(x = pre, color = 'pre'), size = 8) +
        geom_point(mapping = aes(x = post, color = 'post'), size = 8) +
        geom_segment(
          mapping = aes(
            x = pre, xend = post,
            y = description, yend = description,
            color = 'pre'
          ),
          linewidth = 2
        ) +
        geom_label(mapping = aes(x = (pre + post) / 2), size = 5, vjust = -1) +
        scale_color_manual(values = config$colors) +
        facet_grid(cols = vars(value)) +
        xlab('% of respondents') +
        ylab('Type of chore') +
        config$theme()
    } else {
      data |>
        ggplot(mapping = aes(N, description, fill = type, label = N)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_label(position = position_dodge(0.9), size = 4, hjust = -0.2) +
        facet_grid(cols = vars(value)) +
        xlab("% of respondents") +
        ylab('Type of chore') +
        config$theme()
    }
  },
  '^(games|prof|social)$' = function(data, variable, roc = F) {
    if (roc) {
      data |>
        ggplot(mapping = aes(x = description, label = paste0(diff, '%'))) +
        geom_point(mapping = aes(description, pre, color = 'pre'), size = 12) +
        geom_point(mapping = aes(description, post, color = 'post'), size = 12) +
        geom_segment(
          mapping = aes(
            x = description, xend = description,
            y = 0, yend = post,
            color = 'post'
          ),
          linewidth = 2
        ) +
        facet_grid(cols = vars(value)) +
        geom_label(mapping = aes(y = (pre + post) / 2), size = 5) +
        ylab("Difference in %") +
        xlab(config$title(variable)) +
        scale_color_manual(values = config$colors) +
        labs(color = 'Test type') +
        config$theme() +
        config$rotate_labels()
    } else {
      data |>
        ggplot(
          mapping = aes(
            description, N,
            label = paste0(as.integer(N), "%"),
            fill = type
          )
        ) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_label(position = position_dodge(0.9), size = 5) +
        ylab("% of respondents") +
        xlab(config$title(variable)) +
        scale_fill_manual(values = c('lightblue', 'salmon')) +
        facet_grid(rows = vars(value)) +
        labs(fill = 'Test type') +
        config$theme() +
        config$rotate_labels()
    }
  }
)

config$plotters_get <- function(variable) {
  .plotter <- config$plotters[[variable]]
  if (!is.null(.plotter)) {
    .plotter
  } else {
    for (.re in names(config$plotters)) {
      if (str_detect(variable, .re)) {
        .plotter <- config$plotters[[.re]]
        break
      }
    }
    .plotter
  }
}

config$theme <- function() {
  theme_bw() +
    theme(
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      axis.title = element_text(size = 18),
      strip.text = element_text(size = 13),
      legend.text = element_text(size = 16),
      legend.position = 'bottom'
    )
}

config$rotate_labels <- function() {
  theme(axis.text.x = element_text(angle = 90))
}

config$write <- function(x, dest) {
  .dirname <- dirname(dest)
  if (!dir.exists(.dirname)) dir.create(.dirname, recursive = T)
  data.table::fwrite(x, dest)
}

config$read <- function(src) {
  if (file.exists(src)) {
    data.table::fread(src)
  }
}
