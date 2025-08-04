library(data.table)
library(stringr)
library(glue)
library(purrr)
library(R6)

options(error = NULL)

District <- R6Class(
  'District',
  public = list(
    district = NULL,
    name = NULL,
    src = NULL,
    parsed_src = NULL,
    dest = NULL,
    data = NULL,
    type = NULL,
    columns = NULL,
    is_long = FALSE,
    initialize = function(district, .type = 'pre') {
      self$name <- district
      self$type <- .type
      self$district <- self$name
      self$src <- if (.type == 'pre') {
                    glue("csv/raw/{self$name}.csv")
                  } else {
                    glue("csv/raw/{self$name}-post.csv")
                  }
      self$parsed_src <- if (.type == 'pre') {
                        glue("csv/parsed/{self$name}.csv")
                      } else {
                        glue("csv/parsed/{self$name}-post.csv")
                      }
      self$dest <- glue("output/{self$name}")

      stopifnot(file.exists(self$src))

      if (!dir.exists(self$dest)) {
        dir.create(self$dest)
      } else {
        system(
          glue("rm -r {self$dest}"),
          ignore.stdout = T,
          ignore.stderr = T
        )
        dir.create(self$dest)
      }
    },
    melt = function() {
      if (self$is_long) {
        self$data
      } else {

        .id_cols <- c(
          'gender', 'age',
          'grade', 'age_group',
          'district', 'type'
        )
        .measure_cols <- keep(self$columns, \(.col) {
          !(.col %in% .id_cols)
        })
        self$data <- melt(
          self$data,
          id.vars = .id_cols,
          measure.vars = .measure_cols,
          variable.name = 'column',
          value.name = 'code'
        )
        self$data$code <- sapply(self$data$code, \(x) {
          if (is.na(x)) {
            NA
          } else {
            x <- as.character(x)
            if (str_detect(x, '^[0-9]+$')) {
              as.integer(x)
            } else {
              NA
            }
          }
        })
        self$data <- self$data[!is.na(code) & as.integer(code) <= 9]
        self$data <- self$data[!is.na(age) & age > 0 & !is.na(grade) & grade > 0]
        self$data <- self$data[!is.na(gender)]
        self$is_long <- TRUE

        fwrite(self$data, self$parsed_src)
        self$data
      }
    }, 
    read = function(force = F) {
      .read <- function() {
        message(glue('reading: {self$src}'))

        data <- fread(self$src)
        self$columns <- colnames(data)
        data <- data[, -c('uid', 'school')]
        cols <- colnames(data)
        size <- nrow(data)
        data$gender <- factor(
          ifelse(data$gender == 1, 'Boy', 'Girl'),
          levels = c('Boy', 'Girl')
        )
        data$age_group <- factor(
          ifelse(data$age < 14, '10-13 years', '14-17 years'),
          levels = c('10-13 years', '14-17 years')
        )
        data$type <- factor(
          rep(self$type, size),
          levels = c('pre', 'post')
        )
        data$grade <- sapply(data$grade, \(.grade) {
          if (is.na(.grade)) {
            NA
          } else if (str_detect(.grade, '[0-9]')) {
            str_match(.grade, '[0-9]+')[[1]]
          } else {
            NA
          }
        })
        data$district <- rep(self$name, size)
        data
      }
      self$data <- if (force || !file.exists(self$parsed_src)) {
                     .read()
                   } else {
                     fread(self$parsed_src)
                   }
      self$columns <- colnames(self$data)
      if (!file.exists(self$parsed_src)) fwrite(self$data, self$parsed_src)
      self$data
    }
  )
)
