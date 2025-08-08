library(shiny)
library(shinybusy)
library(DT)
library(data.table)
library(stringr)
library(glue)

source("R/utils.R")
source("R/config.R")

server_plot <- function(data, .gender, variable, roc = F) {
  .plotter <- config$plotters_get(variable)
  data <- data[gender == .gender]
  if (!is.null(.plotter)) {
    .plotter(data, variable = variable, roc = roc)
  }
}

server_data <- function(
  root, variable,
  age_groups = NULL,
  districts = NULL
) {
  utils$pct(
    root$data,
    variable_pattern = paste0('^', variable),
    age_groups = age_groups,
    districts = districts,
    by = c('type', 'gender', 'variable', 'column', 'code', 'value', 'description'),
    by_index = c('type', 'gender', 'variable', 'column'),
    by_total = c('type', 'gender', 'variable', 'column')
  )
}

server_table <- function(data, .gender) {
  data <- data[gender == .gender]
  .names <- names(data)
  .formula <- grep(
    '^(gender|type|N|count)$', .names,
    value = T, invert = T
  )
  .formula <- paste0(paste(.formula, collapse = ' + '), " ~ type")
  data <- dcast(data, .formula, value.var = 'N')
  data <- data[order(variable, code)]
  data <-
    if (length(unique(data$column)) == 1) {
      data[, -c('variable', 'column', 'description')]
    } else {
      data
    }
  data <-
    if (!is.na(match('description', names(data)))) {
      data[["Survey Question"]] <- data$description
      data
    } else {
      data
    }
  data[["Given response"]] <- data$value
  data[["Pre (in % of responses)"]] <- data$pre
  data[["Post (in % of responses)"]] <- data$post
  data[["Difference (in %)"]] <- round(data$post - data$pre, 2)
  data[, -c('value', 'pre', 'post', 'code', 'variable', 'column', 'description')]
}

server <- function(root) {
  function(input, output, session) {
    session$onSessionEnded(stopApp)

    data <- reactive({
      req(input$variable)
      server_data(
        root, input$variable,
        age_groups = input$age_group,
        districts = input$district
      )
    })

    data_diff <- reactive({
      utils$diff(data(), value.var = 'N')
    })

    ## Change title according to variable
    observeEvent(input$variable, {
      req(input$variable)
      output$title <- renderText(config$title(input$variable))
    })

    ## Reload data
    observeEvent(input$reload, {
      show_modal_spinner(text = "Reloading data. Please wait")
      root$read(force = T)
      remove_modal_spinner()
    })

    ## Make normal plots
    output$girl_plot <- renderPlot({
      server_plot(data(), 'Girl', input$variable, roc = F)
    })

    output$boy_plot <- renderPlot({
      server_plot(data(), 'Boy', input$variable, roc = F)
    })

    ## Make roc plots
    output$change_girl_plot <- renderPlot({
      if (!(input$variable %in% c('like_games', 'aspirations', 'early_pregnancy'))) {
        server_plot(data_diff(), 'Girl', input$variable, roc = T)
      } else {
        server_plot(data(), 'Girl', input$variable, roc = F)
      }
    })

    output$change_boy_plot <- renderPlot({
      if (!(input$variable %in% c('like_games', 'aspirations', 'early_pregnancy'))) {
        server_plot(data_diff(), 'Boy', input$variable, roc = T)
      } else {
        server_plot(data(), 'Boy', input$variable, roc = F)
      }
    })

    ## Make tables
    output$girl_table <- renderDT({
      server_table(data(), 'Girl')
    })

    output$boy_table <- renderDT({
      server_table(data(), 'Boy')
    })
  }
}
