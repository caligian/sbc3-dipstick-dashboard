source("R/config.R")
source("R/utils.R")

library(bslib)
library(shiny)
library(bslib)

ui <- function(root) {
  variables <- config$variables
  names(variables) <- config$variables.lookup$short_desc[match(variables, config$variables.lookup$variable)]
  districts <- unique(root$data$district)
  names(districts) <- sapply(districts, \(.district) {
    if (.district == 'csn') 'CSN' else str_to_title(.district)
  })
  age_groups <- config$age_groups
  .sidebar <- sidebar(
    position = 'right',
    checkboxGroupInput(
      'district', "District",
      choices = districts,
      selected = districts
    ),
    selectInput(
      'age_group', 'Age group',
      choices = age_groups,
      selected = age_groups,
      multiple = T
    ),
    selectInput( 
      'variable', 'Parameter',
      choices = variables,
      selected = 'serving_guests'
    ),
    actionButton('reload', 'Reload data')
  )

  fluidPage(
    page_sidebar(
      sidebar = .sidebar,
      h4(textOutput('title')),
      navset_tab(
        nav_panel("Summary table",
          navset_underline(
            nav_panel("Girl Respondents",
                      br(),
                      DT::DTOutput("girl_table", height = '1000px')),
            nav_panel("Boy Respondents",
                      br(),
                      DT::DTOutput("boy_table", height = '1000px')),
            )
        ),
        nav_panel(
          "in %",
          navset_underline(
            nav_panel("Boy respondents",
                      br(),
                      plotOutput('boy_plot', height = '700px', width = '100%')),
            nav_panel("Girl respondents",
                      br(),
                      plotOutput('girl_plot', height = '700px', width = '100%')),
            )
        ),
        nav_panel(
          "% change",
          navset_underline(
            nav_panel(
              "Boy respondents",
              br(),
              plotOutput('change_boy_plot', height = '700px', width = '100%')
            ),
            nav_panel(
              "Girl respondents",
              br(),
              plotOutput('change_girl_plot', height = '700px', width = '100%')
            )
          )
        )
      )
    )
  )
}
