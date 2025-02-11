#' Interactive Shiny App for Exploring Sleep Cycles
#'
#' This function launches a Shiny app that allows users to interactively explore and modify sleep cycle detection parameters
#' using the Dude algorithm. The app visualizes Non-REM Periods (NREMP) REM Periods (REMP)
#' while allowing users to adjust key parameters that control how sleep cycles are detected.
#'
#' The **Dude algorithm** identifies sleep cycles based on density thresholds
#' and temporal continuity rules. This app allows users to adjust the following NREMP and REMP parameters:
#'
#' - **Density variable**:
#'   The **EEG feature** or **sleep stage combination** used to measure Non-REM density.
#'
#' - **Density threshold**:
#'   A cutoff value (0.1 - 1.0) that defines what constitutes a Non-REM Period.
#'   Increasing this value increase specificity but may lower sensitivity.
#'
#' - **Minimum gap**:
#'   The minimum duration (in epochs) between two separate NREMP/REMPs.
#'   If two NREMPs are closer than this, they are merged into one.
#'
#' - **Minimum size**:
#'   The shortest duration allowed (in epochs) to count as an NREMP/REMP.
#'
#' ## **Requirements**
#' - The input **must be a `SleepCycle` object** created using the **Dude algorithm**.
#'
#' @param sleepcycles_obj A **`SleepCycle` object** created using `sleepcycles_from_hypnogram()` with `method = "dude"`.
#'
#' @return A Shiny app that runs interactively for adjusting Dude algorithm parameters.
#' @export
#' @examples
#' if (interactive()) {
#'   data("hypnogram_grouped")
#'   run_sleepcycles_app(
#'    sleepcycles_from_hypnogram(
#'      hypnogram_grouped,
#'      epoch_col = "epoch",
#'      stage_col = "stage",
#'      id_col = "id",
#'      verbose = FALSE
#'    )
#'  )
#' }
run_sleepcycles_app <- function(sleepcycles_obj) {

  if (missing(sleepcycles_obj)) {
    stop("Please provide a `SleepCycle` object.")
  }

  if (!inherits(sleepcycles_obj, "SleepCycle")) {
    stop("Invalid input: Expected a `SleepCycle` object.")
  }

  if (sleepcycles_obj$info$method != "dude") {
    stop("This shiny app is only available for the `dude` algorithm.")
  }

  data <- sleepcycles_obj
  id_col_present <- !is.null(data$info$id_col)
  nremp_opts <- data$info$method_opts$NREMP
  remp_opts <- data$info$method_opts$REMP

  ui <- shiny::fluidPage(
    shiny::titlePanel("Sleep Cycles"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::conditionalPanel(
          condition = "output.id_col_present == true",
          shiny::selectInput(
            inputId = "id",
            label = "ID",
            choices = if (id_col_present) unique(data$epoch[[data$info$id_col]]) else NULL
          )
        ),
        shiny::tags$h4("NREMP Parameters"),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::selectInput(
              "nremp_density_col",
              "Density Column",
              choices = data$info$method_opts$density_levels,
              selected = nremp_opts$density_col
            )
          ),
          shiny::column(
            width = 6,
            shiny::numericInput(
              "nremp_min_gap",
              "Minimum Gap",
              min = 5,
              max = 50,
              value = nremp_opts$min_gap,
              step = 1
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::numericInput(
              "nremp_threshold",
              "Threshold",
              min = .1,
              max = 1,
              value = nremp_opts$threshold,
              step = .1
            )
          ),
          shiny::column(
            width = 6,
            shiny::numericInput(
              "nremp_min_size",
              "Minimum Size",
              min = 5,
              max = 50,
              value = nremp_opts$min_size,
              step = 1
            )
          )
        ),
        shiny::tags$h4("REMP Parameters"),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::selectInput(
              "remp_density_col",
              "Density Column",
              choices = data$info$method_opts$density_levels,
              selected = remp_opts$density_col
            )
          ),
          shiny::column(
            width = 6,
            shiny::numericInput(
              "remp_threshold",
              "Threshold",
              min = .1,
              max = 1,
              value = remp_opts$threshold,
              step = .1
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::numericInput(
              "remp_min_gap",
              "Minimum Gap",
              min = 5,
              max = 50,
              value = remp_opts$min_gap,
              step = 1
            )
          ),
          shiny::column(
            width = 6,
            shiny::numericInput(
              "remp_min_size",
              "Minimum Size",
              min = 5,
              max = 50,
              value = remp_opts$min_size,
              step = 1
            )
          )
        ),
        shiny::sliderInput(
          "plot_width",
          "Plot Width (px):",
          min = 500,
          max = 1500,
          value = 1100,
          step = 50
        ),
        shiny::sliderInput(
          "plot_height",
          "Plot Height (px):",
          min = 500,
          max = 1500,
          value = 800,
          step = 50
        )
      ),
      shiny::mainPanel(
        shiny::plotOutput("summary_plot", height = "auto", width = "auto")
      )
    )
  )

  server <- function(input, output, session) {

    output$id_col_present <- shiny::reactive({ id_col_present })
    shiny::outputOptions(output, "id_col_present", suspendWhenHidden = FALSE)

    newdata <- shiny::reactive({
      if (id_col_present) {
        shiny::req(input$id)
        data_sub <- get_id(data, .id = input$id)
      } else {
        data_sub <- data
      }
      data_sub$epoch |>
        sleepcycles_from_hypnogram(
          epoch_col = data$info$epoch_col,
          stage_col = data$info$stage_col,
          method = "dude",
          options = list(
            "NREMP" = list(
              "density_col" = input$nremp_density_col,
              "threshold" = input$nremp_threshold,
              "min_gap" = input$nremp_min_gap,
              "min_size" = input$nremp_min_size
            ),
            "REMP" = list(
              "density_col" = input$remp_density_col,
              "threshold" = input$remp_threshold,
              "min_gap" = input$remp_min_gap,
              "min_size" = input$remp_min_size
            ),
            "kernel" = data$info$method_opts$kernel,
            "combos" = data$info$method_opts$combos
          )
        )
    })

    output$summary_plot <- shiny::renderPlot({
      if (!is.null(newdata())) {
        plot_summary(newdata(), id = if (id_col_present) input$id else NULL)
      }
    }, height = shiny::reactive(input$plot_height), width = shiny::reactive(input$plot_width))
  }

  shiny::shinyApp(ui, server)
}
