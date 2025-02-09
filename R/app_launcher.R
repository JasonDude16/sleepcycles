#' Interactive Shiny App for Exploring SleepCycle Objects
#'
#' This function launches a Shiny app that allows users to **interactively explore and modify sleep cycle detection parameters**
#' using the **Dude algorithm**. The app visualizes **Non-REM Periods (NREMP)** and **REM Periods (REMP)**
#' while allowing users to adjust key parameters that control how sleep cycles are detected.
#'
#' ## **Dude Algorithm Parameters**
#' The **Dude algorithm** (Density-Based Sleep Cycle Detection) identifies sleep cycles based on **density thresholds**
#' and **temporal continuity rules**. This app allows users to adjust the following parameters:
#'
#' - **`NREMP Density Column` (`nremp_density_col`)**:
#'   The **EEG feature** or **sleep stage combination** used to measure Non-REM density.
#'   (Options depend on `sleepcycles_obj$info$method_opts$density_levels`).
#'
#' - **`NREMP Threshold` (`nremp_threshold`)**:
#'   A **cutoff value (0.1 - 1.0)** that defines what constitutes a Non-REM Period.
#'   Increasing this value **reduces false positives** but may exclude shorter NREMPs.
#'
#' - **`NREMP Minimum Gap` (`nremp_min_gap`)**:
#'   The **minimum duration (in epochs) between two separate NREMPs**.
#'   If two NREMPs are closer than this, they are **merged into one**.
#'
#' - **`NREMP Minimum Size` (`nremp_min_size`)**:
#'   The **smallest possible NREMP duration (in epochs)**.
#'   Shorter NREMPs are **filtered out** if they do not meet this criterion.
#'
#' - **`REMP Density Column` (`remp_density_col`)**:
#'   The **EEG feature** or **sleep stage combination** used to measure REM density.
#'   (Options depend on `sleepcycles_obj$info$method_opts$density_levels`).
#'
#' - **`REMP Threshold` (`remp_threshold`)**:
#'   A **cutoff value (0.1 - 1.0)** that defines what constitutes a REM Period.
#'   Increasing this value **filters out weak REM signals** but may exclude **short REM episodes**.
#'
#' - **`REMP Minimum Gap` (`remp_min_gap`)**:
#'   The **minimum duration (in epochs) between two separate REMPs**.
#'   If two REMPs are closer than this, they are **merged into one**.
#'
#' - **`REMP Minimum Size` (`remp_min_size`)**:
#'   The **smallest possible REMP duration (in epochs)**.
#'   Shorter REMPs are **filtered out** if they do not meet this criterion.
#'
#' ## **How to Use the Shiny App**
#' - Run the function interactively:
#'   ```r
#'   if (interactive()) {
#'     sleepcycles_shiny_app(sleepcycles_obj)
#'   }
#'   ```
#' - Modify **NREMP and REMP parameters** using the sidebar panel.
#' - Adjust **plot dimensions** dynamically.
#' - If the data contains multiple subjects, select an **`ID`** from the dropdown.
#'
#' ## **Requirements**
#' - The input **must be a `SleepCycle` object** created using the **Dude algorithm**.
#' - The function **will throw an error** if:
#'   - The input is missing.
#'   - The input is **not a `SleepCycle` object**.
#'   - The `SleepCycle` object **was not generated using the `dude` method**.
#'
#' @param sleepcycles_obj A **`SleepCycle` object** created using `sleepcycles_from_hypnogram()` with `method = "dude"`.
#'
#' @return A **Shiny app** that runs interactively for adjusting **Dude algorithm parameters**.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   sleepcycles_shiny_app(sleepcycles_obj)
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
        data_sub <- get_id(data, id = input$id)
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
            )
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
