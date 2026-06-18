## ----seed, echo=FALSE---------------------------------------------------------
set.seed(42)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
options(tibble.width = Inf, knitr.kable.NA = "")

## ----libraries, message=FALSE-------------------------------------------------
library(fable.intermittent)

## ----data---------------------------------------------------------------------
data(auto)
idx <- paste0("TS", sample(3000, 200))
data <- auto |>
  dplyr::filter(series_id %in% idx)

## ----data_tab, echo=FALSE, results='asis'-------------------------------------
knitr::kable(head(data), caption = "First observations of the data subset")

## ----ts_plot, echo=FALSE, fig.cap="Example time series from the auto data set", fig.width=7, fig.height=4----
one_series <- fable.intermittent::auto |> dplyr::filter(series_id == idx[1])
fabletools::autoplot(one_series, value) +
  ggplot2::labs(title = paste("Time series", idx[1]), x = "Month", y = "Demand")

## ----fit, echo=TRUE-----------------------------------------------------------
fit <- data |>
  dplyr::filter(index <= tsibble::yearmonth("2011 June")) |>
  fabletools::model(
    betanbb = BETANBB(value),
    staticdistr = STATICDISTR(value),
    twees = TWEES(value),
    wss = WSS(value)
  )

## ----fit_tab, echo=FALSE, results='asis'--------------------------------------
knitr::kable(
  head(fit) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(where(is.list), ~vapply(.x, function(m) model_sum(m)[[1]], character(1)))),
  caption = "Fitted models"
)

## ----fc, echo=TRUE------------------------------------------------------------
fc <- fit |>
  fabletools::forecast(h = "6 months")

## ----fc_tab, echo=FALSE, results='asis'---------------------------------------
knitr::kable(head(fc), caption = "Forecasts for the next 6 months")

## ----accuracy, echo=TRUE------------------------------------------------------
results <- fc |>
  fabletools::accuracy(data, measures = list(
    RMSSE = fabletools::RMSSE, 
    pinball_loss = fabletools::pinball_loss
    )) |>
  dplyr::group_by(.model) |>
  dplyr::summarise(
    RMSSE = mean(RMSSE), 
    pinball_loss  = mean(pinball_loss)
    )

## ----accuracy_tab, echo=FALSE, results='asis'---------------------------------
knitr::kable(results, digits = 3, caption = "Forecast accuracy metrics")

## ----fc_plot, echo=FALSE, fig.cap="Example forecast for one time series", fig.width=7, fig.height=4----
one_fc <- fc |> dplyr::filter(series_id == idx[1])
one_series_plot <- dplyr::as_tibble(one_series) |>
  dplyr::mutate(index_date = as.Date(index))
one_fc_q <- one_fc |>
  dplyr::group_by(.model) |>
  dplyr::mutate(q90 = stats::quantile(value, p = 0.9)) |>
  dplyr::ungroup() |>
  dplyr::mutate(index_date = as.Date(index))

ggplot2::ggplot() +
  ggplot2::geom_line(
    data = one_series_plot,
    ggplot2::aes(x = index_date, y = value),
    colour = "black"
  ) +
  ggplot2::geom_line(
    data = one_fc_q,
    ggplot2::aes(x = index_date, y = .mean, colour = .model, linetype = "mean", group = .model)
  ) +
  ggplot2::geom_line(
    data = one_fc_q,
    ggplot2::aes(x = index_date, y = q90, colour = .model, linetype = "q90", group = .model)
  ) +
  ggplot2::scale_linetype_manual(
    name = "Forecast",
    values = c(mean = "solid", q90 = "dashed"),
    labels = c("mean", "q90")
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(title = "Model"),
    linetype = ggplot2::guide_legend(override.aes = list(colour = "black"))
  ) +
  ggplot2::labs(title = paste("Forecast for", idx[1]), x = "Month", y = "Demand")

