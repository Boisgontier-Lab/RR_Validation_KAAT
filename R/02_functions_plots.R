#' Residual diagnostics for model validation
#'
#' Generates diagnostic plots to assess whether a Gaussian model on raw reaction times (RTs) is acceptable.
#' Specifically, it produces:
#' - A QQ-plot of residuals to check normality.
#' - A plot of residuals versus fitted values to detect trends or heteroscedasticity.
#'
#' @param m A fitted model object (e.g., from \code{lmer}).
#' @param label A string used as a label for the plots (e.g., model name).
#'
#' @return No return value. Plots are displayed in the graphics window.
#' @examples
#' diagnose_residuals(my_model, "Pilot model")
diagnose_residuals <- function(m, label){
  op <- par(no.readonly = TRUE)
  par(mfrow = c(1, 2))
  
  # 1) QQ-plot of residuals
  qqnorm(resid(m), main = paste0("QQ-plot résidus — ", label))
  qqline(resid(m))
  
  # 2) Residuals vs fitted
  plot(
    fitted(m), resid(m),
    xlab = "Valeurs ajustées (fitted)",
    ylab = "Résidus",
    main = paste0("Résidus vs fitted — ", label)
  )
  abline(h = 0, lty = 2)
  
  par(op)
}

#' Plot the power curve with exact sample size annotation
#'
#' This function plots the power curve for a sequence of sample sizes (step 5),
#' including 95% confidence intervals if available. It highlights the target power threshold
#' and annotates the exact sample size required to reach the desired power.
#'
#' @param power_summary Data frame. Summary table with sample sizes and power estimates.
#' @param n_exact Integer or NA. Exact sample size required for target power (if available).
#' @param stim_label Character. Label for the target stimulus type (e.g., "PA" or "SED").
#' @param target_power Numeric. Desired power threshold (default: 0.90).
#' @return None. The function produces a plot.
#' @examples
#' plot_power_curve_step5_with_Nexact(power_summary, n_exact, "SED", target_power = 0.90)
plot_power_curve_step5_with_Nexact <- function(
    power_summary,
    n_exact,
    stim_label,
    target_power = 0.90
){
  
  if(!("subjects" %in% names(power_summary)) && ("nlevels" %in% names(power_summary))){
    power_summary$subjects <- power_summary$nlevels
  }
  if(!("power" %in% names(power_summary)) && ("mean" %in% names(power_summary))){
    power_summary$power <- power_summary$mean
  }
  stopifnot("subjects" %in% names(power_summary), "power" %in% names(power_summary))
  
  power_summary <- power_summary[order(power_summary$subjects), ]
  
  N_min <- min(power_summary$subjects)
  N_max <- max(power_summary$subjects)
  N_seq5 <- seq(N_min, N_max, by = 5)
  
  tab5 <- power_summary[power_summary$subjects %in% N_seq5, , drop=FALSE]
  if(nrow(tab5) < 2) tab5 <- power_summary
  
  plot(
    x = tab5$subjects,
    y = tab5$power,
    type = "b", pch = 19,
    xlab = "Number of participants (N)",
    ylab = "Power",
    ylim = c(0, 1),
    main = paste0("Power curve — ", stim_label, " vs NEU (interaction effect)")
  )
  
  if(all(c("lower","upper") %in% names(tab5))){
    lines(tab5$subjects, tab5$lower, lty = 2)
    lines(tab5$subjects, tab5$upper, lty = 2)
  }
  
  abline(h = target_power, lty = 3)
  
  if(!is.na(n_exact)){
    abline(v = n_exact, lty = 3)
    points(n_exact, target_power, pch = 19)
    
    segments(
        x0 = n_exact,
        y0 = 0,
        x1 = n_exact,
        y1 = target_power,
        lty = 3
    )
    segments(
        x0 = par("usr")[1],
        y0 = target_power,
        x1 = n_exact,
        y1 = target_power,
        lty = 3
    )
    
    text(
      x = n_exact,
      y = target_power,
      labels = paste0("N = ", n_exact),
      pos = 4
    )
  }
  
  legend(
    "bottomright",
    legend = c(
        "Power (step 5)",
        "IC 95% (lower/upper)",
        paste0("Seuil ", target_power * 100, "%"),
        "N exact"
    ),
    lty = c(1, 2, 3, 3),
    pch = c(19, NA, NA, 19),
    bty = "n"
  )
}