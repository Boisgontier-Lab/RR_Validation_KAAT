#' Generate a balanced simulated dataset for power analysis
#'
#' This function creates a balanced experimental design consistent with the protocol,
#' for use in simulation-based power analysis (e.g., with simr).
#' For each subject, it generates:
#'   - 96 neutral trials (48 Approach, 48 Avoid) distributed across 20 neutral items
#'   - 48 target trials (24 Approach, 24 Avoid) distributed across 5 target items
#' The items are randomly sampled for each condition and subject.
#' All variables are coded as factors, with -0.5/+0.5 contrasts for Movement and StimulusType.
#' The function checks that the total number of rows is 144 * n_subjects.
#'
#' @param n_subjects Integer. Number of subjects to simulate.
#' @param stim_label Character. Label for the target stimulus type (e.g., "PA" or "SED").
#' @return A data frame with the balanced design, ready for power simulation.
#' @examples
#' df <- make_design_balanced(90, "SED")
make_design_balanced <- function(n_subjects, stim_label){
  
  neutral_item_levels <- paste0("NEU_", 1:20)
  target_item_levels  <- paste0("TGT_", 1:5)
  
  subject_dataframes <- vector("list", n_subjects)
  
  for(i in seq_len(n_subjects)){
    
    neutral_approach_items <- sample(rep(neutral_item_levels, length.out = 48))
    neutral_avoid_items    <- sample(rep(neutral_item_levels, length.out = 48))
    
    neutral_trials <- rbind(
      data.frame(
        subject = i,
        Movement = "Approach",
        StimulusType = "NEU",
        item = neutral_approach_items
      ),
      data.frame(
        subject = i,
        Movement = "Avoid",
        StimulusType = "NEU",
        item = neutral_avoid_items
      )
    )
    
    target_approach_items <- sample(rep(target_item_levels, length.out = 24))
    target_avoid_items    <- sample(rep(target_item_levels, length.out = 24))
    
    target_trials <- rbind(
      data.frame(
        subject = i,
        Movement = "Approach",
        StimulusType = stim_label,
        item = target_approach_items
      ),
      data.frame(
        subject = i,
        Movement = "Avoid",
        StimulusType = stim_label,
        item = target_avoid_items
      )
    )
    
    subject_trials <- rbind(neutral_trials, target_trials)
    subject_trials <- subject_trials[sample.int(nrow(subject_trials)), ]
    subject_dataframes[[i]] <- subject_trials
  }

  design <- bind_rows(subject_dataframes)

  design$subject      <- factor(design$subject)
  design$Movement     <- factor(design$Movement, levels=c("Approach","Avoid"))
  design$StimulusType <- factor(design$StimulusType, levels=c("NEU", stim_label))
  design$item         <- factor(design$item)

  contrasts(design$Movement)     <- matrix(c(-0.5, 0.5), ncol=1)
  contrasts(design$StimulusType) <- matrix(c(-0.5, 0.5), ncol=1)

  stopifnot(nrow(design) == 144 * n_subjects)

  design
}

#' Identify the interaction term name in model fixed effects
#'
#' This helper function searches for the interaction term between Movement and StimulusType
#' in the fixed effects of a linear mixed model. It returns the exact term name as used in the model.
#' If the interaction term is not uniquely identified, the function stops with an error.
#'
#' @param model A fitted linear mixed model (e.g., from lmer).
#' @return Character string. The name of the interaction term in the model.
#' @examples
#' interaction_term <- get_interaction_term(model)
get_interaction_term <- function(model){
  interaction_term <- grep("Movement.*StimulusType", names(fixef(model)), value=TRUE)
  if(length(interaction_term) != 1){
    stop("Interaction term not uniquely identified. Found: ", paste(interaction_term, collapse=", "))
  }
  interaction_term
}

#' Run a power simulation for a linear mixed model using simr
#'
#' This function performs a power analysis for a given pilot linear mixed model,
#' using a balanced simulated dataset and the simr package. It generates the design,
#' simulates the model, identifies the interaction term, and computes power using
#' the Satterthwaite t-test (via lmerTest).
#'
#' @param pilot_model A fitted pilot linear mixed model (from lmer).
#' @param stim_label Character. Label for the target stimulus type (e.g., "PA" or "SED").
#' @param n_subjects Integer. Number of subjects to simulate (default: 90).
#' @param n_simulations Integer. Number of simulations to run (default: 1000).
#' @param seed Integer. Random seed for reproducibility (default: 123).
#' @return A simr powerSim object containing the power estimate and confidence interval.
#' @examples
#' power_result <- run_power_simr(pilot_model, "SED", n_subjects = 90, n_simulations = 1000, seed = 123)
run_power_simr <- function(
    pilot_model,
    stim_label,
    n_subjects = 90,
    n_simulations = 1000,
    seed = 123
){
  
  set.seed(seed)
  
  design_data <- make_design_balanced(n_subjects = n_subjects, stim_label = stim_label)
  
  simulated_model <- makeLmer(
    formula(pilot_model),
    fixef(pilot_model),
    VarCorr(pilot_model),
    sigma(pilot_model),
    data = design_data
  )
  
  interaction_term <- get_interaction_term(simulated_model)
  
  powerSim(
    simulated_model,
    test = fixed(interaction_term, method="t"),  # Satterthwaite via lmerTest
    nsim = n_simulations,
    fitOpts = list(control = lmerControl(optimizer="bobyqa"))
  )
}

#' Run a power curve simulation for a linear mixed model using simr
#'
#' This function computes a power curve for a given pilot linear mixed model,
#' based on a balanced simulated dataset and the simr package. It generates the design,
#' simulates the model, extends it to different sample sizes, and estimates power
#' for the interaction effect across a sequence of participant numbers.
#' The Satterthwaite t-test (via lmerTest) is used for significance testing.
#' The function prints the summary and plots the power curve.
#'
#' @param pilot_model A fitted pilot linear mixed model (from lmer).
#' @param stim_label Character. Label for the target stimulus type (e.g., "PA" or "SED").
#' @param sample_sizes Integer vector. Sequence of sample sizes (number of subjects) to test.
#' @param n_simulations Integer. Number of simulations per sample size (default: 1000).
#' @param seed Integer. Random seed for reproducibility (default: 2001).
#' @return A simr powerCurve object containing power estimates for each sample size.
#' @examples
#' power_curve_result <- run_power_curve(pilot_model, "SED", sample_sizes = seq(40, 120, by = 5), n_simulations = 1000, seed = 2001)
run_power_curve <- function(
    pilot_model,
    stim_label,
    sample_sizes,
    n_simulations = 1000,
    seed = 2001
){
  
  set.seed(seed)
  
  cat("\n=============================\n")
  cat("POWER CURVE —", stim_label, "vs NEU\n")
  cat("=============================\n")
  
  design_data <- make_design_balanced(n_subjects = max(sample_sizes), stim_label = stim_label)
  
  simulated_model <- makeLmer(
    formula(pilot_model),
    fixef(pilot_model),
    VarCorr(pilot_model),
    sigma(pilot_model),
    data = design_data
  )
  
  interaction_term <- get_interaction_term(simulated_model)
  
  extended_model <- extend(simulated_model, along = "subject", n = max(sample_sizes))
  
  power_curve_result <- powerCurve(
    extended_model,
    test = fixed(interaction_term, method = "t"),   
    along = "subject",
    breaks = sample_sizes,
    nsim = n_simulations,
    fitOpts = list(control = lmerControl(optimizer="bobyqa"))
  )
  
  print(summary(power_curve_result))
  plot(power_curve_result)
  
  power_curve_result
}

#' Refine sample size estimation by re-simulation around the power threshold
#'
#' This function takes a power curve object and identifies the interval of sample sizes
#' that brackets the target power (e.g., 90%). It then re-simulates power in steps of 1
#' participant within this interval to estimate the exact sample size required to reach
#' the target power. Results are printed and returned as a list.
#'
#' @param power_curve_obj A simr powerCurve object (from run_power_curve).
#' @param pilot_model A fitted pilot linear mixed model (from lmer).
#' @param stim_label Character. Label for the target stimulus type (e.g., "PA" or "SED").
#' @param target_power Numeric. Desired power threshold (default: 0.90).
#' @param n_simulations_refine Integer. Number of simulations per sample size for refinement (default: 1000).
#' @param base_seed Integer. Base random seed for reproducibility (default: 9000).
#' @return A list with the exact sample size, summary tables, and interval.
#' @examples
#' result <- refine_N_by_resimulation(power_curve_result, pilot_model, "SED", target_power = 0.90, n_simulations_refine = 1000)
refine_N_by_resimulation <- function(
    power_curve_obj,
    pilot_model,
    stim_label,
    target_power = 0.90,
    n_simulations_refine = 1000,
    base_seed = 9000
){
  
  power_summary <- as.data.frame(summary(power_curve_obj))
  
  if(!("subjects" %in% names(power_summary)) && ("nlevels" %in% names(power_summary))){
    power_summary$subjects <- power_summary$nlevels
  }
  
  if(!("power" %in% names(power_summary)) && ("mean" %in% names(power_summary))){
    power_summary$power <- power_summary$mean
  }
  
  stopifnot("subjects" %in% names(power_summary), "power" %in% names(power_summary))
  
  power_summary <- power_summary[order(power_summary$subjects), ]
  
  if(max(power_summary$power, na.rm=TRUE) < target_power){
    cat("\n⚠️  Target power not achieved in the tested range.\n")
    return(list(n_exact = NA, power_summary = power_summary, refinement_table = NULL,
                n_lower = NA, n_upper = NA))
  }
  
  index_above <- which(power_summary$power >= target_power)[1]
  index_below <- index_above - 1
  
  if(power_summary$power[index_above] == target_power){
    n_exact <- power_summary$subjects[index_above]
    cat("\n✅ N exact = ", n_exact, " n", sep="")
    return(list(n_exact = n_exact, power_summary = power_summary, refinement_table = NULL,
                n_lower = n_exact, n_upper = n_exact))
  }
  
  n_lower <- power_summary$subjects[index_below]; power_at_lower <- power_summary$power[index_below]
  n_upper <- power_summary$subjects[index_above]; power_at_upper <- power_summary$power[index_above]
  
  cat("\nDetected framing (power curve) :\n")
  cat("  N =", n_lower, " → power_mean =", round(power_at_lower,4), "\n")
  cat("  N =", n_upper, " → power_mean =", round(power_at_upper,4), "\n")
  cat("\n🔁 SIMR RE-SIMULATION in steps of 1 between ", n_lower, " and ", n_upper, "...\n", sep="")
  
  extract_power_from_powersim <- function(power_sim_object){
    sim_summary <- summary(power_sim_object)
    estimated_power  <- if("power" %in% names(sim_summary)) as.numeric(sim_summary[["power"]]) else
      if("mean"  %in% names(sim_summary)) as.numeric(sim_summary[["mean"]])  else NA_real_
    ci_lower <- if("lower" %in% names(sim_summary)) as.numeric(sim_summary[["lower"]]) else NA_real_
    ci_upper <- if("upper" %in% names(sim_summary)) as.numeric(sim_summary[["upper"]]) else NA_real_
    list(power = estimated_power, lower = ci_lower, upper = ci_upper)
  }
  
  sample_size_grid <- seq(n_lower, n_upper, by = 1)
  refinement_results <- vector("list", length(sample_size_grid))
  
  for(i in seq_along(sample_size_grid)){
    n_current <- sample_size_grid[i]
    current_seed <- base_seed + n_current
    
    power_result <- run_power_simr(pilot_model, stim_label, n_subjects = n_current, n_simulations = n_simulations_refine, seed = current_seed)
    extracted_power <- extract_power_from_powersim(power_result)
    
    refinement_results[[i]] <- data.frame(
      subjects = n_current,
      power = extracted_power$power,
      lower = extracted_power$lower,
      upper = extracted_power$upper
    )
    
    cat("  N=", n_current, " → power_mean=", round(extracted_power$power,4),
        " [", round(extracted_power$lower,4), ", ", round(extracted_power$upper,4), "]\n", sep="")
  }
  
  refinement_table <- bind_rows(refinement_results)
  
  index_target_reached <- which(refinement_table$power >= target_power)[1]
  if(is.na(index_target_reached)){
    cat("\n⚠️  Even after refinement, the target is not achieved.\n")
    n_exact <- NA
  } else {
    n_exact <- refinement_table$subjects[index_target_reached]
    cat("\n✅ N exact (re-simulated) for power_mean  ", target_power*100, "% = ", n_exact, "\n", sep="")
  }
  
  list(n_exact = n_exact, power_summary = power_summary, refinement_table = refinement_table,
       n_lower = n_lower, n_upper = n_upper)
}
