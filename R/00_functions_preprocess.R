#' Load raw data from a CSV file.
#' 
#' @param raw_file Path to the raw CSV data file.
#' @return A data.frame containing the loaded data.
load_raw_data <- function(raw_file) {
  dataMT <- read.csv(raw_file, sep = ";", stringsAsFactors = FALSE)
  dataMT$image <- as.character(dataMT$image)
  dataMT
}

#' Get the list of image sets used in the experiment.
#'
#' @return A named list with vectors of image filenames for each set (PA, SB, neutral_r, neutral_c).
get_image_sets <- function() {
  list(
    PA = c(
        "AP-COUR.jpg",
        "AP-FOOT.jpg",
        "AP-NAT.jpg",
        "AP-RANDO.jpg",
        "AP-VEL.jpg"
    ),
    SB = c(
        "SED-CANAP.jpg",
        "SED-HAMAC.jpg",
        "SED-JVID.jpg",
        "SED-LECT.jpg",
        "SED-TV.jpg"
    ),
    neutral_r = c(
        "AP-COURr.jpg",
        "AP-FOOTr.jpg",
        "AP-NATr.jpg",
        "AP-RANDOr.jpg",
        "AP-VELr.jpg",
        "SED-CANAPr.jpg",
        "SED-HAMACr.jpg",
        "SED-JVIDr.jpg",
        "SED-LECTr.jpg",
        "SED-TVr.jpg"
    ),
    neutral_c = c(
        "AP-COURc.jpg",
        "AP-FOOTc.jpg",
        "AP-NATc.jpg",
        "AP-RANDOc.jpg",
        "AP-VELc.jpg",
        "SED-CANAPc.jpg",
        "SED-HAMACc.jpg",
        "SED-JVIDc.jpg",
        "SED-LECTc.jpg",
        "SED-TVc.jpg"
    )
  )
}

#' Recode image filenames into stimulus categories.
#'
#' @param dataMT Data frame containing an 'image' column.
#' @param img_sets List of image sets as returned by get_image_sets().
#' @return The data frame with a new 'Stimuli' factor column.
recode_stimuli <- function(dataMT, img_sets) {
  dataMT$Stimuli <- NA
  dataMT$Stimuli[dataMT$image %in% img_sets$PA] <- "PA"
  dataMT$Stimuli[dataMT$image %in% img_sets$SB] <- "SB"
  dataMT$Stimuli[dataMT$image %in% img_sets$neutral_r] <- "stimuli_rond"
  dataMT$Stimuli[dataMT$image %in% img_sets$neutral_c] <- "stimuli_carre"
  dataMT$Stimuli <- factor(dataMT$Stimuli)
  dataMT
}

#' Recode movement direction based on stimulus and trial type.
#'
#' @param dataMT Data frame with 'Stimuli' and 'Procedure.Trial.' columns.
#' @return The data frame with a new 'Movement' factor column ("Approach" or "Avoid").
recode_movement <- function(dataMT) {
  dataMT$groups <- interaction(factor(dataMT$Stimuli), factor(dataMT$Procedure.Trial.))
  
  mapping <- c(
    "PA.compat1" = "Approach",
    "PA.compat2" = "Approach",
    "SB.compat1" = "Avoid",
    "SB.compat2" = "Avoid",
    "stimuli_carre.compat1" = "Avoid",
    "stimuli_carre.compat2" = "Avoid",
    "stimuli_rond.compat1" = "Approach",
    "stimuli_rond.compat2" = "Approach",
    "PA.incomp1" = "Avoid",
    "PA.incomp2" = "Avoid",
    "SB.incomp1" = "Approach",
    "SB.incomp2" = "Approach",
    "stimuli_carre.incomp1" = "Approach",
    "stimuli_carre.incomp2" = "Approach",
    "stimuli_rond.incomp1" = "Avoid",
    "stimuli_rond.incomp2" = "Avoid"
  )
  
  dataMT$Movement <- plyr::revalue(as.character(dataMT$groups), mapping, warn_missing = FALSE)
  dataMT$Movement <- factor(dataMT$Movement, levels = c("Approach", "Avoid"))
  dataMT
}

#' Clean and filter the data for analysis.
#'
#' @param dataMT Raw data frame.
#' @param limit_RT Numeric vector of length 2 specifying min and max RT (default: c(150, 1500)).
#' @return A cleaned data.frame with relevant columns and filtered rows.
clean_data <- function(dataMT, limit_RT = c(150, 1500)) {
  dataMT$subject  <- factor(dataMT$Subject)
  dataMT$RT       <- as.numeric(as.character(dataMT$Slide4.RT))
  dataMT$ACC      <- as.numeric(as.character(dataMT$Slide4.ACC))
  dataMT$Training <- as.character(dataMT$Procedure.SubTrial.)
  
  data.frame(
    subject  = dataMT$subject,
    Movement = dataMT$Movement,
    RT       = dataMT$RT,
    image    = as.character(dataMT$image),
    ACC      = dataMT$ACC,
    Training = dataMT$Training
  ) %>%
    dplyr::filter(
      Training == "TrailProc",
      ACC == 1,
      !is.na(RT),
      RT > limit_RT[1],
      RT < limit_RT[2],
      !is.na(Movement)
    ) %>%
    droplevels()
}

#' Create unique item IDs for each image.
#'
#' @param data_main Data frame with an 'image' column.
#' @param neutral_all Vector of filenames considered as neutral images.
#' @return The data frame with a new 'item_id' factor column.
create_item_ids <- function(data_main, neutral_all) {
  strip_ext <- function(x) sub("\\.jpg$", "", x)
  
  is_neu_file <- data_main$image %in% neutral_all
  base_name <- strip_ext(data_main$image)
  
  data_main$item_id <- ifelse(is_neu_file, paste0("NEU_", base_name), paste0("TGT_", base_name))
  data_main$item_id <- factor(data_main$item_id)
  data_main
}

#' Create a dataset for a specific stimulus type (target vs. neutral).
#'
#' @param data_main Main data frame.
#' @param target_set Vector of target image filenames.
#' @param neutral_all Vector of neutral image filenames.
#' @param stim_label Label for the target stimulus type.
#' @return A filtered and recoded data.frame for analysis.
create_stimulus_dataset <- function(
    data_main,
    target_set,
    neutral_all,
    stim_label
) {
  data_main %>%
    dplyr::filter(image %in% c(target_set, neutral_all)) %>%
    dplyr::mutate(
      StimulusType = dplyr::if_else(image %in% target_set, stim_label, "NEU"),
      StimulusType = factor(StimulusType, levels = c("NEU", stim_label)),
      Movement     = factor(Movement, levels = c("Approach", "Avoid")),
      item         = factor(item_id),
      subject      = factor(subject)
    ) %>%
    droplevels()
}

#' Set contrast coding (-0.5, 0.5) for Movement and StimulusType factors.
#'
#' @param df Data frame with 'Movement' and 'StimulusType' columns.
#' @param stim_levels Character vector of length 2 specifying the levels for StimulusType.
#' @return The data frame with updated contrasts.
set_coding_pm05 <- function(df, stim_levels) {
  df$Movement     <- factor(as.character(df$Movement), levels = c("Approach", "Avoid"))
  df$StimulusType <- factor(as.character(df$StimulusType), levels = stim_levels)
  
  stopifnot(nlevels(df$Movement) == 2, nlevels(df$StimulusType) == 2)
  
  contrasts(df$Movement)     <- matrix(c(-0.5, 0.5), ncol = 1)
  contrasts(df$StimulusType) <- matrix(c(-0.5, 0.5), ncol = 1)
  df
}

#' Run basic sanity checks on the dataset.
#'
#' @param dat Data frame to check.
#' @param label Character label for output.
#' @param expected_items Expected number of unique items (default: 25).
#' @return None. Stops execution if checks fail.
run_sanity_checks <- function(dat, label, expected_items = 25) {
  cat("\n", label, " counts (StimulusType x Movement):\n", sep = "")
  print(table(dat$StimulusType, dat$Movement))
  cat(label, " #subjects:", length(unique(dat$subject)), "\n")
  cat(label, " #items:", length(unique(dat$item)), "\n")
  stopifnot(length(unique(dat$item)) == expected_items)
}