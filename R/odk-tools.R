# Description
# Tools for manipulating SurveyCTO style ODK files.
# Very messy, really easy to break.
# BE on 2017-11-30

#' In an ODK survey, split select multiples into a series of binary questions.
#'
#' @param survey An ODK survey sheet.
#' @param choices The corresponding ODK choices sheet.
#' @param language Language to use for labeling. If null, assumes no languages
#' used.
#'
#' @return A copy of the ODK survey sheet with select multiple questions split
#' into binary questions.
split_select_multiples <- function(survey, choices, language = NULL) {

  # What do the labels look like? ie take into account language
  label <- ifelse(is.null(language), "label", paste0("label::", language))

  # subset survey to only select multiples
  survey.sm <- survey[which(grepl("select_multiple", survey$type)), ]
  # Get survey$type without "select_multiple"
  survey.sm$sm_type <- gsub("select_multiple ", "", survey.sm$type)
  # We want this separate for later use
  sm_vars <- survey.sm$name

  # Subset choices to only sm options
  choices <- choices[which(choices$list_name %in% survey.sm$sm_type),]
  # Drop uneeded choices columns
  choices <- choices[, c("list_name", "name", label)]
  # Change names
  colnames(choices) <- c("sm_type", "response_code", "label.y")

  # Mege sm_name with choices, by sm_type
  sm <- merge(survey.sm, choices, by = "sm_type")
  # Create survey names with the response code, we'll need the original
  # name later
  sm$name.y <- paste0(sm$name,
                      "_",
                      # No negatives in names..
                      gsub("-", "_", as.character(sm$response_code)))
  # Create new labels as well.
  sm$label.y <- paste0(sm$label.y, " - ", sm[, label])
  # Remove helper columns
  sm <- sm[, setdiff(names(sm), c("response_code", "sm_type"))]

  # Make sure we can keep order
  survey$id <- 1:nrow(survey)
  # Merge into survey, by name
  survey <- merge(survey, sm,
                  by = intersect(names(survey), names(sm)),
                  all.x = T)
  # Reorder
  survey <- survey[order(survey$id), ]
  rownames(survey) <- 1:nrow(survey)

  # replace names and labels where needed
  survey$name <- ifelse(is.na(survey$name.y), survey$name, survey$name.y)
  survey[, label] <- ifelse(is.na(survey$label.y), survey[, label], survey$label.y)

  # Drop .y columns
  survey <- survey[, setdiff(names(survey), c("label.y", "name.y"))]

  survey$relevant <- ifelse(grepl("selected", survey$relevant) &
                              grepl(paste0("\\$\\{", sm_vars, "\\}", collapse = "|"),
                                    survey$relevant),
                            gsub("'{1,1}",
                                 "}, '1'",
                                 gsub("\\}[[:space:]]{0,},[[:space:]]{0,}'",
                                      "_",
                                      survey$relevant)),
                            survey$relevant)
  # Fix negatives here as well..
  survey$relevant <- gsub("_-", "__", survey$relevant)

  return(survey)
}

#' In an ODK survey, repeat variables in a repeat group into a series of binary questions.
#'
#' @param survey An ODK survey sheet.
#' @param n The number of repeats.
#' @param language Language to use for labeling. If null, assumes no languages
#' used.
#'
#' @return A copy of the ODK survey sheet with select multiple questions split
#' into binary questions.
split_repeats <- function(survey, n, language = NULL) {

  # What do the labels look like? ie take into account language
  label <- ifelse(is.null(language), "label", paste0("label::", language))

  # We assume there are an equal number of begin repeat and
  # end repeat statements.
  repeat_groups <- cbind(start = which(survey$type == "begin repeat"),
                         stop = which(survey$type == "end repeat"))
  # Get the ranges as a list
  repeat_groups <- apply(repeat_groups, 1, function(x) x[1]:x[2])

  # We have to deal with nested repeats as well, so we'll loop each repeat group
  for (rep in repeat_groups) {
    # Create ID so we can easily reorder survey correctly
    survey$id <- 1:nrow(survey)

    # Subset to our repeat group
    survey.rep <- survey[rep, ]
    # Drop anything that isn't a variable
    survey.rep <- survey.rep[which(!(survey.rep$type %in% c("note", "begin group",
                                                            "begin repeat", "end",
                                                            "end group", "end repeat"))), ]
    survey.rep.length <- nrow(survey.rep)
    survey.rep <- survey.rep[rep(seq_len(nrow(survey.rep)), each = n),]

    # modify var names
    survey.rep$name.y <- paste0(survey.rep$name, "_", rep(1:n, survey.rep.length))
    # And labels
    survey.rep$label.y <- paste0(rep(1:n, survey.rep.length), " - ", survey.rep[, label])
    # Fix relevant
    survey.rep$relevant.y <- str_replace_all(survey.rep$relevant,
                                             "\\}",
                                             rep(paste0("_", 1:n, "}"),
                                                 length(survey.rep$relevant)/n))

    # Merge back in with survey
    survey <- merge(survey, survey.rep,
                    by = intersect(names(survey), names(survey.rep)),
                    all.x = T)
    # Reorder
    survey <- survey[order(survey$id), ]
    rownames(survey) <- 1:nrow(survey)

    # replace names and labels where needed
    survey$name <- ifelse(is.na(survey$name.y), survey$name, survey$name.y)
    survey$relevant.y <- ifelse(is.na(survey$relevant.y), survey$relevant, survey$relevant.y)
    survey[, label] <- ifelse(is.na(survey$label.y), survey[, label], survey$label.y)

    # Drop .y columns
    survey <- survey[, setdiff(names(survey), c("label.y", "name.y", "relevant.y", "id"))]

    # Update positions of repeat groups
    repeat_groups <- cbind(start = which(survey$type == "begin repeat"),
                           stop = which(survey$type == "end repeat"))
    # Get the ranges as a list
    repeat_groups <- apply(repeat_groups, 1, function(x) x[1]:x[2])
  }

  # OK, should be all set...
  return(survey)
}

#' In an ODK survey, repeat variables in a repeat group into a series of binary questions.
#'
#' @param survey An ODK survey sheet.
#' @param df The dataset that corresponds to the ODK survey will be used to
#' determine the number of repeats.
#' @param language Language to use for labeling. If null, assumes no languages
#' used.
#'
#' @return A copy of the ODK survey sheet with select multiple questions split
#' into binary questions.
split_repeats_df <- function(survey, df, language = NULL) {
  # What do the labels look like? ie take into account language
  label <- ifelse(is.null(language), "label", paste0("label::", language))

  # We assume there are an equal number of begin repeat and
  # end repeat statements.
  repeat_groups <- cbind(start = which(survey$type == "begin repeat"),
                         stop = which(survey$type == "end repeat"))
  # Get the ranges as a list
  repeat_groups <- apply(repeat_groups, 1, function(x) x[1]:x[2])

  if (class(repeat_groups) != "list")
    repeat_groups <- list(repeat_groups)

  # We have to deal with nested repeats as well, so we'll loop each repeat group
  for (i in 1:length(repeat_groups)) {
    rep <- repeat_groups[[i]]

    # Create ID so we can easily reorder survey correctly
    survey$id <- 1:nrow(survey)

    # Subset to our repeat group
    survey.rep <- survey[rep, ]

    # Get the begin repeat row
    rep_row <- survey.rep[survey.rep$type == "begin repeat", ]

    # Drop anything that isn't a variable
    survey.rep <- survey.rep[which(!(survey.rep$type %in% c("note", "begin group",
                                                            "begin repeat", "end",
                                                            "end group", "end repeat"))), ]

    # How many rows were there in this rep group before changes
    survey.rep.length <- nrow(survey.rep)

    # Figure out how many repeats
    ## We do this by checking var names for the first var in the repeat group
    names(df)[grepl(survey.rep$name[1], names(df))] %>%
      str_extract("[0-9]*$") %>%
      as.numeric %>%
      max(na.rm = T) -> n
    # print(n)

    # Rep.
    survey.rep <- survey.rep[rep(seq_len(nrow(survey.rep)), each = n),]

    # modify var names
    survey.rep$name.y <- paste0(survey.rep$name, "_", rep(1:n, survey.rep.length))

    # And labels
    survey.rep$label.y <- paste0(rep(1:n, survey.rep.length), " - ", survey.rep[, label])
    # Fix relevant
    survey.rep$relevant.y <- str_replace_all(survey.rep$relevant,
                                             "\\}",
                                             rep(paste0("_", 1:n, "}"),
                                                 length(survey.rep$relevant)/n))
    # If the rel column is a var (instead of a fixed number),
    # we need to adjust rel so skips work properly
    if (grepl("\\$", rep_row$repeat_count)) {
      survey.rep$new_rel <- rep(paste0(rep_row$repeat_count, " >= ", 1:n), survey.rep.length)
      survey.rep$new_rel_var <- rep(rep_row$repeat_count, survey.rep.length)
      # print(survey.rep$new_rel)
      survey.rep$relevant.y <- ifelse(is.na(survey.rep$relevant.y) |
                                        grepl("^\\s*$", survey.rep$relevant.y),
                                      paste0("(", survey.rep$new_rel,
                                             "and not(is.na(", survey.rep$new_rel_var, ")))"),
                                      paste0("(", survey.rep$relevant.y, " or (", survey.rep$new_rel,
                                             " and not(is.na(", survey.rep$new_rel_var, "))))"))
    }

    # Merge back in with survey
    survey <- merge(survey, survey.rep,
                    by = intersect(names(survey), names(survey.rep)),
                    all.x = T)
    # Reorder
    survey <- survey[order(survey$id), ]
    rownames(survey) <- 1:nrow(survey)

    # replace names and labels where needed
    survey$name <- ifelse(is.na(survey$name.y), survey$name, survey$name.y)
    survey$relevant <- ifelse(is.na(survey$relevant.y), survey$relevant, survey$relevant.y)
    survey[, label] <- ifelse(is.na(survey$label.y), survey[, label], survey$label.y)

    # Drop .y and helper columns
    survey <- survey[, setdiff(names(survey), c("label.y", "name.y",
                                                "relevant.y", "id", "new_rel",
                                                "new_rel_var"))]

    # Update positions of repeat groups
    repeat_groups <- cbind(start = which(survey$type == "begin repeat"),
                           stop = which(survey$type == "end repeat"))
    # Get the ranges as a list
    repeat_groups <- apply(repeat_groups, 1, function(x) x[1]:x[2])
  }

  # OK, should be all set...
  return(survey)
}

#' Generate a stata labeling script.
#'
#' @param survey An ODK survey
#' @param choices An ODK Choices sheet
#' @param language Optional. The language used for labeling.
#' @param df Optional. The corresponding dataframe.
#' @param file The output file.
odk_to_stata <- function(survey, choices, language = NULL, df, file = "codebook.html") {
  require(tidyverse)
  require(stringr)

  # Take care of label
  label <- ifelse(is.null(language), "label", paste0("label::", language))
  choices$label <- choices[,label]
  survey$label <- survey[,label]

  # Subset to only variables.
  survey <- survey[which(!(survey$type %in% c("note", "begin group",
                                              "begin repeat", "end",
                                              "end group", "end repeat"))), ]

  # Subset further to only variables present in data and survey
  not_present <- setdiff(survey$name, names(df))
  if (!is_empty(not_present)) {
    warning(paste0("The following variables are present in the survey, but not the df: ",
                   paste0(not_present, collapse = ", ")))
    survey <- survey[which(!(survey$name %in% not_present)), ]
  }

  # Create a dataframe to describe our dataset, then convert it to markdown
  survey.st <- data.frame(
    type = gsub("select_.*[[:space:]]", "", survey$type),
    name = survey$name,
    label = survey$label)
  survey.st$type <- as.character(survey.st$type)
  survey.st$name <- as.character(survey.st$name)
  survey.st$label <- as.character(survey.st$label)
  # Create corresponding choices
  choices.st <- choices %>%
    group_by(list_name) %>%
    mutate(mash = paste0(name, ' "', label, '"')) %>%
    select(list_name, mash) %>%
    ungroup() %>%
    mutate(list_name =  as.character(list_name),
           mash =  as.character(mash)) %>%
    group_by(list_name) %>%
    summarise(smash = paste0(mash, collapse = " ")) %>%
    rename(type = list_name)

  # Need to remove tpyes: start, today, deviceid,  date, text, integer, decimal, calculate
  survey.st.val.labs <- subset(survey.st, !(survey.st$type %in% c(
    "start", "end", "today", "deviceid", "calculate",
    "date", "text", "integer", "decimal",
    "geopoint", "calculate")))

  # Create value labels:
  val_labs <- paste0("label define ", choices.st$type, " ", choices.st$smash)

  # Apply value labels, ignoring certain types
  app_val_labs <- paste0("label values ", survey.st.val.labs$name, " ", survey.st.val.labs$type)

  # Create variable labels
  var_labs <- paste0("label variable ", survey.st$name, ' "', survey.st$label, '"')

  st <- c(
    "// Auto generated by benMisc R package on:",
    paste0("// ", Sys.Date()),
    "",
    "// Define Value Labels -----------------------------------------------------------------------------",
    val_labs,
    "",
    "// Apply Value Labels ------------------------------------------------------------------------------",
    app_val_labs,
    "",
    "// Apply Variable Labels ---------------------------------------------------------------------------",
    var_labs)

  fileConn <- file(file)
  writeLines(st, fileConn)
  close(fileConn)
}
