library(xlsx)
library(tidyverse)
library(haven)
library(labelled)
library(here) # Assuming you use 'here' for path management
library(janitor) # For cleaning column names
library(novahelpers)

source(here::here("scripts", "propogate_headers.R"))

# Propagate the headers in the first row# Set seed for reproducibility
set.seed(1234)



# Function to replace strings in column names
replace_strings <- function(name, replacements) {
  for (original in names(replacements)) {
    replacement <- replacements[[original]]
    name <- gsub(original, replacement, name, fixed = TRUE)
  }
  name
}


format_null <- function(x) if (is.null(x)) "NULL" else x


make_group_key <- function(vars, delim = "__") paste(vars, collapse = delim)


# helper-structure --------------------------------------------------------

context_vars <- list(
  project = "xx",
  period = "24i_25i",
  cont_vars_pivot = c("pivoted_root"),
  cont_vars_solo = c("var_name_1", "var_name_2"),
  pivot_var = "topic",
  group_vars = c("grouper_1", "grouper_2"),
  summarization_var = "value"
)

groupers <- c("respondent_id", "year", "area_study", "enrolment", "student_origin")
group_var_list <- setdiff(c("topic", groupers), "respondent_id")


var_dict <- tribble(
  ~var_name, ~label, ~type,
  "satisfaction", "Satisfaction (1 - 5)", "continuous",
  "activity_relevance", "Perceived Relevance  (1 - 4)", "continuous",
  # "topic", "Topic", "group",
  # "year", "Year", "group",
  # "area_study", "Area of Study", "group",
  # "enrolment", "Enrolment Category", "group",
  # "student_origin", "Student Origin", "group",
)


fig_path <- here::here("fig", context_vars$period, "/")



# data-import-and-header-cleaning --------------------------------------------------------



df <- readxl::read_excel(here("data", "survey_monkey_file.xlsx"), col_names = FALSE)


# Extract the first and second rows for the headers
header_row1 <- as.character(df[1, ])
header_row2 <- as.character(df[2, ])

header_row1 <- propagate_headers(header_row1)

# Create composite headers combining row 1 and row 2
composite_header <- sapply(1:length(header_row1), function(i) {
  if (!is.na(header_row2[i]) && header_row2[i] != "") {
    paste(header_row1[i], header_row2[i], sep = " - ")
  } else {
    header_row1[i]
  }
})


# Remove the first two rows from the data
df <- df[-c(1, 2), ]



# Apply the new composite headers to the dataframe
colnames(df) <- composite_header


# renaming ----------------------------------------------------------------



# Define replacements for column names
overall_replacements <- c(
  " - Response" = "",
  " - Open-Ended Response" = ""
)


# Apply replacements to composite headers
new_column_names <- unname(sapply(composite_header, replace_strings, replacements = overall_replacements))


# Assign variable labels using labelled package
for (i in seq_along(new_column_names)) {
  labelled::var_label(df[[i]]) <- new_column_names[i]
}


# Define specific replacements for column names
replacements <- c(
  "old" = "new"
)

# Apply specific replacements to composite headers
new_column_names <- sapply(composite_header, replace_strings, replacements = replacements)

# Assign new column names
colnames(df) <- new_column_names

# Clean column names using janitor package
df <- janitor::clean_names(df)




# dataset-reduction -------------------------------------------------------

cols_to_remove <- c(
  "collector_id",
  "custom_data_1",
  "start_date",
  "end_date",
  "ip_address",
  "email_address",
  "first_name",
  "last_name"
)




# remove those columns from the data frame
df <- df[, !(names(df) %in% cols_to_remove)]

# # Remove unnecessary columns (assuming columns 2 to 9 and column 11)
# df <- df[, -c(2:9)]
