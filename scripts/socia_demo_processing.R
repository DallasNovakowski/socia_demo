# Load necessary libraries and scripts ---------------------------------
library(xlsx)
library(tidyverse)
library(haven)
library(labelled)
library(here) # Assuming you use 'here' for path management
library(janitor) # For cleaning column names
library(readr)
library(scales)
library(ggdist)
library(ggtext)
library(novahelpers)
library(ggpp)

library(wordcloud2)

source(here::here("scripts", "propogate_headers.R"))
source(here::here("scripts", "plot_summary.R"))



firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}



find_common_prefix <- function(strings) {
  if (length(strings) == 0) {
    return("")
  }
  prefix <- strings[1]
  for (s in strings[-1]) {
    while (!startsWith(s, prefix)) {
      prefix <- substr(prefix, 1, nchar(prefix) - 1)
      if (prefix == "") {
        return("")
      }
    }
  }
  return(prefix)
}





# --- Pivot and clean ---
pivot_and_clean <- function(survey_data, groupers, value_prefix, name_col, value_col = "value") {
  pivot_cols <- names(survey_data)[startsWith(names(survey_data), value_prefix)]
  if (length(pivot_cols) == 0) {
    stop(glue::glue("No columns found starting with '{value_prefix}'"))
  }

  common_prefix <- find_common_prefix(pivot_cols)

  survey_data %>%
    select(any_of(groupers), all_of(pivot_cols)) %>%
    pivot_longer(
      cols = all_of(pivot_cols),
      names_to = name_col,
      values_to = value_col
    ) %>%
    mutate(
      {{ name_col }} := .data[[name_col]] |>
        gsub(common_prefix, "", x = _) |>
        gsub("_", " ", x = _) |>
        firstup(),
      {{ value_col }} := na_if(.data[[value_col]], -99)
    )
}















filter_analysis_data <- function(
    individual_data,
    long_data,
    summary_data,
    grouping_vars,
    n_min = 5,
    id_var = NULL,
    na_filter_vars = NULL) {
  # Helper to filter NA only if variables exist
  filter_non_na <- function(data, vars) {
    vars_in_data <- intersect(vars, names(data))
    if (length(vars_in_data) == 0) {
      return(data)
    }
    data %>%
      filter(if_all(all_of(vars_in_data), ~ !is.na(.x)))
  }

  ## Filter summary table and ungroup
  summary_filtered <- summary_data %>%
    filter(n >= n_min) %>%
    ungroup()

  ## Get allowed group combinations
  allowed_combinations <- summary_filtered %>%
    distinct(across(all_of(grouping_vars)))

  ## Join to long data
  long_data_filtered <- long_data %>%
    inner_join(allowed_combinations, by = grouping_vars)

  ## Filter out NAs if specified
  if (!is.null(na_filter_vars)) {
    long_data_filtered <- filter_non_na(long_data_filtered, na_filter_vars)
    summary_filtered <- filter_non_na(summary_filtered, na_filter_vars)
  }

  ## Filter individuals if applicable
  if (!is.null(id_var)) {
    individuals_retained <- long_data_filtered %>%
      distinct(.data[[id_var]])

    individual_data_filtered <- individual_data %>%
      semi_join(individuals_retained, by = id_var)

    initial_ids <- nrow(individual_data)
    retained_ids <- nrow(individual_data_filtered)
  } else {
    individual_data_filtered <- NULL
    initial_ids <- NA
    retained_ids <- NA
  }

  report <- tibble(
    initial_individuals = initial_ids,
    retained_individuals = retained_ids,
    initial_observations = nrow(long_data),
    retained_observations = nrow(long_data_filtered),
    retained_groups = nrow(allowed_combinations)
  )

  list(
    long_data = long_data_filtered,
    summary_data = summary_filtered,
    individual_data = individual_data_filtered,
    report = report
  )
}



# Function to propagate headers
# source(here::here("scripts", "cnc_helpers.R"))

# Propagate the headers in the first row# Set seed for reproducibility


# Function to replace strings in column names
replace_strings <- function(name, replacements) {
  for (original in names(replacements)) {
    replacement <- replacements[[original]]
    name <- gsub(original, replacement, name, fixed = TRUE)
  }
  name
}


format_null <- function(x) if (is.null(x)) "NULL" else x


make_group_key <- function(vars, delim = "_") paste(vars, collapse = delim)







base_palette <- novahelpers::socia_palette




# Apply a custom theme for consistent styling
my_theme <- novahelpers::theme_basic()


# --- Robust accessor for results ---
get_descriptive_result <- function(results, cont_var, group_vars, delim = "_") {
  group_keys <- names(results[[cont_var]])

  matching_keys <- purrr::keep(group_keys, function(key) {
    key_vars <- strsplit(key, delim)[[1]]
    identical(sort(key_vars), sort(group_vars))
  })

  if (length(matching_keys) == 0) {
    stop("No matching group combination found.")
  } else if (length(matching_keys) > 1) {
    warning("Multiple matches found; returning the first.")
  }

  results[[cont_var]][[matching_keys[[1]]]]
}







no_trailing_zero <- function(x) {
  ifelse(x %% 1 == 0,
    formatC(x, format = "f", digits = 0),
    formatC(x, format = "f", digits = 1)
  )
}




# 
# plot_summary <- function(raw_data, summary_data,
#                          y_var,
#                          x_var,
#                          base_palette,
#                          fill_var = NULL,
#                          facet_row_var = NULL,
#                          facet_col_var = NULL,
#                          slab_smooth = 2,
#                          dots = FALSE,
#                          slab_height = 0.4,
#                          flipped = FALSE,
#                          mean_nudge_x = -0.2,
#                          mean_nudge_y = 0.5,
#                          stagger = 0.5,
#                          base_size = 16,
#                          axis_text_rel = 1.5,
#                          axis_title_rel = 2,
#                          label_text_size = 5,
#                          n_label_rel = 0.7,
#                          n_nudge = 0.5,
#                          dotsize = .4,
#                          legend_scale = .8,
#                          x_title = NULL,
#                          y_title = NULL,
#                          fill_title = NULL,
#                          direction = "left") {
#   fill_var <- safe_var(fill_var)
#   facet_row_var <- safe_var(facet_row_var)
#   facet_col_var <- safe_var(facet_col_var)
# 
# # 
# #   y_lab <- if (!is.null(y_var)) var_dict$label[y_var] else NULL
# #   x_lab <- if (!is.null(x_var)) var_dict$label[var_dict$var_name == x_var] else NULL
# #   fill_lab <- if (!is.null(fill_var)) var_dict$label[var_dict$var_name == fill_var] else NULL
# #   facet_row_lab <- if (!is.null(facet_row_var)) var_dict$label[var_dict$var_name == facet_row_var] else NULL
# #   facet_col_lab <- if (!is.null(facet_col_var)) var_dict$label[var_dict$var_name == facet_col_var] else NULL
# 
# 
#   required_cols <- c("mean", "loci", "upci")
# 
#   contrast_palette <- colorspace::darken(base_palette, amount = 0.5, space = "HLS")
# 
#   if (!all(required_cols %in% colnames(summary_data))) {
#     stop("Summary data must contain 'mean', 'loci', and 'upci' columns.")
#   }
# 
#   # Order x_var by mean in summary_data
#   summary_data[[x_var]] <- forcats::fct_reorder(summary_data[[x_var]], summary_data$mean, .na_rm = TRUE)
# 
#   # summary_data <- summary_data %>%
#   #   filter(n != 0) # Remove rows where n is 0
# 
#   # Ensure the same ordering applies to raw_data
#   mean_order <- levels(summary_data[[x_var]])
#   raw_data[[x_var]] <- factor(raw_data[[x_var]], levels = mean_order, ordered = TRUE)
# 
# 
#   hbump <- 0
#   vbump <- .5
#   n_nudge <- n_nudge
#   n_location <- min(summary_data$min, na.rm = TRUE)
# 
#   # contraction <- NA
# 
#   # contraction <- c(0, -.05)
# 
#   y_min <- min(raw_data[[y_var]], na.rm = TRUE)
#   y_max <- max(raw_data[[y_var]], na.rm = TRUE)
# 
#   if (flipped) {
#     mean_nudge_x <- -mean_nudge_x
#     mean_nudge_y <- -mean_nudge_y
#     direction <- "right"
#     hbump <- -0.1
#     vbump <- 0
#     contraction <- c(0, 0)
#     # n_nudge <- 0
#   } else {
#     stagger <- stagger * 1.5
#     # n_nudge <- .5
#     hbump <- .5
#     vbump <- 0
#     contraction <- c(0.0, 0)
#   }
# 
# 
#   p <- ggplot(raw_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
#     theme_bw(base_size = base_size) +
#     my_theme +
#     scale_fill_manual(values = base_palette, 
#                       labels = function(x) stringr::str_wrap(x, width = 10)) +
#     # scale_fill_ramp_discrete(from = "white", range = c(0.6, 1), aesthetics = "fill_ramp") +
#     scale_color_manual(values = contrast_palette) +
#     guides(fill_ramp = "none")
# 
#   if (!is.null(fill_var)) {
#     p <- p + aes(fill = .data[[fill_var]])
#   } else {
#     p <- p + aes(fill = contrast_palette[[1]])
#   }
# 
#   p <- p +
#     ggdist::stat_slab(
#       alpha = 0.4, adjust = slab_smooth, side = direction, scale = slab_height,
#       normalize = "panels", height = .2,
#       position = position_dodge(
#         width = stagger
#         # , justification = 0
#       ),
#       # .width = c(0.50, 1),
#       # aes(fill_ramp = after_stat(level))
#     )
# 
#   if (dots) {
#     p <- p + ggdist::stat_dots(
#       alpha = 0.35, side = direction, scale = slab_height, binwidth = 1, dotsize = dotsize,
#       position = position_dodge(width = stagger), show.legend = FALSE
#     )
#   }
# 
#   p <- p +
#     geom_pointrange(
#       data = summary_data,
#       aes(x = .data[[x_var]], y = mean, ymin = loci, ymax = upci),
#       inherit.aes = FALSE, show.legend = FALSE,
#       linewidth = 1.2,
#       fatten = 6,
#       position = position_dodge2nudge(width = stagger),
#       color = "grey30"
#     ) +
#     geom_label(
#       data = summary_data,
#       aes(x = .data[[x_var]], y = n_location, label = paste("n =", n)),
#       inherit.aes = FALSE,
#       size = label_text_size * n_label_rel,
#       position = position_dodge2nudge(
#         width = stagger,
#         # y = n_nudge
#       ),
#       hjust = hbump,
#       vjust = vbump,
#       fill = "grey50", color = "grey10",
#       label.size = 0, alpha = 0.1
#     )
# 
#   if (!is.null(fill_var)) {
#     p <- p + geom_text(
#       data = summary_data,
#       aes(
#         x = .data[[x_var]], y = mean, label = round(mean, 1),
#         colour = .data[[fill_var]]
#       ),
#       inherit.aes = FALSE, size = label_text_size, show.legend = FALSE,
#       position = position_dodge2nudge(x = mean_nudge_x, y = mean_nudge_y, width = stagger)
#     )
#   } else {
#     p <- p + geom_text(
#       data = summary_data,
#       aes(x = .data[[x_var]], y = mean, label = round(mean, 1)),
#       color = "grey45", inherit.aes = FALSE, size = label_text_size,
#       position = position_dodge2nudge(x = mean_nudge_x, y = mean_nudge_y, width = stagger),
#       show.legend = FALSE
#     )
#   }
# 
#   # if (!is.null(facet_var)) {
#   #   p <- p + facet_wrap(vars(.data[[facet_var]]), ncol = 1) +
#   #     cowplot::panel_border()
#   # }
# 
# 
#   if (!is.null(facet_row_var) || !is.null(facet_col_var)) {
#     row_facet <- if (!is.null(facet_row_var)) sym(facet_row_var) else NULL
#     col_facet <- if (!is.null(facet_col_var)) sym(facet_col_var) else NULL
# 
#     p <- p +
#       facet_grid(
#         rows = if (!is.null(row_facet)) vars(!!row_facet) else NULL,
#         cols = if (!is.null(col_facet)) vars(!!col_facet) else NULL
#       ) +
#       cowplot::panel_border()
#   }
# 
#   if (flipped) {
#     p <- p +
#       scale_y_continuous(
#         labels = no_trailing_zero,
#         limits = c(y_min, y_max), expand = expansion(mult = c(0, 0)),
#         oob = scales::rescale_none, breaks = scales::pretty_breaks(n = 3)
#       ) +
#       coord_flip() +
#       theme(
#         panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_line(color = "grey80", size = 0.5)
#       )
#   } else {
#     p <- p +
#       scale_y_continuous(
#         labels = no_trailing_zero,
#         limits = c(y_min, y_max), expand = expansion(mult = c(0, 0)),
#         oob = scales::rescale_none, breaks = scales::pretty_breaks(n = 3)
#       ) +
#       theme(
#         panel.grid.major.y = element_line(color = "grey80", size = 0.5),
#         panel.grid.major.x = element_blank()
#       )
#   }
# 
#   p <- p + theme(
#     legend.justification = "center",
#     legend.key.size = unit(1.5 * axis_text_rel, "lines"),
#     axis.text = element_text(size = rel(axis_text_rel)),
#     axis.title = element_text(size = rel(axis_title_rel), face = "bold"),
#     legend.title = element_text(size = rel(axis_title_rel) * legend_scale, face = "bold", hjust = 0.5),
#     legend.text = element_text(size = rel(axis_text_rel) * legend_scale, hjust = 0.5),
#     strip.text = element_text(size = rel(axis_text_rel)),
#   ) +
# 
#     scale_x_discrete(
#       labels = function(x) stringr::str_wrap(x, width = 15),
#       expand = expansion(
#         mult = contraction,
#         add = c(0, .8)
#       ),
#       breaks = waiver(),
#       # drop = FALSE
#     )
# 
#   if (!is.null(fill_var)) {
#     p <- p + theme(
#       legend.position = "top"
#     )
#   } else {
#     p <- p + theme(
#       legend.position = "none"
#     )
#   }
# 
# 
# 
#   if (!is.null(x_title)) {
#     p <- p + labs(x = str_wrap(stringr::str_to_title(x_title), width = 40))
#   }
# 
#   if (!is.null(y_title)) {
#     p <- p + labs(y = str_wrap(stringr::str_to_title(y_title), width = 40))
#   }
# 
#   if (!is.null(fill_title) && !is.null(fill_var)) {
#     if (!is.null(fill_title)) {
#       p <- p + labs(fill = str_wrap(stringr::str_to_title(fill_title), width = 12))
#     }
#   } else {
#     p <- p + labs(fill = stringr::str_to_title(fill_var))
#   }
# 
# 
#   return(p)
# }






safe_var <- function(x) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(NULL)
  } else {
    return(x)
  }
}









plot_summary_wrapper <- function(
    cont_focus,
    base_palette = base_palette,
    group_focus = c(),
    results,
    # var_dict,
    stagger = .6,
    slab_height = .7,
    slab_smooth = 6,
    plots = list(),
    fig_path = "figures/",
    context_vars = list(project = NULL, period = NULL),
    height = 6,
    width = 8) {
  # --- Create plot name ---
  fig_name <- paste(c(cont_focus, group_focus), collapse = "_")
  message(glue::glue("Plotting {fig_name}"))

  # --- Get Data ---
  result_list <- get_descriptive_result(
    results = results,
    cont_var = cont_focus,
    group_vars = group_focus
  )

  # --- Labels ---
  get_label <- function(var) {
    if (is.null(var) || is.na(var)) {
      return(NULL)
    }
    out <- var_dict$label[var_dict$var_name == var]
    if (length(out) == 0) {
      return(var)
    }
    out
  }

  base_palette <- base_palette

  x_var <- group_focus[1]
  fill_var <- group_focus[2]
  facet_row_var <- group_focus[3]
  facet_col_var <- group_focus[4]

  y_lab <- get_label(cont_focus)
  x_lab <- get_label(x_var)
  fill_lab <- get_label(fill_var)
  facet_row_lab <- get_label(facet_row_var)
  facet_col_lab <- get_label(facet_col_var)

  # --- Plot ---
  plots[[fig_name]] <- plot_summary(
    raw_data = result_list$long_data,
    summary_data = result_list$summary_data,
    base_palette = base_palette,
    y_var = "value",
    x_var = x_var,
    fill_var = fill_var,
    facet_row_var = facet_row_var,
    facet_col_var = facet_col_var,
    x_title = x_lab,
    y_title = y_lab,
    fill_title = fill_lab,
    stagger = stagger,
    flipped = TRUE,
    slab_height = slab_height,
    mean_nudge_x = -0.1,
    mean_nudge_y = 0.1,
    slab_smooth = slab_smooth
  ) +
    scale_fill_manual(
      values = base_palette,
      labels = function(x) stringr::str_wrap(x, width = 10)
    )

  # --- Save ---
  fig_full_name <- paste(c(
    context_vars$project,
    context_vars$period,
    fig_name,
    "shadeplot"
  ), collapse = "_")

  file_path <- here::here(fig_path, paste0(fig_full_name, ".png"))

  ggsave(
    filename = file_path,
    plot = plots[[fig_name]],
    width = width,
    height = height,
    dpi = 300
  )

  message(glue::glue("Plot saved to: {file_path}"))

  return(plots[[fig_name]])
}



























bar_plot_with_proportions <- function(data,
                                      x_var,
                                      y_var = "proportion",
                                      # fill_var,
                                      fill_var = NULL,
                                      label_var = "count",
                                      palette = NULL,
                                      facet_row = NULL,
                                      facet_col = NULL,
                                      x_lab = NULL,
                                      y_lab = "Proportion of Subgroup",
                                      fill_lab = NULL,
                                      theme_custom = theme_minimal()) {
  # Convert to symbols
  x_sym <- sym(x_var)
  y_sym <- sym(y_var)
  group_sym <- sym(fill_var)
  fill_sym <- if (!is.null(fill_var)) sym(fill_var) else NULL
  label_sym <- sym(label_var)
  facet_row_sym <- if (!is.null(facet_row)) sym(facet_row) else NULL
  facet_col_sym <- if (!is.null(facet_col)) sym(facet_col) else NULL

  # Initialize ggplot with core aesthetics
  p <- ggplot(data, aes(x = !!x_sym, group = !!group_sym)) +
    geom_bar(
      aes(y = !!y_sym, fill = !!(fill_sym %||% group_sym)),
      stat = "identity",
      alpha = 0.7,
      position = position_dodge()
    ) +
    geom_text(
      aes(y = !!y_sym, label = paste0(percent(!!y_sym, accuracy = 1))),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      color = "black"
    ) +
    geom_label(
      aes(y = 0, label = paste("n =", !!label_sym)),
      position = position_dodge(width = 0.8),
      fill = "grey70",
      color = "black",
      size = 3,
      label.size = 0,
      alpha = 0.7,
      vjust = -0.1
    ) +
    scale_x_discrete(labels = ~ str_wrap(.x, width = 20)) +
    scale_y_continuous(
      labels = percent_format(),
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      x = x_lab %||% x_var,
      y = y_lab,
      fill = fill_lab %||% (as_label(fill_sym %||% group_sym))
    ) +
    theme_custom +
    theme(panel.grid.major.y = element_line(color = "grey80", size = 0.5))

  # Apply color palette if provided
  if (!is.null(palette)) {
    p <- p + scale_fill_manual(values = palette)
  }

  # Facets if present
  if (!is.null(facet_row_sym) && !is.null(facet_col_sym)) {
    p <- p + facet_grid(rows = vars(!!facet_row_sym), cols = vars(!!facet_col_sym))
  } else if (!is.null(facet_row_sym)) {
    p <- p + facet_grid(rows = vars(!!facet_row_sym))
  } else if (!is.null(facet_col_sym)) {
    p <- p + facet_grid(cols = vars(!!facet_col_sym))
  }

  return(p)
}































#  Globals ----

set.seed(1234)

source("load_data.R")


base_palette <- socia_palette


small <- 4

smallish <- 5.5


med <- 7

medder <- 9

big <- 11

bigger <- 15

huge <- 20

resolution <- 200





## containers-and-names ----

names(survey_data) <- gsub("_", "", names(survey_data))

long_data <- list()
descriptives <- list()
results <- list()
plots <- list()



context_vars <- list(
  project = "demo",
  period = "2025",
  # cont_vars_pivot = c("satisfactionscore"),
  cont_vars_solo = c("satisfactionscore", "wouldrecommend", "easeofpurchase", "stafffriendliness", 
                     "productquality", "valueformoney", "visitfrequency", "purchaseamount", "customerage"),
  pivot_var = "topic",
  multans_var = "convocinfo",
  group_vars = c("store", "customersegment"),
  summarization_var = "value"
)




## dictionary-and-cleaning-helpers -------------


# var_dict <- read_csv("supporting/var_dict.csv")
# 
# cleaning_rules <- c(
#   "- Response" = "",
#   "Open-Ended Response" = "",
#   " - " = "",
#   "- " = "",
#   " -" = ""
# )


fig_path <- here::here("fig", context_vars$period, "/")



# data-import-and-header-cleaning --------------------------------------------------------






# Apply the new composite headers to the dataframe
# colnames(survey_data) <- composite_header


n_import <- nrow(survey_data)



# prepping-continuous ----










## enjoyment ---------------------------------------------------



cont_focus <- context_vars$cont_vars_solo[1]
cont_focus

group_focus <- context_vars$group_vars[c(1, 2)]
group_focus


paste(c(group_focus), collapse = "_")

transformed <- list() #


# group_var_info <- setdiff(c(group_var_list), "topic")


# group_var_combinations <- unlist(
#   lapply(1:length(group_var_info), function(n) {
#     combn(group_var_info, n, simplify = FALSE)
#   }),
#   recursive = FALSE
# )




# --- Create plot name ---
fig_name <- paste(c(cont_focus, group_focus), collapse = "_")
message(glue::glue("Plotting {fig_name}"))


  group_name <- paste(group_focus, collapse = "_")
  
  transformed <- run_summary(survey_data, group_focus, cont_focus)





  results[[cont_focus]][[group_name]] <- filter_analysis_data(
    individual_data = survey_data,
    long_data = survey_data,
    summary_data = transformed,
    grouping_vars = group_focus,
    n_min = 2
  )


# # Get labels from var_dict
# y_label <- var_dict$label[var_dict$var_name == cont_focus]
# x_label <- var_dict$label[var_dict$var_name == group_focus[1]]
# fill_label <- var_dict$label[var_dict$var_name == group_focus[2]]




plots[[fig_name]] <- plot_summary(
  raw_data = results[[cont_focus]][[group_name]]$long_data, 
  summary_data = results[[cont_focus]][[group_name]]$summary_data,
  y_var = cont_focus,
  x_var = group_focus[1],
  x_title = "Store", 
  y_title = "Satisfaction",
  fill_var = group_focus[2],
  fill_title = "Customer Segment",
  flipped = TRUE,

  # slab_height = .7,
  mean_nudge_x = -.1,
  mean_nudge_y = .1,
  slabjust = 4,
  scaling = 1.5,
  # dots = TRUE,
  # dotsize = .07,
  base_palette = base_palette
) +
  scale_y_continuous(
    breaks = c(1, 4, 7),
    limits = c(1, 7),
    expand = expansion(mult = c(0, 0.05)) # no padding at the bottom
    # force range to include 1 and 7
  ) 

plots[[fig_name]]




# --- Save ---
fig_full_name <- paste(c(
  context_vars$project,
  context_vars$period,
  fig_name,
  "shadeplot"
), collapse = "_")

file_path <- here::here(fig_path, paste0(fig_full_name, ".png"))

ggsave(
  filename = file_path,
  plot = plots[[fig_name]],
  width = big,
  height = big,
  dpi = 300
)





## lengthapp ---------------------------------------------------










# Calculate the total number of students per year
totals <- survey_data %>%
  group_by(store) %>%
  summarise(total = n()) %>%
  ungroup()


counts <- survey_data %>%
  group_by(customersegment, store) %>%
  summarise(count = n()) %>%
  ungroup()


# Remove NA values
# survey_data <- survey_data %>% filter(!is.na(student_origin))



# Merge counts and totals
person_data <- counts %>%
  left_join(totals, by = "store") %>%
  mutate(proportion = count / total)


#
#
# # Convert year to factor with levels in ascending order
# student_origin_data$year <- factor(student_origin_data$year,
#                                    levels = sort(unique(student_origin_data$year))
# )




ceremonylocation_ceremonyrole_barplot <- bar_plot_with_proportions(
  data = person_data,
  x_var = "ceremonylocation",
  fill_var = "ceremonyrole",
  # label_var = "count",
  palette = base_palette,
  x_lab = "Ceremony Location",
  y_lab = "Proportion of Subgroup",
  fill_lab = "Ceremony Role",
  theme_custom = my_theme
)


# --- Save ---
fig_full_name <- paste(c(
  context_vars$project,
  context_vars$period,
  fig_name,
  "barplot"
), collapse = "_")

file_path <- here::here(fig_path, paste0(fig_full_name, ".png"))

ggsave(
  filename = file_path,
  plot = plots[[fig_name]],
  width = med,
  height = smallish,
  dpi = 300
)


















# saving-data -------------------------------------------------------------

save.image(here::here("output", "socia_2025.RData"))
