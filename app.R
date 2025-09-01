#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(bslib)
library(tidyverse)
library(plotly)
library(shinycssloaders)
library(bsicons)
library(novahelpers)
library(shinyBS)
library(htmltools)


addResourcePath("static", "www")  

set.seed(123)  # For reproducibility



nova_palette <- c("#1E716F", "#E8AE00", "#604D75", "#3B8EAD")


custom_theme <- function(dark = FALSE) {
  
  
  base <- novahelpers::theme_basic()
  bg_fill <- if (dark) "#1D1F21" else "#FFFFFF"
  fg_text <- if (dark) "#FFFFFF" else "#000000"
  strip_bg <- if (dark) "#2E3133" else "#F0F0F0"
  strip_text <- if (dark) "#DDDDDD" else "#444444"
  
  
  base <- base +
    theme(
      plot.background   = element_rect(fill = bg_fill, color = NA),
      panel.background  = element_rect(fill = bg_fill, color = NA),
      text              = element_text(color = fg_text),
      axis.text         = element_text(color = fg_text),
      axis.title        = element_text(color = fg_text),
      plot.title        = element_text(color = fg_text),
      
      panel.grid.major        = element_line(color = if (dark) "grey60" else "grey80"),
      panel.grid.major.x      = element_blank(),
      panel.grid.minor        = element_blank(),
      # axis.line               = element_line(color = fg_text),
      # axis.ticks              = element_line(color = fg_text),
      
      strip.background        = element_rect(fill = strip_bg, color = NA),
      strip.text              = element_text(color = strip_text, face = "bold"),
      
      
      legend.position = "top",
      legend.justification = "center",
      legend.box = "horizontal",
      legend.box.just = "center",
      
      # legend.background       = element_rect(fill = bg_fill, color = NA),
      # legend.key              = element_rect(fill = bg_fill, color = NA)
      # ) + theme(
      axis.line = element_line(color = 'grey50', size = 1),
      # axis.text = element_text(color = 'grey50'),
      axis.ticks = element_line(color = 'grey50'),
      plot.caption = element_text(size = 12, hjust=0)
    )

}
  

source("load_data.R")

# ggplot(
#   merged,
#   aes_string(
#     x = input$x_var,
#     y = input$y_var,
#     fill = input$fill_var,
#     color = input$fill_var
#   )
# )






# 
# 
# 
# 
purchase_amount_summary <- novahelpers::run_summary(merged,
                                                    c("customer_segment", "store"),
                                                    "purchase_amount")
# 





card1 <- card(
  card_header(h3("Clarity")), "We make data and decisions understandable.",
  tags$ul(
    tags$li("Highlighting benchmarks (past performance, industry, organization goals)"),
    tags$li(
      a("Advanced data visualizations", 
        href = "https://dallasnova.rbind.io/post/creating-simple-and-transparent-data-graphs-using-faded-dotplots-and-shadeplots/", target = "_blank"),),
    tags$li("Transparent processes to make results and interpretations clear")
  ),
  
  
)
card2 <- card(
  card_header(h3("Rigor")),
  "We hold ourselves to the highest ethical and professional standards.",
  tags$ul(
    tags$li("Documented methodologies and data-handling practices"),
    tags$li("Adherence to Canadian Tri-Council Policy on Ethical Research and Freedom of Information and Privacy Protection Act"),
    tags$li("Quality checks at each stage to ensure accuracy and accountability")
  ),
)
card3 <- card(
  card_header(h3("Impact")), "We drive real-world progress for our partners.",
  tags$ul(
    tags$li("'Skin-In-Game' performance-based pricing options"),
    tags$li("Knowledge-sharing to foster partner autonomy"),
    tags$li("Action-focused deliverables with clear next steps"),
    tags$li("Measurable outcomes that track real-world change"),
  ),
)

card4 <- card(
  card_header(h3("Community")), 
  "We believe our work and successes are best when shared. 
  We engage our partners and communities to share in both our process and successes.",
  
  tags$ul(
    tags$li("Social-purpose pricing"),
    tags$li( 
      a("Open sharing of data scripts", 
        href = "https://github.com/DallasNovakowski/novahelpers", target = "_blank"),),
    # tags$li("Training staff and partners in relevant ideas, strategies, skills, and tools"),
    tags$li("Strategies tailored to needs of organization and key partners")
  ),
)



about_panel <- nav_panel("Home",
                         
                         
                         # tags$style(HTML("p { font-size: 40px; }")),
                         h1("Welcome to a demo of Socia's dashboarding services!"), 
                         # page_fillable(
                         layout_columns(row_heights = c(5,1), 
                                        card(h2("What Will I Find Here?"), 
                                             p("This app showcases a small-scale example of what 
                                    a custom dashboard might look like for your organization."),
                                             # p("Here, you will see how our work identifies and  distill key metrics into meaningful and interactive communications"),
                                             p("This dashboard combines and analyzes data from multiple sources, 
                                    including organization performance data and customer survey data."),), 
                                        
                                        card(h2("What Does Socia Do?"),
                                             p("Socia is a boutique market research and strategy firm, based in North Vancouver."),
                                             # p("We conduct start-to-finish research projects, from developing 
                                             #   the initial research question to bespoke reports and presentations."),
                                             
                                             # and objectives, through to study design,data collection, data analysis, and
                                             
                                             p("We have expertise in conducting beginning-to-end research projects using diverse strategies 
                                    (e.g., Survey Research, Experiments (A/B testing), 
                                    Forecasting, Qualitative Data analysis)"),
                                             
                                             # p("We offer support in translating insights into impactful strategy."),
                                             
                                             
                                        ), 
                                        
                         ),
                         
                         
                         
                         layout_columns(
                           col_widths = c(9, 3),
                           card(h2("What Defines Socia's Approach?"),
                                
                                
                                layout_column_wrap(
                                  width = 1/2, 
                                  
                                  # heights_equal = "row", # Makes all cards in this row equal height
                                  height = 450,
                                  card1, card2, card3, card4
                                ) 
                                # |> anim_width("100%", "100%"),
                                
                                
                           ),  card(h2("Contact"),
                                    p("Socia is owned and operated by Dr. Dallas Novakowski."),
                                    p("Dallas is an expert in consumer behaviour, with a  Ph.D in Marketing, and a Master's Degree in Experimental and Applied Psychology"),
                                    p("He has published multiple scientific articles across several areas of study, including risk-taking, income inequality, and people's reactions to algorithmic decision-making. 
                         He has also taught multiple marketing courses through the University of Calgary and the University of Lethbridge."),
                                    p("In his applied work, Dallas has conducted extensive quantitative and qualitative research projects to support 
                         program evaluation and user insights in public institutions (e.g., Saskatchewan Ministry of Justice, College of New Caledonia)."),
                                    p("You can contact dallas via", a("dallasnovakowski@gmail.com", href = "mailto:dallasnovakowski@gmail.com")),
                           ),
                           
                         ),
                         
                         
                         
                         # ),
)


library(shiny)
library(bslib)
library(shinyWidgets)

# Define value boxes (outputs inserted dynamically from server)
vbs <- list(
  uiOutput("revenue_box"),
  uiOutput("items_sold_box"),
  uiOutput("satisfaction_box")
)


items <- accordion_panel(icon = bsicons::bs_icon("palette"), paste("Plot Features"),
                         # p() is useful to display different elements in separate lines 
                         
                         
                         
                         checkboxInput(
                           inputId = "flipped",
                           label   = "Flipped",
                           value   = FALSE
                         ),
                         
                         checkboxInput(
                           inputId = "barplot",
                           label   = "Barplot",
                           value   = FALSE
                         ),
                         
                         
                         checkboxInput(
                           inputId = "dots",
                           label   = "Dots",
                           value   = FALSE
                         ),
                         
                         checkboxInput(
                           inputId = "density",
                           label   = "Density Slab",
                           value   = TRUE
                         ),
                         
                         checkboxInput(
                           inputId = "note",
                           label   = "Extra notes",
                           value   = FALSE
                         ),
                         
                         
)

extended <- accordion_panel(icon = bsicons::bs_icon("tools"), paste("Extra styling"),
                            
                            
                            
                            
                            conditionalPanel(
                              condition = "input.barplot == true",
                              
                              
                              
                              tagList(
                                sliderInput(
                                  "bar_alpha",
                                  "Bar opacity",
                                  min     = 0.0,
                                  max     = 1,
                                  value   = .3,
                                  step    = 0.05
                                ),
                              ),
                              
                              hr(),
                              
                              
                            ),
                            
                            
                            conditionalPanel(
                              condition = "input.density == true",
                              
                              
                              tagList(
                                sliderInput(
                                  inputId = "slabpha",
                                  label   = "Slab opacity",
                                  min     = 0,
                                  max     = 1,
                                  value   = .15,
                                  step    = 0.05
                                ),
                                numericInput(
                                  inputId = "slabjust",
                                  "Slab smoothing (smaller = bumpy, larger = smooth)",
                                  step    = 0.1,
                                  value   = 1
                                ),
                                sliderInput(
                                  inputId = "scaling",
                                  label   = "Slab height",
                                  min     = 0.1,
                                  max     = 2,
                                  value   = .3,
                                  step    = 0.1
                                )
                              ),
                              hr()                              
                            ),  
                            
                            
                            
                            conditionalPanel(
                              condition = "input.dots == true",
                              
                              tagList(
                                
                                numericInput(
                                  "dotsize",
                                  "Dot size",
                                  step = .05,
                                  value = .2
                                )
                                
                                
                              ),
                              
                              
                              hr()
                            ),
                            
                            
                            # Inputs inside the collapsible panel
                            sliderInput(
                              inputId = "base_size",
                              label   = "Base font size",
                              min     = 1,
                              max     = 30,
                              value   = 7,
                              step    = 1
                            ),
                            
                            hr(),
                            
                            
                            tags$details(
                              tags$summary("More Sizing", style = "font-size: 20px; font-weight: bold; cursor: pointer;"),
                              
                              
                              
                              sliderInput(
                                inputId = "axis_text_rel",
                                label   = "Axis text size (relative)",
                                min     = 0.5,
                                max     = 7,
                                value   = 2.5,
                                step    = 0.1
                              ),
                              
                              sliderInput(
                                inputId = "axis_title_rel",
                                label   = "Axis title size (relative)",
                                min     = 0.5,
                                max     = 7,
                                value   = 4,
                                step    = 0.1
                              ),
                              
                              # sliderInput(
                              #   inputId = "label_text_size",
                              #   label   = "Mean label size",
                              #   min     = 0.5,
                              #   max     = 3,
                              #   value   = 2,
                              #   step    = 0.1
                              # ),
                              
                              sliderInput(
                                inputId = "n_label_rel",
                                label   = "n label size (relative)",
                                min     = 0.1,
                                max     = 3,
                                value   = 0.8,
                                step    = 0.1
                              )
                            ),
                            
                            hr(),
                            
                            
                            tags$details(
                              tags$summary("Nudge mean values", style = "font-size: 20px; font-weight: bold; cursor: pointer;"),
                              
                              
                              numericInput(
                                inputId = "mean_nudge_x",
                                label   = "Nudge mean along primary grouping variable (x)",
                                # value   = FALSE,
                                step = .03,
                                value = -.15
                              ),
                              
                              # hr(),
                              
                              numericInput(
                                inputId = "mean_nudge_y",
                                label   = "Nudge mean along outcome variable (y; in scale units)",
                                # value   = FALSE,
                                step = .03,
                                value = 0
                              ),
                              
                            ),
                            
                            
                            hr(),
                            
                            
)




# Build the dashboard panel
dashboard_panel <- nav_panel(
  "Dashboard",
  
  layout_sidebar(
    sidebar = sidebar(
      title = h1("Global Filters"),
      width = 300,
      helpText("These filters apply to all elements of this app."),
      
      
      selectInput(
        "store_filter", 
        h2("Select store:"),
        choices = unique(survey_data$store),
        multiple = TRUE
      ),
      
      
      dateRangeInput(
        "date_range", 
        h2("Select Date Range:"),
        start = min(transactions_revised$created_at),
        end = max(transactions_revised$created_at),
        format = "yyyy-mm-dd"
      ),
      
    ),
    
    navset_tab(
      nav_panel(
        h2("Summary"),
        
        # Value boxes
        card(
          style = "padding: 0.75rem; border-radius: 0.25rem; margin-bottom: 1.25rem;",
          layout_column_wrap(
            fixed_width = FALSE,
            heights_equal = "row",
            gap = "1.5rem",  # Increased gap between value boxes
            .width = c("xs" = 1, "sm" = 1, "md" = 1/2, "lg" = 1/3),
            !!!vbs
          )
        ),
        
        # Filters + Plot side-by-side in the same card
        card(
          style = "margin-top: 1rem;",  # Increased top margin for better spacing
          card_body(
            layout_columns(
              fillable = TRUE,
              col_widths = c(3, 9),  # Adjusted the width of the columns (left column smaller, right column bigger)
              gap = "2rem",  # Reduced gap between columns for better alignment
              
              # Left side: inputs
              div(
                # style = "min-width: 200px",  # Ensuring the input section does not spill
                selectInput("selected_var", 
                            h2("Select Variable:"), choices = NULL),
                selectInput(
                  "time_unit", 
                  h2("Select Time Unit:"),
                  choices = c("Day" = "day", "Week" = "week", "Month" = "month", "Year" = "year"),
                  selected = "week"
                )
              ),
              
              # Right side: plot
              plotOutput("time_series_plot") %>% withSpinner()
            )
          )
        )
      ),
      
      # in your `dashboard_panel`, replace the existing Pairwise Comparisons section with:
      
      nav_panel(
        h2("Survey Analyses"),
        # navset_card_pill(
        # placement = "above",
        
        
        card(
          style = "margin-top: 1rem;",  # Increased top margin for better spacing
          card_body(
            layout_columns(
              min_height = "1200px",
              fillable = TRUE,
              col_widths = c(3, 9),  # Adjusted the width of the columns (left column smaller, right column bigger)
              gap = "2rem",  # Reduced gap between columns for better alignment
              
              # Left side: inputs
              div(
                style = "min-width: 200px",  # Ensuring the input section does not spill
                
                # — Transactions comparison —
                # nav_panel(
                # "Transactions",
                selectInput(
                  "pair_var", 
                  h3("Measure of interest (y):"),
                  choices = c(
                    # "Total Revenue"    = "total_revenue",
                    # "Items Sold"       = "total_items_sold",
                    # "Avg. Sale Value"  = "av_sale_value",
                    # "None" = NULL,
                    "Purchase Amount" = "purchase_amount",
                    "Satisfaction"       = "satisfaction_score",
                    "Ease of Purchase"   = "ease_of_purchase",
                    "Staff Friendliness" = "staff_friendliness",
                    "Product Quality"    = "product_quality",
                    "Value for Money"    = "value_for_money"
                  ),
                  
                  
                  selected = "purchase_amount",
                  selectize = FALSE      # <-- switch to a plain HTML <select>
                  
                ),
                selectInput(
                  "x_var", 
                  h3("Primary comparison item (x):"),
                  choices = c(
                    # "Total Revenue"    = "total_revenue",
                    # "Items Sold"       = "total_items_sold",
                    # "Avg. Sale Value"  = "av_sale_value",
                    # "None" = "",
                    "Purchase Amount" = "purchase_amount",
                    "Satisfaction"       = "satisfaction_score",
                    "Ease of Purchase"   = "ease_of_purchase",
                    "Staff Friendliness" = "staff_friendliness",
                    "Product Quality"    = "product_quality",
                    "Value for Money"    = "value_for_money",
                    "Customer Segment" = "customer_segment",
                    "Online vs. In-Store"   = "online_vs_instore",
                    "Store" = "store"
                  ),
                  
                  
                  selected = "customer_segment",
                  selectize = FALSE      # <-- switch to a plain HTML <select>
                  
                  
                ),
                
                selectInput(
                  "fill_var", 
                  h3("Secondary comparison variable (colour):"),
                  choices = c(
                    # "Total Revenue"    = "total_revenue",
                    # "Items Sold"       = "total_items_sold",
                    # "Avg. Sale Value"  = "av_sale_value",
                    # "Customer Segment" = "customer_segment",
                    # "Online vs. In-Store"   = "online_vs_instore",
                    # "Ease of Purchase"   = "ease_of_purchase",
                    # "Staff Friendliness" = "staff_friendliness",
                    # "Product Quality"    = "product_quality",
                    # "Value for Money"    = "value_for_money"
                    "None" = "",
                    "Customer Segment" = "customer_segment",
                    "Online vs. In-Store"   = "online_vs_instore",
                    "Store" = "store"
                  ),
                  
                  
                  selected = "",
                  selectize = FALSE      # <-- switch to a plain HTML <select>
                  
                  
                ),
                
                selectInput(
                  "facet_var", 
                  h3("Third comparison measure (panel):"),
                  choices = c(
                    # "Total Revenue"    = "total_revenue",
                    # "Items Sold"       = "total_items_sold",
                    # "Avg. Sale Value"  = "av_sale_value",
                    "None" = "",
                    "Customer Segment" = "customer_segment",
                    "Online vs. In-Store"   = "online_vs_instore",
                    "Store" = "store"
                    # "Ease of Purchase"   = "ease_of_purchase",
                    # "Staff Friendliness" = "staff_friendliness",
                    # "Product Quality"    = "product_quality",
                    # "Value for Money"    = "value_for_money"
                  ),
                  
                  
                  selected = "",
                  selectize = FALSE      # <-- switch to a plain HTML <select>
                  
                  
                ),
                
                
                
                fluidPage(
                  bslib::accordion(
                    id = "acc_palette",  # unique ID
                    height = "20%",
                    width = "100%",
                    icon = bsicons::bs_icon("palette"),
                    open = FALSE,
                    items
                  ),
                  
                  bslib::accordion(
                    id = "acc_tools",  # another unique ID
                    height = "20%",
                    width = "100%",
                    icon = bsicons::bs_icon("tools"),
                    open = FALSE,
                    extended
                  )
                )
                
                
                
                
                
              ),
              
              # Right side: plot
              # plotOutput("time_series_plot") %>% withSpinner()
              
              card_body(
                
                # plotOutput("pairwise_survey_plot") %>% withSpinner(),
                
                
                plotOutput("test_survey_plot") %>% withSpinner()
                
              )
            )
          )
        ),
        
        
        
        
        
      )
      
    )
  )
)





library(shiny)
library(bslib)
library(shinycssloaders)

# a_theme <- bs_theme(base_font = font_google("Roboto"), font_scale = 0.9)

# Custom CSS to fix the navbar height and position the title/logo
custom_css <- "
  .navbar {
    height: 80px !important;
  }
  .navbar .navbar-brand {
    display: flex;
    align-items: center;
    height: 100%;
    gap: 50px; /* creates spacing between image and title */
    margin-right: auto; /* Push the brand to the left */
  }
  .navbar .navbar-brand img {
    height: 60px;
  }
  .navbar .navbar-brand span {
    
  }
  .navbar-nav .nav-item {
    margin-top: 10px;
  }
  .navbar-collapse {
    display: flex;
    align-items: center;
  }
  .navbar-toggler {
    margin-right: 10px;
  }
  .navbar-nav .nav-link {
  font-size: 18px;        /* Bigger text */
}
"
# margin-left: 10px;
# margin-right: 30px;

anim_width <- function(x, width1, width2) {
  x |> tagAppendAttributes(
    class = "animate-width",
    style = css(
      `--width1` = validateCssUnit(width1),
      `--width2` = validateCssUnit(width2),
    ),
  )
}




ui <- page_navbar(
  tags$head(
    tags$style(HTML("
      .selectize-dropdown {
        z-index: 9999 !important;
      }

      body {
        font-size: 14px;  /* Set the base font size */
      }
      h1 {
        font-size: 2.4em;
      }
      h2 {
        font-size: 1.9em;
      }
      h3 {
        font-size: 1.4em;
      }

      /* Fix z-index layering for navbar */
      .navbar-collapse {
        z-index: 1051 !important;
        position: relative;
      }

      .navbar {
        z-index: 1051 !important;
        position: relative;
      }

      /* Add background to expanded mobile navbar */
      .navbar-collapse.collapse.show {
        background-color: #b3b3b3;
        padding: 1rem;
        border-radius: 0 0 0.5rem 0.5rem;
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
      }
    "))
  )
  
  
  
  ,
  
  title = tags$div(
    style = "margin-bottom: 0; display: flex; align-items: center;",
    tags$img(src = "static/socia1.png", height = "50px"),
    tags$span("Socia Research and Strategy - Dashboard Example", style = "font-size: 35px; margin-left: 10px; margin-right: 30px;")
  ),
  
  tags$head(
    tags$style(HTML(custom_css))
  ),
  
  theme = bs_theme(version = 5, 
                   
                   bootswatch = "cosmo",
                   
                   
                   base_font = font_google("Atkinson Hyperlegible"),
                   heading_font = font_google("Open Sans"),  # Different font for headings
                   font_scale = 1  # Scale everything up slightly
  ),
  
  about_panel,
  
  dashboard_panel,
  
  # Main Page
  
  # Additional top-level pages
  # nav_panel("Reports", p("This is where reports will go.")),
  
  
  # nav_panel("Settings", p("Settings options here.")),
  # 
  # # Footer/About
  # nav_menu("More",
  #          nav_panel("Help", p("Help page content"))
  # ),
  
  nav_spacer(),
  
  nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "dark")),
)





pretty_labels <- c(
  purchase_amount     = "Purchase Amount",
  satisfaction_score  = "Satisfaction",
  ease_of_purchase    = "Ease of Purchase",
  staff_friendliness  = "Staff Friendliness",
  product_quality     = "Product Quality",
  value_for_money     = "Value for Money",
  customer_segment    = "Customer Segment",
  online_vs_instore   = "Online vs. In-Store",
  store               = "Store",
  transaction_value = "Transaction Value",
  num_items_sold = "Num. Items Sold",
  av_sale_value = "Av. Sale Value",
  customer_age = "Customer Age"
  
  
)






nova_palette <- c("#1E716F", "#E8AE00", "#604D75", "#3B8EAD")


create_socia_shadeplot <- function (
    raw_data, 
    summary_data, 
    y_var, 
    x_var, 
    fill_var = NULL, 
    facet_var = NULL, 
    x_title = NULL, 
    y_title = NULL, 
    fill_title = NULL, 
    slabpha = 0.4, 
    slabjust = 1, 
    scaling = 0.8,
    base_palette = nova_palette, 
    dots = FALSE, 
    flipped = FALSE, 
    mean_nudge_x = -0.2, 
    mean_nudge_y = 0.5, 
    stagger = 0.5, 
    base_size = 16, 
    axis_text_rel = 1.5, 
    axis_title_rel = 2, 
    label_text_size = 5, 
    n_label_rel = 0.8, 
    dotsize = 0.4, 
    n_nudge = 0.5, 
    dark = TRUE,
    direction = "left") 
{
  
  
  if (dark) {
    contrast_pal <- colorspace::lighten(base_palette, amount =  0.5)
    
    
  } else {
    contrast_pal <- colorspace::darken(base_palette, amount =  0.5)
    
  }
  
  
  
  required_cols <- c("mean", "loci", "upci")
  if (!all(required_cols %in% colnames(summary_data))) {
    stop("Summary data must contain 'mean', 'loci', and 'upci' columns.")
  }
  if (!("n" %in% colnames(summary_data)) || !("min" %in% colnames(summary_data))) {
    stop("Summary data must also contain 'n' and 'min' columns for sample labels.")
  }
  
  contrast_palette <- colorspace::darken(base_palette, amount = 0.5, 
                                         space = "HLS")
  
  summary_data[[x_var]] <- forcats::fct_reorder(summary_data[[x_var]], 
                                                summary_data$mean, .na_rm = TRUE)
  mean_order <- levels(summary_data[[x_var]])
  raw_data[[x_var]] <- factor(raw_data[[x_var]], levels = mean_order, 
                              ordered = TRUE)
  n_location <- min(summary_data$min, na.rm = TRUE)
  y_min <- min(raw_data[[y_var]], na.rm = TRUE)
  y_max <- max(raw_data[[y_var]], na.rm = TRUE)
  
  
  if (flipped) {
    mean_nudge_x <- -mean_nudge_x
    mean_nudge_y <- -mean_nudge_y
    direction <- "right"
    hbump <- -0.1
    vbump <- 0
  }
  else {
    stagger <- stagger * 1.5
    hbump <- 0.5
    vbump <- 0
  }
  
  
  p <- ggplot2::ggplot(raw_data, ggplot2::aes(x = .data[[x_var]], 
                                              y = .data[[y_var]])) + 
    ggplot2::theme_bw(base_size = base_size) + 
    
    theme_basic() + ggplot2::theme(legend.position = "top", 
                                   legend.justification = "center", 
                                   axis.text = ggplot2::element_text(size = ggplot2::rel(axis_text_rel)), 
                                   legend.key.size = grid::unit(1.5 * axis_text_rel, "lines"), 
                                   axis.title = ggplot2::element_text(size = ggplot2::rel(axis_title_rel), 
                                                                      face = "bold"), 
                                   legend.title = ggplot2::element_text(size = ggplot2::rel(axis_title_rel), 
                                                                        face = "bold", hjust = 0.5), 
                                   legend.text = ggplot2::element_text(size = ggplot2::rel(axis_text_rel),hjust = 0.5), 
                                   strip.text = ggplot2::element_text(size = ggplot2::rel(axis_text_rel))) + 
    
    
    
    
    
    ggplot2::scale_fill_manual(values = base_palette, 
                               labels = function(x) stringr::str_wrap(x,width = 5)) + 
    
    ggplot2::scale_color_manual(values = contrast_pal) + 
    
    ggplot2::scale_y_continuous(limits = c(y_min, y_max), 
                                expand = ggplot2::expansion(mult = c(0, 0)), 
                                oob = scales::rescale_none, breaks = scales::pretty_breaks(n = 3))
  
  
  
  if (!is.null(facet_var)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]), 
                                 ncol = 1) + cowplot::panel_border()
  }
  
  
  if (flipped) {
    p <- p + ggplot2::coord_flip() + 
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line(),
                     panel.grid.major.x = ggplot2::element_blank())
    
  }
  if (!is.null(x_title)) {
    p <- p + ggplot2::xlab(x_title)
  }
  if (!is.null(y_title)) {
    p <- p + ggplot2::ylab(y_title)
  }
  if (!is.null(fill_title)) {
    p <- p + ggplot2::labs(fill = fill_title)
  }
  
  # add slab
  
  if (!is.null(fill_var)) {
    p <- p + ggdist::stat_slab(ggplot2::aes(fill = .data[[fill_var]]), 
                               alpha = slabpha, adjust = slabjust, side = direction, 
                               scale = scaling, normalize = "panels", height = 0.2, 
                               # show.legend = TRUE,
                               position = ggplot2::position_dodge(width = stagger))
  }
  else {
    p <- p + ggplot2::aes(fill = contrast_palette[[1]]) + 
      ggdist::stat_slab(alpha = slabpha, adjust = slabjust, 
                        side = direction, scale = scaling, normalize = "panels", 
                        height = 0.2, position = ggplot2::position_dodge(width = stagger), 
                        show.legend = FALSE)
  }
  
  # add dots
  
  if (dots) {
    if (!is.null(fill_var)) {
      p <- p + ggdist::stat_dots(ggplot2::aes(fill = .data[[fill_var]]), 
                                 alpha = 0.35, side = direction, scale = scaling, 
                                 binwidth = 1, dotsize = dotsize, position = ggplot2::position_dodge(width = stagger), 
                                 show.legend = FALSE)
    }
    else {
      p <- p + ggdist::stat_dots(alpha = 0.35, side = direction, 
                                 scale = scaling, binwidth = 1, dotsize = dotsize, 
                                 position = ggplot2::position_dodge(width = stagger), 
                                 show.legend = FALSE)
    }
  }
  
  # add point range
  
  p <- p + ggplot2::geom_pointrange(data = summary_data, 
                                    ggplot2::aes(x = .data[[x_var]], 
                                                 y = mean, ymin = loci, ymax = upci), inherit.aes = FALSE, 
                                    show.legend = FALSE, 
                                    position = ggpp::position_dodge2nudge(width = stagger), 
                                    # color = "grey30"
                                    color = if (dark) "grey90" else "grey30",
  )
  
  # add "n =" strings
  
  p <- p + geom_label(
    data = summary_data,
    aes(y = n_location, label = paste("n =", n)),
    fill   = if (dark) "grey30" else "grey50",
    color  = if (dark) "grey90" else "grey10",
    alpha  = if (dark) 0.3 else 0.1,
    label.size = 0,
    size = label_text_size * n_label_rel,
    position = position_dodge2(width = stagger),
    hjust = hbump, vjust = vbump
  )
  
  
  # p <- p + ggplot2::geom_label(data = summary_data, ggplot2::aes(x = .data[[x_var]],
  #                                                                y = n_location, label = paste("n =", n)), inherit.aes = FALSE,
  #                              size = label_text_size * n_label_rel, position = ggplot2::position_dodge2(width = stagger),
  #                              hjust = hbump, vjust = vbump, fill = "grey50", color = "grey10",
  #                              label.size = 0, alpha = 0.1)
  
  
  
  
  
  if (!is.null(fill_var)) {
    p <- p + ggplot2::geom_text(data = summary_data, ggplot2::aes(x = .data[[x_var]], 
                                                                  y = mean, label = round(mean, 1), colour = .data[[fill_var]]), 
                                inherit.aes = FALSE, size = label_text_size, show.legend = FALSE, 
                                position = ggpp::position_dodge2nudge(x = mean_nudge_x, 
                                                                      y = mean_nudge_y, width = stagger))
  }
  else {
    p <- p + ggplot2::geom_text(data = summary_data, ggplot2::aes(x = .data[[x_var]], 
                                                                  y = mean, label = round(mean, 1)), 
                                color = if (dark) "grey90" else "grey30", 
                                inherit.aes = FALSE, size = label_text_size, 
                                position = ggpp::position_dodge2nudge(x = mean_nudge_x,
                                                                      y = mean_nudge_y, width = stagger))
  }
  
  return(p)
}






nova_palette <- c("#1E716F", "#E8AE00", "#604D75", "#3B8EAD")


# Contrast palette depending on theme
get_contrast_palette <- function(palette, dark) {
  if (dark) colorspace::lighten(palette, 0.4) else colorspace::darken(palette, 0.2)
}

# Check summary_data structure
validate_summary_data <- function(df) {
  required <- c("mean", "loci", "upci", "n", "min")
  missing  <- setdiff(required, colnames(df))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))
}

# Organize plot data
prepare_plot_data <- function(raw_data, summary_data, x_var, y_var) {
  list(
    summary_data = summary_data,
    raw_data = raw_data,
    n_location = min(summary_data$min, na.rm = TRUE),
    y_range = range(raw_data[[y_var]], na.rm = TRUE)
  )
}

# Theme customization
theme_socia <- function(base_size = 12, axis_text_rel = 1.5, axis_title_rel = 3) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      legend.position = "top",
      legend.justification = "center",
      axis.text = element_text(size = rel(axis_text_rel)),
      axis.title = element_text(size = rel(axis_title_rel), face = "bold"),
      legend.title = element_text(size = rel(axis_title_rel), face = "bold", hjust = 0.5),
      legend.text = element_text(size = rel(axis_text_rel), hjust = 0.5),
      legend.key.size = unit(axis_text_rel, "lines"),
      strip.text = element_text(size = rel(axis_text_rel))
    ) 
}

# Core plot function
create_canvas <- function(
    raw_data,
    summary_data,
    y_var, 
    x_var,
    fill_var = NULL, 
    facet_var = NULL,
    x_title = NULL, 
    y_title = NULL, 
    fill_title = NULL,
    label_text_size = 5,
    base_palette = nova_palette,
    dark = FALSE,
    direction = "left",
    slabpha = 0.4, 
    slabjust = 1, 
    scaling = 0.8,
    dots = FALSE, 
    flipped = FALSE,
    mean_nudge_x = -0.2, 
    mean_nudge_y = 0.5,
    stagger = 0.5,
    base_size = 16,
    axis_text_rel = 1.5, 
    axis_title_rel = 2,
    n_label_rel = 0.8, 
    dotsize = 0.4, 
    n_nudge = 0.5
) {
  
  contrast_pal <- get_contrast_palette(base_palette, dark)
  # low_exp <- if (flipped) 0.1 else 0.05
  
  low_exp <- 0.01
  
  nudge_space <- abs(n_nudge) * 2  # or whatever you empirically find enough
  
  # Compute dynamic limits for non-flipped
  y_range <- range(raw_data[[y_var]], na.rm = TRUE)
  y_min_adjusted <- min(summary_data$min, na.rm = TRUE) - nudge_space
  y_range_adjusted <- range(c(y_range, y_min_adjusted), na.rm = TRUE)
  
  p <- ggplot(raw_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    # theme_socia(base_size, axis_text_rel, axis_title_rel) +
    
    
    
    custom_theme(dark) +
    
    scale_fill_manual(values = base_palette) +
    scale_color_manual(values = contrast_pal) +
    scale_y_continuous(
      limits = range(raw_data[[y_var]], na.rm = TRUE),
      expand = expansion(mult = c(low_exp, 0.05)),
      oob = scales::rescale_none,
      breaks = scales::pretty_breaks(n = 3)
    )  + guides(fill = guide_legend(override.aes = list(alpha = 1))) +
    guides(color = guide_legend(override.aes = list(alpha = 1))) 
  
  
  if (!is.null(facet_var)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]), 
                                 ncol = 1) + cowplot::panel_border() + 
      theme(panel.border = element_rect(color = 'grey50', fill = NA, size = 1),
            axis.line = element_line(color = 'grey50', size = 1),
            # axis.text = element_text(color = 'grey50'),
            axis.ticks = element_line(color = 'grey50')
            
      )
    
  }
  
  
  
  
  list(
    p = p,
    contrast_pal = contrast_pal,
    base_palette = base_palette,
    label_text_size = label_text_size
  )
}






# ─────────────────────────────────────────────────────────────────────
# Stitching Function: Combines Summary + Plot
# ─────────────────────────────────────────────────────────────────────

stitching <- function(
    raw_data,
    y_var,
    x_var,
    fill_var = NULL,
    facet_var = NULL,
    bar = FALSE,
    density = TRUE,
    dots = FALSE,
    flipped = FALSE,
    dark = FALSE,
    slabpha = 0.4,
    slabjust = 1,
    scaling = 0.8,
    stagger = 0.5,
    
    base_size = 16,
    axis_text_rel = 1.5,
    axis_title_rel = 2,
    n_label_rel = 0.8,
    label_text_size = 5,
    note = FALSE,
    dotsize = 0.4,
    n_nudge = 2,
    bar_alpha = .4,
    mean_nudge_x = 0.07,
    mean_nudge_y = .05,
    direction = "left"
) {
  
  
  # Sanitize inputs
  y_var <- if (identical(y_var, "")) NULL else y_var
  x_var <- if (identical(x_var, "")) NULL else x_var
  fill_var <- if (identical(fill_var, "")) NULL else fill_var
  facet_var <- if (identical(facet_var, "")) NULL else facet_var
  
  
  group_vars <- unique(c(x_var, fill_var, facet_var))
  group_vars <- group_vars[!vapply(group_vars, is.null, logical(1))]
  
  summary_data <- novahelpers::run_summary(
    data = raw_data,
    group_vars = group_vars,
    summarization_var = y_var
  )
  
  out <- create_canvas(
    raw_data = raw_data,
    summary_data = summary_data,  # <-- pass it here
    
    y_var = y_var,
    x_var = x_var,
    fill_var = fill_var,
    facet_var = facet_var,
    
    dots = dots,
    dark = dark,
    
    flipped = flipped,
    direction = direction,
    
    stagger = stagger,
    
    mean_nudge_y = mean_nudge_y,
    mean_nudge_x = mean_nudge_x,
    
    n_nudge = n_nudge,
    
    dotsize = dotsize,
    
    slabpha = slabpha,
    scaling = scaling,
    
    base_size = base_size,
    n_label_rel = n_label_rel,
    axis_title_rel = axis_title_rel,
    axis_text_rel = axis_text_rel
    
  )
  
  plot <- out$p
  contrast_pal <- out$contrast_pal
  base_palette <- out$base_palette
  label_text_size <- out$label_text_size
  
  if (dark){
    
    grid_colour <- "grey20"
    
  } else {
    grid_colour <- "grey90"
    
  }
  
  # Adjust for flipped coords
  if (flipped) {
    
    # plot <- plot + ggplot2::coord_flip() 
    
    plot <- plot + ggplot2::coord_flip() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_line( colour = grid_colour)
      )
    #       } else {
    
    #         
    
    
    direction <- "right"
    # hbump <- 1.3
    # vbump <- 0
    # hbump <- 0.5
    hbump <- -.2
    vbump <- 0.5
    n_nudge <- 2
  } else {
    
    plot <- plot + ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = grid_colour)
                                  ,
                                  panel.grid.major.x = ggplot2::element_blank()
    )
    
    stagger <- stagger * 1.5
    # hbump <- 0.5
    # vbump <- 1.6
    # hbump <- 1.3
    vbump <- -.2
    hbump <- .5
    n_nudge <- 0
    # mean_nudge_x <- 0
    # mean_nudge_y <- 5
  }
  
  if (note){
    
    notes_pre <- "NOTES:  "
    
    ci_note <- "Dot/lines show the average values and 95% Confidence Intervals, which show where the 'true' average lies 95 times out of 100. Shorter lines mean more certainty. "
    
    slab_note <- "The 'slabs' show how individual data points are distributed. "
    
    notes_jitter <- "Dots jittered to avoid overplotting. Shaded area = 95% confidence intervals, showing the estimated trend 95 times out of 100. "
    
    n_note <- "'n =' values show number of observations. "} else {
      
      notes_pre <- ""
      
      ci_note <- ""
      
      slab_note <- ""
      
      notes_jitter <- ""
      
      n_note <- ""
      
    }
  
  # Plot logic: numeric x → jitter & smooth
  if (is.numeric(raw_data[[x_var]])) {
    
    if(!is.null(fill_var)){
      
      plot <- plot +
        ggplot2::geom_jitter(alpha = 0.3, aes_string(fill = fill_var, color = fill_var)) 
      
    } else {
      
      if (dark){
        
        plot <- plot +
          ggplot2::geom_jitter(alpha = 0.3, aes(color = "grey80", fill =  "grey80"),
                               color = "grey80", fill =  "grey80"
          ) 
      } else
        
        plot <- plot +
          ggplot2::geom_jitter(alpha = 0.3, aes(color = "grey20", fill =  "grey20"),
                               color = "grey20", fill =  "grey20"
          )
      
    }
    
    
    plot <- plot + geom_smooth(aes_string(fill = fill_var, color = fill_var), alpha = 0.1) +
      ggplot2::scale_color_manual(values = base_palette)  + 
      scale_y_continuous(
        # limits = range(raw_data[[y_var]], na.rm = TRUE),
        expand = expansion(mult = c(0, 0.00)),
        # oob = scales::rescale_none,
        # breaks = scales::pretty_breaks(n = 3)
      ) + ggplot2::theme(panel.grid.major.y = ggplot2::element_line( colour = grid_colour),
                         panel.grid.major.x = ggplot2::element_line( colour = grid_colour)
      ) + labs(caption = str_wrap(
        paste0(notes_pre, notes_jitter), 100)) 
    
    
  } else {
    
    plot <- plot + labs(caption = str_wrap(paste0(notes_pre, n_note, ci_note), 100)) 
    
    
    if (density) {
      # plot <- plot + ggdist::stat_slab(
      #   alpha = slabpha, adjust = slabjust,
      #   scale = scaling, normalize = "panels",
      #   side = direction,
      #   aes_string(fill = fill_var),
      #   position = ggplot2::position_dodge(width = stagger)
      # )
      
      if (!is.null(fill_var)) {
        plot <- plot + ggdist::stat_slab(ggplot2::aes(fill = .data[[fill_var]]), 
                                         alpha = slabpha, adjust = slabjust, 
                                         side = direction,
                                         scale = scaling, normalize = "panels", height = 0.2, 
                                         show.legend = TRUE,
                                         position = ggplot2::position_dodge(width = stagger))
      }
      else {
        plot <- plot + 
          ggplot2::aes(fill = base_palette[[1]], show.legend = FALSE) +
          ggdist::stat_slab(alpha = slabpha, adjust = slabjust, 
                            side = direction,
                            scale = scaling, normalize = "panels", 
                            height = 0.2, position = ggplot2::position_dodge(width = stagger), 
                            show.legend = FALSE)  +
          theme(legend.position = "none")
      }
      
      plot <- plot + labs(caption = str_wrap(paste0(notes_pre, n_note, ci_note,  slab_note), 100)) 
      
      
    }
    
    
    if (dots) {
      if (!is.null(fill_var)) {
        plot <- plot + ggdist::stat_dots(ggplot2::aes(fill = .data[[fill_var]]), 
                                         alpha = 0.35, 
                                         side = direction, scale = scaling, 
                                         binwidth = 1, dotsize = dotsize, position = ggplot2::position_dodge(width = stagger), 
                                         # show.legend = FALSE
        )
      }
      else {
        plot <- plot + 
          ggplot2::aes(fill = base_palette[[1]], show.legend = FALSE) +
          ggdist::stat_dots(alpha = 0.35, side = direction, 
                            scale = scaling, binwidth = 1, dotsize = dotsize, 
                            position = ggplot2::position_dodge(width = stagger), 
                            show.legend = FALSE) +
          theme(legend.position = "none")
      }
    }
    
    if (bar) {
      plot <- plot + stat_summary(
        aes_string(fill = fill_var),
        fun = "mean", geom = "col",
        width = 0.5, alpha = bar_alpha,
        position = position_dodge(width = stagger)
      )
    }
    
    plot <- plot + ggplot2::geom_pointrange(
      data = summary_data,
      aes(x = .data[[x_var]], y = mean, ymin = loci, ymax = upci),
      inherit.aes = TRUE,
      show.legend = FALSE,
      fatten = 4,
      linewidth = 1,
      position = ggpp::position_dodge2nudge(width = stagger),
      color = if (dark) "grey80" else "grey30" 
    ) 
    
    
    # Mean value labels
    # label_color <- if (!is.null(fill_var)) aes(colour = .data[[fill_var]]) else NULL
    # label_fixed_color <- if (is.null(fill_var)) if (dark) "grey90" else "grey30" else NULL
    # 
    # plot <- plot + ggplot2::geom_text(
    #   data = summary_data,
    #   aes(x = .data[[x_var]], y = upci, label = round(mean, 1)),
    #   color = label_fixed_color,
    #   size = label_text_size,
    #   show.legend = FALSE,
    #   inherit.aes = FALSE,
    #   position = ggpp::position_dodge2nudge(
    #     x = mean_nudge_x, y = mean_nudge_y, width = stagger
    #   )
    # )
    
    
    raw_data[[y_var]] <- as.numeric(raw_data[[y_var]])
    
    
    if (!is.null(fill_var)) {
      plot <- plot + ggplot2::geom_text(
        data = summary_data,
        aes(
          x = .data[[x_var]],
          y = mean,
          label = round(mean, 1),
          colour = .data[[fill_var]]
        ),
        size = base_size,
        show.legend = FALSE,
        inherit.aes = FALSE,
        position = ggpp::position_dodge2nudge(
          x = mean_nudge_x, y = mean_nudge_y,
          width = stagger
        )
      )
    } else {
      plot <- plot + 
        ggplot2::aes(fill = base_palette[[1]], show.legend = FALSE) +
        ggplot2::geom_text(
          data = summary_data,
          aes(
            x = .data[[x_var]],
            y = mean,
            label = round(mean, 1)
          ),
          color = if (dark) "grey90" else "grey30",
          size = base_size,
          show.legend = FALSE,
          inherit.aes = FALSE,
          position = ggpp::position_dodge2nudge(
            x = mean_nudge_x, y = mean_nudge_y,
            width = stagger
          )
        )
    }
    
    
    # n-labels
    plot <- plot + geom_label(
      data = summary_data,
      aes(x = .data[[x_var]], y = min(raw_data[[y_var]]), label = paste("n =", n)),
      fill = if (dark) "grey30" else "grey50",
      color = if (dark) "grey90" else "grey10",
      alpha = if (dark) 0.3 else 0.1,
      label.size = 0,
      
      position = position_dodge2(width = stagger),
      hjust = hbump, 
      vjust = vbump,
      
      size = base_size * n_label_rel
    )
  }
  
  if (is.null(fill_var)) {
    plot <- plot + theme(legend.position = "none") 
  }
  
  
  plot <- plot + theme(   text = (element_text(size = base_size)),    
                          axis.text = element_text(size = axis_text_rel*base_size
                                                   # rel(axis_text_rel)
                          ),
                          axis.title = element_text(size = 
                                                      # rel(
                                                      axis_title_rel*base_size, face = "bold"),
                          legend.title = element_text(size = axis_title_rel*base_size*.8,
                                                      # rel(
                                                      # axis_title_rel
                                                      face = "bold", hjust = 0.5),
                          legend.text = element_text(size = 
                                                       base_size*axis_text_rel, hjust = 0.5),
                          legend.key.size = unit(base_size*axis_text_rel, "pt"),
                          strip.text = element_text(size = axis_text_rel*base_size)) +
    labs(
      x = ifelse(x_var %in% names(pretty_labels), pretty_labels[[x_var]], x_var),
      y = ifelse(y_var %in% names(pretty_labels), pretty_labels[[y_var]], y_var),
      fill = ifelse(fill_var %in% names(pretty_labels), pretty_labels[[fill_var]], fill_var),
      color= ifelse(fill_var %in% names(pretty_labels), pretty_labels[[fill_var]], fill_var)
    )
  
  
  
  
  return(plot)
}










stitching(
  raw_data = merged,
  # summary_data = purchase_amount_summary,
  y_var = "purchase_amount",
  x_var = "value_for_money",
  
  fill_var = "customer_segment",
  
)

stitching(
  raw_data = merged,
  # summary_data = purchase_amount_summary,
  y_var = "purchase_amount",
  x_var = "store",
  density = FALSE,
  bar = TRUE,
  dark = FALSE
)

stitching(
  raw_data = merged,
  # summary_data = purchase_amount_summary,
  y_var = "purchase_amount",
  x_var = "store",
  
  flipped = TRUE,
  bar = TRUE,
  density = TRUE,
  dark = FALSE
)

stitching(
  raw_data = merged,
  # summary_data = purchase_amount_summary,
  y_var = "purchase_amount",
  x_var = "store",
  fill_var = "customer_segment",
  facet_var = "online_vs_instore",
  density = FALSE,
  dark = FALSE,
  bar = TRUE
)

stitching(
  raw_data = merged,
  # summary_data = purchase_amount_summary,
  y_var = "purchase_amount",
  x_var = "store",
  fill_var = "customer_segment",
  density = FALSE,
  dark = FALSE,
  flipped = TRUE,
  bar = TRUE
  
)

stitching(
  raw_data = merged,
  # summary_data = purchase_amount_summary,
  y_var = "purchase_amount",
  x_var = "store",
  fill_var = "customer_segment",
  density = TRUE,
  dark = FALSE,
  bar = TRUE
)

stitching(
  raw_data = merged,
  # summary_data = purchase_amount_summary,
  y_var = "purchase_amount",
  x_var = "store",
  fill_var = "customer_segment",
  density = TRUE,
  flipped = TRUE,
  dark = FALSE    
)







server <- function(input, output, session) {
  observeEvent(input$dark_mode, {
    if (input$dark_mode == "dark") {
      shinyjs::addClass("body", "bg-dark text-light")
    } else {
      shinyjs::removeClass("body", "bg-dark text-light")
    }
  })
  
  
  
  
  observe({
    num_vars_trans <- names(transactions_revised)[sapply(transactions_revised, is.numeric)]
    num_vars_survey <- names(survey_data)[sapply(survey_data, is.numeric)]
    
    # Apply pretty labels, falling back to raw name if not found
    label_lookup <- function(var, prefix) {
      label <- ifelse(var %in% names(pretty_labels), pretty_labels[[var]], var)
      paste(prefix, "-", label)
    }
    
    trans_labels <- setNames(num_vars_trans, sapply(num_vars_trans, label_lookup, prefix = "Transactions"))
    survey_labels <- setNames(num_vars_survey, sapply(num_vars_survey, label_lookup, prefix = "Survey"))
    
    num_vars <- c(trans_labels, survey_labels)
    
    updateSelectInput(session, "selected_var", choices = num_vars, selected = num_vars[1])
  })
  
  
  
  filtered_summary_data <- reactive({
    transactions_revised %>%
      filter(
        created_at >= input$date_range[1],
        created_at <= input$date_range[2]
      ) %>%
      filter(if (length(input$store_filter) > 0) store %in% input$store_filter else TRUE) %>%
      mutate(time_unit = as.Date(cut(created_at, breaks = input$time_unit))) %>%
      group_by(time_unit, store = if (length(input$store_filter) == 0) "Total" else store) %>%
      summarise(
        total_revenue = sum(transaction_value, na.rm = TRUE),
        total_items_sold = sum(num_items_sold, na.rm = TRUE),
        av_sale_value = mean(av_sale_value, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  
  filtered_survey_data <- reactive({
    survey_data %>%
      filter(
        created_at >= input$date_range[1],
        created_at <= input$date_range[2]
      ) %>%
      filter(if (length(input$store_filter) > 0) store %in% input$store_filter else TRUE) %>%
      mutate(time_unit = as.Date(cut(created_at, breaks = input$time_unit))) %>%
      group_by(time_unit, store = if (length(input$store_filter) == 0) "Total" else store) %>%
      summarise(
        av_satisfaction = mean(satisfaction_score, na.rm = TRUE),
        av_would_recommend = mean(would_recommend, na.rm = TRUE),
        av_ease_of_purchase = mean(ease_of_purchase, na.rm = TRUE),
        av_staff_friendliness = mean(staff_friendliness, na.rm = TRUE),
        av_product_quality = mean(product_quality, na.rm = TRUE),
        av_value_for_money = mean(value_for_money, na.rm = TRUE),
        av_visit_frequency = mean(visit_frequency, na.rm = TRUE),
        av_purchase_amount = mean(purchase_amount, na.rm = TRUE),
        av_customer_age = mean(customer_age, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  
  # , font_scale = 0.9
  
  
  aggregated_data <- reactive({
    req(input$time_unit, input$selected_var)
    
    # Extract selected variable name without prefix
    selected_var <- sub(".* - ", "", input$selected_var)
    
    # Check which dataset the variable belongs to
    if (selected_var %in% names(transactions_revised)) {
      data <- transactions_revised %>%
        filter(created_at >= input$date_range[1], created_at <= input$date_range[2]) %>%
        filter(if (length(input$store_filter) > 0) store %in% input$store_filter else TRUE) %>%
        mutate(time_unit = as.Date(cut(created_at, breaks = input$time_unit))) %>%
        group_by(time_unit, store = if (length(input$store_filter) == 0) "Total" else store) %>%
        summarise(across(all_of(selected_var), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    } else {
      data <- survey_data %>%
        filter(created_at >= input$date_range[1], created_at <= input$date_range[2]) %>%
        filter(if (length(input$store_filter) > 0) store %in% input$store_filter else TRUE) %>%
        mutate(time_unit = as.Date(cut(created_at, breaks = input$time_unit))) %>%
        group_by(time_unit, store = if (length(input$store_filter) == 0) "Total" else store)
      # %>%
      #   summarise(across(all_of(selected_var), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
    }
    
    return(data)
  })
  
  
  filtered_merged_data <- reactive({
    merged <- merged %>% filter(
      created_at >= input$date_range[1],
      created_at <= input$date_range[2]
    ) %>% filter(if (length(input$store_filter) > 0) 
      store %in% input$store_filter else TRUE) 
    
    
  })
  
  
  
  
  output$revenue_box <- renderUI({
    data <- filtered_summary_data()
    total <- sum(data$total_revenue, na.rm = TRUE)
    formatted_total <- scales::dollar(round(total), accuracy = 1)
    
    value_box(
      title = "Total Sales Revenue",
      value = formatted_total,
      showcase = bs_icon("bar-chart", size = "1em"),
      
      style = "font-size: 0.85rem; padding: 0.2rem; min-height: 50px;",
      theme = value_box_theme(bg = "#604D75", fg = "#e6f2fd"),
      # p("The 1st detail")
    )
  })
  
  
  
  output$items_sold_box <- renderUI({
    data <- filtered_summary_data()
    total_items <- sum(data$total_items_sold, na.rm = TRUE)
    formatted_total <- format(round(total_items), big.mark = ",")  # e.g., 1,234
    
    
    av_sale_value <- mean(data$av_sale_value, na.rm = TRUE)
    formatted_av_sale_value <- scales::dollar(round(av_sale_value), accuracy = 1)  # e.g., 1,234
    
    
    
    
    value_box(
      title = "Total Items Sold",
      value = formatted_total,
      showcase = bs_icon("basket", size = "1em"),
      theme = value_box_theme(bg = "#356859", fg = "#ffffff"),
      # style = "min-height: 140px; padding: 1rem;",
      style = "font-size: 0.0001rem; padding: 0.2rem; min-height: 50px;",
      p(paste0("Average revenue per sale: ", formatted_av_sale_value))
    )
  })
  
  
  
  output$satisfaction_box <- renderUI({
    data <- filtered_survey_data()
    total <- mean(data$av_satisfaction, na.rm = TRUE)
    formatted_total <- round(total, 1)
    
    
    value_box(
      title = "Customer Satisfaction",
      value = paste0(formatted_total, " / 7"),
      
      showcase = bs_icon("emoji-smile", size = "1em"),
      showcase_layout = c("left center"),
      theme = value_box_theme(bg = "#444444", fg = "#ffffff"),
      style = "font-size: 0.85rem; padding: 0.2rem; min-height: 50px;",
      # p("The 2nd detail")
    )
  }
  )
  
  
  output$time_series_plot <- renderPlot({
    req(input$selected_var)
    
    y_var <- sub(".* - ", "", input$selected_var)
    plot_data <- aggregated_data()
    
    # Choose geom and adjust mapping and legend suppression
    if (y_var %in% names(transactions_revised)) {
      
      # plot_data <- aggregated_data()
      
      
      time_plot <- ggplot(plot_data, aes(x = time_unit, y = .data[[y_var]], fill = store)) +
        geom_col(alpha = .5) +
        # guides(color = "none") +
        # labs(
        #   title = paste("Time Series of", input$selected_var),
        #   x = "Time",
        #   y = input$selected_var
        # ) +
        custom_theme(input$dark_mode == "dark")
      
    } else {
      time_plot <- ggplot(plot_data, aes(x = time_unit, y = .data[[y_var]], color = store)) +
        # geom_line(size = 1.2) +
        
        geom_smooth(alpha = .5)+
        
        geom_jitter(alpha = .5)+
        # guides(color = "none") +
        # labs(
        #   x = "Time",
        #   y = input$selected_var
        # ) +
        custom_theme(input$dark_mode == "dark") + 
        labs(caption = str_wrap(
          paste0("NOTE:  ", "Dots jittered to avoid overplotting. ", "Shaded area = 95% confidence intervals, showing where the trend is expected to be 95 times out of 100. "), 120))
    }
    
    time_plot <- time_plot + scale_y_continuous(expand = expansion(mult = c(0, 0.00))) +
      labs(
        
        title = paste("Time Series of", 
                      ifelse(input$selected_var %in% names(pretty_labels), 
                             pretty_labels[[input$selected_var]], 
                             input$selected_var)), 
        x = "Time",
        y = ifelse(y_var %in% names(pretty_labels), pretty_labels[[y_var]], y_var),
        # fill_var = ifelse(fill_var %in% names(pretty_labels), pretty_labels[[fill_var]], fill_var)
      ) +
      
      theme(
        legend.position = "right",
        legend.justification = "center",
        legend.box = "vertical",
        legend.box.just = "center",
      ) +
      scale_fill_manual(values = nova_palette) +
      scale_color_manual(values = nova_palette) 
    
    
    time_plot
    
  }
  )
  
  
  
  
  
  # inside server(), *before* your renderPlot calls
  plot_summary_with_shading <- function(data, y_var, x_var, fill_var = NULL, facet_var = NULL, 
                                        base_palette = nova_palette, flipped = FALSE,
                                        x_title = NULL, y_title = NULL, fill_title = NULL, ...) {
    
    if (identical(y_var, ""))    y_var    <- NULL
    if (identical(x_var, ""))    x_var    <- NULL
    if (identical(fill_var, "")) fill_var <- NULL
    if (identical(facet_var, "")) facet_var<- NULL
    
    
    flipped <- input$flipped
    
    
    group_vars <- c(x_var, fill_var, facet_var)
    group_vars <- group_vars[!vapply(group_vars, is.null, logical(1))]
    
    
    # ensure we only pass each grouping var once
    group_vars <- unique(c(x_var, fill_var, facet_var))
    
    summary_df <- run_summary(
      data             = data,
      group_vars       = group_vars,
      summarization_var = y_var
    )
    
    plot <- create_socia_shadeplot(
      raw_data     = data,
      summary_data = summary_df,
      base_palette = nova_palette,
      y_var        = y_var,
      x_var        = x_var,
      fill_var     = fill_var,
      dotsize = input$dotsize,
      
      flipped = input$flipped,
      # dots = input$dots,
      dark =  (input$dark_mode == "dark"),
      # mean_nudge_y = input$mean_nudge_y,
      # mean_nudge_x = input$mean_nudge_x,
      x_title      = x_title,
      y_title      = names(
        c(
          "satisfaction_score"  = "Satisfaction",
          "ease_of_purchase"    = "Ease of Purchase",
          "staff_friendliness"  = "Staff Friendliness",
          "product_quality"     = "Product Quality",
          "value_for_money"     = "Value for Money"
        ))[input$pair_var],
      fill_title   = fill_title,
      facet_var = facet_var, 
      ...
    )  + 
      custom_theme(input$dark_mode == "dark") +
      if (flipped) {
        plot <- plot + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                                      panel.grid.major.x = ggplot2::element_blank()) }
    
    plot <- plot + theme(legend.position = "top", 
                         legend.justification = "center")
    
    plot
    
    
    
  }
  
  
  
  output$pairwise_survey_plot <- renderPlot({
    # req(input$pair_var)
    req(input$pair_var, input$x_var
        # , input$fill_var, input$facet_var
    )
    
    label_map <- c(
      satisfaction_score  = "Satisfaction",
      ease_of_purchase    = "Ease of Purchase",
      staff_friendliness  = "Staff Friendliness",
      product_quality     = "Product Quality",
      value_for_money     = "Value for Money"
    )
    
    
    barplot <- input$barplot
    
    # 2) Lookup y_title by name (and drop the names attribute)
    y_title <- unname(label_map[input$pair_var])
    
    flipped <- input$flipped
    
    df <- filtered_merged_data() %>% 
      ungroup()   # has av_satisfaction, av_ease_of_purchase, etc.
    
    plot  <- plot_summary_with_shading(
      data      = df,
      y_var     = input$pair_var,
      x_var     = input$x_var,
      fill_var  = input$fill_var,
      facet_var   = input$facet_var,
      slabjust = input$slabjust,
      scaling = input$scaling,
      flipped = input$flipped,
      dots = input$dots,
      
      mean_nudge_y = input$mean_nudge_y,
      mean_nudge_x = input$mean_nudge_x,
      
      # dark = (input$dark_mode == "dark"),
      y_title   = y_title,
      fill_title= "Store"
    ) 
    
    
    if (barplot) {
      plot <- plot +  stat_summary(fun = "mean", 
                                   geom = "bar",
                                   # alpha= slabpha,
                                   size = .2,
                                   position = position_dodge(),
                                   # width = .7
      ) 
    }
    
    if (flipped) {
      plot <- plot + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                                    panel.grid.major.x = ggplot2::element_line()) }
    
    
    
    plot
    
    
    # custom_theme(input$dark_mode == "dark")
  }, height = function() {
    
    if (input$facet_var != "") 800 else 500
  })
  
  
  
  
  
  
  
  output$test_survey_plot <- renderPlot({ 
    
    df <- filtered_merged_data() %>% ungroup()
    
    req(nrow(df) > 0, input$pair_var, input$x_var) 
    
    validate(
      need(input$pair_var %in% colnames(df), "Selected y-variable not found."),
      need(input$x_var %in% colnames(df), "Selected x-variable not found.")
    )
    
    stitching(
      raw_data    = df,
      y_var       = input$pair_var,
      x_var       = input$x_var,
      slabjust = input$slabjust,
      slabpha = input$slabpha,
      scaling = input$scaling,
      fill_var    = input$fill_var,
      facet_var   = input$facet_var,
      density = input$density,
      bar = input$barplot,
      bar_alpha = input$bar_alpha,
      flipped = input$flipped,
      dots = input$dots,
      dotsize = input$dotsize,
      mean_nudge_y = input$mean_nudge_y,
      mean_nudge_x = input$mean_nudge_x,
      note = input$note,
      base_size = input$base_size,
      axis_text_rel= input$axis_text_rel,
      axis_title_rel = input$axis_title_rel,
      n_label_rel = input$n_label_rel,
      
      
      dark = (input$dark_mode == "dark")
    )
    
  }, height = function() {
    
    if (input$facet_var != "") 900 else 450
  })
  
  
  
}





# Run the application 
shinyApp(ui = ui, server = server)
