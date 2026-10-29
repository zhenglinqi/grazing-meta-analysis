# =============================================================================
# PROJECT: Plant Responses to Grazing from a Functional Trait Perspective
# SCRIPT: Complete analysis pipeline for the second-revision manuscript
#
# QUICK START
# 1. Place Database.xlsx in the same folder as this script.
# 2. Place the 13 required forest-plot result workbooks listed in Section 0.3
#    in the database folder, script folder, or current working directory.
# 3. Run this script from top to bottom in RStudio or with Rscript.
# 4. All generated results will be saved in NC_Revision_Analysis_Results.
#
# TABLE OF CONTENTS
# 0. Configuration, input discovery, and validation
# 1. Meta-analysis and publication-bias diagnostics
# 2. Forest plots
#    2.1 Overall / functional-group results
#    2.2 Shared forest-plot helpers
#    2.3 Grazing intensity
#    2.4 Aridity classes
#    2.5 Grazing duration
#    2.6 Livestock type
# 3. Spearman correlation heatmap
# 4. Functional trait network (continuous composite centrality)
# 5. RDPI plasticity analysis (four-panel figure: LightG, ModerateG, HighG, FreeG)
# 6. Random forest analysis
#    6.1 Predictor preparation and missing-data handling
#    6.2 Trait-wise model fitting and variable importance
#    6.3 Strong/weak prediction grouping and forest-style plots
#    6.4 Variable-importance heatmap
#    6.5 Partial-dependence response plots
#
# INPUT FILES
# - Raw database: Database.xlsx (Database.xls and Database.xlsm are also supported)
# - Forest-plot tables: see REQUIRED_RESULT_TABLES in Section 0.3.
#
# OUTPUTS
# - All tables and figures are written to NC_Revision_Analysis_Results.
# - Existing analysis methods are retained; section comments are for navigation.
# =============================================================================

# =============================================================================
# SECTION 0.1: PACKAGE CHECK AND ATTACHMENT
# Purpose: Confirm that all required packages are installed before analysis.
# User action: Install any packages named in the error message if needed.
# =============================================================================

required_packages <- c(
  "readxl", "ggplot2", "dplyr", "metafor", "corrplot",
  "igraph", "scales", "mice", "rfPermute", "patchwork"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required R packages: ",
      paste(missing_packages, collapse = ", "),
      ". Install these packages before running the script."
    )
  )
}

invisible(lapply(required_packages, function(pkg) {
  library(pkg, character.only = TRUE)
}))

# =============================================================================
# SECTION 0.2: SCRIPT, DATABASE, AND OUTPUT PATHS
# Purpose: Locate the script and the Database workbook without hard-coded paths.
# User action: Normally no path edits are required if files are placed as described above.
# =============================================================================

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cmd_args[grepl("^--file=", cmd_args)])

  if (length(file_arg) == 1 && file.exists(file_arg)) {
    return(dirname(normalizePath(file_arg, winslash = "/")))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    script_path <- tryCatch(
      rstudioapi::getSourceEditorContext()$path,
      error = function(e) ""
    )

    if (length(script_path) == 1 &&
        nzchar(script_path) &&
        file.exists(script_path)) {
      return(dirname(normalizePath(script_path, winslash = "/")))
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

SCRIPT_DIR <- get_script_dir()

find_database_file <- function(script_dir) {
  database_names <- c("Database.xlsx", "Database.xls", "Database.xlsm")
  search_dirs <- unique(c(script_dir, getwd()))
  candidates <- unlist(
    lapply(search_dirs, function(dir) file.path(dir, database_names)),
    use.names = FALSE
  )
  candidates <- unique(candidates[file.exists(candidates)])

  if (length(candidates) > 0) {
    return(normalizePath(candidates[1], winslash = "/", mustWork = TRUE))
  }

  if (interactive()) {
    selected_file <- file.choose()
    valid_name <- grepl(
      "^Database\\.(xlsx|xls|xlsm)$",
      basename(selected_file),
      ignore.case = TRUE
    )

    if (!file.exists(selected_file) || !valid_name) {
      stop(
        "The selected file must be named Database.xlsx, Database.xls, or Database.xlsm."
      )
    }

    return(normalizePath(selected_file, winslash = "/", mustWork = TRUE))
  }

  stop(
    "Database.xlsx was not found. Place Database.xlsx, Database.xls, or Database.xlsm ",
    "in the script directory or the current working directory."
  )
}

DATABASE_FILE <- find_database_file(SCRIPT_DIR)
DATA_DIR <- dirname(DATABASE_FILE)
OUTPUT_DIR <- file.path(DATA_DIR, "NC_Revision_Analysis_Results")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

output_path <- function(filename) {
  file.path(OUTPUT_DIR, filename)
}

resolve_input_path <- function(filename) {
  search_dirs <- unique(c(DATA_DIR, SCRIPT_DIR, getwd()))
  candidates <- file.path(search_dirs, filename)
  candidates <- unique(candidates[file.exists(candidates)])

  if (length(candidates) == 0) {
    stop(
      sprintf(
        "Required input file '%s' was not found in the database folder, script folder, or working directory.",
        filename
      )
    )
  }

  normalizePath(candidates[1], winslash = "/", mustWork = TRUE)
}

# =============================================================================
# SECTION 0.3: REQUIRED EXTERNAL FOREST-PLOT TABLES
# Purpose: Validate table names and required columns before plotting begins.
# Required columns: Indicator, Mean, Low, Up, Color, N.
# If a table is missing, rename the existing workbook to the expected English filename.
# =============================================================================

forest_required_columns <- c("Indicator", "Mean", "Low", "Up", "Color", "N")

read_result_table <- function(filename, required_columns = forest_required_columns) {
  input_path <- resolve_input_path(filename)
  result_data <- as.data.frame(readxl::read_excel(input_path))
  if (nrow(result_data) == 0) {
    stop(sprintf("Input table '%s' contains zero rows.", filename))
  }

  missing_columns <- setdiff(required_columns, names(result_data))

  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Input table '%s' is missing required columns: %s",
        filename,
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  numeric_columns <- intersect(c("Mean", "Low", "Up", "Color", "N"), names(result_data))
  for (column_name in numeric_columns) {
    result_data[[column_name]] <- suppressWarnings(
      as.numeric(as.character(result_data[[column_name]]))
    )
  }

  result_data
}

REQUIRED_RESULT_TABLES <- c(
  "Overall_New_Results.xlsx",
  "LG_Results.xlsx",
  "MG_Results.xlsx",
  "HG_Results.xlsx",
  "Arid_Results.xlsx",
  "Semi_arid_Results.xlsx",
  "Dry_sub-humid_Results.xlsx",
  "Humid_Results.xlsx",
  "Short_Results.xlsx",
  "Middle_Results.xlsx",
  "Long_Results.xlsx",
  "Sheep_Results.xlsx",
  "Cattle_Results.xlsx"
)

missing_result_tables <- REQUIRED_RESULT_TABLES[
  !vapply(
    REQUIRED_RESULT_TABLES,
    function(filename) {
      search_dirs <- unique(c(DATA_DIR, SCRIPT_DIR, getwd()))
      any(file.exists(file.path(search_dirs, filename)))
    },
    logical(1)
  )
]

if (length(missing_result_tables) > 0) {
  stop(
    paste0(
      "The following forest-plot input tables are missing. Rename the existing result tables ",
      "to the English filenames below and place them beside Database or the script:\n- ",
      paste(missing_result_tables, collapse = "\n- ")
    )
  )
}

message("Database file: ", DATABASE_FILE)
message("Output directory: ", OUTPUT_DIR)


# =============================================================================
# SECTION 1: META-ANALYSIS
# =============================================================================

library(metafor)
library(ggplot2)
library(readxl)

# INPUT: Database workbook, sheet 1; effect-size columns Xt3/Xc3 and their SD/N fields.
# OUTPUT: Meta-analysis model, funnel plot, Egger test, influence diagnostics, and CSV table.
mydata <- read_excel(DATABASE_FILE, sheet = 1)

RR_effect_sizes <- escalc("ROM",
                           m1i  = Xt3,  n1i = Nt3, sd1i = SDt3,
                           m2i  = Xc3,  n2i = Nc3, sd2i = SDc3,
                           data = mydata)

random_effect_model_results <- rma(yi, vi, method = "REML",
                                   data = RR_effect_sizes)
random_effect_model_results

funnel(random_effect_model_results, main = "Leaf Length", level = 0.95)

egger_test <- regtest(random_effect_model_results)
egger_test

influence_results <- influence(random_effect_model_results)
plot(influence_results)

write.csv(data.frame(yi = random_effect_model_results$yi,
                            vi = random_effect_model_results$vi),
          file = output_path("Meta_Analysis_Leaf_Length_Effect_Sizes.csv"),
          row.names = FALSE)


# =============================================================================
# SECTION 2: FOREST PLOTS
# =============================================================================

library(readxl)
library(ggplot2)
library(dplyr)
library(grid)

# -----------------------------------------------------------------------------
# 2-1. Total / Functional Groups (FG) forest plot
# -----------------------------------------------------------------------------

# INPUT: Overall_New_Results.xlsx.
# OUTPUT: Overall / functional-group forest plot files in OUTPUT_DIR.
data1 <- read_result_table("Overall_New_Results.xlsx")
data1$Indicator <- factor(data1$Indicator, levels = data1$Indicator)

forest_plot_ld <- ggplot(
  data1,
  aes(y = Indicator, x = Mean, xmin = Low, xmax = Up,
      fill  = as.factor(Color),
      color = as.factor(Color))
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin= 0.5, ymax= 1.5, alpha=0.6, fill="gray79") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin= 1.5, ymax= 2.5, alpha=0.6, fill="white")  +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin= 2.5, ymax= 3.5, alpha=0.6, fill="gray79") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin= 3.5, ymax= 4.5, alpha=0.6, fill="white")  +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin= 4.5, ymax= 5.5, alpha=0.6, fill="gray79") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin= 5.5, ymax= 6.5, alpha=0.6, fill="white")  +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin= 6.5, ymax= 7.5, alpha=0.6, fill="gray79") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin= 7.5, ymax= 8.5, alpha=0.6, fill="white")  +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin= 8.5, ymax= 9.5, alpha=0.6, fill="gray79") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin= 9.5, ymax=10.5, alpha=0.6, fill="white")  +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=10.5, ymax=11.5, alpha=0.6, fill="gray79") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=11.5, ymax=12.5, alpha=0.6, fill="white")  +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=12.5, ymax=13.5, alpha=0.6, fill="gray79") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=13.5, ymax=14.5, alpha=0.6, fill="white")  +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=14.5, ymax=15.5, alpha=0.6, fill="gray79") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=15.5, ymax=16.5, alpha=0.6, fill="white")  +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=16.5, ymax=17.5, alpha=0.6, fill="gray79") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=17.5, ymax=18.5, alpha=0.6, fill="white")  +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=18.5, ymax=19.5, alpha=0.6, fill="gray79") +
  geom_errorbarh(height = 0, linewidth = 0.8) +
  geom_point(size = 8, shape = 21, stroke = 1) +
  geom_text(
    aes(x = 1.47, y = Indicator, label = paste0("(", N, ")")),
    color        = "black",
    size         = 3.8,
    hjust        = 1,
    inherit.aes  = FALSE,
    data         = data1
  ) +
  coord_cartesian(clip = "off") +
  labs(title = "Total", x = "Weighted ln(RR)", y = NULL) +
  scale_x_continuous(limits = c(-1.5, 1.5),
                     breaks  = seq(-1.5, 1.5, by = 0.5)) +
  scale_y_discrete(limits = rev(levels(data1$Indicator))) +
  scale_fill_manual(values  = c("1"="#79bb56", "0"="#000000", "2"="#c0392b")) +
  scale_color_manual(values = c("1"="#79bb56", "0"="#000000", "2"="#c0392b")) +
  theme_classic(base_size = 13) +
  theme(
    plot.title      = element_text(hjust = 0.5, size = 20),
    legend.position = "none",
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.9),
    axis.line       = element_blank(),
    axis.text.y     = element_text(size = 18),
    axis.title.x    = element_text(size = 18),
    axis.text.x     = element_text(size = 16)
  )

ggsave(output_path("Grazing_FG.jpeg"), plot = forest_plot_ld, device = "jpeg",
       width = 8, height = 10, units = "in", dpi = 600)


# -----------------------------------------------------------------------------
# 2-2. Shared helper objects for sub-group forest plots
# -----------------------------------------------------------------------------

ind_order <- c("Height", "AGB",
                "LL", "LW", "LA", "LT", "LN", "LB",
                "SD", "SB",
                "BGB", "RD", "RL", "RSA", "RV", "RDen",
                "SLA", "SRL", "S:L", "R:S")

make_ind_bg <- function(ind_levels, row_block) {
  data.frame(
    Indicator = ind_levels,
    y_bot     = (seq_along(ind_levels) - 1) * row_block,
    y_top     =  seq_along(ind_levels)      * row_block
  ) %>%
    mutate(bg = ifelse(seq_len(n()) %% 2 == 1, "#e8e8e8", "white"))
}

make_ind_centers <- function(ind_levels, row_block) {
  data.frame(
    Indicator = factor(ind_levels, levels = ind_levels),
    y_center  = (seq_along(ind_levels) - 1) * row_block + row_block / 2
  )
}

shared_theme <- function(legend_x, legend_y) {
  theme_classic(base_size = 13) +
    theme(
      panel.border         = element_rect(color = "black", fill = NA, linewidth = 0.9),
      axis.line            = element_blank(),
      axis.text.y          = element_blank(),
      axis.ticks.y         = element_blank(),
      axis.text.x          = element_text(size = 16, color = "black"),
      axis.title.x         = element_text(size = 18, color = "black"),
      legend.position      = c(legend_x, legend_y),
      legend.justification = c(0, 0),
      legend.title         = element_text(size = 11.5, face = "bold"),
      legend.text          = element_text(size = 11),
      legend.key.size      = unit(0.58, "cm"),
      legend.background    = element_rect(fill = "white", color = "black",
                                          linewidth = 0.65),
      legend.margin        = margin(5, 10, 5, 10),
      plot.margin          = margin(t = 5, r = 10, b = 5, l = 60, unit = "pt"),
      panel.grid           = element_blank()
    )
}


# -----------------------------------------------------------------------------
# 2-3. Grazing Intensity Forest Plot
# -----------------------------------------------------------------------------

# INPUT: LG_Results.xlsx, MG_Results.xlsx, and HG_Results.xlsx.
# OUTPUT: Grazing-intensity forest plot with LightG, ModerateG, and HighG labels.
lg <- read_result_table("LG_Results.xlsx")
mg <- read_result_table("MG_Results.xlsx")
hg <- read_result_table("HG_Results.xlsx")

lg$Intensity <- "LightG"
mg$Intensity <- "ModerateG"
hg$Intensity <- "HighG"

dat <- bind_rows(lg, mg, hg)

ind_order <- c(
  "Height", "AGB",
  "LL", "LW", "LA", "LT", "LN", "LB",
  "SD", "SB",
  "BGB", "RD", "RL", "RSA", "RV", "RDen",
  "SLA", "SRL", "S:L", "R:S"
)

intensity_order <- c("LightG", "ModerateG", "HighG")

dat <- dat %>%
  mutate(
    Indicator = factor(
      Indicator,
      levels = rev(ind_order)
    ),
    Intensity = factor(
      Intensity,
      levels = rev(intensity_order)
    )
  )

# Color mapping: 1 = significant decrease, 2 = significant increase, 0 = not significant.

dat <- dat %>%
  mutate(
    show_point = !is.na(Color),

    pt_fill = case_when(
      Color == 1 ~ "#79bb56",
      Color == 2 ~ "#c0392b",
      Color == 0 ~ "white",
      TRUE ~ NA_character_
    ),

    pt_color = case_when(
      Color == 1 ~ "#79bb56",
      Color == 2 ~ "#c0392b",
      Color == 0 ~ "black",
      TRUE ~ NA_character_
    ),

    N_label = ifelse(
      is.na(N) | N == 0,
      NA_character_,
      paste0("(", as.integer(N), ")")
    )
  )

# Intensity order: LightG, ModerateG, HighG

n_intensity <- 3

row_block <- 5.0

pad <- row_block * 0.20

row_h <- (
  row_block - 2 * pad
) / (n_intensity - 1)

ind_levels <- levels(dat$Indicator)

int_levels <- levels(dat$Intensity)

y_base <- setNames(
  (seq_along(ind_levels) - 1) * row_block + pad,
  ind_levels
)

# HighG = 0
# ModerateG = row_h
# LightG = 2 * row_h

int_offset <- setNames(
  (seq_along(int_levels) - 1) * row_h,
  int_levels
)

dat <- dat %>%
  mutate(
    y_pos = y_base[as.character(Indicator)] +
      int_offset[as.character(Intensity)]
  )

ind_centers <- data.frame(
  Indicator = factor(
    ind_levels,
    levels = ind_levels
  ),

  y_center = (
    seq_along(ind_levels) - 1
  ) * row_block + row_block / 2
)

ind_bg <- data.frame(
  Indicator = ind_levels,

  y_bot = (
    seq_along(ind_levels) - 1
  ) * row_block,

  y_top = seq_along(ind_levels) * row_block
) %>%
  mutate(
    bg = ifelse(
      seq_len(n()) %% 2 == 1,
      "#e8e8e8",
      "white"
    )
  )

y_min_plot <- 0

y_max_plot <- length(ind_levels) * row_block

x_lo <- -1.5
x_hi <- 1.5

n_x_pos <- x_hi - 0.03

legend_x <- 0.72
legend_y <- 0.90

p <- ggplot() +

  geom_rect(
    data = ind_bg,

    aes(
      xmin = -Inf,
      xmax = Inf,
      ymin = y_bot,
      ymax = y_top
    ),

    fill = ind_bg$bg,
    inherit.aes = FALSE
  ) +

  geom_segment(
    data = filter(dat, show_point),

    aes(
      x = Low,
      xend = Up,
      y = y_pos,
      yend = y_pos,
      color = I(pt_color)
    ),

    linewidth = 0.7
  ) +

  geom_point(
    data = filter(dat, show_point),

    aes(
      x = Mean,
      y = y_pos,
      shape = Intensity,
      fill = I(pt_fill),
      color = I(pt_color)
    ),

    size = 4.0,
    stroke = 0.9
  ) +

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.7,
    color = "#c0392b"
  ) +

  geom_text(
    data = filter(
      dat,
      !is.na(N_label)
    ),

    aes(
      x = n_x_pos,
      y = y_pos,
      label = N_label
    ),

    hjust = 1,
    size = 3.8,
    color = "#333333"
  ) +

  geom_text(
    data = ind_centers,

    aes(
      x = x_lo - 0.04,
      y = y_center,
      label = as.character(Indicator)
    ),

    hjust = 1,
    size = 6.5,
    color = "black"
  ) +

scale_shape_manual(
  name = "Grazing Intensity",

  values = c(
    "LightG" = 21,
    "ModerateG" = 24,
    "HighG" = 23
  ),

  breaks = intensity_order,

  guide = guide_legend(
    override.aes = list(
      fill = "white",
      color = "black",
      size = 4.5,
      stroke = 0.9
    )
  )
) +

scale_x_continuous(
  breaks = seq(
    -1.5,
    1.5,
    by = 0.5
  ),

  limits = c(
    x_lo,
    x_hi
  ),

  oob = scales::oob_keep
) +

  coord_cartesian(
    xlim = c(
      x_lo,
      x_hi
    ),

    ylim = c(
      y_min_plot,
      y_max_plot
    ),

    expand = FALSE,
    clip = "off"
  ) +

  labs(
    x = "Weighted ln(RR)",
    y = NULL
  ) +

  theme_classic(
    base_size = 13
  ) +

  theme(
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.9
    ),

    axis.line = element_blank(),

    axis.text.y = element_blank(),

    axis.ticks.y = element_blank(),

    axis.text.x = element_text(
      size = 16,
      color = "black"
    ),

    axis.title.x = element_text(
      size = 18,
      color = "black"
    ),

    legend.position = c(
      legend_x,
      legend_y
    ),

    legend.justification = c(
      0,
      0
    ),

    legend.title = element_text(
      size = 11.5,
      face = "bold"
    ),

    legend.text = element_text(
      size = 11
    ),

    legend.key.size = grid::unit(
      0.58,
      "cm"
    ),

    legend.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 0.65
    ),

    legend.margin = margin(
      5,
      10,
      5,
      10
    ),

    plot.margin = margin(
      t = 5,
      r = 10,
      b = 5,
      l = 60,
      unit = "pt"
    ),

    panel.grid = element_blank()
  )

print(p)

ggsave(
  output_path("Grazing_Intensity_forest_plot.pdf"),
  plot = p,
  width = 8.5,
  height = 13,
  units = "in"
)

ggsave(
  output_path("Grazing_Intensity_forest_plot.png"),
  plot = p,
  width = 8.5,
  height = 13,
  units = "in",
  dpi = 300
)

message(
  "Completed: Grazing_Intensity_forest_plot.pdf / .png"
)

message(
  sprintf(
    "Legend coordinates: legend_x = %.2f, legend_y = %.2f",
    legend_x,
    legend_y
  )
)

message(
  "Grazing intensity labels: LightG, ModerateG, HighG"
)


# -----------------------------------------------------------------------------
# 2-4. Aridity zone forest plot
# -----------------------------------------------------------------------------

# INPUT: Arid_Results.xlsx, Semi_arid_Results.xlsx, Dry_sub-humid_Results.xlsx, and Humid_Results.xlsx.
# OUTPUT: Aridity-class forest plot files in OUTPUT_DIR.
arid <- read_result_table("Arid_Results.xlsx");         arid$Climate <- "Arid"
semi <- read_result_table("Semi_arid_Results.xlsx");    semi$Climate <- "Semi-arid"
dry  <- read_result_table("Dry_sub-humid_Results.xlsx"); dry$Climate  <- "Dry sub-humid"
hum  <- read_result_table("Humid_Results.xlsx");        hum$Climate  <- "Humid"

dat <- bind_rows(hum, dry, semi, arid)

group_info <- data.frame(
  Indicator = ind_order,
  Group = c(rep("Growth",2), rep("Leaf traits",6), rep("Stem traits",2),
            rep("Root traits",6), rep("Functional traits",4)),
  stringsAsFactors = FALSE
)

climate_order <- c("Humid", "Dry sub-humid", "Semi-arid", "Arid")

dat <- dat %>%
  left_join(group_info, by="Indicator") %>%
  mutate(
    Indicator = factor(Indicator, levels=rev(ind_order)),
    Climate   = factor(Climate,   levels=rev(climate_order)),
    Group     = factor(Group,     levels=unique(group_info$Group))
  ) %>%
  mutate(
    is_placeholder = (Mean==0 & Low==0 & Up==0 & is.na(Color)),
    show_point     = !is_placeholder & !is.na(Color),
    pt_fill  = case_when(is_placeholder~NA_character_, Color==1~"#79bb56",
                         Color==2~"#c0392b", Color==0~"white", TRUE~NA_character_),
    pt_color = case_when(is_placeholder~NA_character_, Color==1~"#79bb56",
                         Color==2~"#c0392b", Color==0~"black", TRUE~NA_character_),
    N_label  = ifelse(is_placeholder|is.na(N)|N==0, NA_character_,
                      paste0("(",as.integer(N),")"))
  )

row_block <- 6.0
pad       <- row_block * 0.20
row_h     <- (row_block - 2*pad) / 3

ind_levels  <- levels(dat$Indicator)
cli_levels  <- levels(dat$Climate)

y_base     <- setNames((seq_along(ind_levels)-1)*row_block + pad, ind_levels)
cli_offset <- setNames((seq_along(cli_levels)-1)*row_h, cli_levels)

dat <- dat %>%
  mutate(y_pos = y_base[as.character(Indicator)] + cli_offset[as.character(Climate)])

ind_centers <- make_ind_centers(ind_levels, row_block)
ind_bg      <- make_ind_bg(ind_levels, row_block)
y_min_plot  <- 0
y_max_plot  <- length(ind_levels) * row_block
x_lo <- -1.5; x_hi <- 1.5; n_x_pos <- x_hi - 0.03

legend_x <- 0.72
legend_y <- 0.90

p_aridity <- ggplot() +
  geom_rect(data=ind_bg, aes(xmin=-Inf,xmax=Inf,ymin=y_bot,ymax=y_top),
            fill=ind_bg$bg, inherit.aes=FALSE) +
  geom_segment(data=filter(dat,show_point),
               aes(x=Low,xend=Up,y=y_pos,yend=y_pos,color=I(pt_color)),
               linewidth=0.7) +
  geom_point(data=filter(dat,show_point),
             aes(x=Mean,y=y_pos,shape=Climate,
                 fill=I(pt_fill),color=I(pt_color)),
             size=4.0, stroke=0.9) +
  geom_vline(xintercept=0, linetype="dashed", linewidth=0.7, color="#c0392b") +
  geom_text(data=filter(dat,!is.na(N_label)),
            aes(x=n_x_pos,y=y_pos,label=N_label),
            hjust=1, size=3.8, color="#333333") +
  geom_text(data=ind_centers,
            aes(x=x_lo-0.04,y=y_center,label=as.character(Indicator)),
            hjust=1, size=6.5, color="black") +
  scale_shape_manual(
    name   = "Climate Area",
    values = c("Humid"=22,"Dry sub-humid"=24,"Semi-arid"=21,"Arid"=23),
    breaks = climate_order,
    guide  = guide_legend(override.aes=list(fill="white",color="black",
                                            size=4.5,stroke=0.9))
  ) +
  scale_x_continuous(breaks=seq(-1.5,1.5,by=0.5), limits=c(x_lo,x_hi),
                     oob=scales::oob_keep) +
  coord_cartesian(xlim=c(x_lo,x_hi), ylim=c(y_min_plot,y_max_plot),
                  expand=FALSE, clip="off") +
  labs(x="Weighted ln(RR)", y=NULL) +
  shared_theme(legend_x, legend_y)

ggsave(output_path("Grazing_Aridity_forest_plot.pdf"), plot=p_aridity, width=8.5, height=15, units="in")
ggsave(output_path("Grazing_Aridity_forest_plot.png"), plot=p_aridity, width=8.5, height=15, units="in", dpi=600)


# -----------------------------------------------------------------------------
# 2-5. Grazing Duration forest plot
# -----------------------------------------------------------------------------

# INPUT: Short_Results.xlsx, Middle_Results.xlsx, and Long_Results.xlsx.
# OUTPUT: Grazing-duration forest plot files in OUTPUT_DIR.
short  <- read_result_table("Short_Results.xlsx");  short$Duration  <- "Short"
middle <- read_result_table("Middle_Results.xlsx"); middle$Duration <- "Middle"
long   <- read_result_table("Long_Results.xlsx");   long$Duration   <- "Long"

dat <- bind_rows(short, middle, long)

duration_order <- c("Short", "Middle", "Long")

dat <- dat %>%
  mutate(
    Indicator = factor(Indicator, levels=rev(ind_order)),
    Duration  = factor(Duration,  levels=rev(duration_order))
  ) %>%
  mutate(
    show_point = !is.na(Color),
    pt_fill  = case_when(Color==1~"#79bb56", Color==2~"#c0392b",
                         Color==0~"white",   TRUE~NA_character_),
    pt_color = case_when(Color==1~"#79bb56", Color==2~"#c0392b",
                         Color==0~"black",   TRUE~NA_character_),
    N_label  = ifelse(is.na(N)|N==0, NA_character_, paste0("(",as.integer(N),")"))
  )

n_dur     <- 3
row_block <- 5.0
pad       <- row_block * 0.20
row_h     <- (row_block - 2*pad) / (n_dur - 1)

ind_levels  <- levels(dat$Indicator)
dur_levels  <- levels(dat$Duration)

y_base     <- setNames((seq_along(ind_levels)-1)*row_block + pad, ind_levels)
dur_offset <- setNames((seq_along(dur_levels)-1)*row_h, dur_levels)

dat <- dat %>%
  mutate(y_pos = y_base[as.character(Indicator)] + dur_offset[as.character(Duration)])

ind_centers <- make_ind_centers(ind_levels, row_block)
ind_bg      <- make_ind_bg(ind_levels, row_block)
y_min_plot  <- 0
y_max_plot  <- length(ind_levels) * row_block
x_lo <- -1.5; x_hi <- 1.5; n_x_pos <- x_hi - 0.03

legend_x <- 0.72
legend_y <- 0.90

p_duration <- ggplot() +
  geom_rect(data=ind_bg, aes(xmin=-Inf,xmax=Inf,ymin=y_bot,ymax=y_top),
            fill=ind_bg$bg, inherit.aes=FALSE) +
  geom_segment(data=filter(dat,show_point),
               aes(x=Low,xend=Up,y=y_pos,yend=y_pos,color=I(pt_color)),
               linewidth=0.7) +
  geom_point(data=filter(dat,show_point),
             aes(x=Mean,y=y_pos,shape=Duration,
                 fill=I(pt_fill),color=I(pt_color)),
             size=4.0, stroke=0.9) +
  geom_vline(xintercept=0, linetype="dashed", linewidth=0.7, color="#c0392b") +
  geom_text(data=filter(dat,!is.na(N_label)),
            aes(x=n_x_pos,y=y_pos,label=N_label),
            hjust=1, size=3.8, color="#333333") +
  geom_text(data=ind_centers,
            aes(x=x_lo-0.04,y=y_center,label=as.character(Indicator)),
            hjust=1, size=6.5, color="black") +
  scale_shape_manual(
    name   = "Grazing Duration",
    values = c("Short"=21,"Middle"=24,"Long"=23),
    breaks = duration_order,
    guide  = guide_legend(override.aes=list(fill="white",color="black",
                                            size=4.5,stroke=0.9))
  ) +
  scale_x_continuous(breaks=seq(-1.5,1.5,by=0.5), limits=c(x_lo,x_hi),
                     oob=scales::oob_keep) +
  coord_cartesian(xlim=c(x_lo,x_hi), ylim=c(y_min_plot,y_max_plot),
                  expand=FALSE, clip="off") +
  labs(x="Weighted ln(RR)", y=NULL) +
  shared_theme(legend_x, legend_y)

ggsave(output_path("Grazing_Duration_forest_plot.pdf"), plot=p_duration, width=8.5, height=13, units="in")
ggsave(output_path("Grazing_Duration_forest_plot.png"), plot=p_duration, width=8.5, height=13, units="in", dpi=300)


# -----------------------------------------------------------------------------
# 2-6. Livestock Type forest plot
# -----------------------------------------------------------------------------

# INPUT: Sheep_Results.xlsx and Cattle_Results.xlsx.
# OUTPUT: Livestock-type forest plot files in OUTPUT_DIR.
sheep  <- read_result_table("Sheep_Results.xlsx");  sheep$Type  <- "Sheep"
cattle <- read_result_table("Cattle_Results.xlsx"); cattle$Type <- "Cattle and Horse"

dat <- bind_rows(sheep, cattle)

type_order <- c("Sheep", "Cattle and Horse")

dat <- dat %>%
  mutate(
    Indicator = factor(Indicator, levels=rev(ind_order)),
    Type      = factor(Type,      levels=rev(type_order))
  ) %>%
  mutate(
    is_placeholder = is.na(Color) & is.na(N),
    show_point     = !is_placeholder,
    pt_fill  = case_when(is_placeholder~NA_character_, Color==1~"#79bb56",
                         Color==2~"#c0392b", Color==0~"white", TRUE~NA_character_),
    pt_color = case_when(is_placeholder~NA_character_, Color==1~"#79bb56",
                         Color==2~"#c0392b", Color==0~"black", TRUE~NA_character_),
    N_label  = ifelse(is_placeholder|is.na(N)|N==0, NA_character_,
                      paste0("(",as.integer(N),")"))
  )

n_type    <- 2
row_block <- 4.0
pad       <- row_block * 0.30
row_h     <- row_block - 2*pad

ind_levels   <- levels(dat$Indicator)
type_levels  <- levels(dat$Type)

y_base      <- setNames((seq_along(ind_levels)-1)*row_block + pad, ind_levels)
type_offset <- setNames((seq_along(type_levels)-1)*row_h, type_levels)

dat <- dat %>%
  mutate(y_pos = y_base[as.character(Indicator)] + type_offset[as.character(Type)])

ind_centers <- make_ind_centers(ind_levels, row_block)
ind_bg      <- make_ind_bg(ind_levels, row_block)
y_min_plot  <- 0
y_max_plot  <- length(ind_levels) * row_block
x_lo <- -2.0; x_hi <- 2.0; n_x_pos <- x_hi - 0.03

legend_x <- 0.65
legend_y <- 0.90

p_livestock <- ggplot() +
  geom_rect(data=ind_bg, aes(xmin=-Inf,xmax=Inf,ymin=y_bot,ymax=y_top),
            fill=ind_bg$bg, inherit.aes=FALSE) +
  geom_segment(data=filter(dat,show_point),
               aes(x=Low,xend=Up,y=y_pos,yend=y_pos,color=I(pt_color)),
               linewidth=0.8) +
  geom_point(data=filter(dat,show_point),
             aes(x=Mean,y=y_pos,shape=Type,
                 fill=I(pt_fill),color=I(pt_color)),
             size=5.0, stroke=1.0) +
  geom_vline(xintercept=0, linetype="dashed", linewidth=0.7, color="#c0392b") +
  geom_text(data=filter(dat,!is.na(N_label)),
            aes(x=n_x_pos,y=y_pos,label=N_label),
            hjust=1, size=3.8, color="#333333") +
  geom_text(data=ind_centers,
            aes(x=x_lo-0.04,y=y_center,label=as.character(Indicator)),
            hjust=1, size=6.5, color="black") +
  scale_shape_manual(
    name   = "Livestock Type",
    values = c("Sheep"=21,"Cattle and Horse"=23),
    breaks = type_order,
    guide  = guide_legend(override.aes=list(fill="white",color="black",
                                            size=5.0,stroke=1.0))
  ) +
  scale_x_continuous(breaks=seq(-2.0,2.0,by=0.5), limits=c(x_lo,x_hi),
                     oob=scales::oob_keep) +
  coord_cartesian(xlim=c(x_lo,x_hi), ylim=c(y_min_plot,y_max_plot),
                  expand=FALSE, clip="off") +
  labs(x="Weighted ln(RR)", y=NULL) +
  shared_theme(legend_x, legend_y)

ggsave(output_path("Grazing_LivestockType_forest_plot.pdf"), plot=p_livestock, width=8.5, height=11, units="in")
ggsave(output_path("Grazing_LivestockType_forest_plot.png"), plot=p_livestock, width=8.5, height=11, units="in", dpi=300)


# =============================================================================
# SECTION 3: CORRELATION ANALYSIS (SPEARMAN HEATMAP)
# =============================================================================

library(readxl)
library(metafor)
library(corrplot)
library(dplyr)

# INPUT: Database workbook, sheet 1; treatment/control means, SDs, and sample sizes.
# PROCESS: Calculate SMDs, pairwise Spearman correlations, and p-values.
# OUTPUT: Spearman_Correlation_Heatmap.jpg in OUTPUT_DIR.
df <- read_excel(DATABASE_FILE, sheet = 1)

# 11 traits retained after reviewer revision
# (redundant indices and traits with insufficient co-observations removed)
indicators <- list(
  Height = list(Xc="Xc1",  SDc="SDc1",  Nc="Nc1",  Xt="Xt1",  SDt="SDt1",  Nt="Nt1"),
  AGB    = list(Xc="Xc2",  SDc="SDc2",  Nc="Nc2",  Xt="Xt2",  SDt="SDt2",  Nt="Nt2"),
  LA     = list(Xc="Xc5",  SDc="SDc5",  Nc="Nc5",  Xt="Xt5",  SDt="SDt5",  Nt="Nt5"),
  SLA    = list(Xc="Xc6",  SDc="SDc6",  Nc="Nc6",  Xt="Xt6",  SDt="SDt6",  Nt="Nt6"),
  LN     = list(Xc="Xc7",  SDc="SDc7",  Nc="Nc7",  Xt="Xt7",  SDt="SDt7",  Nt="Nt7"),
  LB     = list(Xc="Xc9",  SDc="SDc9",  Nc="Nc9",  Xt="Xt9",  SDt="SDt9",  Nt="Nt9"),
  SB     = list(Xc="Xc11", SDc="SDc11", Nc="Nc11", Xt="Xt11", SDt="SDt11", Nt="Nt11"),
  `S:L`  = list(Xc="Xc12", SDc="SDc12", Nc="Nc12", Xt="Xt12", SDt="SDt12", Nt="Nt12"),
  BGB    = list(Xc="Xc13", SDc="SDc13", Nc="Nc13", Xt="Xt13", SDt="SDt13", Nt="Nt13"),
  RD     = list(Xc="Xc14", SDc="SDc14", Nc="Nc14", Xt="Xt14", SDt="SDt14", Nt="Nt14"),
  `R:S`  = list(Xc="Xc20", SDc="SDc20", Nc="Nc20", Xt="Xt20", SDt="SDt20", Nt="Nt20")
)

calculate_effect_sizes <- function(df, indicators) {
  effect_sizes <- data.frame(Study = 1:nrow(df))
  for (trait_name in names(indicators)) {
    trait <- indicators[[trait_name]]
    Xc <- df[[trait$Xc]]; SDc <- df[[trait$SDc]]; Nc <- df[[trait$Nc]]
    Xt <- df[[trait$Xt]]; SDt <- df[[trait$SDt]]; Nt <- df[[trait$Nt]]
    es <- escalc(measure="SMD",
                 m1i=Xt, m2i=Xc, sd1i=SDt, sd2i=SDc, n1i=Nt, n2i=Nc)
    effect_sizes[[trait_name]] <- es$yi
  }
  return(effect_sizes)
}

effect_data <- calculate_effect_sizes(df, indicators)

trait_cols <- colnames(effect_data)[-1]
n_traits   <- length(trait_cols)

M_spearman <- matrix(NA, nrow=n_traits, ncol=n_traits,
                     dimnames=list(trait_cols, trait_cols))
P_spearman <- matrix(NA, nrow=n_traits, ncol=n_traits,
                     dimnames=list(trait_cols, trait_cols))

for (i in 1:n_traits) {
  for (j in 1:n_traits) {
    if (i == j) {
      M_spearman[i,j] <- 1; P_spearman[i,j] <- 0
    } else {
      valid_data <- na.omit(effect_data[, c(trait_cols[i], trait_cols[j])])
      if (nrow(valid_data) >= 10) {
        var1 <- valid_data[[1]]; var2 <- valid_data[[2]]
        if (var(var1)>0 && var(var2)>0 &&
            length(unique(var1))>1 && length(unique(var2))>1) {
          ct <- cor.test(var1, var2, method="spearman")
          M_spearman[i,j] <- ct$estimate
          P_spearman[i,j] <- ct$p.value
        }
      }
    }
  }
}

na_positions  <- is.na(M_spearman)
M_plot_clean  <- M_spearman; M_plot_clean[is.na(M_plot_clean)] <- 0; diag(M_plot_clean) <- 1
P_plot_clean  <- P_spearman; P_plot_clean[is.na(P_plot_clean)] <- 1; diag(P_plot_clean) <- 0

col <- colorRampPalette(c("#6D9EC1","white","#E46726"))(100)
n   <- nrow(M_plot_clean)

jpeg(output_path("Spearman_Correlation_Heatmap.jpg"),
     width=14, height=12, units="in", res=600, quality=100)
par(mar=c(0,0,3,0))

corrplot(M_plot_clean, method="color", type="full", order="original",
         tl.col="black", tl.srt=45, tl.cex=1.2, cl.cex=1.2,
         addCoef.col=NULL, col=col, mar=c(0,0,2,0),
         title="Spearman Correlation between Functional Traits",
         cex.main=2.0, diag=TRUE, is.corr=TRUE)

for (i in 1:n) {
  for (j in 1:n) {
    if (na_positions[i,j] && i!=j) {
      text(x=j, y=n+1-i, labels="NA", col="gray60", cex=0.85, font=3)
    } else {
      text(x=j, y=n+1-i, labels=round(M_spearman[i,j],2), col="black", cex=1.0)
    }
  }
}

corrplot(M_plot_clean, p.mat=P_plot_clean, method="color", type="upper",
         diag=FALSE, add=TRUE, tl.pos="n", cl.pos="n",
         insig="label_sig", sig.level=c(0.001,0.01,0.05),
         pch.cex=1.2, pch.col="black", col=col, is.corr=TRUE)

dev.off()


# =============================================================================
# SECTION 4: FUNCTIONAL TRAIT NETWORK
# Continuous composite centrality with a teal-green node color scale
# =============================================================================

library(readxl)
library(metafor)
library(igraph)
library(dplyr)
library(ggplot2)
library(scales)
library(grid)

# ============================================================

# ============================================================

# INPUT: Database workbook, sheet 1; the 11 traits defined below.
# PROCESS: Calculate pairwise correlations, retain edges using the existing threshold,
#          calculate continuous composite centrality, and build the trait network.
# OUTPUT: Network figures and centrality-ranking tables in OUTPUT_DIR.
df <- read_excel(DATABASE_FILE, sheet = 1)

# ============================================================

# ============================================================

indicators <- list(
  Height = 1,
  AGB    = 2,
  LA     = 5,
  SLA    = 6,
  LN     = 7,
  LB     = 9,
  SB     = 11,
  `S:L`  = 12,
  BGB    = 13,
  RD     = 14,
  `R:S`  = 20
)

trait_names <- names(indicators)

# ============================================================

# ============================================================

effect_data <- data.frame(
  Study = seq_len(nrow(df))
)

for (trait in trait_names) {

  i <- indicators[[trait]]

  es <- metafor::escalc(
    measure = "SMD",
    m1i = df[[paste0("Xt", i)]],
    m2i = df[[paste0("Xc", i)]],
    sd1i = df[[paste0("SDt", i)]],
    sd2i = df[[paste0("SDc", i)]],
    n1i = df[[paste0("Nt", i)]],
    n2i = df[[paste0("Nc", i)]]
  )

  effect_data[[trait]] <- es$yi
}

# ============================================================

# n >= 10
# ============================================================

n <- length(trait_names)

cor_matrix <- matrix(
  NA_real_,
  nrow = n,
  ncol = n,
  dimnames = list(trait_names, trait_names)
)

for (i in seq_len(n)) {
  for (j in seq_len(n)) {

    if (i == j) {
      cor_matrix[i, j] <- 1
      next
    }

    dat <- na.omit(
      effect_data[
        ,
        c(trait_names[i], trait_names[j])
      ]
    )

    if (
      nrow(dat) >= 10 &&
      length(unique(dat[[1]])) > 1 &&
      length(unique(dat[[2]])) > 1
    ) {
      cor_matrix[i, j] <- suppressWarnings(
        cor.test(
          dat[[1]],
          dat[[2]],
          method = "spearman",
          exact = FALSE
        )$estimate
      )
    }
  }
}

cor_matrix[is.na(cor_matrix)] <- 0
diag(cor_matrix) <- 0

# ============================================================

# |r| > 0.3
# ============================================================

threshold <- 0.3

adj_matrix <- ifelse(
  abs(cor_matrix) > threshold,
  abs(cor_matrix),
  0
)

diag(adj_matrix) <- 0

net <- igraph::graph_from_adjacency_matrix(
  adj_matrix,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

V(net)$name <- colnames(cor_matrix)

cat("Network nodes:", vcount(net), "\n")
cat("Network edges:", ecount(net), "\n")

# ============================================================

# ============================================================

weights <- E(net)$weight

if (ecount(net) > 0) {

  distances <- 1 / (weights + 0.001)

  cent <- data.frame(
    Trait = V(net)$name,
    Degree = degree(net),
    Strength = strength(
      net,
      weights = weights
    ),
    Betweenness = betweenness(
      net,
      weights = distances,
      directed = FALSE
    ),
    Closeness = closeness(
      net,
      weights = distances,
      normalized = FALSE
    ),
    Eigenvector = eigen_centrality(
      net,
      directed = FALSE,
      weights = weights
    )$vector,
    PageRank = page_rank(net)$vector
  )

} else {

  cent <- data.frame(
    Trait = V(net)$name,
    Degree = 0,
    Strength = 0,
    Betweenness = 0,
    Closeness = 0,
    Eigenvector = 0,
    PageRank = 0
  )
}

# ============================================================

# Strength + Betweenness + Eigenvector

# ============================================================

normalize <- function(x) {
  if (
    all(is.na(x)) ||
    diff(range(x, na.rm = TRUE)) == 0
  ) {
    return(rep(0.5, length(x)))
  }

  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

cent_norm <- cent
cent_norm[, -1] <- lapply(
  cent[, -1],
  normalize
)

cent_norm[is.na(cent_norm)] <- 0

cent_norm$Composite_Centrality <- rowMeans(
  cent_norm[
    ,
    c("Strength", "Betweenness", "Eigenvector")
  ]
)

centrality_ranked <- cent_norm %>%
  arrange(desc(Composite_Centrality))

write.csv(
  centrality_ranked,
  output_path("Figure7_Centrality_Ranking.csv"),
  row.names = FALSE
)

cat("\nComposite centrality ranking:\n")
print(
  centrality_ranked[
    ,
    c("Trait", "Composite_Centrality")
  ]
)

# ============================================================

# ============================================================

node_positions <- data.frame(
  Trait = c(
    "LA",
    "AGB",
    "BGB",
    "SB",
    "LN",
    "LB",
    "Height",
    "SLA",
    "S:L",
    "RD",
    "R:S"
  ),
  x = c(
    -0.35,
    0.45,
    1.03,
    0.40,
    -1.95,
    -2.00,
    1.65,
    -0.42,
    -0.42,
    1.73,
    2.25
  ),
  y = c(
    0.10,
    1.17,
    0.08,
    -0.85,
    0.82,
    -0.63,
    1.42,
    1.85,
    -1.67,
    -1.52,
    -0.24
  )
)

nodes <- node_positions %>%
  left_join(
    centrality_ranked %>%
      select(Trait, Composite_Centrality),
    by = "Trait"
  )

stopifnot(!anyNA(nodes$Composite_Centrality))

# ============================================================

# ============================================================

if (ecount(net) > 0) {

  pairs <- igraph::as_edgelist(
    net,
    names = TRUE
  )

  edges <- data.frame(
    from = pairs[, 1],
    to = pairs[, 2],
    stringsAsFactors = FALSE
  )

  edges$r <- mapply(
    function(a, b) cor_matrix[a, b],
    edges$from,
    edges$to
  )

  edges$Relationship <- ifelse(
    edges$r > 0,
    "Positive correlation",
    "Negative correlation"
  )

  edges$Strength <- abs(edges$r)

  edges <- edges %>%
    left_join(
      nodes %>%
        select(
          from = Trait,
          x_from = x,
          y_from = y
        ),
      by = "from"
    ) %>%
    left_join(
      nodes %>%
        select(
          to = Trait,
          x_to = x,
          y_to = y
        ),
      by = "to"
    )

} else {

  edges <- data.frame(
    x_from = numeric(),
    y_from = numeric(),
    x_to = numeric(),
    y_to = numeric(),
    Relationship = character(),
    Strength = numeric()
  )
}

# ============================================================

# ============================================================

centrality_colors <- c(
  "#EAF4F1",
  "#BFDCD2",
  "#7EB8A5",
  "#3F8F77",
  "#1F5D50"
)

# ============================================================

# ============================================================

theme_network <- theme_void(base_size = 14) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 16)
    ),
    legend.title = element_text(
      size = 13,
      face = "bold"
    ),
    legend.text = element_text(
      size = 12
    ),
    legend.position = "right",
    legend.box = "vertical",
    legend.spacing.y = unit(12, "pt"),
    plot.margin = margin(
      18, 25, 18, 25
    )
  )

# ============================================================

# ============================================================

p <- ggplot()

if (nrow(edges) > 0) {
  p <- p +
    geom_curve(
      data = edges,
      aes(
        x = x_from,
        y = y_from,
        xend = x_to,
        yend = y_to,
        color = Relationship,
        linewidth = Strength
      ),
      curvature = 0.12,
      alpha = 0.68,
      lineend = "round"
    )
}

p <- p +
  geom_point(
    data = nodes,
    aes(
      x = x,
      y = y,
      fill = Composite_Centrality,
      size = Composite_Centrality
    ),
    shape = 21,
    color = "white",
    stroke = 1.3
  )

p <- p +
  geom_text(
    data = subset(
      nodes,
      Composite_Centrality >= 0.45
    ),
    aes(
      x = x,
      y = y,
      label = Trait
    ),
    color = "white",
    fontface = "bold",
    size = 4.3
  ) +
  geom_text(
    data = subset(
      nodes,
      Composite_Centrality < 0.45
    ),
    aes(
      x = x,
      y = y,
      label = Trait
    ),
    color = "#25463E",
    fontface = "bold",
    size = 4.3
  )

p <- p +
  scale_fill_gradientn(
    name = "Composite\ncentrality",
    colours = centrality_colors,
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    guide = guide_colorbar(
      title.position = "top",
      barheight = unit(48, "mm"),
      barwidth = unit(7, "mm"),
      frame.colour = "grey75",
      ticks.colour = "grey40"
    )
  )

p <- p +
  scale_size_continuous(
    range = c(10, 23),
    limits = c(0, 1),
    guide = "none"
  )

p <- p +
  scale_color_manual(
    name = NULL,
    values = c(
      "Positive correlation" = "#CB5D55",
      "Negative correlation" = "#508FBD"
    ),
    breaks = c(
      "Positive correlation",
      "Negative correlation"
    ),
    guide = guide_legend(
      override.aes = list(
        linewidth = 1.5,
        alpha = 1
      )
    )
  )

p <- p +
  scale_linewidth_continuous(
    range = c(0.35, 1.7),
    limits = c(0, 1),
    guide = "none"
  )

p <- p +
  coord_equal(
    xlim = c(-2.65, 2.73),
    ylim = c(-2.12, 2.22),
    expand = FALSE,
    clip = "off"
  ) +
  labs(
    title = "Plant Functional Trait Network Under Grazing"
  ) +
  guides(
    fill = guide_colorbar(
      order = 1,
      title.position = "top",
      barheight = unit(48, "mm"),
      barwidth = unit(7, "mm")
    ),
    color = guide_legend(
      order = 2,
      override.aes = list(
        linewidth = 1.5,
        alpha = 1
      )
    )
  ) +
  theme_network

# ============================================================

# ============================================================

print(p)

# ============================================================

# ============================================================

ggsave(
  filename = output_path("Figure7_TealGreen_v2.jpg"),
  plot = p,
  width = 13,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white"
)

# ============================================================

# ============================================================

ggsave(
  filename = output_path("Figure7_TealGreen_v2.tiff"),
  plot = p,
  width = 13,
  height = 8,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# ============================================================

# ============================================================

ggsave(
  filename = output_path("Figure7_TealGreen_v2.pdf"),
  plot = p,
  width = 13,
  height = 8,
  units = "in",
  bg = "white"
)

# ============================================================

# ============================================================

node_summary <- nodes %>%
  select(
    Trait,
    Composite_Centrality
  ) %>%
  arrange(desc(Composite_Centrality))

write.csv(
  node_summary,
  output_path("Figure7_TealGreen_v2_Node_Centrality.csv"),
  row.names = FALSE
)

# ============================================================

# ============================================================

cat("\n====================================\n")
cat("Figure 7 completed successfully!\n")
cat("====================================\n")

cat("\nNodes:", vcount(net), "\n")
cat("Edges:", ecount(net), "\n")

cat("\nOutput files:\n")
cat("1. Figure7_TealGreen_v2.jpg\n")
cat("2. Figure7_TealGreen_v2.tiff\n")
cat("3. Figure7_TealGreen_v2.pdf\n")
cat("4. Figure7_Centrality_Ranking.csv\n")
cat("5. Figure7_TealGreen_v2_Node_Centrality.csv\n")

cat("\nNode colors represent continuous composite centrality values.\n")
cat("\nNo categorical centrality classification was applied.\n")


# =============================================================================
# SECTION 5: PLASTICITY ANALYSIS (RDPI) - FOUR-PANEL VERSION
# Panels A-D represent LightG, ModerateG, HighG, and FreeG, respectively.
# The original RDPI formula is retained; no between-group significance tests are added.
# All RDPI outputs are written to the shared OUTPUT_DIR.
# =============================================================================

# INPUT: Database workbook, sheet 1; trait-level treatment/control columns.
# PROCESS: Calculate RDPI using the existing formula, summarize mean and SE by group,
#          and create a compact 2 x 2 figure. No between-group significance tests are added.
# OUTPUT: RDPI four-panel figures and summary/raw tables in OUTPUT_DIR.
group_codes <- c("LG", "MG", "HG", "FG")

panel_letters <- c(
  LG = "A",
  MG = "B",
  HG = "C",
  FG = "D"
)


group_titles <- c(
  LG = "LightG",
  MG = "ModerateG",
  HG = "HighG",
  FG = "FreeG"
)


group_labels <- c(
  LG = "LightG",
  MG = "ModerateG",
  HG = "HighG",
  FG = "FreeG"
)


traits_abbr <- c(
  "Height",
  "AGB",
  "LL",
  "LW",
  "LA",
  "SLA",
  "LN",
  "LT",
  "LB",
  "SD",
  "SB",
  "S:L",
  "BGB",
  "RD",
  "RSA",
  "RL",
  "SRL",
  "RDen",
  "RV",
  "R:S"
)


traits_full <- c(
  "Plant height",
  "Aboveground biomass",
  "Leaf length",
  "Leaf width",
  "Leaf area",
  "Specific leaf area",
  "Leaf number",
  "Leaf thickness",
  "Leaf biomass",
  "Stem diameter",
  "Stem biomass",
  "Stem-to-leaf ratio",
  "Belowground biomass",
  "Root diameter",
  "Root surface area",
  "Root length",
  "Specific root length",
  "Root density",
  "Root volume",
  "Root-to-shoot ratio"
)


# ============================================================

# RDPI STEP 1: DEFINE GROUP COLORS, ORDER, AND SHARED AXIS SETTINGS
plasticity_colors <- c(
  High = "#0072B2",
  Moderate = "#56B4E9",
  Low = "#E8F4F8"
)

plasticity_order <- c(
  "High",
  "Moderate",
  "Low"
)


# ============================================================

axis_min <- 0
axis_max <- 0.55

axis_breaks <- seq(
  0,
  0.5,
  by = 0.1
)


# ============================================================

if (!file.exists(DATABASE_FILE)) {
  stop("The database file is no longer available. Check the path: ", DATABASE_FILE)
}

# RDPI STEP 2: READ REQUIRED TRAIT COLUMNS AND CHECK INPUT STRUCTURE
# The source is the Database workbook; group labels are mapped from the grazing-intensity column.
dat <- as.data.frame(
  read_excel(DATABASE_FILE, sheet = 1)
)


required_cols <- c(
  "ID",
  "Author",
  "Year",
  "Grazing Intensities",
  unlist(
    lapply(
      1:20,
      function(i) {
        c(
          paste0("Xc", i),
          paste0("Xt", i)
        )
      }
    )
  )
)


missing_cols <- setdiff(
  required_cols,
  names(dat)
)

if (length(missing_cols) > 0) {
  stop(
    "The database is missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}


dat$Intensity <- toupper(
  trimws(
    as.character(
      dat[["Grazing Intensities"]]
    )
  )
)


dat <- dat[
  dat$Intensity %in% group_codes,
  ,
  drop = FALSE
]


cat("\n====================================\n")
cat("Database loaded successfully\n")
cat("====================================\n")

cat("Total records: ", nrow(dat), "\n")

cat("\nGrazing-intensity groups: \n")

print(
  table(
    factor(
      dat$Intensity,
      levels = group_codes
    )
  )
)


# ============================================================

# RDPI = |Xt - Xc| / (Xt + Xc)


# RDPI STEP 3: CALCULATE RAW RDPI VALUES
# Formula retained from the approved analysis: abs(Xt - Xc) / (Xt + Xc).
rdpi_raw <- data.frame(
  Row_ID = seq_len(nrow(dat)),
  Study_ID = dat$ID,
  Author = dat$Author,
  Year = dat$Year,
  Intensity = dat$Intensity,
  stringsAsFactors = FALSE
)


for (i in seq_along(traits_abbr)) {
  
  xc <- suppressWarnings(
    as.numeric(
      dat[[paste0("Xc", i)]]
    )
  )
  
  xt <- suppressWarnings(
    as.numeric(
      dat[[paste0("Xt", i)]]
    )
  )
  
  
  valid <- is.finite(xc) &
    is.finite(xt) &
    xc > 0 &
    xt > 0
  
  
  rdpi <- rep(
    NA_real_,
    nrow(dat)
  )
  
  
  rdpi[valid] <- abs(
    xt[valid] - xc[valid]
  ) / (
    xt[valid] + xc[valid]
  )
  
  
  rdpi_raw[[traits_abbr[i]]] <- rdpi
  
}


write.csv(
  rdpi_raw,
  file = file.path(
    OUTPUT_DIR,
    "RDPI_raw.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ============================================================

# RDPI STEP 4: APPLY THE EXISTING DESCRIPTIVE RDPI CATEGORIES
# These labels describe RDPI magnitude only; they are not between-group significance tests.
classify_rdpi <- function(x) {
  
  ifelse(
    is.na(x),
    NA_character_,
    ifelse(
      x < 0.2,
      "Low",
      ifelse(
        x < 0.5,
        "Moderate",
        "High"
      )
    )
  )
  
}


# ============================================================

# RDPI STEP 5: SUMMARIZE EACH GRAZING GROUP
# Calculates trait-level mean RDPI and standard error for LightG, ModerateG, HighG, and FreeG.
summarize_group <- function(code) {
  
  subdat <- rdpi_raw[
    rdpi_raw$Intensity == code,
    ,
    drop = FALSE
  ]
  
  
  res <- data.frame(
    Intensity = code,
    Trait = traits_abbr,
    Trait_Full = traits_full,
    
    RDPI_mean = NA_real_,
    RDPI_median = NA_real_,
    RDPI_sd = NA_real_,
    RDPI_se = NA_real_,
    RDPI_min = NA_real_,
    RDPI_max = NA_real_,
    
    n_records = integer(
      length(traits_abbr)
    ),
    
    stringsAsFactors = FALSE
  )
  
  
  for (i in seq_along(traits_abbr)) {
    
    values <- subdat[[traits_abbr[i]]]
    
    values <- values[
      is.finite(values)
    ]
    
    n <- length(values)
    
    res$n_records[i] <- n
    
    
    if (n > 0) {
      
      res$RDPI_mean[i] <- mean(values)
      
      res$RDPI_median[i] <- median(values)
      
      res$RDPI_min[i] <- min(values)
      
      res$RDPI_max[i] <- max(values)
      
    }
    
    
    if (n >= 2) {
      
      res$RDPI_sd[i] <- sd(values)
      
      res$RDPI_se[i] <- sd(values) / sqrt(n)
      
    }
    
  }
  
  
  res$Plasticity_Level <- classify_rdpi(
    res$RDPI_mean
  )
  
  
  res <- res[
    order(
      -res$RDPI_mean,
      res$Trait,
      na.last = TRUE
    ),
    ,
    drop = FALSE
  ]
  
  
  rownames(res) <- NULL
  
  res$Rank <- seq_len(
    nrow(res)
  )
  
  
  return(res)
  
}


# ============================================================

# RDPI STEP 6: BUILD EACH PANEL
# A = LightG, B = ModerateG, C = HighG, D = FreeG.
plot_group <- function(
    summary_data,
    code,
    show_legend = FALSE
) {
  
  plot_dat <- summary_data[
    is.finite(summary_data$RDPI_mean),
    ,
    drop = FALSE
  ]
  
  
  if (nrow(plot_dat) == 0) {
    
    stop(
      "No valid RDPI values are available for group: ",
      code
    )
    
  }
  
  
  plot_dat$Trait <- factor(
    plot_dat$Trait,
    levels = rev(plot_dat$Trait)
  )
  
  
  # ========================================================
  # Mean +/- SE
  # ========================================================
  
  plot_dat$SE_plot <- ifelse(
    is.na(plot_dat$RDPI_se),
    0,
    plot_dat$RDPI_se
  )
  
  
  plot_dat$Lower <- pmax(
    0,
    plot_dat$RDPI_mean -
      plot_dat$SE_plot
  )
  
  
  plot_dat$Upper <- (
    plot_dat$RDPI_mean +
      plot_dat$SE_plot
  )
  
  
  if (any(plot_dat$Upper > axis_max)) {
    
    warning(
      code,
      "has RDPI mean + SE values exceeding the axis maximum."
    )
    
  }
  
  
  # ========================================================
  # ========================================================
  
  plot_dat$Label_X <- pmin(
    0.535,
    plot_dat$Upper + 0.012
  )
  
  
  plot_dat$Label_Hjust <- ifelse(
    plot_dat$Upper > 0.50,
    1,
    0
  )
  
  
  # ========================================================
  # ========================================================
  
  present_levels <- intersect(
    plasticity_order,
    unique(
      plot_dat$Plasticity_Level
    )
  )
  
  
  present_levels <- present_levels[
    !is.na(present_levels)
  ]
  
  
  plot_dat$Plasticity_Level <- factor(
    plot_dat$Plasticity_Level,
    levels = present_levels
  )
  
  
  # ========================================================
  # ========================================================
  
  legend_pos <- if (show_legend) {
    c(0.62, 0.96)
  } else {
    "none"
  }
  
  
  # ========================================================
  # ========================================================
  
  p <- ggplot(
    plot_dat,
    aes(
      x = Trait,
      y = RDPI_mean,
      fill = Plasticity_Level
    )
  ) +
    
    
    geom_col(
      width = 0.70,
      color = "white",
      linewidth = 0.45
    ) +
    
    
    geom_errorbar(
      aes(
        ymin = Lower,
        ymax = Upper
      ),
      width = 0.23,
      linewidth = 0.65,
      color = "black",
      na.rm = TRUE
    ) +
    
    
    geom_hline(
      yintercept = c(0.2, 0.5),
      linetype = "dashed",
      linewidth = 0.50,
      color = "black"
    ) +
    
    
    geom_text(
      aes(
        y = Label_X,
        label = sprintf(
          "%.3f",
          RDPI_mean
        ),
        hjust = Label_Hjust
      ),
      size = 4.2,
      color = "black"
    ) +
    
    
    coord_flip(
      ylim = c(axis_min, axis_max),
      clip = "off"
    ) +
    
    
    scale_fill_manual(
      values = plasticity_colors[
        present_levels
      ],
      breaks = present_levels,
      drop = TRUE,
      name = "Plasticity level"
    ) +
    
    
    scale_y_continuous(
      breaks = axis_breaks,
      limits = c(
        axis_min,
        axis_max
      ),
      expand = c(0, 0)
    ) +
    
    
    labs(
      title = unname(
        group_titles[code]
      ),
      tag = unname(
        panel_letters[code]
      ),
      x = NULL,
      y = "RDPI"
    ) +
    
    
    theme_bw(
      base_size = 15,
      base_family = "Arial"
    ) +
    
    
    theme(
      
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 18,
        family = "Arial",
        color = "black",
        margin = margin(b = 4)
      ),

      plot.tag = element_text(
        family = "Arial",
        face = "bold",
        size = 18,
        color = "black",
        hjust = 0,
        vjust = 1
      ),
      plot.tag.position = c(0, 1),
      
      
      axis.text.y = element_text(
        size = 15.5,
        family = "Arial",
        color = "black"
      ),
      
      
      axis.text.x = element_text(
        size = 13.5,
        family = "Arial",
        color = "black"
      ),
      
      
      axis.title.x = element_text(
        size = 15.5,
        family = "Arial",
        face = "bold",
        color = "black",
        margin = margin(t = 5)
      ),
      
      
      axis.title.y = element_blank(),
      
      
      legend.position = legend_pos,
      
      legend.justification = c(0, 1),
      
      
      legend.title = element_text(
        family = "Arial",
        face = "bold",
        size = 12.5
      ),
      
      
      legend.text = element_text(
        family = "Arial",
        size = 11.5
      ),
      
      
      legend.background = element_rect(
        fill = "white",
        color = "grey60",
        linewidth = 0.4
      ),
      
      
      legend.key.size = grid::unit(
        0.38,
        "cm"
      ),
      
      
      legend.margin = margin(
        3,
        3,
        3,
        3
      ),
      
      
      panel.grid.major = element_line(
        color = "grey90",
        linewidth = 0.35
      ),
      
      
      panel.grid.minor = element_blank(),
      
      
      panel.border = element_rect(
        color = "black",
        linewidth = 0.6
      ),
      
      
      plot.margin = margin(
        5,
        5,
        5,
        5
      )
      
    )
  
  
  return(p)
  
}


# ============================================================

all_summaries <- list()

all_plots <- list()


for (code in group_codes) {
  
  cat("\n====================================\n")
  
  cat(
    "Processing: ",
    group_labels[code],
    "\n"
  )
  
  cat("====================================\n")
  
  
  summary_res <- summarize_group(code)
  
  
  all_summaries[[code]] <- summary_res
  
  
  write.csv(
    summary_res,
    file.path(
      OUTPUT_DIR,
      paste0(
        "RDPI_summary_",
        group_labels[code],
        ".csv"
      )
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  
  all_plots[[code]] <- plot_group(
    summary_data = summary_res,
    code = code,
    show_legend = (code == "LG")
  )
  
  
  cat(
    "Number of traits with valid estimates: ",
    sum(
      is.finite(
        summary_res$RDPI_mean
      )
    ),
    "\n"
  )
  
  
  cat(
    "Maximum mean RDPI: ",
    round(
      max(
        summary_res$RDPI_mean,
        na.rm = TRUE
      ),
      3
    ),
    "\n"
  )
  
  
  cat(
    "Plasticity levels: ",
    paste(
      unique(
        na.omit(
          summary_res$Plasticity_Level
        )
      ),
      collapse = ", "
    ),
    "\n"
  )
  
}


# ============================================================

summary_all <- do.call(
  rbind,
  all_summaries
)


write.csv(
  summary_all,
  file.path(
    OUTPUT_DIR,
    "RDPI_summary_all_four_groups.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ============================================================

# RDPI STEP 7: COMBINE THE FOUR PANELS AND EXPORT FIGURES
combined_plot <- wrap_plots(
  
  all_plots[["LG"]],
  
  all_plots[["MG"]],
  
  all_plots[["HG"]],
  
  all_plots[["FG"]],
  
  ncol = 2,
  
  nrow = 2,
  
  byrow = TRUE
  
) +
  
  plot_annotation(
    theme = theme(
      plot.margin = margin(0, 0, 0, 0)
    )
  )


# ============================================================

fig_width <- 14
fig_height <- 12


ggsave(
  filename = file.path(
    OUTPUT_DIR,
    "RDPI_ABCD_Final_v8.pdf"
  ),
  
  plot = combined_plot,
  
  width = fig_width,
  height = fig_height,
  units = "in",
  
  bg = "white"
)


# TIFF 600 dpi
ggsave(
  filename = file.path(
    OUTPUT_DIR,
    "RDPI_ABCD_Final_v8.tiff"
  ),
  
  plot = combined_plot,
  
  width = fig_width,
  height = fig_height,
  units = "in",
  
  dpi = 600,
  compression = "lzw",
  
  bg = "white"
)


# JPEG 600 dpi
ggsave(
  filename = file.path(
    OUTPUT_DIR,
    "RDPI_ABCD_Final_v8.jpg"
  ),
  
  plot = combined_plot,
  
  width = fig_width,
  height = fig_height,
  units = "in",
  
  dpi = 600,
  
  bg = "white"
)


# ============================================================

print(combined_plot)


# ============================================================

cat("\n====================================\n")
cat("RDPI four-panel figure export completed\n")
cat("====================================\n")

cat("\nPanel arrangement: \n")
cat("A = LightG\n")
cat("B = ModerateG\n")
cat("C = HighG\n")
cat("D = FreeG\n")

cat("\nLayout: 2 x 2\n")
cat("Legend: Panel A only, in the upper-right empty area\n")
cat("Axis range: 0-0.55 for all panels\n")
cat("Error bars: Mean +/- SE\n")

cat("\nOutput directory: \n")

cat(
  normalizePath(OUTPUT_DIR),
  "\n"
)

cat("\nExported files: \n")
cat("RDPI_ABCD_Final_v8.pdf\n")
cat("RDPI_ABCD_Final_v8.tiff\n")
cat("RDPI_ABCD_Final_v8.jpg\n")
cat("RDPI_summary_all_four_groups.csv\n")
cat("RDPI_raw.csv\n")

# =============================================================================
# END OF PIPELINE
# If a section fails, use the numbered section headers above to locate the module.
# Check the input file list in Section 0.3 before rerunning the full script.
# =============================================================================
cat("\nAll done.\n")

# =============================================================================
# SECTION 6: RANDOM FOREST MODELS
# Variable importance, OOB R-squared grouping, heatmap, and response curves
# =============================================================================

library(readxl)
library(dplyr)
library(metafor)
library(mice)
library(rfPermute)
library(ggplot2)
library(patchwork)
library(grid)

FONT_FAMILY       <- "Arial"
FONT_FIGURE_TITLE <- 20
FONT_PANEL_TITLE  <- 16
FONT_R2           <- 13.5
FONT_AXIS_TITLE   <- 14.5
FONT_AXIS_TEXT    <- 12.5
FONT_PANEL_TAG    <- 18

# INPUT: Database workbook, sheet 1; six predictors and 20 trait-specific treatment/control datasets.
# PROCESS: Validate metadata, prepare predictors, fit one RF model per trait, and classify
#          traits as strong or weak predictors using the established OOB R-squared threshold.
# OUTPUT: RF importance table, strong/weak trait plots, heatmap, and driver-response plots.
mydata <- read_excel(DATABASE_FILE, sheet = 1)
if (ncol(mydata) < 15) {
  stop("The database has fewer than 15 columns; the metadata schema cannot be mapped.")
}

expected_metadata_names <- c(
  "Longitude", "Latitude", "Elevation", "MAT", "MAP", "AI",
  "Aridity_Class", "Grassland type", "Livestock type",
  "Grazing Duration", "Grazing Intensities"
)

if (!identical(names(mydata)[5:15], expected_metadata_names)) {
  stop(
    "Database columns 5-15 do not match the expected metadata schema. ",
    "Check the column order before running the random-forest analysis."
  )
}
stopifnot("Data read failed: zero rows" = nrow(mydata) > 0)
cat(sprintf("Data read successfully: %d rows x %d columns\n", nrow(mydata), ncol(mydata)))

colnames(mydata)[5]  <- "lon"
colnames(mydata)[6]  <- "lat"
colnames(mydata)[7]  <- "elev"
colnames(mydata)[8]  <- "MAT"
colnames(mydata)[9]  <- "MAP"
colnames(mydata)[10] <- "AI"
colnames(mydata)[11] <- "Aridity_Class"
colnames(mydata)[12] <- "Grassland_type"
colnames(mydata)[13] <- "livestock"
colnames(mydata)[14] <- "duration_raw"
colnames(mydata)[15] <- "intensity"

cat("Metadata columns renamed.\n")

mydata <- mydata %>%
  mutate(
    duration_num = suppressWarnings(as.numeric(duration_raw)),
    intensity_f  = factor(intensity,
                          levels  = c("LG", "MG", "HG", "FG"),
                          ordered = TRUE),
    livestock_f  = factor(livestock),
    AI           = suppressWarnings(as.numeric(AI)),
    lat          = suppressWarnings(as.numeric(lat)),
    elev         = suppressWarnings(as.numeric(elev))
  )

cat("Predictor preprocessing completed.\n")

# SUBSECTION 6.1: PREDICTOR PREPARATION AND MISSING-DATA HANDLING
# Creates the six predictor variables and imputes missing predictor values when required.
predictors_base <- mydata %>%
  select(lat, elev, AI, livestock_f, duration_num, intensity_f) %>%
  as.data.frame()

stopifnot("Predictor matrix has zero rows; verify the metadata columns." = nrow(predictors_base) > 0)
stopifnot("Predictor matrix must contain six columns." = ncol(predictors_base) == 6)

cat(sprintf("Predictor matrix created: %d rows x %d columns\n",
            nrow(predictors_base), ncol(predictors_base)))

cat("\nMissingness by predictor (%):\n")
miss_pct <- round(colSums(is.na(predictors_base)) /
                    nrow(predictors_base) * 100, 1)
print(miss_pct)

all_na_cols <- names(miss_pct[miss_pct == 100])
if (length(all_na_cols) > 0) {
  stop(paste("The following predictors are entirely missing; verify the metadata mapping:", paste(all_na_cols, collapse = ", ")))
}

# -----------------------------------------------------------------------------

cat("\nStarting predictor imputation with the mice random-forest method...\n")
set.seed(2024)

has_missing <- any(miss_pct > 0)

if (has_missing) {
  imp_global   <- mice(predictors_base, method = "rf",
                       m = 1, maxit = 10, printFlag = FALSE)
  pred_imputed <- complete(imp_global, 1)
  cat(sprintf("Imputation completed; remaining missing values: %d\n\n", sum(is.na(pred_imputed))))
} else {

  pred_imputed <- predictors_base
  cat("No missing values detected; imputation skipped.\n\n")
}

stopifnot("Missing values remain after imputation" = sum(is.na(pred_imputed)) == 0)
stopifnot("Imputed row count does not match the original data" = nrow(pred_imputed) == nrow(mydata))

# -----------------------------------------------------------------------------

# SUBSECTION 6.2: TRAIT DEFINITIONS AND RANDOM FOREST MODEL FITTING
# The mapping below links each of the 20 traits to its control/treatment columns.
indicators <- list(
  'Height'               = list(Xc="Xc1",  SDc="SDc1",  Nc="Nc1",  Xt="Xt1",  SDt="SDt1",  Nt="Nt1"),
  'Aboveground Biomass'  = list(Xc="Xc2",  SDc="SDc2",  Nc="Nc2",  Xt="Xt2",  SDt="SDt2",  Nt="Nt2"),
  'Leaf Length'          = list(Xc="Xc3",  SDc="SDc3",  Nc="Nc3",  Xt="Xt3",  SDt="SDt3",  Nt="Nt3"),
  'Leaf Width'           = list(Xc="Xc4",  SDc="SDc4",  Nc="Nc4",  Xt="Xt4",  SDt="SDt4",  Nt="Nt4"),
  'Leaf Area'            = list(Xc="Xc5",  SDc="SDc5",  Nc="Nc5",  Xt="Xt5",  SDt="SDt5",  Nt="Nt5"),
  'Specific Leaf Area'   = list(Xc="Xc6",  SDc="SDc6",  Nc="Nc6",  Xt="Xt6",  SDt="SDt6",  Nt="Nt6"),
  'Leaf Number'          = list(Xc="Xc7",  SDc="SDc7",  Nc="Nc7",  Xt="Xt7",  SDt="SDt7",  Nt="Nt7"),
  'Leaf Thickness'       = list(Xc="Xc8",  SDc="SDc8",  Nc="Nc8",  Xt="Xt8",  SDt="SDt8",  Nt="Nt8"),
  'Leaf Biomass'         = list(Xc="Xc9",  SDc="SDc9",  Nc="Nc9",  Xt="Xt9",  SDt="SDt9",  Nt="Nt9"),
  'Stem Diameter'        = list(Xc="Xc10", SDc="SDc10", Nc="Nc10", Xt="Xt10", SDt="SDt10", Nt="Nt10"),
  'Stem Biomass'         = list(Xc="Xc11", SDc="SDc11", Nc="Nc11", Xt="Xt11", SDt="SDt11", Nt="Nt11"),
  'Stem Leaf Ratio'      = list(Xc="Xc12", SDc="SDc12", Nc="Nc12", Xt="Xt12", SDt="SDt12", Nt="Nt12"),
  'Belowground Biomass'  = list(Xc="Xc13", SDc="SDc13", Nc="Nc13", Xt="Xt13", SDt="SDt13", Nt="Nt13"),
  'Root Diameter'        = list(Xc="Xc14", SDc="SDc14", Nc="Nc14", Xt="Xt14", SDt="SDt14", Nt="Nt14"),
  'Root Surface Area'    = list(Xc="Xc15", SDc="SDc15", Nc="Nc15", Xt="Xt15", SDt="SDt15", Nt="Nt15"),
  'Root Length'          = list(Xc="Xc16", SDc="SDc16", Nc="Nc16", Xt="Xt16", SDt="SDt16", Nt="Nt16"),
  'Specific Root Length' = list(Xc="Xc17", SDc="SDc17", Nc="Nc17", Xt="Xt17", SDt="SDt17", Nt="Nt17"),
  'Root Density'         = list(Xc="Xc18", SDc="SDc18", Nc="Nc18", Xt="Xt18", SDt="SDt18", Nt="Nt18"),
  'Root Volume'          = list(Xc="Xc19", SDc="SDc19", Nc="Nc19", Xt="Xt19", SDt="SDt19", Nt="Nt19"),
  'Root Shoot Ratio'     = list(Xc="Xc20", SDc="SDc20", Nc="Nc20", Xt="Xt20", SDt="SDt20", Nt="Nt20")
)

all_cols_needed <- unique(unlist(lapply(indicators, unlist)))
missing_in_data <- all_cols_needed[!all_cols_needed %in% colnames(mydata)]
if (length(missing_in_data) > 0) {
  stop(paste("The database is missing required columns:", paste(missing_in_data, collapse = ", ")))
}
cat(sprintf("Trait column validation passed for %d traits.\n\n", length(indicators)))

predictor_labels <- c(
  "lat"          = "Latitude",
  "elev"         = "Elevation",
  "AI"           = "Aridity Index",
  "livestock_f"  = "Livestock Type",
  "duration_num" = "Grazing Duration",
  "intensity_f"  = "Grazing Intensity"
)

# -----------------------------------------------------------------------------

cat("========== Starting random forest analysis for 20 traits ==========\n")
cat("Each trait uses ntree = 1000 and nrep = 999; runtime depends on hardware and data size.\n\n")

all_results <- list()
rf_models <- list()
trait_names <- names(indicators)

for (i in seq_along(indicators)) {

  label <- trait_names[i]
  cols  <- indicators[[label]]

  cat(sprintf("[%2d/20] %s\n", i, label))

  trait_cols <- mydata %>%
    select(Xc  = all_of(cols$Xc),
           SDc = all_of(cols$SDc),
           Nc  = all_of(cols$Nc),
           Xt  = all_of(cols$Xt),
           SDt = all_of(cols$SDt),
           Nt  = all_of(cols$Nt)) %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

  eff <- tryCatch(
    escalc(measure = "ROM",
           m1i = Xt, sd1i = SDt, n1i = Nt,
           m2i = Xc, sd2i = SDc, n2i = Nc,
           data = trait_cols),
    error = function(e) {
      cat(sprintf("  WARNING: escalc failed: %s\n", e$message))
      NULL
    }
  )

  if (is.null(eff)) { cat("\n"); next }

  valid_idx <- which(!is.na(eff$yi) & !is.na(eff$vi))
  n_valid   <- length(valid_idx)
  cat(sprintf("  Valid sample size: %d\n", n_valid))

  if (n_valid < 30) {
    cat(sprintf("  WARNING: skipped because fewer than 30 valid observations were available.\n\n")); next
  }

  df_trait <- pred_imputed[valid_idx, ] %>%
    mutate(lnRR = as.numeric(eff$yi[valid_idx]))

  if (any(is.na(df_trait))) {
    cat("  WARNING: skipped because the modeling data contain missing values.\n\n"); next
  }

  set.seed(2024)
  rf_fit <- tryCatch(
    rfPermute(lnRR ~ ., data = df_trait,
              ntree = 1000, nrep = 999, num.cores = 1),
    error = function(e) {
      cat(sprintf("  WARNING: model fitting failed: %s\n", e$message))
      NULL
    }
  )

  if (is.null(rf_fit)) { cat("\n"); next }

  oob_r2 <- rf_fit$rf$rsq[1000]
  cat(sprintf("  OOB R² = %.3f\n", oob_r2))

  imp_scaled <- data.frame(importance(rf_fit, scale = TRUE),
                            check.names = FALSE)
  pval_mat   <- rf_fit$pval[ , , 2]

  imp_df <- data.frame(
    Predictor  = rownames(imp_scaled),
    Importance = imp_scaled$`%IncMSE`,
    pval       = pval_mat[rownames(imp_scaled), "%IncMSE"]
  ) %>%
    mutate(
      sig = case_when(
        pval >= 0.05                  ~ "",
        pval >= 0.01 & pval < 0.05   ~ "*",
        pval >= 0.001 & pval < 0.01  ~ "**",
        pval < 0.001                  ~ "***"
      ),
      Predictor = dplyr::recode(Predictor, !!!predictor_labels),
      Trait     = label,
      OOB_R2    = oob_r2
    )

  all_results[[label]] <- imp_df

  rf_models[[label]] <- list(
    model = rf_fit$rf,
    data = df_trait,
    oob_r2 = as.numeric(oob_r2),
    n_valid = n_valid,
    pvals_internal = setNames(
      as.numeric(pval_mat[rownames(imp_scaled), "%IncMSE"]),
      rownames(imp_scaled)
    )
  )

  cat(sprintf("  Completed.\n\n"))
}

cat(sprintf("Modeling completed: %d of %d traits succeeded.\n\n",
            length(all_results), length(indicators)))

if (length(all_results) == 0) {
  stop("All trait models failed. Check the database and preprocessing steps.")
}

# SUBSECTION 6.3: EXPORT MODEL RESULTS AND CLASSIFY STRONG/WEAK PREDICTION
# The importance table is the primary RF output used by the plots below.
summary_df <- bind_rows(all_results)
write.csv(summary_df, file.path(OUTPUT_DIR, "RF_all_traits_importance.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("Saved summary table: RF_all_traits_importance.csv\n\n")

cat("========== OOB R-squared summary by trait ==========\n")
oob_tbl <- summary_df %>%
  group_by(Trait) %>%
  summarise(OOB_R2 = round(unique(OOB_R2), 3), .groups = "drop") %>%
  arrange(desc(OOB_R2))
print(oob_tbl)
write.csv(oob_tbl, file.path(OUTPUT_DIR, "RF_OOB_R2_summary.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# -----------------------------------------------------------------------------

make_single_plot <- function(trait_name, panel_letter = NULL) {

  imp_df <- all_results[[trait_name]]
  if (is.null(imp_df)) return(NULL)

  oob_r2 <- unique(imp_df$OOB_R2)

  imp_df <- imp_df %>%
    arrange(Importance) %>%
    mutate(
      Predictor = factor(Predictor, levels = Predictor),
      bar_color = ifelse(pval < 0.05, "#5b9bd5", "#b0b0b0")
    )

  x_max <- max(imp_df$Importance, na.rm = TRUE) * 1.35

  ggplot(imp_df, aes(x = Predictor, y = Importance, fill = bar_color)) +
    geom_col(width = 0.6, color = NA) +
    geom_text(aes(label = sig, y = Importance),
              hjust = -0.15, size = 4.5,
              color = "black", fontface = "bold") +
    scale_fill_identity() +
    scale_y_continuous(expand = c(0, 0), limits = c(0, x_max)) +
    coord_flip() +
    labs(
      title    = trait_name,
      subtitle = paste0("R² = ", round(oob_r2, 3)),
      tag      = panel_letter,
      x        = NULL,
      y        = "Increase in MSE (%)"
    ) +
    theme_bw(base_size = FONT_AXIS_TEXT, base_family = FONT_FAMILY) +
    theme(
      text            = element_text(family = FONT_FAMILY),
      plot.title      = element_text(hjust = 0.5, size = FONT_PANEL_TITLE, face = "bold"),
      plot.subtitle   = element_text(hjust = 0.5, size = FONT_R2, color = "grey40"),
      plot.tag        = element_text(size = FONT_PANEL_TAG, face = "bold", family = FONT_FAMILY, color = "black"),
      plot.tag.position = c(0.015, 0.985),
      axis.text.y     = element_text(size = FONT_AXIS_TEXT, color = "black"),
      axis.ticks.y    = element_line(color = "black"),
      axis.text.x     = element_text(size = FONT_AXIS_TEXT, color = "black"),
      axis.title.x    = element_text(size = FONT_AXIS_TITLE, face = "bold"),
      panel.grid      = element_blank(),
      axis.line       = element_line(colour = "black"),
      legend.position = "none",
      plot.margin     = unit(c(7, 10, 5, 7), "pt")
    )
}

# -----------------------------------------------------------------------------

forest_order <- c(
  "Height", "Aboveground Biomass", "Leaf Length", "Leaf Width",
  "Leaf Area", "Leaf Thickness", "Leaf Number", "Leaf Biomass",
  "Stem Diameter", "Stem Biomass", "Belowground Biomass",
  "Root Diameter", "Root Length", "Root Surface Area",
  "Root Volume", "Root Density", "Specific Leaf Area",
  "Specific Root Length", "Stem Leaf Ratio", "Root Shoot Ratio"
)

oob_lookup <- summary_df %>%
  group_by(Trait) %>%
  summarise(OOB_R2 = unique(OOB_R2), .groups = "drop")

trait_order_df <- data.frame(
  Trait      = forest_order,
  plot_order = seq_along(forest_order)
) %>%
  left_join(oob_lookup, by = "Trait") %>%
  filter(!is.na(OOB_R2)) %>%
  mutate(group = ifelse(OOB_R2 >= 0.1, "high", "low"))

high_traits <- trait_order_df %>% filter(group == "high") %>% pull(Trait)
low_traits  <- trait_order_df %>% filter(group == "low")  %>% pull(Trait)

cat(sprintf("\nTraits with OOB R-squared >= 0.1: %d\nTraits with OOB R-squared < 0.1: %d\n\n",
            length(high_traits), length(low_traits)))

combine_plots <- function(trait_list, title_text, ncols = 3) {
  plots <- lapply(seq_along(trait_list), function(i) {
    make_single_plot(trait_list[[i]], panel_letter = LETTERS[i])
  })
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)
  wrap_plots(plots, ncol = ncols) +
    plot_annotation(
      title = title_text,
      theme = theme(
        text = element_text(family = FONT_FAMILY),
        plot.title = element_text(hjust = 0.5, size = FONT_FIGURE_TITLE, face = "bold",
                                  family = FONT_FAMILY, margin = margin(b = 8))
      )
    )
}

cat("Generating the combined plot for traits with OOB R-squared >= 0.1...\n")
p_high <- combine_plots(
  high_traits,
  "Random Forest Variable Importance (OOB R² ≥ 0.1)",
  ncols = 3
)
if (!is.null(p_high)) {
  nrows_high <- ceiling(length(high_traits) / 3)
  h_high <- nrows_high * 5.0 + 1.0
  ggsave(file.path(OUTPUT_DIR, "RF_combined_high_R2.jpeg"), plot = p_high, device = "jpeg",
         width = 16, height = h_high, dpi = 600, bg = "white")
  ggsave(file.path(OUTPUT_DIR, "RF_combined_high_R2.pdf"), plot = p_high, device = grDevices::cairo_pdf,
         width = 16, height = h_high, bg = "white")
  cat("Saved: RF_combined_high_R2.jpeg\n\n")
} else {
  message("No traits met the OOB R-squared >= 0.1 threshold; the high-R-squared plot was skipped.")
}

cat("Generating the combined plot for traits with OOB R-squared < 0.1...\n")
p_low <- combine_plots(
  low_traits,
  "Random Forest Variable Importance (OOB R² < 0.1)",
  ncols = 3
)
if (!is.null(p_low)) {
  nrows_low <- ceiling(length(low_traits) / 3)
  h_low <- nrows_low * 5.0 + 1.0
  ggsave(file.path(OUTPUT_DIR, "RF_combined_low_R2.jpeg"), plot = p_low, device = "jpeg",
         width = 16, height = h_low, dpi = 600, bg = "white")
  ggsave(file.path(OUTPUT_DIR, "RF_combined_low_R2.pdf"), plot = p_low, device = grDevices::cairo_pdf,
         width = 16, height = h_low, bg = "white")
  cat("Saved: RF_combined_low_R2.jpeg\n\n")
} else {
  message("No traits fell below the OOB R-squared 0.1 threshold; the low-R-squared plot was skipped.")
}

# -----------------------------------------------------------------------------

# SUBSECTION 6.4: VARIABLE-IMPORTANCE HEATMAP
# Uses the RF importance table generated above; no external result table is read here.
cat("Generating the variable-importance heatmap...\n")

heatmap_df <- summary_df %>%
  filter(Trait %in% forest_order) %>%
  mutate(
    Trait = factor(Trait, levels = rev(forest_order)),
    sig_label = case_when(
      pval >= 0.05                  ~ "",
      pval >= 0.01 & pval < 0.05   ~ "*",
      pval >= 0.001 & pval < 0.01  ~ "**",
      pval < 0.001                  ~ "***"
    )
  )

pred_order <- heatmap_df %>%
  group_by(Predictor) %>%
  summarise(mean_imp = mean(Importance, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_imp)) %>%
  pull(Predictor)

heatmap_df <- heatmap_df %>%
  mutate(Predictor = factor(Predictor, levels = pred_order))

p_heat <- ggplot(heatmap_df,
                 aes(x = Predictor, y = Trait, fill = Importance)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sig_label),
            size = 5.2, color = "white", fontface = "bold") +
  scale_fill_gradient2(
    low      = "#eef3ff",
    mid      = "#5b9bd5",
    high     = "#1a3a8f",
    midpoint = median(heatmap_df$Importance, na.rm = TRUE),
    name     = "Increase\nin MSE (%)",
    na.value = "grey90"
  ) +
  scale_y_discrete(labels = function(x) x) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  labs(
    title = "Random Forest Variable Importance Across Functional Traits",
    x = NULL, y = NULL
  ) +
  theme_bw(base_size = FONT_AXIS_TEXT, base_family = FONT_FAMILY) +
  theme(
    text            = element_text(family = FONT_FAMILY),
    plot.title      = element_text(hjust = 0.5, size = FONT_FIGURE_TITLE, face = "bold",
                                   family = FONT_FAMILY, margin = margin(b = 10)),
    axis.text.y     = element_text(size = FONT_AXIS_TEXT, color = "black", family = FONT_FAMILY),
    axis.text.x     = element_text(size = FONT_AXIS_TEXT + 0.5, angle = 30,
                                   hjust = 1, face = "bold", family = FONT_FAMILY),
    legend.position = "right",
    panel.grid      = element_blank(),
    plot.margin     = unit(c(10, 10, 10, 10), "pt")
  )

ggsave(file.path(OUTPUT_DIR, "RF_heatmap_all_traits.jpeg"), plot = p_heat, device = "jpeg",
       width = 14, height = 12, dpi = 600, bg = "white")
ggsave(file.path(OUTPUT_DIR, "RF_heatmap_all_traits.pdf"), plot = p_heat, device = grDevices::cairo_pdf,
       width = 14, height = 12, bg = "white")
cat("Saved heatmap: RF_heatmap_all_traits.jpeg\n")

# -----------------------------------------------------------------------------
cat("\nAll random-forest analyses completed. Output files:\n")
cat("  RF_all_traits_importance.csv    Variable importance and permutation p-values for all traits\n")
cat("  RF_combined_high_R2.jpeg/pdf    Combined plots for traits with OOB R-squared >= 0.1\n")
cat("  RF_combined_low_R2.jpeg/pdf     Combined plots for traits with OOB R-squared < 0.1\n")
cat("  RF_heatmap_all_traits.jpeg/pdf  Variable-importance heatmap across traits\n")

# -----------------------------------------------------------------------------

# SUBSECTION 6.5: PARTIAL-DEPENDENCE RESPONSE PLOTS AND SOURCE-DATA EXPORT
# Uses the fitted RF models stored in rf_models; it does not require a separate input table.
# Continuous drivers are plotted as response curves; categorical drivers are plotted by level.
# Source data are exported from the exact data objects used by the plotted panels.
cat("\n========== Generating random-forest driver response plots ==========\n")

response_dir <- OUTPUT_DIR

response_drivers <- c("AI", "duration_num", "intensity_f")
driver_display <- c(
  AI = "Aridity index",
  duration_num = "Grazing duration",
  intensity_f = "Grazing intensity"
)
driver_x_labels <- c(
  AI = "Aridity index",
  duration_num = "Grazing duration",
  intensity_f = "Grazing intensity"
)
response_colors <- c(
  AI = "#2C7C93",
  duration_num = "#3E8B73",
  intensity_f = "#B9783F"
)

trait_display_order <- names(indicators)
trait_full_labels <- c(
  "Height" = "Plant height",
  "Aboveground Biomass" = "Aboveground biomass",
  "Leaf Length" = "Leaf length",
  "Leaf Width" = "Leaf width",
  "Leaf Area" = "Leaf area",
  "Specific Leaf Area" = "Specific leaf area",
  "Leaf Number" = "Leaf number",
  "Leaf Thickness" = "Leaf thickness",
  "Leaf Biomass" = "Leaf biomass",
  "Stem Diameter" = "Stem diameter",
  "Stem Biomass" = "Stem biomass",
  "Stem Leaf Ratio" = "Stem-to-leaf ratio",
  "Belowground Biomass" = "Belowground biomass",
  "Root Diameter" = "Root diameter",
  "Root Surface Area" = "Root surface area",
  "Root Length" = "Root length",
  "Specific Root Length" = "Specific root length",
  "Root Density" = "Root density",
  "Root Volume" = "Root volume",
  "Root Shoot Ratio" = "Root-to-shoot ratio"
)

# Collect the exact plotted values without recalculating model predictions.
# Numeric driver values remain numeric; grazing-intensity labels remain categorical.
collect_pd_source_data <- function(panel_plot, fit_obj, variable, trait) {
  pd <- panel_plot$data
  stopifnot(
    is.data.frame(pd),
    nrow(pd) > 0L,
    all(c("Value", "Predicted_lnRR") %in% names(pd)),
    is.numeric(pd$Predicted_lnRR)
  )

  n_points <- nrow(pd)
  out <- data.frame(
    Figure = paste0("RF_response_", variable),
    Driver = unname(driver_display[[variable]]),
    Driver_variable = variable,
    Trait = trait,
    Trait_full = as.character(panel_plot$labels$title),
    Panel = as.character(panel_plot$labels$tag),
    OOB_R2 = fit_obj$oob_r2,
    Importance_p = unname(fit_obj$pvals_internal[[variable]]),
    N_model = nrow(fit_obj$data),
    Point_index = seq_len(n_points),
    AI = rep(NA_real_, n_points),
    duration_num = rep(NA_real_, n_points),
    intensity_f = rep(NA_character_, n_points),
    Intensity_label = rep(NA_character_, n_points),
    N_at_level = rep(NA_integer_, n_points),
    Level_observed = rep(NA, n_points),
    Predicted_lnRR = pd$Predicted_lnRR,
    stringsAsFactors = FALSE
  )

  if (variable %in% c("AI", "duration_num")) {
    if (!is.numeric(fit_obj$data[[variable]]) || !is.numeric(pd$Value)) {
      stop(sprintf(
        "Expected numeric model and plotted values for %s (%s); no automatic coercion was applied.",
        variable, trait
      ))
    }
    out[[variable]] <- pd$Value
    stopifnot(identical(out[[variable]], pd$Value))
  } else if (identical(variable, "intensity_f")) {
    stopifnot("Value_label" %in% names(pd))
    out$intensity_f <- as.character(pd$Value)
    out$Intensity_label <- as.character(pd$Value_label)
    observed <- as.character(fit_obj$data[[variable]])
    out$N_at_level <- vapply(out$intensity_f, function(level) {
      as.integer(sum(observed == level, na.rm = TRUE))
    }, integer(1))
    out$Level_observed <- out$N_at_level > 0L
    stopifnot(
      identical(out$intensity_f, as.character(pd$Value)),
      identical(out$Intensity_label, as.character(pd$Value_label))
    )
  } else {
    stop(sprintf("Unsupported partial-dependence source-data driver: %s", variable))
  }

  stopifnot(identical(out$Predicted_lnRR, pd$Predicted_lnRR))
  out
}

# Preserve floating-point precision when writing source-data CSV files.
write_pd_source_csv <- function(data, path) {
  csv_data <- data
  numeric_columns <- vapply(data, is.numeric, logical(1))
  csv_data[numeric_columns] <- lapply(data[numeric_columns], function(x) {
    formatted <- sprintf("%.17g", x)
    formatted[is.na(x)] <- NA_character_
    formatted
  })
  write.table(
    csv_data,
    file = path,
    sep = ",",
    dec = ".",
    quote = which(vapply(data, is.character, logical(1))),
    row.names = FALSE,
    col.names = TRUE,
    na = "",
    qmethod = "double",
    fileEncoding = "UTF-8"
  )
}

# Reset the source-data collection when this section is rerun interactively.
response_source_data <- list()

response_selection <- list()
for (v in response_drivers) {
  selected_traits <- character(0)
  for (tr in trait_display_order) {
    fit_obj <- rf_models[[tr]]
    if (is.null(fit_obj)) next
    p_driver <- fit_obj$pvals_internal[[v]]
    if (is.null(p_driver) || is.na(p_driver)) next
    if (is.finite(fit_obj$oob_r2) && fit_obj$oob_r2 >= 0.1 && p_driver < 0.05) {
      selected_traits <- c(selected_traits, tr)
    }
  }
  response_selection[[v]] <- selected_traits
  cat(sprintf("%s: %d traits selected\n", driver_display[[v]], length(selected_traits)))
}

if (sum(lengths(response_selection)) == 0) {
  warning("No traits met OOB R-squared >= 0.1 and p < 0.05 for any selected driver; response plots were not generated.")
} else {

  get_pdp_continuous <- function(fit_obj, variable, n_grid = 60L) {
    dat <- fit_obj$data
    x <- suppressWarnings(as.numeric(dat[[variable]]))
    x <- x[is.finite(x)]
    if (length(x) < 2L || length(unique(x)) < 2L) return(NULL)

    xr <- range(x, na.rm = TRUE)
    grid_x <- seq(xr[1], xr[2], length.out = n_grid)

    base <- dat[, setdiff(names(dat), "lnRR"), drop = FALSE]
    n <- nrow(base)
    newdata <- base[rep(seq_len(n), times = length(grid_x)), , drop = FALSE]
    newdata[[variable]] <- rep(grid_x, each = n)

    pred <- tryCatch(
      as.numeric(predict(fit_obj$model, newdata = newdata)),
      error = function(e) {
        stop(sprintf("PDP prediction failed for variable %s: %s", variable, e$message))
      }
    )
    pred_mat <- matrix(pred, nrow = n, ncol = length(grid_x))
    data.frame(Value = grid_x, Predicted_lnRR = colMeans(pred_mat, na.rm = TRUE))
  }

  get_pdp_categorical <- function(fit_obj, variable) {
    dat <- fit_obj$data
    original <- dat[[variable]]
    lev <- if (is.factor(original)) levels(original) else unique(as.character(original))
    lev <- lev[!is.na(lev)]
    if (length(lev) < 2L) return(NULL)

    base <- dat[, setdiff(names(dat), "lnRR"), drop = FALSE]
    n <- nrow(base)
    newdata <- base[rep(seq_len(n), times = length(lev)), , drop = FALSE]
    if (is.ordered(original)) {
      newdata[[variable]] <- ordered(rep(lev, each = n), levels = levels(original))
    } else if (is.factor(original)) {
      newdata[[variable]] <- factor(rep(lev, each = n), levels = levels(original))
    } else {
      newdata[[variable]] <- rep(lev, each = n)
    }

    pred <- tryCatch(
      as.numeric(predict(fit_obj$model, newdata = newdata)),
      error = function(e) {
        stop(sprintf("Categorical prediction failed for variable %s: %s", variable, e$message))
      }
    )
    pred_mat <- matrix(pred, nrow = n, ncol = length(lev))
    out <- data.frame(Value = lev, Predicted_lnRR = colMeans(pred_mat, na.rm = TRUE))
    out
  }

  theme_response_panel <- function() {
    theme_classic(base_size = FONT_AXIS_TEXT, base_family = FONT_FAMILY) +
      theme(
        text = element_text(family = FONT_FAMILY),
        plot.title = element_text(size = FONT_PANEL_TITLE, face = "bold", hjust = 0.5,
                                  family = FONT_FAMILY, color = "black", margin = margin(b = 5)),
        plot.tag = element_text(size = FONT_PANEL_TAG, face = "bold", family = FONT_FAMILY, color = "black"),
        plot.tag.position = c(0.015, 0.985),
        axis.title = element_text(size = FONT_AXIS_TITLE, family = FONT_FAMILY, color = "black"),
        axis.text = element_text(size = FONT_AXIS_TEXT, family = FONT_FAMILY, color = "black"),
        axis.line = element_line(linewidth = 0.5, color = "black"),
        axis.ticks = element_line(linewidth = 0.45, color = "black"),
        plot.margin = margin(9, 8, 8, 8),
        legend.position = "none"
      )
  }

  make_response_panel <- function(trait, variable, panel_letter) {
    fit_obj <- rf_models[[trait]]
    if (is.null(fit_obj)) return(NULL)

    trait_label <- unname(trait_full_labels[trait])
    if (is.na(trait_label) || length(trait_label) == 0) trait_label <- trait
    color_use <- unname(response_colors[[variable]])

    if (variable %in% c("AI", "duration_num")) {
      pd <- get_pdp_continuous(fit_obj, variable)
      if (is.null(pd) || nrow(pd) == 0) return(NULL)
      rug_data <- data.frame(rug_x = suppressWarnings(as.numeric(fit_obj$data[[variable]])))
      rug_data <- rug_data[is.finite(rug_data$rug_x), , drop = FALSE]
      p <- ggplot(pd, aes(x = Value, y = Predicted_lnRR)) +
        geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.45, color = "grey65") +
        geom_line(linewidth = 0.9, color = color_use, lineend = "round") +
        geom_rug(data = rug_data, aes(x = rug_x), inherit.aes = FALSE,
                 sides = "b", alpha = 0.16, linewidth = 0.2,
                 length = grid::unit(1.2, "mm")) +
        labs(
          title = trait_label,
          tag = panel_letter,
          x = unname(driver_x_labels[[variable]]),
          y = "Predicted ln(RR)"
        ) +
        theme_response_panel()
    } else {
      pd <- get_pdp_categorical(fit_obj, variable)
      if (is.null(pd) || nrow(pd) == 0) return(NULL)
      intensity_labels <- c("LG" = "LightG", "MG" = "ModerateG", "HG" = "HighG", "FG" = "FreeG")
      pd$Value_label <- ifelse(pd$Value %in% names(intensity_labels),
                               unname(intensity_labels[pd$Value]), as.character(pd$Value))
      pd$Value_label <- factor(pd$Value_label,
                               levels = unname(intensity_labels[c("LG", "MG", "HG", "FG")]))
      p <- ggplot(pd, aes(x = Value_label, y = Predicted_lnRR)) +
        geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.45, color = "grey65") +
        geom_point(size = 3.0, color = color_use) +
        labs(
          title = trait_label,
          tag = panel_letter,
          x = "Grazing intensity",
          y = "Predicted ln(RR)"
        ) +
        theme_response_panel() +
        theme(axis.text.x = element_text(size = FONT_AXIS_TEXT, family = FONT_FAMILY, color = "black"))
    }
    p
  }

  response_figures <- list()
  response_manifest <- list()

  for (v in response_drivers) {
    selected_traits <- response_selection[[v]]
    if (length(selected_traits) == 0) next

    panels <- list()
    for (j in seq_along(selected_traits)) {
      tr <- selected_traits[j]
      panel_plot <- make_response_panel(tr, v, LETTERS[j])
      panels[[length(panels) + 1L]] <- panel_plot

      if (!is.null(panel_plot)) {
        response_source_data[[length(response_source_data) + 1L]] <-
          collect_pd_source_data(panel_plot, rf_models[[tr]], v, tr)
      }

      response_manifest[[length(response_manifest) + 1L]] <- data.frame(
        Driver = driver_display[[v]],
        Trait = tr,
        Trait_full = unname(trait_full_labels[tr]),
        OOB_R2 = rf_models[[tr]]$oob_r2,
        Importance_p = rf_models[[tr]]$pvals_internal[[v]],
        Panel = LETTERS[j],
        stringsAsFactors = FALSE
      )
    }
    panels <- Filter(Negate(is.null), panels)
    if (length(panels) == 0) next

    ncol_use <- 3L
    nrow_use <- ceiling(length(panels) / ncol_use)
    fig <- wrap_plots(panels, ncol = ncol_use, byrow = TRUE) +
      plot_annotation(
        title = switch(v,
          AI = "Relationships between aridity index and functional trait responses",
          duration_num = "Relationships between grazing duration and functional trait responses",
          intensity_f = "Functional trait responses across grazing intensity categories"
        ),
        theme = theme(
          text = element_text(family = FONT_FAMILY),
          plot.title = element_text(size = FONT_FIGURE_TITLE, face = "bold", hjust = 0.5,
                                    family = FONT_FAMILY, color = "black", margin = margin(b = 8)),
          plot.margin = margin(10, 10, 10, 10)
        )
      )

    response_figures[[v]] <- fig
    height_use <- max(5.5, nrow_use * 3.1 + 1.2)
    stem <- file.path(response_dir, paste0("RF_response_", v))
    ggsave(paste0(stem, ".pdf"), fig, device = grDevices::cairo_pdf,
           width = 12, height = height_use, units = "in", bg = "white")
    ggsave(paste0(stem, ".png"), fig,
           width = 12, height = height_use, units = "in", dpi = 600, bg = "white")
    ggsave(paste0(stem, ".tiff"), fig, device = "tiff", compression = "lzw",
           width = 12, height = height_use, units = "in", dpi = 600, bg = "white")
    cat(sprintf("Saved %s with %d panels.\n", stem, length(panels)))
  }

  if (length(response_manifest) > 0) {
    response_manifest_df <- dplyr::bind_rows(response_manifest)
    write.csv(response_manifest_df,
              file.path(response_dir, "RF_response_panel_selection.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
    cat("Saved response-panel selection table: RF_response_panel_selection.csv\n")
  }
}

# Combine source data for all successfully plotted panels.
# If no panel was produced, save header-only tables to document the empty result.
if (length(response_source_data) > 0L) {
  response_source_data_df <- dplyr::bind_rows(response_source_data)
} else {
  response_source_data_df <- data.frame(
    Figure = character(),
    Driver = character(),
    Driver_variable = character(),
    Trait = character(),
    Trait_full = character(),
    Panel = character(),
    OOB_R2 = double(),
    Importance_p = double(),
    N_model = integer(),
    Point_index = integer(),
    AI = double(),
    duration_num = double(),
    intensity_f = character(),
    Intensity_label = character(),
    N_at_level = integer(),
    Level_observed = logical(),
    Predicted_lnRR = double(),
    stringsAsFactors = FALSE
  )
}

for (v in response_drivers) {
  driver_data <- response_source_data_df[
    response_source_data_df$Driver_variable == v,
    ,
    drop = FALSE
  ]
  write_pd_source_csv(
    driver_data,
    file.path(response_dir, paste0("RF_PD_Source_Data_", v, ".csv"))
  )
}

write_pd_source_csv(
  response_source_data_df,
  file.path(response_dir, "RF_PD_Source_Data_All.csv")
)
saveRDS(
  response_source_data_df,
  file.path(response_dir, "RF_PD_Source_Data_All.rds")
)
cat(sprintf(
  "Partial-dependence source data exported: %d rows; three driver CSVs, one combined CSV, and one RDS file.\n",
  nrow(response_source_data_df)
))

cat("\nRandom-forest modeling, importance analysis, figures, response curves, and source-data export completed.\n")
cat("All output files are located in: ", OUTPUT_DIR, "\n")
