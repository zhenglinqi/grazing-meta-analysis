# =============================================================================
# Plant Responses to Grazing from a Functional Trait Perspective:
# Global Meta-Analysis — Complete Analysis Pipeline
#
# Sections:
#   1. Meta-Analysis (ROM effect sizes, publication bias)
#   2. Forest Plots (Total/FG, Intensity, Aridity, Duration, Livestock Type)
#   3. Correlation Analysis (Spearman heatmap)
#   4. Functional Trait Network
#   5. Plasticity Analysis (RDPI)
#   6. Random Forest Models
#
# Input data: Database.xlsx
# =============================================================================


# =============================================================================
# SECTION 1: META-ANALYSIS
# =============================================================================

library(DT)
library(Matrix)
library(metadat)
library(metafor)
library(ggplot2)
library(readxl)

mydata <- read_excel("Database.xlsx")

RR_effect_sizes <- escalc("ROM",
                           m1i  = Xt3,  n1i = Nt3, sd1i = SDt3,
                           m2i  = Xc3,  n2i = Nc3, sd2i = SDc3,
                           data = mydata)

random_effect_model_results <- rma(yi, vi, method = "REML",
                                   data = RR_effect_sizes)
random_effect_model_results

funnel(random_effect_model_results, main = "SLR", level = 0.95)

egger_test <- regtest(random_effect_model_results)
egger_test

influence_results <- influence(random_effect_model_results)
plot(influence_results)

write.csv(random_effect_model_results$yi, file = "Height.csv",
          row.names = FALSE)


# =============================================================================
# SECTION 2: FOREST PLOTS
# =============================================================================

library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(grid)

# -----------------------------------------------------------------------------
# 2-1. Total / Functional Groups (FG) forest plot
# -----------------------------------------------------------------------------

data1 <- read_excel("总体新结果.xlsx")
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
  labs(title = "Total", x = "Weighted In(RR)", y = NULL) +
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

ggsave("Grazing_FG.jpeg", plot = forest_plot_ld, device = "jpeg",
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
# 2-3. Grazing Intensity forest plot
# -----------------------------------------------------------------------------

lg <- read_excel("LG结果表.xlsx"); lg$Intensity <- "LG"
mg <- read_excel("MG结果表.xlsx"); mg$Intensity <- "MG"
hg <- read_excel("HG结果表.xlsx"); hg$Intensity <- "HG"

dat <- bind_rows(lg, mg, hg)

intensity_order <- c("LG", "MG", "HG")

dat <- dat %>%
  mutate(
    Indicator = factor(Indicator, levels = rev(ind_order)),
    Intensity = factor(Intensity, levels = rev(intensity_order))
  ) %>%
  mutate(
    show_point = !is.na(Color),
    pt_fill  = case_when(Color==1~"#79bb56", Color==2~"#c0392b",
                         Color==0~"white",   TRUE~NA_character_),
    pt_color = case_when(Color==1~"#79bb56", Color==2~"#c0392b",
                         Color==0~"black",   TRUE~NA_character_),
    N_label  = ifelse(is.na(N)|N==0, NA_character_, paste0("(",as.integer(N),")"))
  )

n_intensity <- 3
row_block   <- 5.0
pad         <- row_block * 0.20
row_h       <- (row_block - 2*pad) / (n_intensity - 1)

ind_levels  <- levels(dat$Indicator)
int_levels  <- levels(dat$Intensity)

y_base      <- setNames((seq_along(ind_levels)-1)*row_block + pad, ind_levels)
int_offset  <- setNames((seq_along(int_levels)-1)*row_h, int_levels)

dat <- dat %>%
  mutate(y_pos = y_base[as.character(Indicator)] + int_offset[as.character(Intensity)])

ind_centers  <- make_ind_centers(ind_levels, row_block)
ind_bg       <- make_ind_bg(ind_levels, row_block)
y_min_plot   <- 0
y_max_plot   <- length(ind_levels) * row_block
x_lo <- -1.5; x_hi <- 1.5; n_x_pos <- x_hi - 0.03

legend_x <- 0.72
legend_y <- 0.90

p_intensity <- ggplot() +
  geom_rect(data=ind_bg, aes(xmin=-Inf,xmax=Inf,ymin=y_bot,ymax=y_top),
            fill=ind_bg$bg, inherit.aes=FALSE) +
  geom_segment(data=filter(dat,show_point),
               aes(x=Low,xend=Up,y=y_pos,yend=y_pos,color=I(pt_color)),
               linewidth=0.7) +
  geom_point(data=filter(dat,show_point),
             aes(x=Mean,y=y_pos,shape=Intensity,
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
    name   = "Grazing Intensity",
    values = c("LG"=21,"MG"=24,"HG"=23),
    breaks = intensity_order,
    guide  = guide_legend(override.aes=list(fill="white",color="black",
                                            size=4.5,stroke=0.9))
  ) +
  scale_x_continuous(breaks=seq(-1.5,1.5,by=0.5), limits=c(x_lo,x_hi),
                     oob=scales::oob_keep) +
  coord_cartesian(xlim=c(x_lo,x_hi), ylim=c(y_min_plot,y_max_plot),
                  expand=FALSE, clip="off") +
  labs(x="Weighted ln(RR)", y=NULL) +
  shared_theme(legend_x, legend_y)

ggsave("Grazing_Intensity_forest_plot.pdf",  plot=p_intensity, width=8.5, height=13, units="in")
ggsave("Grazing_Intensity_forest_plot.png",  plot=p_intensity, width=8.5, height=13, units="in", dpi=300)


# -----------------------------------------------------------------------------
# 2-4. Aridity zone forest plot
# -----------------------------------------------------------------------------

arid <- read_excel("Arid结果表.xlsx");         arid$Climate <- "Arid"
semi <- read_excel("Semi-arid结果表.xlsx");    semi$Climate <- "Semi-arid"
dry  <- read_excel("Dry_sub-humid结果表.xlsx"); dry$Climate  <- "Dry sub-humid"
hum  <- read_excel("Humid结果表.xlsx");        hum$Climate  <- "Humid"

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

ggsave("Grazing_Aridity_forest_plot.pdf", plot=p_aridity, width=8.5, height=15, units="in")
ggsave("Grazing_Aridity_forest_plot.png", plot=p_aridity, width=8.5, height=15, units="in", dpi=600)


# -----------------------------------------------------------------------------
# 2-5. Grazing Duration forest plot
# -----------------------------------------------------------------------------

short  <- read_excel("Short结果表.xlsx");  short$Duration  <- "Short"
middle <- read_excel("Middle结果表.xlsx"); middle$Duration <- "Middle"
long   <- read_excel("Long结果表.xlsx");   long$Duration   <- "Long"

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

ggsave("Grazing_Duration_forest_plot.pdf", plot=p_duration, width=8.5, height=13, units="in")
ggsave("Grazing_Duration_forest_plot.png", plot=p_duration, width=8.5, height=13, units="in", dpi=300)


# -----------------------------------------------------------------------------
# 2-6. Livestock Type forest plot
# -----------------------------------------------------------------------------

sheep  <- read_excel("Sheep结果表.xlsx");  sheep$Type  <- "Sheep"
cattle <- read_excel("Cattle结果表.xlsx"); cattle$Type <- "Cattle and Horse"

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

ggsave("Grazing_LivestockType_forest_plot.pdf", plot=p_livestock, width=8.5, height=11, units="in")
ggsave("Grazing_LivestockType_forest_plot.png", plot=p_livestock, width=8.5, height=11, units="in", dpi=300)


# =============================================================================
# SECTION 3: CORRELATION ANALYSIS (SPEARMAN HEATMAP)
# =============================================================================

library(readxl)
library(metafor)
library(corrplot)
library(tidyverse)

df <- read_excel("Database.xlsx")

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

jpeg("Spearman_Correlation_Heatmap.jpg",
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
# =============================================================================

library(readxl)
library(metafor)
library(igraph)
library(tidyverse)

df <- read_excel("Database.xlsx")

# Re-use same 11-trait indicator list and effect-size function from Section 3
effect_data <- calculate_effect_sizes(df, indicators)

trait_cols <- colnames(effect_data)[-1]
n_traits   <- length(trait_cols)

M_spearman <- matrix(NA, nrow=n_traits, ncol=n_traits,
                     dimnames=list(trait_cols, trait_cols))

for (i in 1:n_traits) {
  for (j in 1:n_traits) {
    if (i == j) {
      M_spearman[i,j] <- 1
    } else {
      valid_data <- na.omit(effect_data[, c(trait_cols[i], trait_cols[j])])
      if (nrow(valid_data) >= 10) {
        var1 <- valid_data[[1]]; var2 <- valid_data[[2]]
        if (var(var1)>0 && var(var2)>0 &&
            length(unique(var1))>1 && length(unique(var2))>1) {
          M_spearman[i,j] <- cor.test(var1, var2, method="spearman")$estimate
        }
      }
    }
  }
}

cor_matrix <- M_spearman
cor_matrix[is.na(cor_matrix)] <- 0
diag(cor_matrix) <- 0

threshold  <- 0.3
adj_matrix <- ifelse(abs(cor_matrix) > threshold, abs(cor_matrix), 0)
diag(adj_matrix) <- 0
adj_matrix[is.na(adj_matrix)] <- 0

trait_network <- graph_from_adjacency_matrix(adj_matrix,
                                              mode="undirected", weighted=TRUE, diag=FALSE)
V(trait_network)$name <- colnames(cor_matrix)
edge_weights <- E(trait_network)$weight

if (length(edge_weights) > 0) {
  edge_weights[edge_weights <= 0] <- 0.001
  distance_weights <- 1 / (edge_weights + 0.001)
} else {
  distance_weights <- numeric(0)
}

if (length(edge_weights) > 0) {
  centrality_measures <- data.frame(
    Trait       = V(trait_network)$name,
    Degree      = degree(trait_network),
    Strength    = strength(trait_network, weights=edge_weights),
    Betweenness = betweenness(trait_network, weights=distance_weights),
    Closeness   = closeness(trait_network, weights=distance_weights),
    Eigenvector = eigen_centrality(trait_network)$vector,
    PageRank    = page_rank(trait_network)$vector
  )
} else {
  centrality_measures <- data.frame(
    Trait       = V(trait_network)$name,
    Degree      = rep(0, vcount(trait_network)),
    Strength    = rep(0, vcount(trait_network)),
    Betweenness = rep(0, vcount(trait_network)),
    Closeness   = rep(0, vcount(trait_network)),
    Eigenvector = rep(0, vcount(trait_network)),
    PageRank    = rep(0, vcount(trait_network))
  )
}

safe_normalize <- function(x) {
  if (all(is.na(x)) || diff(range(x, na.rm=TRUE)) == 0) return(rep(0.5, length(x)))
  (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
}

centrality_normalized <- centrality_measures
centrality_normalized[,-1] <- apply(centrality_measures[,-1], 2, safe_normalize)
centrality_normalized[is.na(centrality_normalized)] <- 0
centrality_normalized$Composite_Centrality <- rowMeans(
  centrality_normalized[, c("Strength","Betweenness","Eigenvector")], na.rm=TRUE
)

centrality_ranked <- centrality_normalized %>% arrange(desc(Composite_Centrality))
write.csv(centrality_ranked, "Functional_Trait_Centrality_Ranking_11.csv", row.names=FALSE)

discrete_colors <- c(Core="coral2", Important="#CA60AB",
                     General="#5B68AD", Peripheral="#4FB5A0")

centrality_ordered <- centrality_ranked[match(V(trait_network)$name, centrality_ranked$Trait), ]

centrality_categories_ordered <- cut(
  centrality_ordered$Composite_Centrality,
  breaks = c(0, 0.2, 0.4, 0.7, 1),
  labels = c("Peripheral","General","Important","Core"),
  include.lowest = TRUE
)

node_colors <- discrete_colors[as.character(centrality_categories_ordered)]
node_sizes  <- centrality_ordered$Composite_Centrality * 22 + 12

create_core_periphery_layout <- function(network, cats) {
  n            <- vcount(network)
  core_idx     <- which(as.character(cats) == "Core")
  non_core_idx <- which(as.character(cats) != "Core")
  layout       <- matrix(0, nrow=n, ncol=2)
  if (length(core_idx) > 0) {
    angles <- seq(0, 2*pi, length.out=length(core_idx)+1)[-1]
    layout[core_idx, 1] <- 0.45 * cos(angles)
    layout[core_idx, 2] <- 0.45 * sin(angles)
  }
  if (length(non_core_idx) > 0) {
    angles <- seq(0, 2*pi, length.out=length(non_core_idx)+1)[-1]
    layout[non_core_idx, 1] <- 1.4 * cos(angles)
    layout[non_core_idx, 2] <- 1.4 * sin(angles)
  }
  return(layout)
}

layout_net <- create_core_periphery_layout(trait_network, centrality_categories_ordered)

jpeg("Plant_Functional_Trait_Network_11.jpg",
     width=11, height=8, units="in", res=600, quality=100)
par(mar=c(1,1,2,1), bg="white")

plot_args <- list(
  trait_network,
  layout             = layout_net,
  vertex.size        = node_sizes,
  vertex.color       = node_colors,
  vertex.label.cex   = 1.0,
  vertex.label.color = "#2F4F4F",
  vertex.label.family= "sans",
  vertex.label.font  = 2,
  vertex.frame.color = "white",
  vertex.frame.width = 1,
  edge.color         = adjustcolor("#4682B4", alpha.f=0.5),
  edge.curved        = 0.15,
  xlim               = c(-1.6, 1.6),
  ylim               = c(-1.6, 1.6),
  main               = "Plant Functional Trait Network Under Grazing",
  cex.main           = 1.3,
  font.main          = 2,
  col.main           = "#2F4F4F",
  rescale            = FALSE
)

if (ecount(trait_network) > 0) {
  plot_args$edge.width <- E(trait_network)$weight * 4 + 0.5
} else {
  plot_args$edge.width <- 0
}

do.call(plot, plot_args)

legend("topleft",
       legend    = c("Core traits","Important traits","General traits","Peripheral traits"),
       pt.cex    = c(2.5, 2.0, 1.5, 1.0),
       pch       = 21,
       pt.bg     = c("#E18712","#CA60AB","#5B68AD","#4FB5A0"),
       col       = "black",
       bg        = "white",
       box.col   = "grey80",
       bty       = "o",
       cex       = 1.1,
       y.intersp = 1.4,
       inset     = c(0.01, 0.02))

dev.off()


# =============================================================================
# SECTION 5: PLASTICITY ANALYSIS (RDPI)
# =============================================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# RDPI = |Xt - Xc| / (Xt + Xc)
# Range: 0 (no plasticity) to 1 (maximum plasticity)
# Reference: Valladares et al. (2006) Journal of Ecology, 94: 1103-1116

data <- read_excel("Database.xlsx")

traits_abbr <- c(
  "Height","AGB","LL","LW","LA","SLA",
  "LN","LT","LB","SD","SB","SLR",
  "BGB","RDen","RSA","RL","SRL",
  "RD","RV","R:S"
)

rdpi_data <- data.frame(Study_ID=data$ID, Author=data$Author, Year=data$Year)

for (i in 1:20) {
  Xc <- data[[paste0("Xc", i)]]
  Xt <- data[[paste0("Xt", i)]]
  RDPI <- rep(NA, nrow(data))
  valid <- !is.na(Xc) & !is.na(Xt) & Xc > 0 & Xt > 0
  if (sum(valid) > 0) RDPI[valid] <- abs(Xt[valid]-Xc[valid]) / (Xt[valid]+Xc[valid])
  rdpi_data[[traits_abbr[i]]] <- RDPI
}

write.csv(rdpi_data, "RDPI_raw_data.csv", row.names=FALSE, fileEncoding="UTF-8")

rdpi_summary <- data.frame(
  Trait      = traits_abbr,
  RDPI_mean  = NA, RDPI_median = NA, RDPI_sd  = NA,
  RDPI_se    = NA, RDPI_min    = NA, RDPI_max  = NA,
  n_studies  = NA
)

for (i in 1:20) {
  vals <- rdpi_data[[traits_abbr[i]]]
  vals <- vals[!is.na(vals)]
  n    <- length(vals)
  rdpi_summary$n_studies[i]  <- n
  if (n > 0) {
    rdpi_summary$RDPI_mean[i]   <- mean(vals)
    rdpi_summary$RDPI_median[i] <- median(vals)
    rdpi_summary$RDPI_sd[i]     <- sd(vals)
    rdpi_summary$RDPI_se[i]     <- sd(vals) / sqrt(n)
    rdpi_summary$RDPI_min[i]    <- min(vals)
    rdpi_summary$RDPI_max[i]    <- max(vals)
  }
}

rdpi_summary <- rdpi_summary %>%
  arrange(desc(RDPI_mean)) %>%
  mutate(
    Rank = 1:n(),
    Plasticity_Level = case_when(
      RDPI_mean < 0.2                      ~ "Low",
      RDPI_mean >= 0.2 & RDPI_mean < 0.5  ~ "Moderate",
      RDPI_mean >= 0.5                     ~ "High",
      TRUE ~ NA_character_
    )
  )

write.csv(rdpi_summary, "RDPI_summary.csv", row.names=FALSE, fileEncoding="UTF-8")

color_plasticity <- c("High"="#0072B2", "Moderate"="#56B4E9", "Low"="#E8F4F8")

rdpi_plot <- ggplot(
  rdpi_summary %>% filter(!is.na(Plasticity_Level)),
  aes(x=reorder(Trait,RDPI_mean), y=RDPI_mean, fill=Plasticity_Level)
) +
  geom_bar(stat="identity", width=0.7, color="white", size=0.5) +
  geom_errorbar(aes(ymin=RDPI_mean-RDPI_se, ymax=RDPI_mean+RDPI_se),
                width=0.3, size=0.6, color="black") +
  geom_hline(yintercept=0.2, linetype="dashed", color="black", size=0.8) +
  geom_hline(yintercept=0.5, linetype="dashed", color="black", size=0.8) +
  geom_text(aes(label=sprintf("%.3f",RDPI_mean), y=RDPI_mean+RDPI_se+0.01),
            hjust=0, size=3.5, color="black") +
  scale_fill_manual(values=color_plasticity, name="Plasticity Level",
                    breaks=c("High","Moderate","Low")) +
  coord_flip() +
  labs(title="Phenotypic Plasticity Index (RDPI) by Plasticity Level",
       x=NULL, y="RDPI") +
  scale_y_continuous(limits=c(0, max(rdpi_summary$RDPI_mean+rdpi_summary$RDPI_se,
                                     na.rm=TRUE)*1.25)) +
  theme_bw(base_size=14) +
  theme(
    plot.title      = element_text(hjust=0.5, face="bold", size=16),
    axis.text       = element_text(color="black", size=12),
    axis.text.y     = element_text(size=11),
    axis.title      = element_text(face="bold", size=14),
    legend.position = "right",
    legend.title    = element_text(face="bold", size=12),
    legend.text     = element_text(size=11),
    panel.grid.major= element_line(color="gray90", size=0.3),
    panel.grid.minor= element_blank(),
    plot.margin     = margin(10,10,10,10)
  )

pdf("RDPI_Plasticity_Ranking.pdf", width=10, height=8)
print(rdpi_plot)
dev.off()

jpeg("RDPI_Plasticity_Ranking.jpg",
     width=10, height=8, units="in", res=600, quality=100)
print(rdpi_plot)
dev.off()


# =============================================================================
# SECTION 6: RANDOM FOREST MODELS
# =============================================================================

library(readxl)
library(dplyr)
library(metafor)
library(mice)
library(rfPermute)
library(ggplot2)
library(patchwork)

mydata <- read_excel("Database.xlsx", sheet=1)
stopifnot("Data read failure: zero rows" = nrow(mydata) > 0)

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

mydata <- mydata %>%
  mutate(
    duration_num = suppressWarnings(as.numeric(duration_raw)),
    intensity_f  = factor(intensity, levels=c("LG","MG","HG","FG"), ordered=TRUE),
    livestock_f  = factor(livestock),
    AI           = suppressWarnings(as.numeric(AI)),
    lat          = suppressWarnings(as.numeric(lat)),
    elev         = suppressWarnings(as.numeric(elev))
  )

predictors_base <- mydata %>%
  select(lat, elev, AI, livestock_f, duration_num, intensity_f) %>%
  as.data.frame()

stopifnot("Predictor matrix has zero rows"    = nrow(predictors_base) > 0)
stopifnot("Predictor matrix column count < 6" = ncol(predictors_base) == 6)

miss_pct <- round(colSums(is.na(predictors_base)) / nrow(predictors_base) * 100, 1)
all_na_cols <- names(miss_pct[miss_pct == 100])
if (length(all_na_cols) > 0)
  stop(paste("Columns fully missing — check column indices:",
             paste(all_na_cols, collapse=", ")))

set.seed(2024)
has_missing <- any(miss_pct > 0)
if (has_missing) {
  imp_global   <- mice(predictors_base, method="rf", m=1, maxit=10, printFlag=FALSE)
  pred_imputed <- complete(imp_global, 1)
} else {
  pred_imputed <- predictors_base
}
stopifnot("Imputation left residual missing values"   = sum(is.na(pred_imputed)) == 0)
stopifnot("Imputed row count does not match raw data" = nrow(pred_imputed) == nrow(mydata))

rf_indicators <- list(
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

all_cols_needed   <- unique(unlist(lapply(rf_indicators, unlist)))
missing_in_data   <- all_cols_needed[!all_cols_needed %in% colnames(mydata)]
if (length(missing_in_data) > 0)
  stop(paste("Columns missing from data:", paste(missing_in_data, collapse=", ")))

predictor_labels <- c(
  "lat"          = "Latitude",
  "elev"         = "Elevation",
  "AI"           = "Aridity Index",
  "livestock_f"  = "Livestock Type",
  "duration_num" = "Grazing Duration",
  "intensity_f"  = "Grazing Intensity"
)

plot_rf_importance <- function(imp_df, trait_label, oob_r2) {
  imp_df <- imp_df %>%
    arrange(Importance) %>%
    mutate(Predictor=factor(Predictor, levels=Predictor),
           bar_color=ifelse(pval<0.05,"#5b9bd5","#b0b0b0"))
  x_max <- max(imp_df$Importance, na.rm=TRUE) * 1.35
  ggplot(imp_df, aes(x=Predictor, y=Importance, fill=bar_color)) +
    geom_col(width=0.6, color=NA) +
    geom_text(aes(label=sig, y=Importance), hjust=-0.15, size=4.5,
              color="black", fontface="bold") +
    scale_fill_identity() +
    scale_y_continuous(expand=c(0,0), limits=c(0,x_max)) +
    coord_flip() +
    labs(title=trait_label, subtitle=paste0("OOB R² = ",round(oob_r2,3)),
         x=NULL, y="Increase in MSE (%)") +
    theme_bw() +
    theme(
      plot.title      = element_text(hjust=0.5, size=12, face="bold"),
      plot.subtitle   = element_text(hjust=0.5, size=9, color="grey40"),
      axis.text.y     = element_text(size=9, color="black"),
      axis.ticks.y    = element_line(color="black"),
      axis.text.x     = element_text(size=8),
      axis.title.x    = element_text(size=9, face="bold"),
      panel.grid      = element_blank(),
      axis.line       = element_line(colour="black"),
      legend.position = "none",
      plot.margin     = unit(c(4,10,4,4),"pt")
    )
}

all_results <- list()
trait_names <- names(rf_indicators)

for (i in seq_along(rf_indicators)) {

  label <- trait_names[i]
  cols  <- rf_indicators[[label]]

  needed       <- unlist(cols)
  missing_cols <- needed[!needed %in% colnames(mydata)]
  if (length(missing_cols) > 0) { next }

  trait_cols <- mydata %>%
    select(Xc=all_of(cols$Xc), SDc=all_of(cols$SDc), Nc=all_of(cols$Nc),
           Xt=all_of(cols$Xt), SDt=all_of(cols$SDt), Nt=all_of(cols$Nt)) %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

  eff <- tryCatch(
    escalc(measure="ROM",
           m1i=Xt, sd1i=SDt, n1i=Nt,
           m2i=Xc, sd2i=SDc, n2i=Nc,
           data=trait_cols),
    error=function(e) NULL
  )
  if (is.null(eff)) { next }

  valid_idx <- which(!is.na(eff$yi) & !is.na(eff$vi))
  n_valid   <- length(valid_idx)
  if (n_valid < 30) { next }

  df_trait <- pred_imputed[valid_idx, ] %>%
    mutate(lnRR=as.numeric(eff$yi[valid_idx]))
  if (any(is.na(df_trait))) { next }

  set.seed(2024)
  rf_fit <- tryCatch(
    rfPermute(lnRR ~ ., data=df_trait, ntree=1000, nrep=999, num.cores=1),
    error=function(e) NULL
  )
  if (is.null(rf_fit)) { next }

  oob_r2   <- rf_fit$rf$rsq[1000]
  imp_scaled <- data.frame(importance(rf_fit, scale=TRUE), check.names=FALSE)
  pval_mat   <- rf_fit$pval[,,2]

  imp_df <- data.frame(
    Predictor  = rownames(imp_scaled),
    Importance = imp_scaled$`%IncMSE`,
    pval       = pval_mat[rownames(imp_scaled), "%IncMSE"]
  ) %>%
    mutate(
      sig = case_when(
        pval >= 0.05                 ~ "",
        pval >= 0.01 & pval < 0.05  ~ "*",
        pval >= 0.001 & pval < 0.01 ~ "**",
        pval < 0.001                 ~ "***"
      ),
      Predictor = dplyr::recode(Predictor, !!!predictor_labels),
      Trait     = label,
      OOB_R2    = oob_r2
    )

  all_results[[label]] <- imp_df

  p     <- plot_rf_importance(imp_df, label, oob_r2)
  fname <- sprintf("RF_Importance_%02d_%s", i, gsub("[^A-Za-z0-9]","_",label))
  ggsave(paste0(fname,".jpeg"), plot=p, device="jpeg", width=8, height=6, dpi=600)
  ggsave(paste0(fname,".pdf"),  plot=p, width=8, height=6)
}

if (length(all_results) > 0) {
  summary_df <- bind_rows(all_results)
  write.csv(summary_df, "RF_all_traits_importance.csv", row.names=FALSE)

  oob_tbl <- summary_df %>%
    group_by(Trait) %>%
    summarise(OOB_R2=round(unique(OOB_R2),3), .groups="drop") %>%
    arrange(desc(OOB_R2))
  print(oob_tbl)
}

# --- Combined panel plots ---

forest_order <- c(
  "Height","Aboveground Biomass","Leaf Length","Leaf Width",
  "Leaf Area","Leaf Thickness","Leaf Number","Leaf Biomass",
  "Stem Diameter","Stem Biomass","Belowground Biomass",
  "Root Diameter","Root Length","Root Surface Area",
  "Root Volume","Root Density","Specific Leaf Area",
  "Specific Root Length","Stem Leaf Ratio","Root Shoot Ratio"
)

oob_lookup <- summary_df %>%
  group_by(Trait) %>%
  summarise(OOB_R2=unique(OOB_R2), .groups="drop")

trait_order_df <- data.frame(Trait=forest_order, plot_order=seq_along(forest_order)) %>%
  left_join(oob_lookup, by="Trait") %>%
  filter(!is.na(OOB_R2)) %>%
  mutate(group=ifelse(OOB_R2>=0.1,"high","low"))

high_traits <- trait_order_df %>% filter(group=="high") %>% pull(Trait)
low_traits  <- trait_order_df %>% filter(group=="low")  %>% pull(Trait)

make_single_plot <- function(trait_name) {
  imp_df <- all_results[[trait_name]]
  if (is.null(imp_df)) return(NULL)
  oob_r2 <- unique(imp_df$OOB_R2)
  imp_df <- imp_df %>%
    arrange(Importance) %>%
    mutate(Predictor=factor(Predictor,levels=Predictor),
           bar_color=ifelse(pval<0.05,"#5b9bd5","#b0b0b0"))
  x_max <- max(imp_df$Importance,na.rm=TRUE)*1.35
  ggplot(imp_df,aes(x=Predictor,y=Importance,fill=bar_color)) +
    geom_col(width=0.6,color=NA) +
    geom_text(aes(label=sig,y=Importance),hjust=-0.15,size=4.5,
              color="black",fontface="bold") +
    scale_fill_identity() +
    scale_y_continuous(expand=c(0,0),limits=c(0,x_max)) +
    coord_flip() +
    labs(title=trait_name, subtitle=paste0("R² = ",round(oob_r2,3)),
         x=NULL, y="Increase in MSE (%)") +
    theme_bw() +
    theme(
      plot.title=element_text(hjust=0.5,size=12,face="bold"),
      plot.subtitle=element_text(hjust=0.5,size=9,color="grey40"),
      axis.text.y=element_text(size=9,color="black"),
      axis.ticks.y=element_line(color="black"),
      axis.text.x=element_text(size=8),
      axis.title.x=element_text(size=9,face="bold"),
      panel.grid=element_blank(),
      axis.line=element_line(colour="black"),
      legend.position="none",
      plot.margin=unit(c(4,10,4,4),"pt")
    )
}

combine_plots <- function(trait_list, title_text, ncols=3) {
  plots <- Filter(Negate(is.null), lapply(trait_list, make_single_plot))
  if (length(plots)==0) return(NULL)
  wrap_plots(plots,ncol=ncols) +
    plot_annotation(title=title_text,
                    theme=theme(plot.title=element_text(hjust=0.5,size=14,face="bold")))
}

p_high     <- combine_plots(high_traits, "Random Forest Variable Importance (OOB R² ≥ 0.1)", ncols=3)
nrows_high <- ceiling(length(high_traits)/3)
h_high     <- nrows_high*4.5 + 0.8
ggsave("RF_combined_high_R2.jpeg", plot=p_high, device="jpeg", width=15, height=h_high, dpi=600)
ggsave("RF_combined_high_R2.pdf",  plot=p_high, width=15, height=h_high)

p_low     <- combine_plots(low_traits, "Random Forest Variable Importance (OOB R² < 0.1)", ncols=3)
nrows_low <- ceiling(length(low_traits)/3)
h_low     <- nrows_low*4.5 + 0.8
ggsave("RF_combined_low_R2.jpeg", plot=p_low, device="jpeg", width=15, height=h_low, dpi=600)
ggsave("RF_combined_low_R2.pdf",  plot=p_low, width=15, height=h_low)

# --- Heatmap ---

y_label_df <- data.frame(Trait=forest_order) %>%
  left_join(oob_lookup,by="Trait") %>%
  mutate(y_label=ifelse(is.na(OOB_R2),Trait,
                        paste0(Trait," (R²=",round(OOB_R2,3),")")))
label_map <- setNames(y_label_df$y_label, y_label_df$Trait)

heatmap_df <- summary_df %>%
  filter(Trait %in% forest_order) %>%
  mutate(
    Trait = factor(Trait, levels=rev(forest_order)),
    sig_label = case_when(
      pval >= 0.05                 ~ "",
      pval >= 0.01 & pval < 0.05  ~ "*",
      pval >= 0.001 & pval < 0.01 ~ "**",
      pval < 0.001                 ~ "***"
    )
  )

pred_order <- heatmap_df %>%
  group_by(Predictor) %>%
  summarise(mean_imp=mean(Importance,na.rm=TRUE),.groups="drop") %>%
  arrange(desc(mean_imp)) %>% pull(Predictor)

heatmap_df <- heatmap_df %>% mutate(Predictor=factor(Predictor,levels=pred_order))

p_heat <- ggplot(heatmap_df, aes(x=Predictor,y=Trait,fill=Importance)) +
  geom_tile(color="white", linewidth=0.6) +
  geom_text(aes(label=sig_label), size=4.5, color="white", fontface="bold") +
  scale_fill_gradient2(
    low="#eef3ff", mid="#5b9bd5", high="#1a3a8f",
    midpoint=median(heatmap_df$Importance,na.rm=TRUE),
    name="Increase\nin MSE (%)", na.value="grey90"
  ) +
  scale_y_discrete(labels=label_map) +
  scale_x_discrete(expand=expansion(add=c(0.5,0.5))) +
  labs(title="Random Forest Variable Importance Across Functional Traits",
       x=NULL, y=NULL) +
  theme_bw(base_size=12) +
  theme(
    plot.title=element_text(hjust=0.5,size=13,face="bold"),
    axis.text.y=element_text(size=10,color="black"),
    axis.text.x=element_text(size=11,angle=30,hjust=1,face="bold"),
    legend.position="right",
    panel.grid=element_blank(),
    plot.margin=unit(c(10,10,10,10),"pt")
  )

ggsave("RF_heatmap_all_traits.jpeg", plot=p_heat, device="jpeg", width=12, height=10, dpi=600)
ggsave("RF_heatmap_all_traits.pdf",  plot=p_heat, width=12, height=10)
