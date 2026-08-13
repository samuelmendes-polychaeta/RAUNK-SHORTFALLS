#' =============================================================================
#  Descriptive information and exploration of data
#' =============================================================================


# =============================================================================
# 1. SETUP & PACKAGES
# =============================================================================

{
  # Package repository and configuration
  lib <- .libPaths()[1]
  
  required_packages <- c(
    "vegan",
    "FD",
    "dplyr",
    "writexl",
    "openxlsx",
    "readxl",
    "here",
    "tidyr",
    "tidyverse",
    "car",
    "tidyselect",
    "tidygraph",
    "ggeffects",
    "patchwork",
    "MuMIn",
    "DHARMa",
    "adiv",
    "adegraphics",
    "PERMANOVA",
    "gridExtra",
    "FactoMineR",
    "ggalluvial",
    "factoextra",
    "jtools",
    "picante",
    "bibliometrix",
    "RColorBrewer",
    "scales",
    "ggrepel",
    "viridis"
  )
  
  # Install missing packages
  missing_pkgs <- required_packages[
    !(required_packages %in% installed.packages()[, "Package"])
  ]
  
  if (length(missing_pkgs)) {
    install.packages(
      missing_pkgs,
      dependencies = TRUE,
      lib = lib
    )
  }
  
  # Load packages
  lapply(
    required_packages,
    require,
    character.only = TRUE
  )
}


# =============================================================================
# 2. DATA IMPORT
# =============================================================================

data <- read.xlsx(
  here("data", "data.xlsx")
)

traits <- read.xlsx(
  here("data", "data.xlsx"),
  sheet = "traits"
)


# =============================================================================
# 3. DATA PREPARATION
# =============================================================================


# -----------------------------------------------------------------------------
# 3.1 Traits (Long Format)
# -----------------------------------------------------------------------------

traits_long <- traits %>%
  pivot_longer(
    cols = -c(1:6),
    names_to = "trait",
    values_to = "presence"
  )

trait_frequency <- traits_long %>%
  filter(presence == 1) %>%
  count(trait) %>%
  mutate(
    relative_frequency = n / sum(n)
  )


# -----------------------------------------------------------------------------
# 3.2 Study Area Frequency by Year
# -----------------------------------------------------------------------------

study_area_year_frequency <- data %>%
  count(
    PY,
    SA
  ) %>%
  group_by(PY) %>%
  mutate(
    relative_frequency = n / sum(n)
  )


# -----------------------------------------------------------------------------
# 3.3 Focus (Top 10 FOC)
# -----------------------------------------------------------------------------

top10_focus <- data %>%
  count(
    FOC,
    sort = TRUE
  ) %>%
  slice_head(n = 10) %>%
  pull(FOC)

focus_year_frequency <- data %>%
  mutate(
    FOC = if_else(
      FOC %in% top10_focus,
      FOC,
      "Others"
    )
  ) %>%
  count(
    PY,
    FOC
  ) %>%
  group_by(PY) %>%
  mutate(
    relative_frequency = n / sum(n)
  )


# -----------------------------------------------------------------------------
# 3.4 Database (TDB)
# -----------------------------------------------------------------------------

database_long <- data %>%
  separate_rows(
    TDB,
    sep = ","
  ) %>%
  mutate(
    TDB = str_trim(TDB)
  )


# Top 15 most frequently consulted databases
top_databases <- database_long %>%
  count(
    TDB,
    sort = TRUE
  ) %>%
  slice_head(n = 15) %>%
  pull(TDB)


# Total frequency of database use
total_database_frequency <- database_long %>%
  count(
    TDB,
    sort = TRUE
  )



database_frequency <- database_long %>%
  mutate(
    TDB = if_else(
      TDB %in% top_databases,
      TDB,
      "Others"
    )
  ) %>%
  count(
    PY,
    TDB
  ) %>%
  group_by(PY) %>%
  mutate(
    relative_frequency = n / sum(n),
    TDB = factor(
      TDB,
      levels = c(
        top_databases,
        "Others"
      )
    )
  )


# -----------------------------------------------------------------------------
# 3.5 Methodology (Study Approach)
# -----------------------------------------------------------------------------

methodology_data <- data %>%
  mutate(
    methodology = if_else(
      str_detect(TT, "D"),
      "Direct observation",
      "Literature consult"
    )
  ) %>%
  group_by(
    PY,
    methodology
  ) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )


# =============================================================================
# 4. MAIN PLOTS
# =============================================================================


# -----------------------------------------------------------------------------
# P3: Traits
# -----------------------------------------------------------------------------

p3 <- ggplot(
  trait_frequency,
  aes(
    x = reorder(trait, relative_frequency),
    y = relative_frequency
  )
) +
  geom_col(
    fill = "darkgray",
    color = "black",
    width = 0.7
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    title = "Trait Frequency Distribution",
    x = "Trait",
    y = "Relative frequency"
  ) +
  theme_classic()

p3


# -----------------------------------------------------------------------------
# P4: Study Area
# -----------------------------------------------------------------------------

p4 <- ggplot(
  study_area_year_frequency,
  aes(
    x = factor(PY),
    y = relative_frequency,
    fill = SA
  )
) +
  geom_col(
    color = "black",
    linewidth = 0.3
  ) +
  scale_fill_viridis_d(
    option = "magma"
  ) +
  scale_y_continuous(
    labels = percent,
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    x = "Year",
    y = "Relative frequency",
    fill = "Study area"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5
    ),
    legend.position = "bottom"
  )

p4


# -----------------------------------------------------------------------------
# P5: Focus (FOC)
# -----------------------------------------------------------------------------

p5 <- ggplot(
  focus_year_frequency,
  aes(
    x = factor(PY),
    y = relative_frequency,
    fill = FOC
  )
) +
  geom_col(
    color = "black",
    linewidth = 0.3
  ) +
  scale_fill_viridis_d(
    option = "mako",
    begin = 0.3,
    end = 1
  ) +
  scale_y_continuous(
    labels = percent,
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    x = "Year",
    y = "Relative frequency",
    fill = "Focus"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5
    ),
    legend.position = "bottom"
  )

p5


# -----------------------------------------------------------------------------
# P6: Methodology
# -----------------------------------------------------------------------------

p6 <- ggplot(
  methodology_data,
  aes(
    x = factor(PY),
    y = n,
    fill = methodology
  )
) +
  geom_bar(
    stat = "identity",
    position = "fill",
    color = "black",
    linewidth = 0.3
  ) +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  scale_fill_manual(
    values = c(
      "Literature consult" = "#4F6D7A",
      "Direct observation" = "#C0D6DF"
    )
  ) +
  labs(
    x = "Year",
    y = "Relative frequency",
    fill = "Study approach"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5
    ),
    legend.position = "bottom"
  )

p6

#Literature consult = PotentialL trait information
#Direct observation = Realized trait information

# -----------------------------------------------------------------------------
# P7: Database
# -----------------------------------------------------------------------------

p7 <- ggplot(
  database_frequency,
  aes(
    x = factor(PY),
    y = relative_frequency,
    fill = TDB
  )
) +
  geom_col(
    color = "black",
    linewidth = 0.4
  ) +
  scale_fill_viridis_d(
    option = "magma",
    begin = 1,
    end = 0
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    x = "Year",
    y = "Relative frequency",
    fill = "Database"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5
    ),
    legend.position = "bottom"
  )

p7


# =============================================================================
# 5. DONUT PLOTS: OTHER RELEVANT INFORMATION
# =============================================================================


# -----------------------------------------------------------------------------
# 5.1 Donut Plot Function
# -----------------------------------------------------------------------------

make_donut <- function(df, var, title_label) {
  
  df_sum <- df %>%
    count(
      label = as.factor({{ var }})
    ) %>%
    mutate(
      prop = n / sum(n),
      ymax = cumsum(prop),
      ymin = lag(
        ymax,
        default = 0
      ),
      label_pos = (ymax + ymin) / 2,
      label_text = paste0(
        label,
        "\n(",
        percent(prop, accuracy = 1),
        ")"
      )
    )
  
  ggplot(
    df_sum,
    aes(
      ymax = ymax,
      ymin = ymin,
      xmax = 4,
      xmin = 2.2,
      fill = label
    )
  ) +
    
    geom_rect(
      color = "white",
      linewidth = 0.7
    ) +
    
    scale_fill_viridis_d(
      option = "magma",
      guide = "none"
    ) +
    
    coord_polar(
      theta = "y"
    ) +
    
    xlim(
      1,
      6
    ) +
    
    geom_text_repel(
      aes(
        x = 4.2,
        y = label_pos,
        label = label_text
      ),
      nudge_x = 0.8,
      segment.size = 0.4,
      segment.color = "grey40",
      direction = "y",
      size = 3.3,
      fontface = "bold",
      lineheight = 0.85
    ) +
    
    annotate(
      "text",
      x = 1,
      y = 0,
      label = title_label,
      size = 4.5,
      fontface = "bold",
      color = "grey20"
    ) +
    
    theme_void() +
    
    theme(
      plot.margin = margin(
        10,
        10,
        10,
        10
      )
    )
}


# -----------------------------------------------------------------------------
# 5.2 Create Donut Plots
# -----------------------------------------------------------------------------

p1 <- make_donut(
  data,
  CP,
  "Crss-phyla Scope"
)

p2 <- make_donut(
  data,
  Momentum.acessed,
  "Momentum"
)

p3_donut <- make_donut(
  data,
  SB,
  "Substrate type"
)

p4_donut <- make_donut(
  data,
  ZN,
  "Environment"
)


# -----------------------------------------------------------------------------
# 5.3 Combine Donut Plots
# -----------------------------------------------------------------------------

donut_panel <- (p1 + p2) / 
  (p3_donut + p4_donut) +
  
  plot_annotation(
    tag_levels = "A",
    tag_suffix = ")"
  ) &
  
  theme(
    plot.tag = element_text(
      face = "bold",
      size = 12
    )
  )


# Display donut panel
donut_panel


