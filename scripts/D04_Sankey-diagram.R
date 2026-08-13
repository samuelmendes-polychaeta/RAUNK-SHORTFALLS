#' =============================================================================
#' Author:  Samuel Lucas da S. Delgado Mendes, Paulo Cesar de Paiva,
#'          Rodolfo L. Nascimento
#' Subject: REVIEW
#' Journal: DECIDING
#' Analysis: Sankey diagram
#' =============================================================================


# =============================================================================
# 1. PACKAGES
# =============================================================================

{
  required.packages <- c(
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
    "networkD3",
    "magrittr",
    "stringr",
    "viridis"
  )
  
  # Install missing packages
  missing.packages <- required.packages[
    !required.packages %in% rownames(installed.packages())
  ]
  
  if (length(missing.packages) > 0) {
    install.packages(
      missing.packages,
      dependencies = TRUE
    )
  }
  
  # Load packages
  lapply(
    required.packages,
    require,
    character.only = TRUE
  )
}


# =============================================================================
# 2. IMPORT DATA
# =============================================================================

data <- read.xlsx(
  here("data", "data.xlsx")
)


# =============================================================================
# 3. DATA PREPARATION
# =============================================================================

# Variables containing multiple categories
# separated by commas

sankey_expanded_data <- data %>%
  
  mutate(
    across(
      c(DB, TT, OPP, TDB, OC, TN),
      as.character
    )
  ) %>%
  
  separate_rows(
    DB,
    sep = ",\\s*"
  ) %>%
  
  separate_rows(
    TT,
    sep = ",\\s*"
  ) %>%
  
  separate_rows(
    OPP,
    sep = ",\\s*"
  ) %>%
  
  separate_rows(
    TDB,
    sep = ",\\s*"
  ) %>%
  
  separate_rows(
    OC,
    sep = ",\\s*"
  ) %>%
  
  separate_rows(
    TN,
    sep = ",\\s*"
  ) %>%
  
  mutate(
    across(
      everything(),
      str_trim
    )
  )


# =============================================================================
# 4. ORDER OF SANKEY AXES
# =============================================================================

flow_axes <- c(
  "OC",
  "TDB",
  "TN",
  "SA",
  "FOC",
  "OPP"
)


# =============================================================================
# 5. FILTER TOP CATEGORIES
# =============================================================================

sankey_data_top <- sankey_expanded_data %>%
  
  filter(
    if_all(
      all_of(flow_axes),
      ~ .x %in% names(
        sort(
          table(.x),
          decreasing = TRUE
        )[1:3]
      )
    )
  ) %>%
  
  count(
    across(
      all_of(flow_axes)
    )
  )


# =============================================================================
# 6. PREPARE DATA
# =============================================================================

sankey_data_long <- sankey_data_top %>%
  mutate(
    across(
      everything(),
      as.character
    )
  )


# =============================================================================
# 7. CREATE LINKS
# =============================================================================

links_list <- list()

for (i in seq_along(flow_axes)[-length(flow_axes)]) {
  
  source_column <- flow_axes[i]
  target_column <- flow_axes[i + 1]
  
  temp_links <- sankey_data_long %>%
    
    group_by(
      OC,
      source = !!sym(source_column),
      target = !!sym(target_column)
    ) %>%
    
    summarise(
      value = sum(as.numeric(n)),
      .groups = "drop"
    )
  
  links_list[[i]] <- temp_links
}

links <- bind_rows(links_list)


# =============================================================================
# 8. CREATE NODES
# =============================================================================

nodes <- data.frame(
  name = unique(
    c(
      links$source,
      links$target
    )
  ),
  group = "neutral_node"
)


# =============================================================================
# 9. CREATE NUMERIC IDs
# =============================================================================

links$source_id <- match(
  links$source,
  nodes$name
) - 1

links$target_id <- match(
  links$target,
  nodes$name
) - 1


# Links coloured according to OC
links$group <- links$OC


# =============================================================================
# 10. COLOUR PALETTE
# =============================================================================

# Number of OC categories
number_of_categories <- length(
  unique(links$OC)
)


# Mako palette
mako_colours <- viridis_pal(
  option = "mako",
  begin = 0.4,
  end = 0.9
)(
  number_of_categories
)


# Add neutral grey for nodes
final_colours <- c(
  mako_colours,
  "#cccccc"
)


# Categories used in the JavaScript colour scale
oc_categories <- unique(
  links$OC
)

javascript_domains <- c(
  oc_categories,
  "neutral_node"
)


# =============================================================================
# 11. NETWORKD3 COLOUR SCALE
# =============================================================================

colour_scale <- JS(
  paste0(
    'd3.scaleOrdinal()',
    '.domain(["',
    paste(
      javascript_domains,
      collapse = '","'
    ),
    '"])',
    '.range(["',
    paste(
      final_colours,
      collapse = '","'
    ),
    '"])'
  )
)


# Ensure groups are correctly defined
links$group <- links$OC

nodes$group <- "neutral_node"


# =============================================================================
# 12. SANKEY DIAGRAM
# =============================================================================

sankey_interactive_mako <- sankeyNetwork(
  
  Links = links,
  Nodes = nodes,
  
  Source = "source_id",
  Target = "target_id",
  Value = "value",
  NodeID = "name",
  
  NodeGroup = "group",
  LinkGroup = "group",
  
  colourScale = colour_scale,
  
  sinksRight = TRUE,
  
  nodeWidth = 25,
  fontSize = 12,
  nodePadding = 15
)


# =============================================================================
# 13. VISUALIZE
# =============================================================================

sankey_interactive_mako
