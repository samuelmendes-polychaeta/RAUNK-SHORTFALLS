# ---------------------------------------------------------
# 1. Loading pakages
# ---------------------------------------------------------

{
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  vegan, FD, tidyverse, openxlsx, here, ggrepel, patchwork, gridExtra, zoo
)
}

# ---------------------------------------------------------
# 2. Import data
# ---------------------------------------------------------
data_raw   <- read.xlsx(here("data", "data.xlsx"), sheet = "data")
traits_raw <- read.xlsx(here("data", "data.xlsx"), sheet = "traits")

# ---------------------------------------------------------
# 3. Prepare data
# ---------------------------------------------------------

traits_sp <- traits_raw %>%
  separate_rows(taxa, sep = ",\\s*") %>%
  filter(
    str_detect(taxa, "^[A-Z][a-z]+ [a-z]+$"), 
    !str_detect(taxa, "ae$|sp$|spp$")
  ) %>%
  # Tapp column as reference 
  mutate(Tapp = as.factor(Tapp))


# Top 4 traits (but you can write the name of any trait here)
top_traits <- c("Feeding.Mode", "Size", "Motility", "Bioturbation")


colors_tapp <- c("Direct" = "#C0D6DF", "Literature consult" = "#4F6D7A")

# 3. SAC FUNCTION --------------------------------------------------------------
get_sac <- function(df, attr_name, method = "random", perms = 1000) {
  df_sub <- df %>% filter(!!sym(attr_name) == 1)
  
  if(length(unique(df_sub$DOI)) < 3) return(NULL)
  
  if(method == "collector") {
    df_sub <- df_sub %>% arrange(PY, DOI)
  }
  
  matriz <- df_sub %>%
    distinct(DOI, taxa, .keep_all = TRUE) %>%
    mutate(presenca = 1) %>%
    pivot_wider(
      id_cols = any_of(c("DOI", "PY")),
      names_from = taxa, 
      values_from = presenca, 
      values_fill = 0
    ) %>%
    dplyr::select(-any_of(c("DOI", "PY")))
  
  sac <- specaccum(matriz, method = method, permutations = perms)
  
  res <- data.frame(
    Effort = sac$sites, 
    Richness = sac$richness, 
    SD = if(method == "random") sac$sd else 0,
    Method = method
  )
  
  if(method == "collector") {
    years <- df_sub %>% distinct(DOI, .keep_all = TRUE) %>% pull(PY)
    res$PY <- years[1:nrow(res)]
  }
  
  return(res)
}

# 4. PLOTTING FUNCTION ---------------------------------------------------------
plot_sac <- function(df_dir, df_lit, attr_label, method) {
  df_plot <- bind_rows(
    mutate(df_dir, Strategy = "Direct"),
    mutate(df_lit, Strategy = "Literature consult")
  )
  
  p <- ggplot(df_plot, aes(x = Effort, y = Richness, color = Strategy, fill = Strategy))
  
  if(method == "random") {
    p <- p + geom_ribbon(aes(ymin = Richness - SD, ymax = Richness + SD), alpha = 0.1, color = NA)
  }
  
  p <- p + 
    geom_line(linewidth = 1) +
    labs(subtitle = paste(attr_label),
         x = "Number of papers", y = "Cumulative Richness (S)") +
    theme_classic() +
    scale_color_manual(values = colors_tapp) +
    scale_fill_manual(values = colors_tapp) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(face = "bold"))
  
  if(method == "collector") {
    df_labels <- df_plot %>%
      group_by(Strategy) %>%
      filter(PY %% 5 == 0 | row_number() == n()) %>%
      distinct(Strategy, PY, .keep_all = TRUE)
    
    p <- p + geom_text_repel(
      data = df_labels, aes(label = PY), size = 2.8,
      nudge_y = 5, segment.color = "grey50", segment.size = 0.2
    ) +
      geom_point(data = df_labels, size = 1, alpha = 0.6)
  }
  return(p)
}

# 5. GENERATION WORKFLOW -------------------------------------------------------
generate_all_plots <- function(method) {
  plot_list <- list()
  
  data_dir <- traits_sp %>% filter(Tapp == "Direct")
  data_lit <- traits_sp %>% filter(Tapp == "Literature consult")
  
  for(attr in top_traits) {
    sac_dir <- get_sac(data_dir, attr, method = method)
    sac_lit <- get_sac(data_lit, attr, method = method)
    
    if(!is.null(sac_dir) & !is.null(sac_lit)) {
      plot_list[[attr]] <- plot_sac(sac_dir, sac_lit, attr, method)
    }
  }
  return(plot_list)
}

# Loadin plates
random_plots <- generate_all_plots("random")

plate_random <- (random_plots[[1]] | random_plots[[2]]) / 
  (random_plots[[3]] | random_plots[[4]]) +
  plot_layout(guides = 'collect') +
  plot_annotation(title = "Species Rarefaction Curves (Random)") & 
  theme(legend.position = "bottom");plate_random

collector_plots <- generate_all_plots("collector")
plate_collector <- (collector_plots[[1]] | collector_plots[[2]]) / 
  (collector_plots[[3]] | collector_plots[[4]]) +
  plot_layout(guides = 'collect') +
  plot_annotation(title = "Species Accumulation Curves (Collector)") & 
  theme(legend.position = "bottom");plate_collector

# Combining plates
# Usamos o operador '/' para colocar a Random em cima da Collector
full_plate <- (plate_random / plate_collector) + 
  plot_layout(guides = 'collect') + 
  plot_annotation(
    tag_levels = 'A', 
    tag_suffix = ')'
  ) & 
  theme(
    legend.position = "bottom",
    plot.tag = element_text(face = "bold", size = 16) # Deixa o A), B) bem visível para publicação
  )

# Para visualizar
full_plate


ggsave("prancha_final_rarefacao.tiff", full_plate, width = 12, height = 16, dpi = 300)

# 6. SUMMARY PLOTS (RICHNESS BY Tapp) ------------------------------------------

df_total_attr <- map_df(top_traits, function(attr) {
  traits_sp %>% filter(!!sym(attr) == 1) %>%
    summarise(Attribute = attr, Total_Species = n_distinct(taxa))
})

df_metodo_attr <- map_df(top_traits, function(attr) {
  traits_sp %>% filter(!!sym(attr) == 1) %>%
    group_by(Attribute = attr, Tapp) %>%
    summarise(Total_Species = n_distinct(taxa), .groups = "drop")
})


df_tt <- data_raw %>%
  filter(!is.na(TT)) %>%
  mutate(TT = str_split(TT, ",")) %>%
  unnest(TT) %>%
  mutate(TT = str_trim(TT))

df_grouped <- df_tt %>%
  mutate(group = ifelse(TT == "D", "Direct (D)", "Qualitative/Semi-quantitative (F,B,C)")) %>%
  count(group) %>%
  mutate(prop = n / sum(n),
         perc = prop * 100)

df_fbc <- df_tt %>%
  filter(TT %in% c("F", "B", "C")) %>%
  count(TT) %>%
  mutate(prop = n / sum(n),
         perc = prop * 100)

plot_D_vs_others <- ggplot(df_grouped, aes(x = reorder(group, -prop), y = prop, fill = group)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(perc, 1), "%")),
            vjust = -0.5) +
  geom_col(fill = "grey40", width = 0.7) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)))+
  labs(subtitle = "A) Relative frequency of trait types",
       x = "", y = "Proportion") +
  theme_classic() +
  theme(legend.position = "none")


plot_FBC <- ggplot(df_fbc, aes(x = reorder(TT, -prop), y = prop, fill = TT)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(perc, 1), "%")),
            vjust = -0.5) +
  geom_col(fill = "grey40", width = 0.7) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)))+
  labs(subtitle = "B). Relative Frequency of qualitative and semi-quantitative methods",
       x = "Type", y = "Proportion") +
  theme_classic() +
  theme(legend.position = "none")

plot_total <- ggplot(na.omit(df_total_attr), aes(x = reorder(Attribute, -Total_Species), y = Total_Species)) +
  geom_col(fill = "grey40", width = 0.7) +
  geom_text(aes(label = Total_Species), vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)))+
  labs(subtitle = "C) Species richness per trait", x = "", y = "Richness") +
  theme_classic()

plot_method <- ggplot(df_metodo_attr, aes(x = reorder(Attribute, -Total_Species), y = Total_Species, fill = Tapp)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = Total_Species), position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  scale_fill_manual(values = colors_tapp) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)))+
  labs(subtitle = "D) Species richness per trait assessment method",x = "Trait", y = "Richness") +
  theme_classic() + theme(legend.position = "bottom")

plate_summary <- (plot_D_vs_others / plot_total) | (plot_FBC / plot_method) + plot_annotation(title = "Species Richness Summary")

# Print/Save
print(plate_random)
print(plate_collector)
print(plate_summary)

# Para salvar:
ggsave("plate_random.pdf", plate_random, width = 10, height = 8)
ggsave("plate_collector.pdf", plate_collector, width = 10, height = 8)
ggsave("plate_species_summary.pdf", plate_summary, width = 8, height = 14)
