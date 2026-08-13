# =========================================================
# Run bibliometric analyses
# =========================================================

# Packages ---------------------------------------------------------------------

{
  lib <- .libPaths()[1] 
  required.packages <- c("vegan", "FD", "dplyr","ggspatial",
                         "writexl", "openxlsx", "countrycode",
                         "readxl",  "here", "tidyr",
                         "tidyverse", "car", "tidyselect","tidygraph", 
                         "ggeffects", "patchwork",
                         "MuMIn", "DHARMa", "adiv", 
                         "adegraphics", "PERMANOVA",
                         "gridExtra", "FactoMineR","ggalluvial", 
                         "factoextra", "jtools", "picante","rcrossref", 
                         "bibliometrix", "countrycode", "rnaturalearth") 
  i1 <- !(required.packages %in% row.names(installed.packages())) 
  if(any(i1)) { 
    install.packages(required.packages[i1], dependencies = TRUE, lib = lib) 
  } 
  lapply(required.packages, require, character.only = TRUE)
  
} # required packages

# ---------------------------------------------------------
# 1. Import
# ---------------------------------------------------------

final_base_bbmetrix <- readRDS("data/final_base_bbmetrix.rds")

data <- read.xlsx(
  here("data", "data.xlsx")
)

# ---------------------------------------------------------
# 2. Basic analyses
# ---------------------------------------------------------

# -------------
#a. Results summary
#---------------

results <- biblioAnalysis(final_base_bbmetrix, sep = ";");results
S <- summary(object = results, k = 10, pause = FALSE)
plot(x = results, k = 10)

publication_data <- final_base_bbmetrix %>%
  group_by(PY) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )


# Publications per year
publications_by_year <- ggplot(
  publication_data,
  aes(
    x = as.factor(PY),
    y = n,
    group = 1
  )
) +
  
  geom_line(
    color = "darkgray",
    linewidth = 1
  ) +
  
  geom_point(
    size = 4,
    color = "black"
  ) +
  
  labs(
    x = "Year",
    y = "Published papers"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5
    ),
    legend.position = "bottom"
  )


# -------------
#b. Core sources
#---------------

BR <- bradford(final_base_bbmetrix);BR


res_sources <- sourceGrowth(final_base_bbmetrix, top = 6)

res_long <- pivot_longer(res_sources, 
                         cols = -Year, 
                         names_to = "Journal", 
                         values_to = "Publications")


ggplot(res_long, aes(x = Year, y = Publications, color = Journal)) +
geom_line(size = 1) +
geom_point() +
theme_bw() +
labs(x = "Year", y = "Published papers", color = "Journal") +
theme_classic()+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5), legend.position = "bottom")



# -------------
#c. Production map
#---------------

C <- metaTagExtraction(final_base_bbmetrix, Field = "AU1_CO", sep = ";")



country_data <- C %>%
  separate_rows(C1, sep = ";") %>%
  mutate(country = sub(".*,\\s*", "", C1)) %>%
  mutate(country = trimws(country)) %>%
  filter(country != "") %>%
  mutate(iso3 = countrycode(country,
                            origin = "country.name",
                            destination = "iso3c"))

country_count <- country_data %>%
  filter(!is.na(iso3)) %>%
  count(iso3, name = "n")

world <- ne_countries(scale = "medium", returnclass = "sf")

map_data <- world %>%
  left_join(country_count, by = c("adm0_a3" = "iso3"))

map = ggplot(map_data) +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(option = "magma", na.value = "grey90", begin = 0.9, end = 0) +
  theme_classic() +
  labs(title = "Most productive countries",
       fill = "Published papers");map


# -------------
#d. Production by countries
#---------------

df_countries <- as.data.frame(results$Countries)


colnames(df_countries) <- c("Country", "Frequency")

# Organizing by rank (ex: top 10)
df_top <- df_countries %>%
  arrange(desc(Frequency)) %>%
  slice_head(n = 10)

ggplot(df_top, aes(x = reorder(Country, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Países mais produtivos",
    x = "País",
    y = "Número de publicações"
  ) +
  theme_classic()



# -------------
#e. Social structure of the field
#---------------



NetMatrix <- biblioNetwork(final_base_bbmetrix, analysis = "collaboration", 
                           network = "authors", sep = ";")


net <- networkPlot(NetMatrix, 
            n = 20, #author number (top  10, 15 or 20)
            type = "circle", # Layout 
            size = TRUE,         # Nodes size affected
            size.cex = TRUE,     # Name size affected
            remove.multiple = F, 
            labelsize = 0.7,     # Font size
            cluster = "leiden",
            halo = F, weighted = TRUE, edgesize = 10, 
            edges.min = 1, remove.isolates = T)


#Collaboration analysis of institutions 

NetMatrix2 <- biblioNetwork(final_base_bbmetrix, analysis = "collaboration", 
                           network = "universities", sep = ";")


net2 <- networkPlot(NetMatrix2, 
                    n = 20, #Número de autores no gráfico (os top # 10, 15 or 20)
                    type = "circle", # Layout de distribuição
                    size = TRUE,         # ATENÇÃO: Isso faz a bolinha variar de tamanho!
                    size.cex = TRUE,     # Escala o tamanho dos nomes também
                    remove.multiple = F, 
                    labelsize = 0.7,     # Tamanho da fonte dos nomes
                    cluster = "leiden",# Agrupa autores que trabalham juntos (cores)
                    halo = F, weighted = TRUE, edgesize = 10, 
                    edges.min = 1, remove.isolates = T)# halo Adiciona uma sombra colorida nos grupos


# -------------
#f. Cognitive structure
#---------------



CS <- conceptualStructure(final_base_bbmetrix, 
                          field = "DE", 
                          method = "MCA", 
                          minDegree = 6,      
                          clust = "auto", 
                          stemming = F,
                          documents = 5,
                          labelsize = 10)



# -------------
#f. Thematic maps
#---------------

terms <- results$DE %>% as.data.frame()

nrow(terms)

res_map <- thematicMap(final_base_bbmetrix, 
                       field = "DE", 
                       n = nrow(terms), 
                       minfreq = 6, 
                       stemming = FALSE, 
                       size = 0.4, subgraphs = T, cluster = "leiden")


plot(res_map$map)


# Split dataset
pre_pandemic <- final_base_bbmetrix %>%
  dplyr::filter(PY < 2020)

post_pandemic <- final_base_bbmetrix %>%
  dplyr::filter(PY >= 2020)

# Run thematic maps
res_map_pre <- thematicMap(pre_pandemic, 
                           field = "DE", 
                           n = nrow(terms), 
                           minfreq = 6, 
                           stemming = F, 
                           size = 0.4, 
                           subgraphs = TRUE, 
                           cluster = "leiden")

res_map_post <- thematicMap(post_pandemic, 
                            field = "DE", 
                            n = nrow(terms), 
                            minfreq = 6, 
                            stemming = F, 
                            size = 0.4, 
                            subgraphs = TRUE, 
                            cluster = "leiden")
plot(res_map_pre$map)
plot(res_map_post$map)

