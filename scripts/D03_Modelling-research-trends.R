# =========================================================
# Modelling research trends
# =========================================================


# -------------------------------------------------------------------------
# 1. Load packages
# -------------------------------------------------------------------------

# required packages
{
  lib <- .libPaths()[1] 
  required.packages <- c("dplyr","ggspatial", "nnet",
                         "writexl", "openxlsx", "countrycode",
                         "readxl",  "here", "tidyr",
                         "tidyverse", "car", "tidyselect","tidygraph", 
                         "ggeffects", "patchwork",
                         "MuMIn", "DHARMa", "adiv", 
                         "adegraphics", "PERMANOVA",
                         "gridExtra", "FactoMineR", 
                         "factoextra", "jtools", 
                         "bibliometrix") 
  i1 <- !(required.packages %in% row.names(installed.packages())) 
  if(any(i1)) { 
    install.packages(required.packages[i1], dependencies = TRUE, lib = lib) 
  } 
  lapply(required.packages, require, character.only = TRUE)
  
} # required packages

# -------------------------------------------------------------------------
# 2. Import data
# -------------------------------------------------------------------------

{
data<- read.xlsx(here("data", "data.xlsx"))
traits<-read.xlsx(here("data", "data.xlsx"), sheet = "traits") 
}

# -------------------------------------------------------------------------
# 3. Prepare data
# -------------------------------------------------------------------------
data <- data %>%
  mutate(
    REP = factor(REP,
                 levels = c(0, 1, 2),
                 labels = c("None", "Partial", "Complete")),
    
    TQ = factor(TQ,
                levels = c(0, 1, 2),
                labels = c("Not clear", "Weakly justified", "Robust"))
  )


data$PY<- as.numeric(data$PY) #Publication year
data$SA<- as.factor(data$SA) #Study area
data$Tapp<- as.factor(data$Tapp) #Trait measurement approach
data$TQ<- as.factor(data$TQ) #Trait selection x study question relation
data$REP<- as.factor(data$REP) #Reproductibility level


# -------------------------------------------------------------------------
# 4. Models
# -------------------------------------------------------------------------

#---------------------------
# a. Model setting
#---------------------------
mod_SA<- multinom(SA ~ PY, data = data)

data$Direct_measurements<-ifelse(data$Tapp == "Direct", 1,0)
mod_Tapp <- glm(Direct_measurements ~ PY, data = data, family = "binomial")

mod_TQ <- multinom(TQ ~ PY, data = data)

mod_REP  <- multinom(REP  ~ PY, data = data)

#---------------------------
# b. Model selection function
#---------------------------
model_selection <- function(response, data){
  
  form_null <- as.formula(paste(response, "~ 1"))
  form_full <- as.formula(paste(response, "~ PY"))
  
  mod_null <- multinom(form_null, data = data, trace = FALSE)
  mod_full <- multinom(form_full, data = data, trace = FALSE)
  
  sel <- model.sel(mod_null, mod_full)
  
  df <- as.data.frame(sel)
  
  df$model <- rownames(df)
  df$response <- response
  
  rownames(df) <- NULL
  
  # Standardize names
  names(df) <- tolower(names(df))
  
  
  df <- df %>%
    rename(
      AIC   = any_of(c("aic", "aicc")),
      delta = any_of(c("delta")),
      weight = any_of(c("weight", "weights"))
    )
  
  
  df <- df %>%
    dplyr::select(any_of(c("model", "df", "loglik", "AIC", "delta", "weight", "response")))
  
  return(df)
}

#---------------------------
# c. Multinomial models
#---------------------------
sel_SA  <- model_selection("SA", data)
sel_TQ  <- model_selection("TQ", data)
sel_REP <- model_selection("REP", data)

#---------------------------
# d. Trait measurement approach (GLM - binomial) model
#---------------------------
mod_null_Tapp <- glm(Direct_measurements ~ 1, data = data, family = "binomial")
mod_full_Tapp <- glm(Direct_measurements ~ PY, data = data, family = "binomial")
summary(mod_full_Tapp)

sel_Tapp <- model.sel(mod_null_Tapp, mod_full_Tapp) %>%
  as.data.frame()

sel_Tapp$model <- rownames(sel_Tapp)
sel_Tapp$response <- "Tapp"

rownames(sel_Tapp) <- NULL

names(sel_Tapp) <- tolower(names(sel_Tapp))

sel_Tapp <- sel_Tapp %>%
  rename(
    AIC   = any_of(c("aic", "aicc")),
    delta = any_of(c("delta")),
    weight = any_of(c("weight", "weights"))
  ) %>%
  dplyr::select(any_of(c("model", "df", "loglik", "AIC", "delta", "weight", "response")))

#---------------------------
# e. Combine
#---------------------------
final_selection <- dplyr::bind_rows(
  sel_SA,
  sel_TQ,
  sel_REP,
  sel_Tapp
)



# -------------------------------------------------------------------------
# 5. Graphic - reproducibility
# -------------------------------------------------------------------------
pred_rep <- ggeffect(mod_REP, terms = "PY [all]")

g1 <- ggplot(pred_rep, aes(x = x, y = predicted, color = response.level, fill = response.level)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  geom_line(size = 1) +
  scale_color_viridis_d(option = "mako", end = 0.7) + 
  scale_fill_viridis_d(option = "cividis", end = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Reproductibility Level", x = "Year", y = "Probability", color = "Level", fill = "Level") +
  theme_classic() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# -------------------------------------------------------------------------
# 6. Graphic - trait selection
# -------------------------------------------------------------------------
pred_tq <- ggeffect(mod_TQ, terms = "PY [all]")

g2 <- ggplot(pred_tq, aes(x = x, y = predicted, color = response.level, fill = response.level)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  geom_line(size = 1) +
  scale_color_viridis_d(option = "magma", end = 0.7) + 
  scale_fill_viridis_d(option = "magma", end = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Trait Selection vs Study question", x = "Year", y = "Probability", color = "Quality", fill = "Quality") +
  theme_classic() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# -------------------------------------------------------------------------
# 7. Graphic - study thematic area
# -------------------------------------------------------------------------
pred_sa <- ggeffect(mod_SA, terms = "PY [all]")

g3 <- ggplot(pred_sa, aes(x = x, y = predicted, color = response.level, fill = response.level)) +
  # Fita de confiança leve
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  # Linhas de tendência
  geom_line(size = 1) +
  # A paleta "turbo" é a melhor do pacote viridis para 7+ níveis
  scale_color_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Study Area", 
    x = "Year", 
    y = "Probability", 
    color = "Area", 
    fill = "Area"
  ) +
  theme_classic() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
  guides(color = guide_legend(nrow = 3), fill = guide_legend(nrow = 3))

# -------------------------------------------------------------------------
# 8. Graphic - Trait measurement approach
# -------------------------------------------------------------------------
pred_tapp_raw <- ggeffect(mod_Tapp, terms = "PY [all]")

df_direct <- data.frame(
  x = pred_tapp_raw$x, predicted = pred_tapp_raw$predicted,
  conf.low = pred_tapp_raw$conf.low, conf.high = pred_tapp_raw$conf.high,
  Approach = "Direct"
)
df_literature <- data.frame(
  x = pred_tapp_raw$x, predicted = 1 - pred_tapp_raw$predicted,
  conf.low = 1 - pred_tapp_raw$conf.high, conf.high = 1 - pred_tapp_raw$conf.low,
  Approach = "Literature consult"
)
df_final_binom <- rbind(df_direct, df_literature)

g4 <- ggplot(df_final_binom, aes(x = x, y = predicted, color = Approach, fill = Approach)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  geom_line(size = 1) +
  scale_color_viridis_d(option = "plasma", end = 0.8) + 
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Trait Measurement Approach", x = "Year", y = "Probability", color = "Approach", fill = "Approach") +
  theme_classic() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# -------------------------------------------------------------------------
# 9. Final plate
# -------------------------------------------------------------------------

p_final <- (g2 + g3) + (g4) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(face = 'bold'))

# Visualizar
p_final




