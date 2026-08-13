# =========================================================
# Comparing bases and merging in a single bibtext for bbmetrix
# =========================================================

{
lib <- .libPaths()[1] 
required.packages <- c("bibliometrix", "tidyverse", "here", 
                       "readxl", "openxlsx", "dplyr", "stringr") 
i1 <- !(required.packages %in% row.names(installed.packages())) 
if(any(i1)) { 
  install.packages(required.packages[i1], dependencies = TRUE, lib = lib) 
} 
lapply(required.packages, require, character.only = TRUE)
} #Installing and/or Loading required packages

# ---------------------------------------------------------
# 1. Loading selected papers data frame
# ---------------------------------------------------------

#Import the Eligible papers from WOS  and Scopus 

#WOS core collection
#Scopus All collections
#Search conducted by the same keywords in "All fields"

data <- read.xlsx(here("data", 
                       "data.xlsx"), 
                  sheet = "data")  

#For bibliometrix, it is necessary a bibtext though.
#So We redownload the queries bibtexts, set the differeces from "data" and merge.

# ---------------------------------------------------------
# 2. Filtering DOI per base in the selected papers DataFrame
# ---------------------------------------------------------

# Scopus (Scopus + Both)
dois_scopus <- data %>% 
  filter(DB %in% c("Scopus", "Both")) %>% 
  pull(DOI)

# Web of Science (WOS (core collection) + Both)
dois_wos <- data %>% 
  filter(DB %in% c("WOS", "Both")) %>% 
  pull(DOI)


# ---------------------------------------------------------
# 3. Importing and converting bibtext files to Dataframe (Scopus)
# ---------------------------------------------------------

scopus_full <- convert2df(
  "data/SCOPUS_Apr_9_26.bib",
  dbsource = "scopus",
  format = "bibtex"
)#5096 dataset without references


scopus_final <- scopus_full %>% 
  filter(toupper(DI) %in% toupper(dois_scopus))

# ---------------------------------------------------------
# 4. Importing and converting bibtext files to Dataframe (Web of Science)
# ---------------------------------------------------------

wos_corecollection <- convert2df(
  "data/WOS_Apr_9_26.bib",
  dbsource = "wos",
  format = "bibtex"
) #All fields, Core Collection (cc)


wos_final_cc <- wos_corecollection %>% 
  filter(DI %in% dois_wos)

# ---------------------------------------------------------
# 5. MERGE 
# ---------------------------------------------------------

first_merge <- mergeDbSources(
  wos_final_cc,
  scopus_final,
  remove.duplicated = TRUE
)

cat("Total first_merge:", nrow(first_merge), "\n")

# ---------------------------------------------------------
# 6. Putting DOI's in upper case 
# ---------------------------------------------------------

dois_data  <- str_trim(str_to_lower(data$DOI)) 
dois_firstmerge <- str_trim(str_to_lower(first_merge$DI))

# Missing papers
lacking_in_firstmerge <- setdiff(dois_data, dois_firstmerge)

# Extra ?
extras_from_firstmerge <- setdiff(dois_firstmerge, dois_data)

cat("Missing from final base:", length(lacking_in_firstmerge), "\n")
cat("Excedent from final base:", length(extras_from_firstmerge), "\n")


final_base_bbmetrix<- mergeDbSources(
  scopus_final, 
  wos_final_cc,
  remove.duplicated = TRUE
) 

dois_finalbase <- str_trim(str_to_lower(final_base_bbmetrix$DI))
lacking_in_finalbase <- setdiff(dois_data, dois_finalbase)

# ---------------------------------------------------------
# 11. Standardizing keywords manually (method in the paper)
# ---------------------------------------------------------

keywords<- data.frame(DI = final_base_bbmetrix$DI, 
                     DE = final_base_bbmetrix$DE, 
                     AU = final_base_bbmetrix$AU, 
                     KW = final_base_bbmetrix$KW_Merged)

#export for manual standardization
write.xlsx(keywords, file = "keywords_new.xlsx")

#Import the manually standardized keywords
keywords_standardized<-read.xlsx(here("data", "keywords_STANDARDIZED.xlsx")) 

#Inserting std keywords in the final base
final_base_bbmetrix$DE <- keywords_standardized$DE  

#Exporting the final base to be read again
saveRDS(final_base_bbmetrix, "final_base_bbmetrix.rds")

# =========================================================
# Final check for duplicates
# =========================================================

data$DOI_clean <- tolower(trimws(data$DOI))
final_base_bbmetrix$DI_clean <- tolower(trimws(final_base_bbmetrix$DI))

missing <- data[!data$DOI_clean %in% final_base_bbmetrix$DI_clean, ]

nrow(missing)

length(unique(data$DOI_clean))
nrow(data) - length(unique(data$DOI_clean))

data[duplicated(data$DOI_clean) | duplicated(data$DOI_clean, fromLast = TRUE),
     c("DOI", "DB")] 

# =========================================================
# End
# =========================================================