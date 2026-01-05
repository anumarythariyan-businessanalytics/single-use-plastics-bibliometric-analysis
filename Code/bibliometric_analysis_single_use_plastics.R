# Bibliometric Analysis Workflow (WoS + Rayyan screening)
# Topic: Single-Use Plastics | Period: 2019–2025


# 1) Load required libraries
# bibliometrix: main package to convert WoS files, run biblioAnalysis, networks, thematic map
library(bibliometrix)

# readr: fast import for Rayyan CSV export (screening decisions)
library(readr)

# dplyr: reproducible data manipulation (filtering, counting, mutating)
library(dplyr)

# stringr: consistent text cleaning for DOI/title matching (lower/trim/squish)
library(stringr)

# ggplot2: publication trend figure (annual scientific production)
library(ggplot2)

# tidyr is required here because unnest() is used above (ensures script runs cleanly)
library(tidyr)


# 2) Set working directory
#setwd("C:/Users/ADMIN/Downloads")

# Confirms working directory to ensure file paths resolve correctly (reproducibility check)
#getwd()


# 3) Import WoS BibTeX files and convert to bibliometrix DF
# convert2df standardizes bibliographic fields into a bibliometrix-compatible data frame
# dbsource = "wos" ensures correct parsing according to Web of Science format
# format = "bibtex" indicates the input file type
M_wos <- convert2df(
  file = c("data/raw/WOS_2019_2020.bib",
           "data/raw/WOS_2021_2022.bib",
           "data/raw/WOS_2023_2024.bib",
           "data/raw/WOS_2025.bib"),
  dbsource = "wos",
  format = "bibtex"
)

# Quick diagnostic: number of records and variables imported
dim(M_wos)


# 4) Import Rayyan screening output
# Rayyan CSV typically contains screening decisions and metadata (e.g., DOI, title)
rayyan <- read_csv("data/raw/rayyan.csv")

# Inspect available columns to confirm the DOI/title column names match your script
colnames(rayyan)


# 5) Initial DOI-based filtering (exact match) 
# Creates a subset of WoS records whose DOI is present in the Rayyan DOI list
# This is a strict inclusion approach but may miss records due to formatting differences (case/whitespace)
M_final <- M_wos[M_wos$DI %in% rayyan$doi, ]

# Diagnostic: how many records were retained using direct DOI matching
dim(M_final)


# 6) Robust matching via cleaned DOI and cleaned Title 
# To improve sensitivity, you clean DOIs and Titles to avoid mismatch due to:
#  upper/lower case differences
#  extra spaces
#  missing DOI but matching title


# Clean Rayyan DOI list (lowercase + trim), remove blanks/NA, keep unique
rayyan_doi <- rayyan %>%
  mutate(doi_clean = str_to_lower(str_trim(doi))) %>%
  filter(!is.na(doi_clean), doi_clean != "") %>%
  pull(doi_clean) %>% unique()

# Clean Rayyan Title list (lowercase + squish spaces), remove blanks/NA, keep unique
rayyan_title <- rayyan %>%
  mutate(title_clean = str_to_lower(str_squish(title))) %>%
  filter(!is.na(title_clean), title_clean != "") %>%
  pull(title_clean) %>% unique()

# Create cleaned fields in WoS dataset for matching
# DI = DOI field in WoS; TI = title field in WoS (bibliometrix naming convention)
M_wos <- M_wos %>%
  mutate(
    DI_clean = str_to_lower(str_trim(DI)),
    TI_clean = str_to_lower(str_squish(TI))
  )

# Keep records if either:
#  cleaned DOI matches Rayyan cleaned DOI list OR
#  cleaned Title matches Rayyan cleaned Title list
# This reduces false negatives caused by missing DOI or inconsistent formatting.
M_final2 <- M_wos %>%
  filter(DI_clean %in% rayyan_doi | TI_clean %in% rayyan_title)

# Diagnostic: final included dataset size after robust matching
dim(M_final2)


# 7) Limit analysis period (2019–2025)
# Ensures your dataset aligns with dissertation timeframe
# Converting PY to numeric avoids type issues in filtering
M_final2$PY <- as.numeric(M_final2$PY)

# Subset to the years specified in your dissertation protocol
M <- subset(M_final2, PY >= 2019 & PY <= 2025)

# Diagnostic: confirms how many documents remain in analysis window
dim(M)

# Year distribution check (useful for reporting annual coverage and PRISMA results section)
table(M$PY)


# 8) Descriptive bibliometric analysis 
# biblioAnalysis computes core indicators:
# production, citations, authorship patterns, sources, keywords, etc.
results <- biblioAnalysis(M, sep=";")

# summary() extracts top-k results used for dissertation tables/interpretation
# pause=FALSE avoids interactive pause (important for reproducible scripts)
S <- summary(results, k=10, pause=FALSE)

# Main information: total docs, sources, authors, time span, etc. (typically Table 1 in bibliometrics)
S$MainInformation

# Most relevant sources: top journals/sources by number of documents
S$MostRelSources

# Most productive authors: author productivity measures
S$MostProdAuthors

# Most relevant keywords: dominant themes via author keywords
S$MostRelKeywords


# 9) Inspect available fields 
# Useful to verify presence of key bibliometrix tags:
# AU, SO, PY, DE, ID, C1, CR, TC, etc.
colnames(M)


# 10) Extract author-country metadata (AU_CO) 
# metaTagExtraction derives country information linked to authors/affiliations
# AU_CO is later used to build collaboration networks by countries
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

# Quick check: confirms AU_CO field was created/populated
head(M$AU_CO)


# 11) Remove duplicate countries within a record 
# Some records may list the same country multiple times if many authors share affiliations.
# This step keeps unique country names per document to avoid inflating collaboration counts.
M$AU_CO <- sapply(M$AU_CO, function(x){
  if (is.na(x) || x == "") return(NA)
  paste(unique(strsplit(x, ";")[[1]]), collapse=";")
})

# Verify deduplication output
head(M$AU_CO)


# 12) Annual scientific production plot 
# Counts publications per year for your trend analysis (Figure 1)
annual_df <- M %>%
  count(PY, name="Publications") %>%
  arrange(PY)

# Line + points visualization is standard in bibliometrics reporting
p1 <- ggplot(annual_df, aes(x=PY, y=Publications)) +
  geom_line() + geom_point() +
  labs(title="Annual Publications on Single-Use Plastics (2019–2025)",
       x="Year", y="Number of publications")

# Display plot in R session
p1

# Create outputs folder (if not already present) to standardize saving figures
dir.create("outputs", showWarnings = FALSE)

# Save high-resolution figure for dissertation (300 dpi is publication-quality)
ggsave("outputs/Figure1_Annual_Publications.png", p1, width=8, height=5, dpi=300)


# 13) Country collaboration network (Figure 2)
# biblioNetwork creates adjacency matrix for collaboration analysis
# analysis="collaboration" and network="countries" builds country-to-country co-authorship links
country_net <- biblioNetwork(
  M,
  analysis = "collaboration",
  network = "countries",
  sep = ";"
)

# Visualize collaboration network
# normalize="association" helps standardize link strengths
# n=30 limits to most connected countries for readability (common dissertation practice)
# type="fruchterman" is a force-directed layout suitable for collaboration networks
networkPlot(
  country_net,
  normalize = "association",
  weighted = TRUE,
  n = 30,
  type = "fruchterman",
  size = TRUE,
  labelsize = 0.7,
  Title = "International Collaboration Network on Single-Use Plastics (2019–2025)"
)

# Save collaboration network 
png("outputs/Figure2_Country_Collaboration.png", width=2200, height=1400, res=250)
networkPlot(
  country_net,
  normalize = "association",
  weighted = TRUE,
  n = 30,
  type = "fruchterman",
  size = TRUE,
  labelsize = 0.7,
  Title = "International Collaboration Network on Single-Use Plastics (2019–2025)"
)

# Close device to finalize file writing (critical—otherwise file may be blank/corrupt)
dev.off()


# 14) Keyword co-occurrence network (Figure 3)
# Builds co-occurrence links between keywords (typically author keywords / DE field)
# Helps reveal thematic structure of the literature
keyword_net <- biblioNetwork(
  M,
  analysis = "co-occurrences",
  network = "keywords",
  sep = ";"
)

# Quick on-screen network plot for inspection
networkPlot(
  keyword_net,
  normalize = "association",
  weighted = TRUE,
  n = 30,
  type = "fruchterman",
  size = TRUE,
  labelsize = 0.7,
  Title = "Keyword Co-occurrence Network in Single-Use Plastics Research (2019–2025)"
)


png("outputs/Figure3_Keyword_Network.png", width=2200, height=1400, res=250)
networkPlot(keyword_net, normalize="association", weighted=TRUE, n=30,
            type="fruchterman", size=TRUE, labelsize=0.7,
            Title="Keyword Co-occurrence Network in Single-Use Plastics Research (2019–2025)")

dev.off()


# 15) Thematic map (strategic diagram)(Figure 4)
# thematicMap positions themes into quadrants based on:
# centrality (relevance/connectedness) and density (development/maturity)
# minfreq=10 filters to stable themes (improves interpretability and reduces noise)
thematicMap(
  M,
  field = "DE",
  minfreq = 10,
  stemming = FALSE,
  size = 0.5,
  n.labels = 1,
  repel = TRUE
)

# Save thematic map figure (high resolution)
png("outputs/Figure4_Thematic_Map.png", width=2000, height=1500, res=250)
thematicMap(
  M,
  field = "DE",
  minfreq = 10,
  stemming = FALSE,
  size = 0.5,
  n.labels = 1,
  repel = TRUE
)
dev.off() 


# 16) Time-slicing for keyword evolution (Figure 5)
# Splits the dataset into meaningful periods for temporal comparison
# This supports a dissertation narrative: early vs mid vs recent topic shifts
M <- M %>%
  mutate(Period = case_when(
    PY <= 2021 ~ "2019–2021",
    PY <= 2023 ~ "2022–2023",
    TRUE       ~ "2024–2025"
  ))

# Check that records are distributed across periods
table(M$Period)


# Function: get top keywords per period 
# Extracts and ranks author keywords (DE) within a subset of data
# Steps:
# - drop missing DE
# - split semicolon-separated keywords
# - unnest into one keyword per row
# - standardize spacing and case
# - count frequency and return top_n
get_top_keywords <- function(df, top_n = 15){
  df %>%
    filter(!is.na(DE), DE != "") %>%
    mutate(DE = str_split(DE, ";")) %>%
    tidyr::unnest(DE) %>%
    mutate(DE = str_squish(str_to_lower(DE))) %>%
    count(DE, sort = TRUE) %>%
    slice_head(n = top_n)
}


#Compute top keywords for each period 
# Using top 20 per slice improves interpretability for evolution plotting
kw_2019_2021 <- get_top_keywords(filter(M, Period == "2019–2021"), 20)
kw_2022_2023 <- get_top_keywords(filter(M, Period == "2022–2023"), 20)
kw_2024_2025 <- get_top_keywords(filter(M, Period == "2024–2025"), 20)

# Display frequency tables
kw_2019_2021
kw_2022_2023
kw_2024_2025


# Combine slices for faceted visualization 
# Adds Period labels and stacks results for plotting
kw_2019_2021$Period <- "2019–2021"
kw_2022_2023$Period <- "2022–2023"
kw_2024_2025$Period <- "2024–2025"

kw_all <- bind_rows(kw_2019_2021, kw_2022_2023, kw_2024_2025)

# Optional (commented out): keep only keywords appearing across multiple periods
# This would emphasize persistent themes vs short-lived ones
# kw_all <- kw_all %>% group_by(DE) %>% filter(n() >= 2) %>% ungroup()



# Keyword evolution plot
# Facet by period to compare dominant keywords over time
# scales="free_y" allows each facet to show its own frequency range (more readable)
p_ev <- ggplot(kw_all, aes(x = reorder(DE, n), y = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~Period, scales = "free_y") +
  labs(
    title = "Top Author Keywords by Time Period (Single-Use Plastics, 2019–2025)",
    x = "Keyword", y = "Frequency"
  )

# Display plot in session
p_ev

# Save as a dissertation-ready figure
ggsave("outputs/Figure5_Keyword_Evolution.png", p_ev, width = 12, height = 7, dpi = 300)
