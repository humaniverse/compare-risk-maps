# For all indices:
# - lower ranks = more vulnerable / least resilience
# - quartile 1 = most vulnerable / least resilient

library(tidyverse)
library(geographr)
library(readxl)
library(httr2)
library(IMD)

# ---- Local Authority lookups ----
ltla23 <- lookup_ltla_ltla |> 
  distinct(ltla23_code, ltla23_name)

# The Community Resilience Index uses 2021 codes for Local Authorities
ltla21_23 <- lookup_ltla_ltla |> 
  distinct(ltla21_code, ltla23_code)

# The Social Fabric Index uses 2019 codes for Local Authorities
ltla19_23 <- lookup_ltla_ltla |> 
  distinct(ltla19_code, ltla23_code)

ltla22_23 <- lookup_ltla_ltla |> 
  distinct(ltla22_code, ltla23_code)

setdiff(
  unique(lookup_ltla_ltla$ltla22_code),
  unique(lookup_ltla_ltla$ltla23_code)
)

lsoa11_ltla23 <- 
  lookup_lsoa11_lsoa21_ltla22 |> 
  left_join(ltla22_23) |> 
  distinct(lsoa11_code, ltla23_code)

# ---- Community Resilience Index for England ----
# https://christine-l-camacho.github.io/CRI_England/
# Data: https://github.com/Christine-L-Camacho/CRI_England
cri_raw <- read_csv("https://github.com/Christine-L-Camacho/CRI_England/raw/refs/heads/main/index.csv")

# Find which set of LA codes this index uses
setdiff(cri_raw$lac, lookup_ltla_ltla$ltla21_code)
#---> 2021

cri <- 
  cri_raw |> 
  select(ltla21_code = lac, ltla21_name = lan, cri = index) |> 
  left_join(ltla21_23, by = join_by(ltla21_code)) |> 
  
  # Where old Local Authorities merged into one new LA, take their average SoFI score
  group_by(ltla23_code) |> 
  summarise(cri_score = mean(cri)) |>
  ungroup() |> 
  
  # Calculate ranks and quintiles
  mutate(
    cri_rank = rank(cri_score, ties.method = "min"),
    cri_quintile = ntile(cri_rank, 5)
  ) |> 
  select(-cri_score)

# ---- Social Fabric Index ----
# Source: Onward's UK Social Fabric Index 2023
# - https://www.ukonward.com/reports/2023-social-fabric-index/
# Can directly download this via https://datawrapper.dwcdn.net/dR8tf/6/dataset.csv
#
# Higher scores = better social fabric
sofi_raw <- read_delim("https://datawrapper.dwcdn.net/dR8tf/6/dataset.csv", delim = ";")

sofi <- 
  sofi_raw |> 
  select(ltla19_code = codes, sofi_score = `SF score`) |> 
  left_join(ltla19_23, by = join_by(ltla19_code)) |> 
  
  # Where old Local Authorities merged into one new LA, take their average SoFI score
  group_by(ltla23_code) |> 
  summarise(sofi_score = mean(sofi_score)) |>
  ungroup() |> 
  
  # Calculate ranks and quintiles within nations
  mutate(nation = str_sub(ltla23_code, 1, 1)) |> 
  
  group_by(nation) |>
  mutate(
    sofi_rank = rank(sofi_score, ties.method = "min"),
    sofi_quintile = ntile(sofi_rank, 5)
  ) |> 
  ungroup() |> 
  
  select(-sofi_score, -nation)

# ---- Index of Multiple Deprivation ----
imd_england <- 
  IMD::imd2019_england_ltla23 |> 
  # Calculate ranks and quintiles
  mutate(
    imd_rank = rank(-Score, ties.method = "min"),
    imd_quintile = ntile(imd_rank, 5)
  ) |> 
  select(ltla23_code, imd_rank, imd_quintile)

# ---- Flood vulnerability ----
## England
tf <- tempfile(fileext = ".zip")

request("https://www.climatejust.org.uk/files/FinalDataEnglandFlood2022.zip") |>
  req_perform(path = tf)

unzip(tf, exdir = tempdir())

list.files(file.path(tempdir(), "Final Data England Flood 2022"))

flood_england_raw <- 
  read_excel(
    file.path(tempdir(), "Final Data England Flood 2022/Social Vulnerability and Disadvantage to Flood for England 2022.xlsx"),
    sheet = "England Flood Disadvantage"
  )

flood_england_ltla <- 
  flood_england_raw |> 
  select(lsoa11_code = `LSOA Code`, flood_vuln = `Social Vulnerability to Flood`, population = Population) |> 
  left_join(lsoa11_ltla23) |> 
  IMD:::calculate_pop_weighted_score(flood_vuln, ltla23_code, population)

# Calculate ranks and quintiles
flood_england_ltla <- 
  flood_england_ltla |> 
  mutate(
    flooding_rank = rank(-Score, ties.method = "min"),
    flooding_quintile = ntile(Score, 5)
  ) |> 
  select(ltla23_code, flooding_rank, flooding_quintile)

## Wales
tf <- tempfile(fileext = ".zip")

request("https://www.climatejust.org.uk/files/FinalDataWalesFlood2022.zip") |>
  req_perform(path = tf)

unzip(tf, exdir = tempdir())

list.files(file.path(tempdir(), "Final Data Wales Flood 2022"))

flood_wales_raw <- 
  read_excel(
    file.path(tempdir(), "Final Data Wales Flood 2022/Social Vulnerability and Disadvantage to Flood for Wales 2022.xlsx"),
    sheet = "Wales Flood Disadvantage"
  )

flood_wales_ltla <- 
  flood_wales_raw |> 
  select(lsoa11_code = `Code`, flood_vuln = `Social Vulnerability to Flood`, population = Population) |> 
  left_join(lsoa11_ltla23) |> 
  IMD:::calculate_pop_weighted_score(flood_vuln, ltla23_code, population)

# Calculate ranks and quintiles
flood_wales_ltla <- 
  flood_wales_ltla |> 
  mutate(
    flooding_rank = rank(-Score, ties.method = "min"),
    flooding_quintile = ntile(Score, 5)
  ) |> 
  select(ltla23_code, flooding_rank, flooding_quintile)

## Combine
flood_ltla <- bind_rows(flood_england_ltla, flood_wales_ltla)

# ---- Combine all indices ----
indices <- 
  ltla23 |> 
  # filter(str_detect(ltla23_code, "^E")) |>
  left_join(cri, by = join_by(ltla23_code)) |>
  left_join(sofi, by = join_by(ltla23_code)) |>
  left_join(imd_england, by = join_by(ltla23_code)) |> 
  left_join(flood_ltla, by = join_by(ltla23_code))

# Save the indices
write_rds(indices, "data/indices.rds")

# Separate out ranks
indices_ranks <- 
  indices |> 
  select(
    ltla23_code, 
    ltla23_name,
    cri = cri_rank,
    sofi = sofi_rank,
    imd = imd_rank,
    flooding = flooding_rank
  )

write_rds(indices_ranks, "data/indices-ranks.rds")

# Separate out quintiles
indices_quintiles <- 
  indices |> 
  select(
    ltla23_code, 
    ltla23_name,
    cri = cri_quintile,
    sofi = sofi_quintile,
    imd = imd_quintile,
    flooding = flooding_quintile
  )

write_rds(indices_quintiles, "data/indices-quintile.rds")

# ---- Local Authority boundaries ----
lad_boundaries <- 
  boundaries_ltla24 |>
  left_join(
    indices_quintiles |> select(-ltla23_name), 
    by = join_by(ltla24_code == ltla23_code)
  )

write_rds(lad_boundaries, "data/lad-boundaries.rds")
