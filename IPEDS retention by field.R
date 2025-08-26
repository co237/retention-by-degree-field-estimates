# International Student Retention by Degree Field

#############################################################################
# Preliminaries: Loading packages 
#############################################################################
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(educationdata)
library(purrr)
library(readxl)

#############################################################################
# Set up crosswalks for IPEDS data
#############################################################################

# Fix crosswalk to have one row per observation
## Load raw crosswalk provided by NSF that crosswalks NSCG field codes to CIP codes
raw_crosswalk <- read_xlsx("NSCG-CIP_Crosswalk_2022.xlsx", sheet = "NSCG_CIP")
## Fix crosswalk
crosswalk_long <- raw_crosswalk %>%
  mutate(
    # keep CIP codes as text so leading zeros aren't lost
    CIP_codes = as.character(`CIP Code(s) -2020`),
    # normalize commas to semicolons for consistency
    CIP_codes = str_replace_all(CIP_codes, ",", ";")
  ) %>%
  # split on semicolons OR whitespace (one or more spaces/tabs)
  separate_rows(CIP_codes, sep = "\\s*;\\s*|\\s+") %>%
  # trim extra spaces
  mutate(CIP_code = str_squish(CIP_codes)) %>%
  # drop blanks/NA
  filter(CIP_code != "", !is.na(CIP_code)) %>%
  select(`NSCG Code - 2023`, `NSCG Field - 2023`,
         `CIP Field(s) - 2020`, CIP_code) %>%
  distinct()

# Manually fix weird import values for a few CIP codes
crosswalk_long <- crosswalk_long %>%
  mutate(
    CIP_code = case_when(
      CIP_code == "26.080500000000001" ~ "26.0805",
      CIP_code == "54.010399999999997" ~ "54.0104",
      CIP_code == "40.049999999999997" ~ "40.05",
      TRUE ~ CIP_code
    )
  )

# Take out periods and leading zeros 
crosswalk_long <- crosswalk_long %>%
  mutate(
    CIP2 = CIP_code %>%
      str_remove("^0") %>%      # remove leading zero if present
      str_remove_all("\\.")     # remove all periods
  )

# 2010 to 2020 CIP code crosswalk

cip_10_to_20_crosswalk <- read.csv("Crosswalk2010to2020.csv")

# remove periods and rename

cip_10_to_20_crosswalk <- cip_10_to_20_crosswalk %>%
  mutate(
    CIPCode2010.1 = gsub("\\.", "", CIPCode2010.1),
    CIPCode2020.1 = gsub("\\.", "", CIPCode2020.1)
  ) %>%
  rename(
    CIP_2010 = CIPCode2010.1,
    CIP_2020 = CIPCode2020.1
  )


#############################################################################
# Pull IPEDS completions data from 2012 through 2021 by nativity status and crosswalk to NSCG codes
#############################################################################

# Function to pull data from the IPEDS API (using educationdata)

pull_year <- function(y) {
  get_education_data(
    level   = "college-university",
    source  = "ipeds",
    topic   = "completions-cip-6",
    filters = list(year = y),
    add_labels = TRUE,  # keep readable labels for race/sex
    csv = TRUE
  )
}

# Pull each year's data and combine them together

raw_12 <- map_dfr(2012, pull_year) %>%
  filter(majornum == "First major",
         sex == "Total",
         race %in% c("Total", "Nonresident alien"),
         award_level %in% c("Bachelor's degree", "Master's degree", "Doctor's degree research/scholarship (starting 2007)", "Doctor's degree professional practice (starting 2007)", "Doctor's degree other (starting 2007)"))
raw_13 <- map_dfr(2013, pull_year)%>%
  filter(majornum == "First major",
         sex == "Total",
         race %in% c("Total", "Nonresident alien"),
         award_level %in% c("Bachelor's degree", "Master's degree", "Doctor's degree research/scholarship (starting 2007)", "Doctor's degree professional practice (starting 2007)", "Doctor's degree other (starting 2007)"))
raw_14 <- map_dfr(2014, pull_year)%>%
  filter(majornum == "First major",
         sex == "Total",
         race %in% c("Total", "Nonresident alien"),
         award_level %in% c("Bachelor's degree", "Master's degree", "Doctor's degree research/scholarship (starting 2007)", "Doctor's degree professional practice (starting 2007)", "Doctor's degree other (starting 2007)"))
raw_15 <- map_dfr(2015, pull_year)%>%
  filter(majornum == "First major",
         sex == "Total",
         race %in% c("Total", "Nonresident alien"),
         award_level %in% c("Bachelor's degree", "Master's degree", "Doctor's degree research/scholarship (starting 2007)", "Doctor's degree professional practice (starting 2007)", "Doctor's degree other (starting 2007)"))
raw_16 <- map_dfr(2016, pull_year)%>%
  filter(majornum == "First major",
         sex == "Total",
         race %in% c("Total", "Nonresident alien"),
         award_level %in% c("Bachelor's degree", "Master's degree", "Doctor's degree research/scholarship (starting 2007)", "Doctor's degree professional practice (starting 2007)", "Doctor's degree other (starting 2007)"))
raw_17 <- map_dfr(2017, pull_year)%>%
  filter(majornum == "First major",
         sex == "Total",
         race %in% c("Total", "Nonresident alien"),
         award_level %in% c("Bachelor's degree", "Master's degree", "Doctor's degree research/scholarship (starting 2007)", "Doctor's degree professional practice (starting 2007)", "Doctor's degree other (starting 2007)"))
raw_18 <- map_dfr(2018, pull_year)%>%
  filter(majornum == "First major",
         sex == "Total",
         race %in% c("Total", "Nonresident alien"),
         award_level %in% c("Bachelor's degree", "Master's degree", "Doctor's degree research/scholarship (starting 2007)", "Doctor's degree professional practice (starting 2007)", "Doctor's degree other (starting 2007)"))
raw_19 <- map_dfr(2019, pull_year)%>%
  filter(majornum == "First major",
         sex == "Total",
         race %in% c("Total", "Nonresident alien"),
         award_level %in% c("Bachelor's degree", "Master's degree", "Doctor's degree research/scholarship (starting 2007)", "Doctor's degree professional practice (starting 2007)", "Doctor's degree other (starting 2007)"))
raw_20 <- map_dfr(2020, pull_year)%>%
  filter(majornum == "First major",
         sex == "Total",
         race %in% c("Total", "Nonresident alien"),
         award_level %in% c("Bachelor's degree", "Master's degree", "Doctor's degree research/scholarship (starting 2007)", "Doctor's degree professional practice (starting 2007)", "Doctor's degree other (starting 2007)"))
raw_21 <- map_dfr(2021, pull_year)%>%
  filter(majornum == "First major",
         sex == "Total",
         race %in% c("Total", "Nonresident alien"),
         award_level %in% c("Bachelor's degree", "Master's degree", "Doctor's degree research/scholarship (starting 2007)", "Doctor's degree professional practice (starting 2007)", "Doctor's degree other (starting 2007)"))

raw_combined <- rbind(raw_12, raw_13, raw_14, raw_15, raw_16, raw_17, raw_18, raw_19, raw_20, raw_21)


# Check total degrees awarded at each level 
Total_degrees_year <- raw_combined %>%
  filter(cipcode_6digit !="99") %>% # Filter out the total, which would double our numbers
  group_by(year, award_level, cipcode_6digit, race) %>%
  summarise(total = sum(awards_6digit))

# Nonresident alien total degrees by 6 digit CIP code by year by degree level
Immigrant_degrees_year <- raw_combined %>%
  filter(cipcode_6digit !="99",
         race == "Nonresident alien") %>%
  group_by(year, award_level, cipcode_6digit) %>%
  summarise(total = sum(awards_6digit))

# Total nonresident alien degrees by degree level (not CIP codes)
deg_totals_immigrants <- Immigrant_degrees_year %>%
  group_by(year, award_level) %>%
  summarise(total = sum(total))

# Total degrees awarded per year, by level 
deg_totals_all <- raw_combined %>%
  filter(cipcode_6digit !="99") %>%
  group_by(year, award_level, cipcode_6digit) %>%
  summarise(total = sum(awards_6digit))%>%
  ungroup() %>%
  group_by(year, award_level) %>%
  summarise(total = sum(total))


# Note: The crosswalk between NSCG degree field codes and CIP codes needs some massaging.
# Some 3-digit NSCG codes are crosswalked to 2 digit CIP codes, others to 3 digit, 4 digit, 5 digit, and 6 digit CIP codes. 
# The following code creates a function that assigns 2 digit through 6 digit CIP codes to the IPEDS data, 
# so that each row can be crosswalked to its correct NSCG code. 

# 1) Normalizing function
normalize_cip <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("[^0-9]", "") %>%     # keep digits only
    str_replace("^0+(?=\\d)", "") %>%     # drop leading zeros (but keep a single 0)
    str_squish() %>%
    na_if("")
}

# 2) Clean crosswalk once
crosswalk_lu <- crosswalk_long %>%
  transmute(
    CIP2_norm = normalize_cip(CIP2),
    NSCG_2023 = `NSCG Code - 2023`
  ) %>%
  filter(!is.na(CIP2_norm)) %>%
  distinct(CIP2_norm, .keep_all = TRUE)   # if duplicates exist, pick first (adjust if needed)


# 3) Build prefixes from the normalized 6-digit code in Immigrant_degree_year
imm_long <- Immigrant_degrees_year %>%
  mutate(
    row_id = row_number(),
    cip6_norm = normalize_cip(cipcode_6digit)
  ) %>%
  transmute(
    row_id, across(everything(), \(x) x),  # keep originals
    p2 = str_sub(cip6_norm, 1, 2),
    p3 = str_sub(cip6_norm, 1, 3),
    p4 = str_sub(cip6_norm, 1, 4),
    p5 = str_sub(cip6_norm, 1, 5),
    p6 = cip6_norm
  ) %>%
  pivot_longer(p2:p6, names_to = "pref", values_to = "CIP2_norm") %>%
  mutate(prefix_len = readr::parse_number(pref)) %>%  # 2..6
  left_join(crosswalk_lu, by = "CIP2_norm") %>%
  arrange(row_id, desc(prefix_len)) %>%
  group_by(row_id) %>%
  summarise(NSCG_Code_2023 = NSCG_2023[match(TRUE, !is.na(NSCG_2023))],
            .groups = "drop")

# 4) Join back to the original data
Immigrant_degrees_year_with_nscg <- Immigrant_degrees_year %>%
  mutate(row_id = row_number()) %>%
  left_join(imm_long, by = "row_id") %>%
  select(-row_id)


######

# CIP 2010 -> CIP 2020 lookup
cip10to20_lu <- cip_10_to_20_crosswalk %>%
  mutate(
    CIP_2010_norm = normalize_cip(CIP_2010),
    CIP_2020_norm = normalize_cip(CIP_2020)
  ) %>%
  filter(!is.na(CIP_2010_norm), !is.na(CIP_2020_norm)) %>%
  distinct(CIP_2010_norm, CIP_2020_norm)   # keep unique pairs


# Mapping 2012–2019 from 2010->2020; keep 2020–2021 as-is
imm_base <- Immigrant_degrees_year %>%
  mutate(
    row_id      = row_number(),
    cip6_norm   = normalize_cip(cipcode_6digit),
    is_2010_vtg = between(year, 2012, 2019)   # adjust if your year col is named differently
  )

imm_2012_2019 <- imm_base %>%
  filter(is_2010_vtg) %>%
  left_join(cip10to20_lu, by = c("cip6_norm" = "CIP_2010_norm")) %>%
  mutate(
    # If a 2010 code doesn't map, fall back to original; otherwise use mapped 2020 code
    cip2020_norm = coalesce(CIP_2020_norm, cip6_norm),
    mapped_from_2010 = !is.na(CIP_2020_norm)
  ) %>%
  select(-CIP_2020_norm)

imm_2020_2021 <- imm_base %>%
  filter(!is_2010_vtg) %>%
  mutate(
    cip2020_norm = cip6_norm,
    mapped_from_2010 = FALSE
  )

imm_all <- bind_rows(imm_2012_2019, imm_2020_2021)

# Longest-prefix match (6→2, whichever is available first) against NSCG lookup built from CIP2
imm_long <- imm_all %>%
  transmute(
    row_id, across(-c(row_id, cip6_norm, is_2010_vtg)),  # keep your original columns
    cip2020_norm,
    p2 = str_sub(cip2020_norm, 1, 2),
    p3 = str_sub(cip2020_norm, 1, 3),
    p4 = str_sub(cip2020_norm, 1, 4),
    p5 = str_sub(cip2020_norm, 1, 5),
    p6 = cip2020_norm
  ) %>%
  pivot_longer(p2:p6, names_to = "pref", values_to = "CIP2_norm") %>%
  mutate(prefix_len = readr::parse_number(pref)) %>%
  left_join(crosswalk_lu, by = "CIP2_norm") %>%
  arrange(row_id, desc(prefix_len)) %>%
  group_by(row_id) %>%
  summarise(NSCG_Code_2023 = NSCG_2023[match(TRUE, !is.na(NSCG_2023))],
            .groups = "drop")

# Attach the NSCG code back to the original rows, leaving us with 2012-2021 data with 2023 NSCG codes. 
Immigrant_degrees_year_with_nscg <- Immigrant_degrees_year %>%
  mutate(row_id = row_number()) %>%
  left_join(imm_long, by = "row_id") %>%
  select(-row_id)

# Apply BA, MA, PhD, and professional degree labels 
Immigrant_degrees_year_with_nscg <- Immigrant_degrees_year_with_nscg %>%
  mutate(degree_level = case_when(award_level == "Bachelor's degree" ~ "Bachelor's",
                                  award_level == "Master's degree" ~ "Master's",
                                  award_level == "Doctor's degree professional practice (starting 2007)" ~ "Professional",
                                  TRUE ~ "Doctoral"))


#############################################################################
# Pull NSCG 2023 data 
#############################################################################

# Note: NSCG categories provided in the NSF crosswalk are 3 digit and vintage 2022.
# In the microdata itself, definitions are vintage 2023 (a handful of 2022 cateogires cat collapsed into others)
# and, for some reason, use a different code nomenclature than the NSF-provided crosswalk, even though they are the exact same categories.
# This section applies two crosswalks to STEM/Non-STEM (Major) and smaller (Minor) degree code buckets 
# for both IPEDS completions and NSCG totals. That gives us completions and records of graduates still in the U.S.
# according to standardized categories, which we can use to calculate a (rough) retention rate by field. 

# Crosswalk from 6 digit NSCG microdata codes to Minor and Major categories (FOR NSCG MICRODATA)
microdata_crosswalk <- read_xlsx("microdata_categories_crosswalk.xlsx")
# Crosswalk from 3 digit NSCG codes to Minor and Major categories (FOR IPEDS DATA)
categories_crosswalk <- read_xlsx("categories_crosswalk.xlsx")

# Load 2023 NSCG microdata
nscg_23 <- read.csv("epcg23.csv")

# Filter NSCG data for just the relevant graduates
International_graduates <- nscg_23 %>%
  filter(FNVSATP == 3, # first came on student visa
         MRDG >0 & MRDG < 4, # Most recent degree either BA, MA, PhD
         MRST_TOGA == 099, # Most recent degree earned in the US
         MRYR>2011 & MRYR <2022) %>% # Graduated from 2012 through 2021
  mutate(degree_level = case_when(MRDG == 1 ~ "Bachelor's",
                                  MRDG == 2 ~ "Master's", 
                                  MRDG == 3 ~ "Doctoral")) 

# Graduates still here by STEM/non-STEM and degree level
International_graduate_by_field <- International_graduates %>%
  group_by(N2MRMED, degree_level) %>%
  summarise(total = sum(WTSURVY)) %>%
  left_join(microdata_crosswalk, by = c("N2MRMED" = "Microdata_code")) %>%
  ungroup() %>%
  group_by(`Major cat`, degree_level) %>%
  summarise(still_here = sum(total))

# Graduates still here by more specific categories and degree level 
International_graduate_by_minor_cat <- International_graduates %>%
  group_by(N2MRMED, degree_level) %>%
  summarise(total = sum(WTSURVY)) %>%
  left_join(microdata_crosswalk, by = c("N2MRMED" = "Microdata_code")) %>%
  ungroup() %>%
  group_by(`Minor cat`, degree_level) %>%
  summarise(still_here = sum(total))

#############################################################################
# Retention calculations
#############################################################################

# IPEDS completions by nonresident aliens by STEM/non-STEM and degree level 
Nonresident_degrees_awarded_subject <- Immigrant_degrees_year_with_nscg %>%
  filter(degree_level != "Professional") %>% # filter out professional degrees
  group_by(NSCG_Code_2023, degree_level) %>%
  summarise(total = sum(total)) %>%
  left_join(categories_crosswalk, by = c("NSCG_Code_2023" = "NSCG_code")) %>%
  group_by(`Major cat`, degree_level) %>%
  summarise(degrees_awarded = sum(total))

# IPEDS completions by nonresident aliens by more specific categories and degree level
Nonresident_degrees_awarded_subject_minor_cat <- Immigrant_degrees_year_with_nscg %>%
  filter(degree_level != "Professional") %>%
  group_by(NSCG_Code_2023, degree_level) %>%
  summarise(total = sum(total)) %>%
  left_join(categories_crosswalk, by = c("NSCG_Code_2023" = "NSCG_code")) %>%
  group_by(`Minor cat`, degree_level) %>%
  summarise(degrees_awarded = sum(total))

Nonresident_degrees_awarded_subject_minor_cat_yr <- Immigrant_degrees_year_with_nscg %>%
  filter(degree_level != "Professional") %>%
  group_by(NSCG_Code_2023, degree_level, year) %>%
  summarise(total = sum(total)) %>%
  left_join(categories_crosswalk, by = c("NSCG_Code_2023" = "NSCG_code")) %>%
  group_by(`Minor cat`, degree_level, year) %>%
  summarise(degrees_awarded = sum(total))

# Retention by STEM/Non-STEM and degree level
immigrant_retention_categories <- left_join(International_graduate_by_field, Nonresident_degrees_awarded_subject, by = c("Major cat" = "Major cat", "degree_level" = "degree_level")) %>%
  mutate(retention_rate = still_here/degrees_awarded)

# Retention by more specific degree fields and degree level
immigrant_retention_categories_minor_cat <- left_join(International_graduate_by_minor_cat, Nonresident_degrees_awarded_subject_minor_cat, by = c("Minor cat" = "Minor cat", "degree_level" = "degree_level")) %>%
  mutate(retention_rate = still_here/degrees_awarded)

# Calculate retention rates indifferent to degree levels
grads_by_STEM <- Nonresident_degrees_awarded_subject %>% group_by(`Major cat`) %>% summarise(degrees = sum(degrees_awarded)) %>% filter(!is.na(`Major cat`))
grads_by_categories <- Nonresident_degrees_awarded_subject_minor_cat %>% group_by(`Minor cat`) %>% summarise(degrees = sum(degrees_awarded)) %>% filter(!is.na(`Minor cat`))

still_here_by_STEM <- International_graduate_by_field %>% group_by(`Major cat`) %>% summarise(still_here = sum(still_here)) %>% filter(!is.na(`Major cat`))
still_here_by_categories <- International_graduate_by_minor_cat %>% group_by(`Minor cat`) %>% summarise(still_here = sum(still_here)) %>% filter(!is.na(`Minor cat`))

#############################################################################
# Best estimates (use these, with caution)
#############################################################################

# Retention across STEM/Non-STEM, indifferent to degree levels
retention_STEM <- left_join(grads_by_STEM, still_here_by_STEM, by = c("Major cat" = "Major cat")) %>% 
  mutate(retention_rate = still_here/degrees)
# Retention across categories, indifferent to degree levels
retention_categories <- left_join(grads_by_categories, still_here_by_categories, by = c("Minor cat" = "Minor cat")) %>% 
  mutate(retention_rate = still_here/degrees)



