library(rio)
library(dplyr)
library(tidyr)
library(stringr)

data <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/candidate-level-results-general-election-04-07-2024.csv')
notional <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/candidate-level-results-notional-general-election-12-12-2019.csv')
census <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/all_data.csv')
brexit <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/BES-2024-General-Election-results-file-v1.0.dta')

# Geographic code, name, country name, electorate, valid vote, result, party abbreviation, vote share
uk <- data %>% select(13,19,20,26,30,33,41,49)

# First fix column names
names(uk) <- c(
  "country_name",
  "constituency_name",
  "constituency_geographic_code",
  "electorate",
  "valid_vote_count",
  "result_summary",
  "party_abbr",
  "vote_share"
)

# 2) Parties to keep
keep <- c(
  "Con","Lab","LD","Green","RUK","WPB","SDP",
  "SNP","Alba",
  "PC",
  "APNI","DUP","UUP","SDLP","SF",
  "TUSC","UKIP","REU"
)

# 3) Collapse to keep vs Other, aggregate per constituency–party
uk_collapsed <- uk %>%
  mutate(
    party_abbr = party_abbr %>% as.character() %>% str_squish(),
    party_abbr = if_else(is.na(party_abbr) | party_abbr == "", "Other", party_abbr),
    party_collapsed = if_else(party_abbr %in% keep, party_abbr, "Other"),
    vote_share = as.numeric(vote_share)  # already 0–1
  ) %>%
  group_by(
    country_name, constituency_name, constituency_geographic_code,
    electorate, valid_vote_count, result_summary, party_collapsed
  ) %>%
  summarise(vote_share = sum(vote_share, na.rm = TRUE), .groups = "drop")

# 4) Pivot wider: one column per kept party + Other
uk_collapsed <- uk_collapsed %>%
  pivot_wider(
    id_cols = c(
      country_name, constituency_name, constituency_geographic_code,
      electorate, valid_vote_count, result_summary
    ),
    names_from  = party_collapsed,
    values_from = vote_share,
    values_fill = 0
  )

# 5) Turnout
uk_collapsed$turnout <- uk_collapsed$valid_vote_count / uk_collapsed$electorate


# 6) Constituency from Con in past election
uk_collapsed <- uk_collapsed %>%
  mutate(
    prev_con = if_else(
      str_detect(
        str_squish(result_summary),
        regex("\\bCon\\s+hold\\b|\\bgain\\s+from\\s+Con\\b", ignore_case = TRUE)
      ),
      1L, 0L, missing = 0L
    )
  )

# 6) Constituency from Lab in past election
uk_collapsed <- uk_collapsed %>%
  mutate(
    prev_lab = if_else(
      str_detect(
        str_squish(result_summary),
        regex("\\bLab\\s+hold\\b|\\bgain\\s+from\\s+Lab\\b", ignore_case = TRUE)
      ),
      1L, 0L, missing = 0L
    )
  )

# 7) Constituency lost by Cons
uk_collapsed <- uk_collapsed %>%
  mutate(
    con_lost = if_else(
      str_detect(
        str_squish(result_summary),
        regex("gain\\s+from\\s+Con", ignore_case = TRUE)
      ),
      1L, 0L, missing = 0L
    )
  )



#### NOTIONAL RESULTS FOR 2019 
notional <- notional %>% select(20,26,30,41,49)

# Names
names(notional) <- c(
  "constituency_geographic_code",
  "electorate",
  "valid_vote_count",
  "party_abbr",
  "vote_share"
)

# Collapse notional 2019 (on 2024 boundaries) to kept parties + Other
notional_collapsed <- notional %>%
  mutate(
    party_abbr = party_abbr %>% as.character() %>% str_squish(),
    # Recode Brexit Party -> RUK for comparability with 2024
    party_abbr = case_when(
      party_abbr %in% c("BRX", "BXP", "Brexit") ~ "RUK",
      TRUE ~ party_abbr
    ),
    # Blank/NA -> Other
    party_abbr = if_else(is.na(party_abbr) | party_abbr == "", "Other", party_abbr),
    party_collapsed = if_else(party_abbr %in% keep, party_abbr, "Other"),
    vote_share = as.numeric(vote_share)  # already 0–1
  ) %>%
  group_by(constituency_geographic_code, party_collapsed) %>%
  summarise(vote_share = sum(vote_share, na.rm = TRUE), .groups = "drop")

# Pivot wider: one column per kept party + Other
notional_wide <- notional_collapsed %>%
  pivot_wider(
    id_cols = constituency_geographic_code,
    names_from  = party_collapsed,
    values_from = vote_share,
    values_fill = 0
  )

# Turnout from 2019
notional$turnout <- notional$valid_vote_count/notional$electorate


### Combining datasets:
# 2024
ukResults <- uk_collapsed %>% select(1:3,6,7,10,12,26:29)
names(ukResults)[4:ncol(ukResults)] <- paste0(names(ukResults)[4:ncol(ukResults)], "_2024")

# 2019
notional2019 <- notional_wide %>% select(1,2,5)
names(notional2019)[2:ncol(notional2019)] <- paste0(names(notional2019)[2:ncol(notional2019)], "_2019")

# Turnout
notional_turnout <- notional %>%
  select(constituency_geographic_code, turnout) %>%
  distinct()

# Names
colnames(notional_turnout) <- c("constituency_geographic_code","turnout_2019")
notional2019 <- merge(notional2019, notional_turnout, by = "constituency_geographic_code")

# Final:
ukResults <- merge(ukResults, notional2019, by = "constituency_geographic_code")




### Census data:
keep_vars <- c(
  "Percentage of 16+ year olds with higher education qualifications",
  "Percentage of 16+ year olds with no educational qualifications",
  "Percentage of households that are owner-occupiers",
  "Percentage of people aged 16+ identifying as LGB+",
  "Percentage of people aged 18-24",
  "Percentage of people aged 65+",
  "Percentage of people born outside the UK",
  "Percentage of people who are White",
  "Percentage of people who have no religion",
  "Percentage of people who are Christian"
)

census_subset <- census[census$var_name %in% keep_vars, ]

census_long <- census_subset %>%
  select(1, 2, 5) %>%                                   # ons_code, var_name, var_pc
  distinct() %>%                                        # guard against accidental duplicates
  rename(
    constituency_geographic_code = ons_code,
    variable_name = var_name,
    variable_value = var_pc
  )

census_wide <- census_long %>%
  pivot_wider(
    names_from  = variable_name,
    values_from = variable_value
  )

# Combine data:
ukResults <- merge(ukResults, census_wide, by = "constituency_geographic_code")


#### BREXIT
brexitLeave <- brexit %>% select("ONSConstID","HanrettyLeave")
colnames(brexitLeave) <- c("constituency_geographic_code", "HanrettyLeave")
brexitLeave$HanrettyLeave <- brexitLeave$HanrettyLeave/100
 
# Combine data:
ukResults <- merge(ukResults, brexitLeave, by = "constituency_geographic_code", all.x = TRUE)

names(ukResults) <- c(
  "constituency_geographic_code",
  "country_name",
  "constituency_name",
  "result_summary_2024",
  "con_2024",
  "lab_2024",
  "ruk_2024",
  "turnout_2024",
  "prev_con_2024",
  "prev_lab_2024",
  "con_lost_2024",
  "con_2019",
  "lab_2019",
  "turnout_2019",
  "pct_age_18_24",
  "pct_age_65_plus",
  "pct_born_outside_uk",
  "pct_christian",
  "pct_no_religion",
  "pct_owner_occupiers",
  "pct_no_qualifications",
  "pct_higher_education",
  "pct_white",
  "pct_lgb_plus",
  "hanretty_leave"
)

# Differences
ukResults$DIF_Cons <- ukResults$con_2024 - ukResults$con_2019
ukResults$DIF_Turnout <- ukResults$turnout_2024 - ukResults$turnout_2019


