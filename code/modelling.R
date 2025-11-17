library(rio)
library(dplyr)
library(corrplot)
library(tidyverse)
library(car)
library(modelsummary)

data <- import('/Users/alexander/Documents/benites-unsworth/ukResultsData.csv')

# 1) Keep only numeric vars and drop the specified columns
drop_cols <- c("prev_con_2024", "prev_lab_2024", "con_lost_2024")

num_df <- data %>%
  select(where(is.numeric), -any_of(drop_cols))

# 2) Compute Pearson (lower) and Spearman (upper)
pear <- cor(num_df, use = "pairwise.complete.obs", method = "pearson")
spear <- cor(num_df, use = "pairwise.complete.obs", method = "spearman")

# 3) Combine into one matrix: lower = Pearson, upper = Spearman
mixed_cor <- pear
mixed_cor[upper.tri(mixed_cor)] <- spear[upper.tri(spear)]
diag(mixed_cor) <- 1

# 4) (optional) Round for printing
print(round(mixed_cor, 3))

# 5) (optional) Visualize
corrplot(mixed_cor, method = "color", tl.cex = 0.7, number.cex = 0.6,
         addCoef.col = "black", # write coefficients
         mar = c(0,0,1,0), title = "Lower: Pearson  |  Upper: Spearman")


#### OLS
# Model 2: Demography & Education
model1 <- lm(DIF_Cons ~ con_2019 + pct_age_18_24 + pct_age_65_plus + pct_no_qualifications, 
             data = data)

# Core specification
model2 <- lm(
  DIF_Cons ~ con_2019 + pct_born_outside_uk + hanretty_leave + pct_no_religion,
  data = data
)


vif(model1)
vif(model2)


# Create a list of models
models <- list(
  "Demography & Education"= model1,
  "Culture & Brexit"      = model2
)

# Export regression table to Word
modelsummary(
  models,
  output = "regression_models.docx",   # saves file in your working directory
  stars = TRUE,                        # adds significance stars
  gof_omit = "IC|Log|Adj|F"            # cleaner table (omit AIC/BIC etc.)
)

