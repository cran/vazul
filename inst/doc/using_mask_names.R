## -----------------------------------------------------------------------------
#| label: setup
#| message: false

library(vazul)
library(dplyr)
library(stats)



## -----------------------------------------------------------------------------
data("williams", package = "vazul")

head(williams)
glimpse(williams)



## -----------------------------------------------------------------------------
set.seed(84)

# Sample 5 random letters for the 5 variable groups
random_prefixes <- paste0(sample(LETTERS, 5), "_")

masked_williams <-
    williams |> 
    mask_names(starts_with("SexUnres"), prefix = random_prefixes[1]) |>
    mask_names(starts_with("Impul"), prefix = random_prefixes[2]) |>
    mask_names(starts_with("Opport"), prefix = random_prefixes[3]) |>
    mask_names(starts_with("InvEdu"), prefix = random_prefixes[4]) |>
    mask_names(starts_with("InvChild"), prefix = random_prefixes[5])

# Show the randomized prefixes used (but not which corresponds to which)
sort(unique(sub("_.*", "_", grep("^[A-Z]_", names(masked_williams), value = TRUE))))


## -----------------------------------------------------------------------------

set.seed(123)
efa_blind <-
    masked_williams |> 
    select(matches("^[A-Z]_")) |>
    factanal(factors = 5, rotation = "varimax")
    
# Get the loadings of the EFA on the masked data
efa_blind |> 
    loadings() |> 
    print(cutoff = 0.3, sort = TRUE)



## -----------------------------------------------------------------------------
set.seed(84)
masked_with_suffix <-
    williams |>
    mask_names(starts_with("SexUnres"), prefix = "C_", keep_suffixes = "_r")

masked_with_suffix |>
    select(matches("^C_")) |>
    names()


## -----------------------------------------------------------------------------
#| message: false
library(tidyr)

wide_demo <- data.frame(
  id = 1:4,
  exp_pre = c(10, 12, 9, 11),
  exp_post = c(15, 14, 13, 16),
  ctl_pre = c(8, 9, 10, 7),
  ctl_post = c(9, 10, 11, 8)
)
wide_demo


## -----------------------------------------------------------------------------
# 1) Reshape to long format, splitting the column names into
#    a `condition` and a `time` variable
long_demo <- wide_demo |>
  pivot_longer(
    cols = -id,
    names_to = c("condition", "time"),
    names_sep = "_"
  )
long_demo


## -----------------------------------------------------------------------------
# 2) Mask the condition and time variables. Each variable keeps its own,
#    internally consistent mapping (every "exp" becomes the same masked
#    label, every "pre" becomes the same masked label, and so on).
set.seed(2024)
long_masked <- long_demo |>
  mask_variables(condition, time)

long_masked |>
  count(condition, time)


## -----------------------------------------------------------------------------
# 3) Reshape back to wide format if a wide layout is needed for analysis
long_masked |>
  unite(masked_name, condition, time) |>
  pivot_wider(names_from = masked_name, values_from = value)


## -----------------------------------------------------------------------------
set.seed(123)
efa_orig <-
    williams |> 
    select(SexUnres_1:InvChild_2_r) |>
    factanal(factors = 5, rotation = "varimax")

# Get the loadings of the EFA on the original data

efa_orig |> 
    loadings() |> 
    print(cutoff = 0.3, sort = TRUE)


