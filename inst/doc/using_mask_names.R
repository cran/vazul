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
set.seed(123)
efa_orig <-
    williams |> 
    select(SexUnres_1:InvChild_2_r) |>
    factanal(factors = 5, rotation = "varimax")

# Get the loadings of the EFA on the original data

efa_orig |> 
    loadings() |> 
    print(cutoff = 0.3, sort = TRUE)


