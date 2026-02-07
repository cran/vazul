## -----------------------------------------------------------------------------
#| label: setup
#| message: false

library(vazul)
library(dplyr)


## -----------------------------------------------------------------------------
# Create a simple treatment vector
treatment <- c("control", "treatment", "control", "treatment", "control")

# Mask the labels
set.seed(1037)
masked_treatment <- mask_labels(treatment)
masked_treatment


## -----------------------------------------------------------------------------
set.seed(456)
mask_labels(treatment, prefix = "group_")


## -----------------------------------------------------------------------------
set.seed(789)
mask_labels(treatment, prefix = "condition_")


## -----------------------------------------------------------------------------
# Create a factor vector
ecology <- factor(c("Desperate", "Hopeful", "Desperate", "Hopeful"))

set.seed(123)
masked_ecology <- mask_labels(ecology)
masked_ecology
class(masked_ecology)


## -----------------------------------------------------------------------------
data(williams)

set.seed(42)
williams$ecology_masked <- mask_labels(williams$ecology)

# Compare original and masked values
head(williams[c("subject", "ecology", "ecology_masked")], 10)


## -----------------------------------------------------------------------------
df <- data.frame(
  treatment = c("control", "intervention", "control", "intervention"),
  outcome = c("success", "failure", "success", "failure"),
  score = c(85, 92, 78, 88)
)

set.seed(123)
result <- mask_variables(df, c("treatment", "outcome"))
result


## -----------------------------------------------------------------------------
df2 <- data.frame(
  pre_condition = c("A", "B", "C", "A"),
  post_condition = c("B", "A", "A", "C"),
  score = c(1, 2, 3, 4)
)

set.seed(456)
result_shared <- mask_variables(df2, c("pre_condition", "post_condition"),
                                .across_variables = TRUE)
result_shared


## -----------------------------------------------------------------------------
set.seed(789)
mask_variables(df, where(is.character))


## -----------------------------------------------------------------------------
# Numeric data
set.seed(123)
numbers <- 1:10
scramble_values(numbers)


## -----------------------------------------------------------------------------
# Character data
set.seed(456)
letters_vec <- letters[1:5]
scramble_values(letters_vec)


## -----------------------------------------------------------------------------
# Factor data
set.seed(789)
conditions <- factor(c("A", "B", "C", "A", "B"))
scramble_values(conditions)


## -----------------------------------------------------------------------------
set.seed(1037)
original <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
scrambled <- scramble_values(original)

# The scrambled vector has the same values, just in a different order 
scrambled

# Same values, different order
sort(original) == sort(scrambled)

# Same frequency distribution
table(original)
table(scrambled)


## -----------------------------------------------------------------------------
df <- data.frame(
  x = 1:6,
  y = letters[1:6],
  group = c("A", "A", "A", "B", "B", "B")
)

set.seed(123)
scramble_variables(df, c("x", "y"))


## -----------------------------------------------------------------------------
set.seed(456)
scramble_variables(df, c("x", "y"), .together = TRUE)


## -----------------------------------------------------------------------------
set.seed(2)
scramble_variables(df, "x", .groups = "group")


## -----------------------------------------------------------------------------
set.seed(100)
scramble_variables(df, c("x", "y"), .groups = "group", .together = TRUE)


## -----------------------------------------------------------------------------
data(williams)

# Scramble age and ecology within gender groups
set.seed(42)
williams_scrambled <- williams |>
  scramble_variables(c("age", "ecology"), .groups = "gender")

# Check that values are preserved within groups
williams |>
  group_by(gender) |>
  summarise(mean_age = mean(age, na.rm = TRUE))

williams_scrambled |>
  group_by(gender) |>
  summarise(mean_age = mean(age, na.rm = TRUE))


## -----------------------------------------------------------------------------
df <- data.frame(
  item1 = c(1, 4, 7),
  item2 = c(2, 5, 8),
  item3 = c(3, 6, 9),
  id = 1:3
)

set.seed(123)
result <- scramble_variables(df, c("item1", "item2", "item3"), .byrow = TRUE)
result


## -----------------------------------------------------------------------------
df2 <- data.frame(
  day_1 = c(1, 4, 7),
  day_2 = c(2, 5, 8),
  day_3 = c(3, 6, 9),
  score_a = c(10, 40, 70),
  score_b = c(20, 50, 80),
  id = 1:3
)

set.seed(2)
result2 <- scramble_variables(df2, starts_with("day_"), starts_with("score_"), .byrow = TRUE)
result2


## -----------------------------------------------------------------------------
set.seed(42)
result3 <- df2 |>
  scramble_variables(starts_with("day_"), .byrow = TRUE) |>
  scramble_variables(starts_with("score_"), .byrow = TRUE)
result3


## -----------------------------------------------------------------------------
# Vector with NA values
x <- c("A", "B", NA, "A", NA, "C")

set.seed(123)
masked_x <- mask_labels(x)
masked_x

# NA positions are preserved
which(is.na(masked_x))


## -----------------------------------------------------------------------------
x_all_na <- c(NA_character_, NA_character_, NA_character_)
mask_labels(x_all_na)


## -----------------------------------------------------------------------------
x_with_empty <- c("A", "", "B", "", "C")

set.seed(456)
masked_with_empty <- mask_labels(x_with_empty)
masked_with_empty

# Empty strings get their own masked label
unique(masked_with_empty)


## -----------------------------------------------------------------------------
data(marp)
dim(marp)

# Example: Scramble religiosity scores within countries
set.seed(42)
marp_blinded <- marp |>
  scramble_variables(starts_with("rel_"), .groups = "country")

# Original and scrambled have same country-level means
original_means <- marp |>
  group_by(country) |>
  summarise(rel_1_mean = mean(rel_1, na.rm = TRUE), .groups = "drop")

scrambled_means <- marp_blinded |>
  group_by(country) |>
  summarise(rel_1_mean = mean(rel_1, na.rm = TRUE), .groups = "drop")

all.equal(original_means$rel_1_mean, scrambled_means$rel_1_mean)


## -----------------------------------------------------------------------------
data(williams)
dim(williams)

# Example: Mask the ecology condition for blind analysis
set.seed(42)
williams_blinded <- williams |>
  mask_variables("ecology")

# Analysts can work with masked conditions
williams_blinded |>
  group_by(ecology) |>
  summarise(
    n = n(),
    mean_impulsivity = mean(Impuls_1, na.rm = TRUE),
    .groups = "drop"
  )

