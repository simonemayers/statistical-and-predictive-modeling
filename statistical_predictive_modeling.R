
# Define parameters
n <- 3      # number of coin flips
k <- 2      # number of heads desired
p <- 0.5    # probability of getting heads in each flip (since the coin is fair)

# Calculate probability using binomial distribution
probability <- dbinom(k, size = n, prob = p)
probability
    
    

# Define parameters
n <- 20     # number of coin flips
k <- 8      # number of heads desired
p <- 0.5    # probability of getting heads in each flip (since the coin is fair)

# Calculate probability using binomial distribution
probability <- dbinom(k, size = n, prob = p)
probability


# Define parameters
n <- 20     # number of coin flips
k <- 4      # minimum number of heads of interest
p <- 0.5    # probability of getting heads in each flip (since the coin is fair)

# Calculate probability of getting fewer than 4 heads
probability_less_than_4 <- pbinom(k - 1, size = n, prob = p)

# Calculate probability of getting at least 4 heads
probability_at_least_4 <- 1 - probability_less_than_4
probability_at_least_4


library(readr)
df1 <- readr::read_csv("./meningitis.csv", col_names = FALSE)
column_names <- c("ID", "Headache", "Fever", "Vomiting", "Meningitis")
colnames(df1) <- column_names

knitr::kable(df1, caption = 'Data for a predicting Meningitis.', align = "lrrrr")

# Total number of records
total_records <- nrow(df1)

# Count where Vomiting is true
vomiting_true <- sum(df1$Vomiting == "true")

# Calculate probability
p_vomiting_true <- vomiting_true / total_records
p_vomiting_true


# Count where Headache is false
headache_false <- sum(df1$Headache == "false")

# Calculate probability
p_headache_false <- headache_false / total_records
p_headache_false

# Count where Headache is true and Vomiting is false
headache_true_vomiting_false <- sum(df1$Headache == "true" & df1$Vomiting == "false")

# Calculate joint probability
p_headache_true_vomiting_false <- headache_true_vomiting_false / total_records
p_headache_true_vomiting_false


# Load necessary libraries
library(readr)
library(dplyr)

# Convert to lowercase for consistency
df1 <- df1 %>% mutate(across(c(Headache, Fever, Vomiting, Meningitis), tolower))

# Calculate P(Vomiting = false | Headache = true)
# Subset data where Headache is true
headache_true_subset <- df1 %>% filter(Headache == "true")
headache_true_count <- nrow(headache_true_subset)

# Count where Vomiting is false within subset where Headache is true
if (headache_true_count > 0) {
  vomiting_false_given_headache_true <- sum(headache_true_subset$Vomiting == "false")
  p_vomiting_false_given_headache_true <- vomiting_false_given_headache_true / headache_true_count
} else {
  p_vomiting_false_given_headache_true <- NA  # Handle zero denominator case
}

# Output the result
p_vomiting_false_given_headache_true



# Subset where Fever is true and Vomiting is false
fever_true_vomiting_false_subset <- df1 %>% filter(Fever == "true" & Vomiting == "false")
fever_true_vomiting_false_count <- nrow(fever_true_vomiting_false_subset)

# Count where Meningitis is true within the subset
meningitis_given_fever_true_vomiting_false <- sum(fever_true_vomiting_false_subset$Meningitis == "true")

# Calculate conditional probability
p_meningitis_given_fever_true_vomiting_false <- meningitis_given_fever_true_vomiting_false / fever_true_vomiting_false_count
p_meningitis_given_fever_true_vomiting_false


library(readr)
df2 <- readr::read_csv("./naive-bayes.csv", col_names = FALSE)
column_names <- c("ID", "SS-IN", "SED-IN", "COND-IN", "SS-OUT", "SED-OUT", "COND-OUT", "STATUS")
colnames(df2) <- column_names

knitr::kable(df2, caption = 'Data for a naive Bayes model.', align = "lrrrrrrr")

# Load necessary libraries
library(readr)
library(dplyr)

# Calculate mean and standard deviation for each feature by STATUS
model_params <- df2 %>%
  group_by(STATUS) %>%
  summarise(
    mean_ss_in = mean(`SS-IN`),
    sd_ss_in = sd(`SS-IN`),
    mean_sed_in = mean(`SED-IN`),
    sd_sed_in = sd(`SED-IN`),
    mean_cond_in = mean(`COND-IN`),
    sd_cond_in = sd(`COND-IN`),
    mean_ss_out = mean(`SS-OUT`),
    sd_ss_out = sd(`SS-OUT`),
    mean_sed_out = mean(`SED-OUT`),
    sd_sed_out = sd(`SED-OUT`),
    mean_cond_out = mean(`COND-OUT`),
    sd_cond_out = sd(`COND-OUT`)
  )
print(model_params)

# Function to calculate likelihood for each STATUS given query values
calculate_likelihood <- function(query, params) {
  params %>%
    rowwise() %>%
    mutate(
      likelihood = dnorm(query["SS-IN"], mean_ss_in, sd_ss_in) *
                   dnorm(query["SED-IN"], mean_sed_in, sd_sed_in) *
                   dnorm(query["COND-IN"], mean_cond_in, sd_cond_in) *
                   dnorm(query["SS-OUT"], mean_ss_out, sd_ss_out) *
                   dnorm(query["SED-OUT"], mean_sed_out, sd_sed_out) *
                   dnorm(query["COND-OUT"], mean_cond_out, sd_cond_out)
    ) %>%
    select(STATUS, likelihood)
}



# Load necessary libraries
library(readr)
library(dplyr)


# Define the query with specified feature values
query <- c(`SS-IN` = 222, `SED-IN` = 4.5, `COND-IN` = 1518, `SS-OUT` = 74, `SED-OUT` = 0.25, `COND-OUT` = 1642)

# Calculate likelihoods for the query
likelihoods <- calculate_likelihood(query, model_params)
print(likelihoods)

# Find the STATUS with the maximum likelihood
predicted_status <- likelihoods %>%
  filter(likelihood == max(likelihood)) %>%
  pull(STATUS)

predicted_status


library(readr)
df3 <- readr::read_csv("./insurance-policy.csv", col_names = FALSE)
column_names <- c("ID", "Occupation", "Gender", "Age", "Policy Type", "Pref Channel")
colnames(df3) <- column_names

knitr::kable(df3, caption = 'Data for a insurance policy model.', align = "llllll")

# Load necessary libraries
library(readr)
library(dplyr)

# Transform Age into three equal-frequency bins
df3 <- df3 %>%
  mutate(
    Age_Group = case_when(
      ntile(Age, 3) == 1 ~ "young",
      ntile(Age, 3) == 2 ~ "middle-aged",
      ntile(Age, 3) == 3 ~ "mature"
    )
  )

# View the transformed dataset
print(df3)


# Load necessary libraries
library(readr)
library(dplyr)

# Exclude the ID column as it is not predictive
df3 <- df3 %>% select(-ID)

# Calculate prior probabilities for each Pref Channel category
prior_probs <- df3 %>%
  count(`Pref Channel`) %>%
  mutate(prior_prob = n / sum(n)) %>%
  select(`Pref Channel`, prior_prob)
print(prior_probs)

# Calculate conditional probabilities for Occupation given Pref Channel
occupation_probs <- df3 %>%
  group_by(`Pref Channel`, Occupation) %>%
  summarise(count = n()) %>%
  mutate(cond_prob = count / sum(count)) %>%
  select(`Pref Channel`, Occupation, cond_prob)
print(occupation_probs)

# Calculate conditional probabilities for Gender given Pref Channel
gender_probs <- df3 %>%
  group_by(`Pref Channel`, Gender) %>%
  summarise(count = n()) %>%
  mutate(cond_prob = count / sum(count)) %>%
  select(`Pref Channel`, Gender, cond_prob)
print(gender_probs)

# Calculate conditional probabilities for Age given Pref Channel
age_probs <- df3 %>%
  group_by(`Pref Channel`, Age) %>%
  summarise(count = n()) %>%
  mutate(cond_prob = count / sum(count)) %>%
  select(`Pref Channel`, Age, cond_prob)
print(age_probs)

# Calculate conditional probabilities for Policy Type given Pref Channel
policy_type_probs <- df3 %>%
  group_by(`Pref Channel`, `Policy Type`) %>%
  summarise(count = n()) %>%
  mutate(cond_prob = count / sum(count)) %>%
  select(`Pref Channel`, `Policy Type`, cond_prob)
print(policy_type_probs)


# Load necessary libraries
library(readr)
library(dplyr)

# Calculate conditional probabilities for Gender = female given Pref Channel
gender_probs <- df3 %>%
  filter(Gender == "female") %>%
  group_by(`Pref Channel`) %>%
  summarise(count = n()) %>%
  mutate(cond_prob_gender = count / sum(count)) %>%
  select(`Pref Channel`, cond_prob_gender)
print(gender_probs)

# For simplicity, we assume Age has already been transformed (e.g., "young" if 30 falls under this bin)
# Calculate conditional probabilities for Age group (e.g., "young") given Pref Channel
age_probs <- df3 %>%
  filter(Age == 30) %>% # Replace with bin if transformed
  group_by(`Pref Channel`) %>%
  summarise(count = n()) %>%
  mutate(cond_prob_age = count / sum(count)) %>%
  select(`Pref Channel`, cond_prob_age)
print(age_probs)

# Calculate conditional probabilities for Policy Type = planA given Pref Channel
policy_type_probs <- df3 %>%
  filter(`Policy Type` == "planA") %>%
  group_by(`Pref Channel`) %>%
  summarise(count = n()) %>%
  mutate(cond_prob_policy = count / sum(count)) %>%
  select(`Pref Channel`, cond_prob_policy)
print(policy_type_probs)

# Merge prior and conditional probabilities
posteriors <- prior_probs %>%
  left_join(gender_probs, by = "Pref Channel") %>%
  left_join(age_probs, by = "Pref Channel") %>%
  left_join(policy_type_probs, by = "Pref Channel") %>%
  mutate(
    posterior = prior_prob * cond_prob_gender * cond_prob_age * cond_prob_policy
  )

# Display the posterior probabilities
print(posteriors)

# Find the Pref Channel with the maximum posterior probability
predicted_pref_channel <- posteriors %>%
  filter(posterior == max(posterior)) %>%
  pull(`Pref Channel`)

predicted_pref_channel


library(readr)
df4 <- readr::read_csv("./entertainment.csv", col_names = FALSE)

knitr::kable(df4, col.names = NULL, caption = 'Word-document counts for the entertainment dataset.', align = "rrrrrr")

library(readr)
df5 <- readr::read_csv("./education.csv", col_names = FALSE)

knitr::kable(df5, col.names = NULL, caption = 'Word-document counts for the education dataset.', align = "rrrrrr")

# Load necessary libraries
library(readr)
library(dplyr)

# Convert columns from the second onward to numeric directly by column index
for (i in 2:ncol(df4)) {
  df4[[i]] <- as.numeric(df4[[i]])
}
for (i in 2:ncol(df5)) {
  df5[[i]] <- as.numeric(df5[[i]])
}

# Prior probabilities
prior_entertainment <- 700 / 1000
prior_education <- 300 / 1000

# Total documents for each category
total_entertainment_docs <- 700
total_education_docs <- 300

# Define a small smoothing value to replace any NAs (similar to Laplace smoothing)
smoothing_value <- 1

# Calculate conditional probabilities using column indices, with NA handling
# If a count is missing (NA), we replace it with the smoothing value
p_machine_entertainment <- ifelse(is.na(df4[[2]][1]), smoothing_value / total_entertainment_docs, df4[[2]][1] / total_entertainment_docs)
p_machine_education <- ifelse(is.na(df5[[2]][1]), smoothing_value / total_education_docs, df5[[2]][1] / total_education_docs)

p_learning_entertainment <- ifelse(is.na(df4[[3]][1]), smoothing_value / total_entertainment_docs, df4[[3]][1] / total_entertainment_docs)
p_learning_education <- ifelse(is.na(df5[[3]][1]), smoothing_value / total_education_docs, df5[[3]][1] / total_education_docs)

p_is_entertainment <- ifelse(is.na(df4[[4]][1]), smoothing_value / total_entertainment_docs, df4[[4]][1] / total_entertainment_docs)
p_is_education <- ifelse(is.na(df5[[4]][1]), smoothing_value / total_education_docs, df5[[4]][1] / total_education_docs)

p_fun_entertainment <- ifelse(is.na(df4[[5]][1]), smoothing_value / total_entertainment_docs, df4[[5]][1] / total_entertainment_docs)
p_fun_education <- ifelse(is.na(df5[[5]][1]), smoothing_value / total_education_docs, df5[[5]][1] / total_education_docs)

# Posterior probability for entertainment
posterior_entertainment <- prior_entertainment * 
                           p_machine_entertainment * 
                           p_learning_entertainment * 
                           p_is_entertainment * 
                           p_fun_entertainment

# Posterior probability for education
posterior_education <- prior_education * 
                       p_machine_education * 
                       p_learning_education * 
                       p_is_education * 
                       p_fun_education

# Predict the topic with the highest posterior probability
if (!is.na(posterior_entertainment) && !is.na(posterior_education)) {
  if (posterior_entertainment > posterior_education) {
    prediction <- "entertainment"
  } else {
    prediction <- "education"
  }
} else {
  prediction <- "Unable to make a prediction due to missing values"
}

print(prediction)


# Calculate conditional probabilities for each word in "christmas family fun"

# For "christmas"
p_christmas_entertainment <- ifelse(is.na(df4[[2]][1]), smoothing_value / total_entertainment_docs, df4[[2]][1] / total_entertainment_docs)
p_christmas_education <- ifelse(is.na(df5[[2]][1]), smoothing_value / total_education_docs, df5[[2]][1] / total_education_docs)

# For "family"
p_family_entertainment <- ifelse(is.na(df4[[3]][1]), smoothing_value / total_entertainment_docs, df4[[3]][1] / total_entertainment_docs)
p_family_education <- ifelse(is.na(df5[[3]][1]), smoothing_value / total_education_docs, df5[[3]][1] / total_education_docs)

# For "fun"
p_fun_entertainment <- ifelse(is.na(df4[[5]][1]), smoothing_value / total_entertainment_docs, df4[[5]][1] / total_entertainment_docs)
p_fun_education <- ifelse(is.na(df5[[5]][1]), smoothing_value / total_education_docs, df5[[5]][1] / total_education_docs)

# Calculate posterior probability for each target level
posterior_entertainment <- prior_entertainment * 
                           p_christmas_entertainment * 
                           p_family_entertainment * 
                           p_fun_entertainment

posterior_education <- prior_education * 
                       p_christmas_education * 
                       p_family_education * 
                       p_fun_education

# Predict the topic with the highest posterior probability
if (!is.na(posterior_entertainment) && !is.na(posterior_education)) {
  if (posterior_entertainment > posterior_education) {
    prediction <- "entertainment"
  } else {
    prediction <- "education"
  }
} else {
  prediction <- "Unable to make a prediction due to missing values"
}

print(prediction)

# Laplace smoothing parameters
k <- 10
vocabulary_size <- 6

# Calculate conditional probabilities for each word in "christmas family fun" with Laplace smoothing

# For "christmas"
p_christmas_entertainment <- (ifelse(is.na(df4[[2]][1]), 0, df4[[2]][1]) + k) / (total_entertainment_docs + k * vocabulary_size)
p_christmas_education <- (ifelse(is.na(df5[[2]][1]), 0, df5[[2]][1]) + k) / (total_education_docs + k * vocabulary_size)

# For "family"
p_family_entertainment <- (ifelse(is.na(df4[[3]][1]), 0, df4[[3]][1]) + k) / (total_entertainment_docs + k * vocabulary_size)
p_family_education <- (ifelse(is.na(df5[[3]][1]), 0, df5[[3]][1]) + k) / (total_education_docs + k * vocabulary_size)

# For "fun"
p_fun_entertainment <- (ifelse(is.na(df4[[5]][1]), 0, df4[[5]][1]) + k) / (total_entertainment_docs + k * vocabulary_size)
p_fun_education <- (ifelse(is.na(df5[[5]][1]), 0, df5[[5]][1]) + k) / (total_education_docs + k * vocabulary_size)

# Calculate posterior probability for each target level
posterior_entertainment <- prior_entertainment * 
                           p_christmas_entertainment * 
                           p_family_entertainment * 
                           p_fun_entertainment

posterior_education <- prior_education * 
                       p_christmas_education * 
                       p_family_education * 
                       p_fun_education

# Predict the topic with the highest posterior probability
if (!is.na(posterior_entertainment) && !is.na(posterior_education)) {
  if (posterior_entertainment > posterior_education) {
    prediction <- "entertainment"
  } else {
    prediction <- "education"
  }
} else {
  prediction <- "Unable to make a prediction due to missing values"
}

print(prediction)







