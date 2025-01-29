# Simulate data for COVID-19 deaths in the Netherlands with identical format as the Global
# Burden of Disease (GBD) data

set.seed(2024)

age_groups <- c("< 5 years", "5-9 years", "10-14 years",
          "15-19 years", "20-24 years", "25-29 years",
         "30-34 years", "35-39 years", "40-44 years",
         "45-49 years", "50-54 years", "55-59 years",
         "60-64 years", "65-69 years", "70-74 years",
         "75-79 years", "80-84 years", "85-89 years",
         "90-94 years", "95+ years")

measures <- c("Deaths", "Prevalence", "Incidence", "DALYs (Disability-Adjusted Life Years)")

metrics <- c("Number", "Percent", "Rate")

genders <-  c("Males", "Females")

n_rows <- length(age_groups) * length(genders) * length(metrics) * length(measures)

age_group_idx <- match(age_groups, age_groups)

pop_by_age <- 1/sum(age_group_idx) * c(age_group_idx[1:(length(age_groups)/2)],
                          age_group_idx[(length(age_groups)/2):1])

# Base rate of COVID-19 cases plus linear age effect
prevalence_rate <- 0.05 + age_group_idx * 0.01

incidence_rate <- 0.005 + age_group_idx * 0.01

pop_size <- 17500000

number_of_cases <- pop_size * pop_by_age * rnorm(1, 1, 0.1) * prevalence_rate

number_of_new_cases <- pop_size * pop_by_age * rnorm(1, 1, 0.1) * incidence_rate

# Base DALY weight 0.2 times duration 0.25 (3 months)
daly_weights <- pop_size * pop_by_age * rnorm(1, 1, 0.1) * 0.2 * 0.25

# Base rate of COVID-19 deaths plus exponential age effect
number_of_deaths <- number_of_cases * 0.0001 * exp(0.4 *age_group_idx) * rnorm(1, 1, 0.1)

covid19nl <- data.frame(
  Measure = rep(measures, each = length(age_groups) * length(metrics) * length(genders)),
  Metric = rep(rep(metrics, each = length(genders)), length(age_groups) * length(measures)),
  Cause = "COVID-19",
  Location = "Netherlands",
  Age = rep(rep(age_groups, each = length(metrics) * length(genders)), length(measures)),
  Sex = rep(genders, length(age_groups) * length(metrics) * length(measures)),
  Year = 2021,
  Value = NA,
  Upper = NA,
  Lower = NA
)

covid19nl$Value[covid19nl$Metric == "Number" & covid19nl$Measure == "Deaths"] <- number_of_deaths
covid19nl$Value[covid19nl$Metric == "Rate" & covid19nl$Measure == "Deaths"] <- number_of_deaths / pop_size * 100000
covid19nl$Value[covid19nl$Metric == "Percent" & covid19nl$Measure == "Deaths"] <- number_of_deaths / sum(number_of_deaths) * 100

covid19nl$Value[covid19nl$Metric == "Number" & covid19nl$Measure == "Prevalence"] <- number_of_cases
covid19nl$Value[covid19nl$Metric == "Rate" & covid19nl$Measure == "Prevalence"] <- number_of_cases / pop_size * 100000
covid19nl$Value[covid19nl$Metric == "Percent" & covid19nl$Measure == "Prevalence"] <- number_of_cases / sum(number_of_cases) * 100

covid19nl$Value[covid19nl$Metric == "Number" & covid19nl$Measure == "Incidence"] <- number_of_new_cases
covid19nl$Value[covid19nl$Metric == "Rate" & covid19nl$Measure == "Incidence"] <- number_of_new_cases / pop_size * 100000
covid19nl$Value[covid19nl$Metric == "Percent" & covid19nl$Measure == "Incidence"] <- number_of_new_cases / sum(number_of_cases) * 100

covid19nl$Value[covid19nl$Metric == "Number" & covid19nl$Measure == "DALYs (Disability-Adjusted Life Years)"] <- daly_weights
covid19nl$Value[covid19nl$Metric == "Rate" & covid19nl$Measure == "DALYs (Disability-Adjusted Life Years)"] <- daly_weights / pop_size * 100000
covid19nl$Value[covid19nl$Metric == "Percent" & covid19nl$Measure == "DALYs (Disability-Adjusted Life Years)"] <- daly_weights / sum(daly_weights) * 100

covid19nl$Upper <- covid19nl$Value * 1.1
covid19nl$Lower <- covid19nl$Value * 0.9

usethis::use_data(covid19nl, overwrite = TRUE)
