# analyses ideas - simulated data -----------------------------------------

library(lme4)
library(vcd)
library(epitools)
library(reshape2)
library(tidyverse)
library(ggalluvial)

# chi sq test -------------------------------------------------------------

#simulate data

set.seed(123)
survey_data_sim <- data.frame(method = rep(c("Drone", "Spotlight"), each = 200),
                              behav = c(sample(c("Changed", "NoChange"), 200, replace = TRUE, prob = c(0.1, 0.9)),
                                           sample(c("Changed", "NoChange"), 200, replace = TRUE, prob = c(0.8, 0.2))))


# cont. table
tab <- table(survey_data_sim$method, survey_data_sim$behav)
tab

# chi-square test - both types
chisq.test(tab)

#Since p-value is tiny, reject H0.
#This means the distribution of behaviors (Changed vs NoChange) depends on the survey method.
#Looking at the table:
#Drone: 16/200 = 8% changed
#Spotlight: 45/200 = 22.5% changed 
#Animals are ~3× more likely to change behavior under Spotlight surveys.

assocstats(tab)

# The chi-square test showed a significant association between survey method and behavioral 
# response (??² = 192.3, df = 1, p < 0.001). A
# nimals were significantly more likely to change behavior during spotlight 
# surveys compared to drone surveys. The effect size was large (Cramér???s V = 0.69), 
# indicating a strong relationship between survey method and behavioral response.

# Cramér???s V (0.693): ranges from 0 (no association) to 1 (perfect association).
#0.693 = very strong association.
#Phi (0.693) is identical to Cramér???s V in a 2×2 table.
#contingency Coefficient (0.57) also indicates a strong relationship.

vcd::oddsratio(tab) #this is log odds = to get perc do exp(value) - same as in epitools
epitools::oddsratio(tab)

# That means the odds of changing behavior under Spotlight are only 2.7% of the odds under Drone.
# Put differently: the odds of change under Drone are about 37 times higher than under Spotlight (1 ÷ 0.027 ??? 36.7).

# When Drone is the reference:
# OR for Spotlight = 0.027 ??? this just means the odds of "not changing" (or the way the comparison was set up) 
#is smaller for Spotlight.
# You should read it carefully: it reflects the way the reference group is set, not the intuitive magnitude.
# When you flip the reference to Spotlight:
#OR for Drone = 0.027 ??? now it???s clear: the odds of changing under Drone are ~37× lower than Spotlight.
#flip to get positive odds ratio

epitools::oddsratio(tab, rev = "rows")

# Animals were significantly more likely to change behavior during 
# Spotlight surveys compared to Drone surveys. The odds of behavioral change under Drone 
# were about 37 times lower than under Spotlight (OR = 0.027, 95% CI: 0.014???0.049, p < 0.001).

#by survey type

drone_tab <- survey_data_sim%>%filter(method == 'Drone')%>%table()
chisq.test(drone_tab)

# Since p < 0.05 (actually way smaller), you reject the null hypothesis.
# that means behavioral responses are not evenly distributed.
# In fact, animals under Drone surveys were much more likely to show no behavioral change (184) than to change (16).

spot_tab <- survey_data_sim%>%filter(method == 'Spotlight')%>%table()
chisq.test(spot_tab)


# mixed model -------------------------------------------------------------

set.seed(123)

# Parameters
n_sites <- 5
n_surveys <- 20             # total surveys across all sites
n_animals_per_survey <- 10  # observations per survey

# Create survey-level data
survey_data <- data.frame(
  site = sample(paste0("Site", 1:n_sites), n_surveys, replace = TRUE),
  survey = paste0("Survey", 1:n_surveys),
  method = sample(c("Drone", "Spotlight"), n_surveys, replace = TRUE)
)

# Expand to animal-level observations
sim_data <- survey_data %>%
  slice(rep(1:n(), each = n_animals_per_survey)) %>%
  mutate(
    animal = 1:nrow(.)
  )

# Simulate adverse behavior (1 = frozen/adverse, 0 = normal)
# Random effect for site and survey
site_effect <- rnorm(n_sites, 0, 0.5)
survey_effect <- rnorm(n_surveys, 0, 0.3)

sim_data <- sim_data %>%
  mutate(
    site_num = as.numeric(factor(site)),
    survey_num = as.numeric(factor(survey)),
    logit_p = qlogis(ifelse(method=="Drone", 0.05, 0.4)) +
      site_effect[site_num] +
      survey_effect[survey_num],
    adverse = rbinom(n(), 1, plogis(logit_p))
  )

mod <- glmer(adverse ~ method + (1 | site) + (1 | survey),
             data = sim_data,
             family = binomial)

summary(mod)


# simple glm model --------------------------------------------------------

set.seed(123)
# Parameters
n_surveys <- 400

# Random number of animals per survey, e.g., 5???15
animals_per_survey <- sample(5:15, n_surveys, replace = TRUE)

# Create survey-level data
survey_data <- data.frame(
  method = sample(c("Drone", "Spotlight"), n_surveys, replace = TRUE),
  site = paste0("site", sample(1:50, n_surveys, replace = TRUE)), # 50 sites
  date = sample(seq(as.Date('2025-01-01'), as.Date('2025-06-30'), by='day'), n_surveys, replace = TRUE))

# Expand to animal-level observations
sim_data_mixed <- survey_data %>%
  mutate(n_animals = animals_per_survey) %>%
  slice(rep(1:n(), n_animals)) %>%
  mutate(animal = 1:nrow(.))%>%
  mutate(adverse = rbinom(n(), 1, ifelse(method=="Drone", 0.1, 0.7)))

mod <- glm(adverse ~ method, data = sim_data_mixed, family = binomial)

summary(mod)

# Intercept = log-odds of adverse behavior (frozen) for the reference group, which is Drone.
# log-odds = -2.2225
# Odds = exp(-2.2225) ??? 0.108 ??? probability = 0.108 / (1+0.108) ??? 0.097, or ~9% adverse behavior under Drone.
# methodSpotlight = difference in log-odds for Spotlight vs Drone.
# 3.2136 ??? exp(3.2136) ??? 24.87 ??? the odds of adverse behavior under Spotlight are ~25× higher than under Drone.
#Corresponding probability = 24.87 / (1+24.87) ??? 0.961 ??? very high probability under Spotlight, consistent with your simulation.
# Both coefficients are highly significant (p < 2e-16) ??? extremely strong evidence that method affects adverse behavior.

# Odds ratio
exp(coef(mod))

# Interpretation:
# Reference (Drone) ??? baseline odds of freezing ~0.108 (~9% probability)
# Spotlight ??? odds are ~25 times higher than Drone
# Animals are much more likely to exhibit adverse behavior (frozen) under Spotlight surveys.

# Animals were significantly more likely to show adverse behavior (frozen) 
# under Spotlight surveys compared to Drone surveys (OR = 24.9, p < 0.001). 
# The estimated probability of freezing was ~9% under Drone and ~96% under Spotlight.


# transition probability --------------------------------------------------

set.seed(123)

n_surveys <- 100           # number of drone surveys
n_animals_per_survey <- sample(5:15, n_surveys, replace = TRUE)

# Create survey-level data
drone_data <- data.frame(
  survey = paste0("Survey", 1:n_surveys),
  site = paste0("Site", sample(1:20, n_surveys, replace = TRUE))
) %>%
  mutate(n_animals = n_animals_per_survey) %>%
  slice(rep(1:n(), n_animals)) %>%
  mutate(animal = 1:n())

# Simulate pre-observation behavior
drone_data <- drone_data %>%
  mutate(pre_behav = "normal")

# Simulate post-observation behavior
# Assume probability of change is 0.1 (10%)
# If behavior changed, probability that it is adverse (freeze) = 0.7
drone_data <- drone_data %>%
  mutate(
    changed = rbinom(n(), 1, 0.1),
    post_behav = ifelse(
      changed == 0,
      "normal",
      ifelse(runif(n()) < 0.7, "frozen", "normal")  # 70% of changed become frozen
    )
  )

transition_summary <- drone_data %>%
  group_by(pre_behav) %>%
  summarise(
    total = n(),
    frozen = sum(post_behav == "frozen"),
    normal = sum(post_behav == "normal"),
    prob_frozen = frozen / total,
    prob_normal = normal / total
  )

transition_summary

transition_counts <- table(drone_data$pre_behav, drone_data$post_behav)

transition_probs <- prop.table(transition_counts, margin = 1)  # divide by row sums
transition_probs


tm_long <- melt(transition_probs)
colnames(tm_long) <- c("Pre", "Post", "Probability")

ggplot(tm_long, aes(x=Post, y=Pre, fill=Probability)) +
  geom_tile() +
  geom_text(aes(label=sprintf("%.2f", Probability)), color="white") +
  scale_fill_gradient(low="blue", high="red") +
  theme_minimal() +
  labs(title="Transition Probability Matrix", y="Pre-behavior", x="Post-behavior")

# transition prob - multiple behav ----------------------------------------

set.seed(123)

n_surveys <- 200
n_animals_per_survey <- sample(1:9, n_surveys, replace = TRUE)

# Create survey-level data
survey_data <- data.frame(
  survey = paste0("Survey", 1:n_surveys),
  site = paste0("Site", sample(1:20, n_surveys, replace = TRUE))) %>%
  mutate(n_animals = n_animals_per_survey) %>%
  slice(rep(1:n(), n_animals)) %>%
  mutate(animal = 1:n())

# Pre-behavior states
pre_states <- c("feeding", "moving", "frozen", "other")

# Assign pre-behavior randomly
survey_data <- survey_data %>%
  mutate(pre_behav = sample(pre_states, n(), replace = TRUE, prob = c(0.4, 0.3, 0.1, 0.2)))

# Simulate post-behavior transitions
survey_data <- survey_data %>%
  rowwise() %>%
  mutate(post_behav = case_when(
    pre_behav == "feeding" ~ sample(pre_states, 1, prob = c(0.5, 0.3, 0.15, 0.05)),
    pre_behav == "moving"  ~ sample(pre_states, 1, prob = c(0.2, 0.5, 0.25, 0.05)),
    pre_behav == "frozen"  ~ sample(pre_states, 1, prob = c(0.05, 0.1, 0.8, 0.05)),
    pre_behav == "other"   ~ sample(pre_states, 1, prob = c(0.25, 0.25, 0.3, 0.2)))) %>%
  ungroup()

transition_counts <- table(survey_data$pre_behav, survey_data$post_behav)
transition_probs <- prop.table(transition_counts, margin = 1)  # row-wise probabilities

transition_probs

tm_long <- melt(transition_probs)
colnames(tm_long) <- c("Pre", "Post", "Probability")

ggplot(tm_long, aes(x=Post, y=Pre, fill=Probability)) +
  geom_tile() +
  geom_text(aes(label=sprintf("%.2f", Probability)), color="white") +
  scale_fill_gradient(low="gold", high="purple") +
  theme_minimal() +
  labs(title="Transition Probability Matrix", y="Pre-behavior", x="Post-behavior")

# pattern analysis --------------------------------------------------------

set.seed(123)

n_surveys <- 100
n_animals_per_survey <- sample(5:15, n_surveys, replace = TRUE)

# Create survey-level data
drone_data <- data.frame(
  survey = paste0("Survey", 1:n_surveys),
  site = paste0("Site", sample(1:20, n_surveys, replace = TRUE))
) %>%
  mutate(n_animals = n_animals_per_survey) %>%
  slice(rep(1:n(), n_animals)) %>%
  mutate(animal = 1:n())

# Pre-behavior
pre_states <- c("feeding", "moving", "frozen", "other")
drone_data <- drone_data %>%
  mutate(pre_behav = sample(pre_states, n(), replace = TRUE, prob = c(0.4, 0.3, 0.1, 0.2)))

# Whether drone was noticed
drone_data <- drone_data %>%
  mutate(noticed = rbinom(n(), 1, 0.5))  # 50% chance animal noticed drone

# Post-behavior depends on pre-behavior AND if drone was noticed
drone_data <- drone_data %>%
  rowwise() %>%
  mutate(post_behav = case_when(
    noticed == 1 & pre_behav == "feeding" ~ sample(c("frozen","feeding","moving","other"), 1, prob=c(0.6,0.2,0.15,0.05)),
    noticed == 1 & pre_behav == "moving"  ~ sample(c("frozen","moving","feeding","other"), 1, prob=c(0.7,0.15,0.1,0.05)),
    noticed == 1 & pre_behav == "frozen"  ~ "frozen",  # stays frozen
    noticed == 1 & pre_behav == "other"   ~ sample(c("frozen","feeding","moving","other"), 1, prob=c(0.5,0.2,0.2,0.1)),
    noticed == 0                           ~ pre_behav  # if not noticed, behavior stays the same
  )) %>%
  ungroup()

pattern_summary <- drone_data %>%
  mutate(adverse = ifelse(post_behav == "frozen", 1, 0),
         resumed_natural = ifelse(post_behav == pre_behav, 1, 0)) %>%
  group_by(noticed) %>%
  summarise(
    n = n(),
    prop_adverse = mean(adverse),
    prop_resumed = mean(resumed_natural)
  )

pattern_summary

pattern_long <- pattern_summary %>%
  pivot_longer(cols = c(prop_adverse, prop_resumed), names_to="behavior_type", values_to="probability")

ggplot(pattern_long, aes(x=factor(noticed), y=probability, fill=behavior_type)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete(labels=c("0"="Not noticed", "1"="Noticed")) +
  ylab("Probability") +
  xlab("Drone noticed") +
  scale_fill_manual(values=c("prop_adverse"="red","prop_resumed"="green3"), labels=c("Adverse (frozen)","Resumed natural")) +
  theme_minimal()

#model

drone_data <- drone_data %>%
  mutate(adverse = ifelse(post_behav == "frozen", 1, 0),
         pre_fac = as.factor(pre_behav))

change_mod<-glm(adverse ~ noticed + pre_fac, data=drone_data, family=binomial)

summary(change_mod)

#viz

drone_data <- drone_data %>%
  mutate(
    pre_behav = factor(pre_behav, levels=c("feeding","moving","frozen","other")),
    noticed = factor(noticed, levels=c(0,1), labels=c("Not noticed","Noticed")),
    post_behav = factor(post_behav, levels=c("feeding","moving","frozen","other"))
  )

# Summarize counts for each flow
flow_data <- drone_data %>%
  group_by(pre_behav, noticed, post_behav) %>%
  summarise(count = n(), .groups="drop")

ggplot(flow_data,
       aes(axis1 = pre_behav, axis2 = noticed, axis3 = post_behav,
           y = count)) +
  geom_alluvium(aes(fill = post_behav), width = 1/12) +
  geom_stratum(width = 1/10, fill = "white", color = "darkgrey") +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum)),
            angle = 90) +
  scale_x_discrete(limits = c("Pre-behavior", "Drone noticed?", "Post-behavior"), expand = c(.05, .05)) +
  scale_fill_manual(values = c("feeding"="lightgreen", "moving"="mediumseagreen",
                               "other"="darkgreen", "frozen"="red")) +
  theme_minimal() +
  labs(title="Behavioral flow after drone exposure",
       y="Number of observations",
       x="")
