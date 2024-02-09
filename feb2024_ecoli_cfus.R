# follow up data February 2024. find CFUs of E.coli in BHI broth and in milk after 72hrs. 
setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2022_fall_exp_leale/data/ecoli_CFUs")
library(dplyr)
library(ggplot2)
library(lme4)#needed for anovas, t-tests later
library(car)
library(vegan) #needed for emmeans
library(emmeans)


library(readxl)
data <- readxl::read_xlsx(path="feb2024_ecoli.xlsx", sheet = "results") 
# calculate CFUs/mL 
data <- data %>% 
  mutate(bhi_cfu6 = bhi_6*10^6/0.05,
         bhi_cfu7 = bhi_7*10^7/0.05,
         milk_cfu5 = milk_5*10^5/0.05,
         milk_cfu6 = milk_6*10^6/0.05 ) %>%
  mutate(bhiCFU = (bhi_cfu6 + bhi_cfu7)/2,
         milkCFU = (milk_cfu5 + milk_cfu6)/2)


# milk vs. ecoli 72 hrs
data %>%
  ggplot(mapping = aes(x = treatment, y = milk_pH)) +
  geom_point(position = position_jitter(width = 0.2)) +
  scale_x_discrete(labels = c("ecoli" = "E. coli + milk", 
                              "negative" = "milk")) +
  theme_bw(base_size = 14) +
  labs(y = "pH 72hrs")


lm_ph <- lm(milk_pH ~ treatment, data = data) 
Anova(lm_ph, type = 3) 
summary(lm_ph)




# summary stats milk
summary_stats <- data %>%
  filter(treatment == "ecoli") %>%
  group_by(treatment) %>%
  summarize(
    mean_bhiCFU = mean(bhiCFU, na.rm = TRUE),
    se_bhiCFU = sd(bhiCFU, na.rm = TRUE) / sqrt(n()),
    mean_milkCFU = mean(milkCFU, na.rm = TRUE),
    se_milkCFU = sd(milkCFU, na.rm = TRUE) / sqrt(n()),
  )

# ecoli in bhi
data %>% 
  filter(treatment == "ecoli")%>%
  ggplot(aes(x = treatment, y = bhiCFU)) +
  geom_point(position = position_jitter(width = 0.02)) +
  scale_x_discrete(labels = c("ecoli" = "E. coli in BHI")) +
  theme_bw(base_size = 14) +
  labs(y = "E. coli CFU/mL", x="") +
  geom_point(data = summary_stats, aes(y = mean_bhiCFU), color = "purple", size = 2) +
  geom_errorbar(data = summary_stats, 
                aes(y = mean_bhiCFU, 
                    ymin = mean_bhiCFU - se_bhiCFU, 
                    ymax = mean_bhiCFU + se_bhiCFU), 
                color = "purple", width = 0.1)


# ecoli in milk plot
data %>% 
  filter(treatment == "ecoli") %>%
  ggplot(aes(x = treatment, y = milkCFU)) +
  geom_point(position = position_jitter(width = 0.02)) +
  scale_x_discrete(labels = c("ecoli" = "E. coli in milk")) +
  theme_bw(base_size = 14) +
  labs(y = "E. coli CFU/mL", x = "") +  # corrected the 'y' label
  geom_point(data = summary_stats, aes(y = mean_milkCFU), color = "purple", size = 2) +
  geom_errorbar(data = summary_stats, 
                aes(y = mean_milkCFU, 
                    ymin = mean_milkCFU - se_milkCFU, 
                    ymax = mean_milkCFU + se_milkCFU), 
                color = "purple", width = 0.1)



         