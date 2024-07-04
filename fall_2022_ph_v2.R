#v2: february 2024 - removing plotting/stats related to spatial experiment (not written for thesis chapter)

setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2022_fall_exp_leale/data/ph")
library(dplyr)
library(ggplot2)
library(lme4)#needed for anovas, t-tests later
library(car)
library(vegan) #needed for emmeans
library(emmeans)


library(readxl)
ph_data <- readxl::read_xlsx(path="fall_2022_ph.xlsx", sheet = "forR") 
ph_data$treatment <- factor(ph_data$treatment, 
                            levels = c("con", "mix", "top", "bot","inv", "ecoli","rec", "milk" )) #define ordering later for legends


ph_data2 <- filter(ph_data # remove milk, plan ecoli, and spatial experiment data 
       , treatment != "milk"
       , treatment != "ecoli"
       , treatment != "top" 
       ,treatment != "bot"
       , treatment != "mix"
)


### mean pH overtime
ph_summary <- ph_data2 %>%
  group_by(treatment, transfer) %>%
  summarize_at(c("ph"), list(mean = mean, sd = sd), na.rm = TRUE)
ph_summary$se = ph_summary$sd/sqrt(6) #b/c 6 replicates each


ph_summary %>% 
  ggplot(mapping = aes(x = transfer, y= mean, colour=treatment))+
  scale_x_continuous(breaks = c(2,4,6,8,10,12,14,16, 18))+
  scale_colour_manual(values = c("grey", "purple", "orange"),
                      labels = c("control", "introduction", "recovery")) +
  geom_line(size=1.2) +
  geom_point(size=1.2) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5,
                position=position_dodge(0.05)) +
  labs(y = "mean pH") +
  ylim(3.0, 3.5) +
  theme_bw(base_size = 14) 



###### STATISTICAL TESTS #######
## post-hoc tests
ph_data2$transfer<- as.factor(ph_data2$transfer)
ph_data3 <- ph_data2 %>% 
  filter(treatment != "rec") # need to remove for anova b/c it only has data at 3 time points

lm_ph <- lm(ph ~ treatment*transfer, data = ph_data3) 
Anova(lm_ph, type = 3) 
summary(lm_ph)
emmeans(lm_ph, pairwise ~ treatment|transfer) 
emmeans(lm_ph, pairwise ~ transfer|treatment) 


