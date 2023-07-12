#v1: started in fall 2023. continued in June 2023 (colours, full data set)


setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2022_fall_exp/data/ph")
library(dplyr)
library(ggplot2)
library(lme4) #needed for anovas, t-tests later

library(readxl)
ph_data <- readxl::read_xlsx(path="fall_2022_ph.xlsx", sheet = "forR") 
# ph_data <- ph_data %>% filter(!is.na(pH)) %>% 
#   filter(name != "M_8")  #replicate M8 lost early in transfers, therefore remove throughout
milk <- filter(ph_data, treatment == "milk") # separate milk out before you remove it
ph_data$treatment <- factor(ph_data$treatment, 
                            levels = c("con", "mix", "top", "bot","inv", "ecoli","rec", "milk" )) #define ordering later for legends

#see what simply milk and milk with ecoli did
ph_data %>%
  filter(treatment == "ecoli"| treatment == "milk" |  treatment == "con" ) %>%
  ggplot(mapping = aes(x = transfer, y= ph, colour=treatment))+
  scale_colour_manual(values=c("grey", "coral2", "pink2"))+ #colours for invasion
  geom_point(colour="white") +
  geom_line(aes(group = code)) + #will connect dots based on "code"
  # ylim(3.05, 3.45) +
  theme_bw() 

ph_data2 <- filter(ph_data
       , treatment != "milk"#remove milk negative controls
       , treatment != "ecoli"#remove milk negative controls
)

#make groupings
top <- filter(ph_data2, treatment == "top") 
bottom <- filter(ph_data2, treatment == "bot")
mixed <- filter(ph_data2, treatment == "mix")
control <- filter(ph_data2, treatment == "con")
invasion <- filter(ph_data2, treatment == "inv")
recover <- filter(ph_data2, treatment == "rec")
spatial <- rbind(top, bottom, mixed, control)
invade <- rbind(invasion, recover, control)



# connect points overtime
# ph_data2 %>%
# spatial %>%
invade %>%
  ggplot(mapping = aes(x = transfer, y= ph, colour=treatment))+
  # scale_colour_manual(values=c("grey", "violet", "deepskyblue", "coral2", "chartreuse3", "orange"))+ #colours for all together
  # scale_colour_manual(values=c("grey", "violet", "deepskyblue", "coral2"))+ #colours for spatial
  scale_colour_manual(values=c("grey", "chartreuse3", "orange"))+ #colours for invasion
  geom_point(colour="white") +
  geom_line(aes(group = code)) + #will connect dots based on "code"
  # annotate("text", x = 2, y = 3.875, size = 7, label = "B") +
  ylim(3.05, 3.45) +
  theme_bw() 
# +
# facet_wrap(~treatment, scale="free")


ph_data2 %>% 
  filter(treatment == "rec" |treatment ==  "inv") %>%
  filter(transfer > 12) %>%
  ggplot(mapping = aes(x = transfer, y= ph, colour=treatment))+
  scale_colour_manual(values=c("violet", "orange"))+
  geom_point() +
  geom_line(aes(group = code)) + #will connect dots based on "code"
  # annotate("text", x = 2, y = 3.875, size = 7, label = "B") +
  # ylim(3.1, 3.4) +
  theme_bw() 


### avgs

ph_summary <- ph_data2 %>%
  group_by(treatment, transfer) %>%
  summarize_at(c("ph"), funs(mean, sd), na.rm =TRUE)
ph_summary$se = ph_summary$sd/sqrt(6) #b/c 6 replicates each

ph_summary %>% 
  # filter(treatment != "inv") %>% #spatial
  # filter(treatment != "rec") %>% #spatial
  filter(treatment != "mix") %>% #invade
  filter(treatment != "top") %>% #invade
  filter(treatment != "bot") %>% #invade
  ggplot(mapping = aes(x = transfer, y= mean, colour=treatment))+
  # scale_colour_manual(values=c("grey", "violet", "deepskyblue", "coral2", "chartreuse3", "orange"))+ #colours for all together
  # scale_colour_manual(values=c("grey", "violet", "deepskyblue", "coral2"))+ #colours for spatial
  scale_colour_manual(values=c("grey", "chartreuse3", "orange"))+ #colours for invasion
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05)) +
  annotate(geom="text", x=6, y=3.125, label="bars = std.error")+
  annotate(geom="text", x=6, y=3.1, label="replication = 6")+
  labs(y = "average pH") +
  ylim(3.05, 3.45) +
  theme_bw() 











###### test top vs. bottom at t6
topbottom <- readxl::read_xlsx(path="fall_2022_ph.xlsx", sheet = "topbottomtest") 

topbottom %>% 
  # filter(transfer == "8")%>%
  ggplot(mapping = aes(x = level, y= ph, colour=level))+
  geom_boxplot() +
  geom_point() +
  ylim(3.1, 3.45) +
  facet_wrap(~transfer*treatment, scale="free")

con2 <- filter(topbottom, treatment == "control")
t.test(con2$ph~con2$level)

top2 <- filter(topbottom, treatment == "top")
t.test(top2$ph~top2$level)


model.ph <- lm(ph ~ level + treatment, data = topbottom)
Anova(model.ph, type = 3) 
summary(model.ph)


# 'tomato1', 'deepskyblue2','olivedrab3', 'mediumpurple2'
# full=red, low=blue, med=green,  synt=violet 

model.ph <- lm(pH ~ treatment * transfer, data = ph_data2) # interaction included
Anova(model.ph, type = 3) # interaction is significant
summary(model.ph)


### avgs
ph_summary <- 
  filter(ph_data2) %>% 
  group_by(treatment, transfer) %>% 
  summarise(avg_ph = mean(ph), sd = sd(ph), se = sd(ph)/sqrt(6))  #6 is number samples (not accurate for med8, because only 7 sampels there)
# View(ph_summary)


# ### avgs
# ph_summary_test <- 
#   filter(ph_data, transfer != "2") %>% 
#   group_by(treatment, transfer) %>% 
#   summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
# View(ph_summary_test)


ph_summary %>% 
  filter(treatment != "ecoli") %>%
  filter(treatment != "milk") %>%
  ggplot(mapping = aes(x = transfer, y= avg_ph, colour=treatment))+
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=avg_ph-se, ymax=avg_ph+se), width=.2,
                position=position_dodge(0.05)) +
  annotate(geom="text", x=6, y=3.125, label="bars = std.error")+
  annotate(geom="text", x=6, y=3.1, label="replication = 6")+
  labs(y = "average pH") +
  # ylim(3.35, 3.9) +
  theme_bw() 




