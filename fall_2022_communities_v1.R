# adapted from Leale et al. 2023 FEMS Microbiology Ecology
# see "read.me" tab of "fall2022_META.csv" and "output_barcoded_mabisi_2plates_0.95_clustering.csv" for variable descriptions

setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2022_fall_exp_leale/data/communites")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr) 
library(vegan) #needed for permanova

CLUST <-read.csv("output_barcoded_mabisi_2plates_0.95_clustering.csv", sep=(",")) 
CLUST$cluster <- gsub(".*;","",CLUST$cluster) # summarise long complicated name
CLUST <- CLUST %>% rename(PLATE_bc = sample)
CLUST$PLATE_bc <- gsub("barcode","bc",CLUST$PLATE_bc) #take "barcode" and replace with "bc"
CLUST$PLATE_bc <- gsub(".summary.txt","",CLUST$PLATE_bc) #take ".summary.txt" and replace with nothing
CLUST <- CLUST[,2:4]
#dot - pick up everything, * is multiple of, up to ; then replace before ; with nothing
# https://regex101.com/

#inner_join [add information of community, transfer]
META <-read.csv("fall2022_META.csv", sep=(",")) #descriptor variables for each barcode 
META$PLATE_bc <- paste(META$PLATE, META$barcode, sep= "_")
META$SAMPLE <- gsub("s","", META$SAMPLE) #remove "s" since can then later filter out controls easier
# there are no reads for F1.1, neg, or t0
merged <- merge(CLUST, META, by = "PLATE_bc", all = TRUE) 
# I am loosing t0 here and F1.1 with aggregate
merged <- aggregate(.~PLATE_bc + cluster + TRANSFER+ COMMUNITY+ REP +SPACE +SAMPLE+ PLATE+ barcode, 
                    merged, FUN = sum)  #need to combine when multiple clusters of same species >1%

# merged <- aggregate(.~barcode + cluster + SAMPLE + TRANSFER + COMMUNITY, merged, FUN = sum)  #need to combine when multiple clusters of same species >1%
merged <- merged %>% filter(abundance > 0.01) #filter only clusters of abundance over 1% 
merged$TRANSFER <- factor(merged$TRANSFER, levels = c("t00","t01", "t09", "t12", "t19")) # makes plotting better later
merged$COMMUNITY <- factor(merged$COMMUNITY, levels = c("con", "top", "bot", "mix", "inv", "rec")) # makes plotting better later
merged$SPACE <- factor(merged$SPACE, levels = c("all", "up", "low")) # was relavant for data from another experiment

########## BARPLOTS #################
reduced <- merged %>%
  filter(PLATE == "plate2") %>% # remove plate 1 
  filter(COMMUNITY != "bot") %>% # remove different experiment data
  filter(COMMUNITY != "top") %>% # remove different experiment data
  filter(COMMUNITY != "mix") %>% # remove different experiment data
  filter(SPACE == "all") # # remove different experiment data
  #### above filtering already removes control samples 
  # filter(barcode != "bc93") %>% # F1.1 from Leale et al. 2023 (FEMS Microbiology Ecology)
  # filter(barcode != "bc94") %>% # M+ mabisi mock community
  # filter(barcode != "bc95") %>% # zymo control
  # filter(barcode != "bc96")  # negative

# make already here data set of just control samples  
controls <- merged %>%
  # filter(PLATE == "plate2") %>% # remove plate 1 
  filter(barcode %in% c("bc93", "bc94", "bc95", "bc96"))


# reduced$replicate <- substr(reduced$replicate, 2, 2) #remove letter from replicate to just have number
reduced$abundance <- as.numeric(reduced$abundance) #rbind made it a character, so need to go back to numeric

# see how many unique clusters there are
unique <- unique(reduced$cluster) 
#TAKE THIS into .csv file and manually chose colour
# https://chichacha.netlify.app/post/2018-12-09-having-bit-of-party-with-material-colour-palette_files/figure-html/materialColourCheatSheet-1.png
# write.csv(unique,"~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2021_evolution_exp/data/bioinformatics/clustered//unique.csv", row.names = FALSE)

## need to make list that also includes cluster name with colour
# if just list of colours, then they are reordered to the clusters found at each plot (i.e., changes based on community / trasnfer)
COLOURS <- read.csv("colours.csv", sep=(","))
COLOURS_use <- COLOURS$colour
names(COLOURS_use) <- COLOURS$type

# #SPECIFY ORDER OF COLOURS
# reduced$cluster <- factor(reduced$cluster, levels = COLOURS_use$type) 
new_names <- read.csv("new_names.csv", sep=(",")) #rename the groups (i.e., not species name)

new_names$name <- factor(new_names$name, levels =c("Limosilactobacillus", "Lactobacillus A", "Lactobacillus B", "Lactobacillus C",
                   "Acetobacter A", "Acetobacter B", "Acetobacter C",
                   "Propionibacterium", "Rhodanobacter", "Pseudomonas", "Staphylococcus", "Moraxella",
                   "Enterococcus A", "Enterococcus B", "NA"))

# Define a custom labeling function
transfer_labels <- function(variable) {
  full_names <- c("t01" = "Transfer 01", "t09" = "Transfer 09", "t12" = "Transfer 12", "t19" = "Transfer 19")
  return(full_names[as.character(variable)])
}

community_labels <- function(variable) {
  full_names <- c("con" = "control", "inv" = "introduction", "rec" = "recovery")
  return(full_names[as.character(variable)])
}

# MAIN FIGURE: Apply the custom labels in facet_wrap
reduced %>% 
  ggplot(aes(x = as.factor(REP), y = abundance, fill = cluster)) +
  geom_col() + ylim(0, 1) +
  scale_fill_manual(values = COLOURS_use,
                    labels = c("Acetobacter A", "Lactobacillus A", "Limosilactobacillus A", "Lactobacillus B")) +
  facet_wrap(
    ~COMMUNITY ~ TRANSFER,
    scales = "free_x", ncol = 4,
    labeller = labeller(TRANSFER = transfer_labels, COMMUNITY = community_labels) # Apply custom labels to TRANSFER variable
  ) +
  theme_bw() +
  labs(x = "replicate", y = "relative abundance") +
  theme(
    axis.ticks.x = element_blank(),
    legend.position = c(0.8, 0.16)
  )


## figure of control samples 
controls %>% 
  # filter(barcode != "bc95") %>%
  filter(barcode == "bc95") %>%
  ggplot(aes(x = PLATE, y = abundance, fill = cluster)) +
  geom_col() + ylim(0, 1) +
  # scale_fill_manual(values = COLOURS_use ) +
  facet_wrap(~barcode, labeller = labeller(barcode = c(bc93 = "F1.1 control", bc94 = "M+ control", bc95 = "Zymo control"))) +
  theme_bw() +
  labs(x = "sequencing round", y = "relative abundance") +
  theme(
    axis.ticks.x = element_blank(),
    # legend.position = c(0.8, 0.16)
  )



#########################################
##### CALCULATING RICHNESS / SHANNON ######\
reduced <- merged %>%  # again, want version without missing barcodes added, and just plate 2, not data from spatial experiment 
  filter(barcode != "bc90") %>% 
  filter(barcode != "bc94") %>% 
  filter(barcode != "bc86") %>% 
  filter(PLATE == "plate2") %>%
  filter(COMMUNITY != "bot") %>%
  filter(COMMUNITY != "top") %>%
  filter(COMMUNITY != "mix") %>%
  filter(SPACE == "all") 

# transform to horizontal
wide <- reduced %>% 
  pivot_wider(names_from = cluster, values_from = abundance, 
              id_cols = c("barcode", "SAMPLE", "COMMUNITY", "TRANSFER"))

wide <- as.data.frame(wide)
wide[is.na(wide)] <- 0

###calcualte richness & shannon (vegan pkg)
wide$richness <- specnumber(wide[c(5:8)]) 
wide$shannon <- diversity(wide[c(5:8)])

summarised <- wide %>% 
  group_by(COMMUNITY, TRANSFER) %>% 
  summarise(across(c(richness, shannon), mean))
summarised

### RICHNESS
wide %>%
  # filter(TRANSFER == "t01") %>%
  ggplot(aes(x = COMMUNITY, y=richness, fill = COMMUNITY)) +
  geom_boxplot() +
  facet_wrap(~TRANSFER,
             # ~COMMUNITY, 
             scales="free_x", nrow=1) +
  theme_bw() 

library(emmeans)
rich_lm <- lm(richness ~ COMMUNITY*TRANSFER, data = wide) 
# Anova(rich_lm, type = 3)
summary(rich_lm)
emmeans(rich_lm, pairwise ~ COMMUNITY|TRANSFER)  #never significant



###############################
##### ADONIS PERMANOVA ########
reduced <- merged %>%  # again, just check have version without missing barcodes added, and just plate 2, not data from spatial experiment 
  filter(barcode != "bc90") %>% 
  filter(barcode != "bc94") %>% 
  filter(barcode != "bc86") %>% 
  filter(PLATE == "plate2") %>%
  filter(COMMUNITY != "bot") %>%
  filter(COMMUNITY != "top") %>%
  filter(COMMUNITY != "mix") %>%
  filter(SPACE == "all") 

# transform to horizontal
wide <- reduced  %>% 
  pivot_wider(names_from = cluster, values_from = abundance, values_fill = 0)

#everything, with interaction (take 9-12 = abdunances, take 2-3 = variables of interest)
adonis2(wide[ ,9:12] ~ COMMUNITY*TRANSFER, data = wide[ ,2:3], method = "bray") 


# pairwise comparisons for checing post-hoc differences
# https://raunakms.github.io/diversity_cooking_fuel/06_01_permanova_test.html
wide_subset <- wide %>% filter(TRANSFER == "t01") # PROBLEM - error for t12, signifcant community effect at t19...

dist_dml <- vegan::vegdist(x=as.matrix(wide_subset[,9:12]), 
                           method="bray", binary=FALSE, diag=TRUE, upper=TRUE, na.rm=FALSE)

y_permanova <- vegan::adonis2(
                      dist_dml ~ COMMUNITY,
                      # dist_dml ~ COMMUNITY*TRANSFER,#should be same as above lines
                      data=wide_subset, permutations=999, method="euclidean", parallel=4)
y_permanova

# I think this here is oonly possible with >2 groups (i.e., only at t19 when there's recovery line)
permtst <- RVAideMemoire::pairwise.perm.manova(resp = dist_dml, fact = wide_subset$COMMUNITY,
                                               test = "Pillai", nperm = 999, progress = TRUE, p.method = "none")
df <- reshape2::melt(permtst$p.value)
colnames(df) <- c("comm1", "comm2", "pvalue")
df <- df[-which(is.na(df$pvalue)), ]
df$pvalue.adj <- p.adjust(p = df$pvalue, method = "bonferroni", n = length(df$pvalue))
df


# use betadisper to see if dispersions differ 
# here done at t1....
# https://mattsigal.github.io/eqcov_supp/betadisp-ex.html

beta <- wide %>%  filter(TRANSFER == "t19")
dst <- dist(beta[,9:12])
wide.bd <- betadisper(dst, beta$COMMUNITY)
anova(wide.bd)
# permutest(wide.bd)
boxplot(wide.bd, xlab="COMMUNITY", main = "t17")


# 












# 
# 
# #by transfer
# adonis2(wide[wide$TRANSFER=="t01",5:18] ~ COMMUNITY, 
#         data = wide[wide$TRANSFER=="t01",1:4], method = "bray") 
# 
# adonis2(wide[wide$TRANSFER=="t05",5:18] ~ COMMUNITY, 
#         data = wide[wide$TRANSFER=="t05",1:4], method = "bray") 
# 
# adonis2(wide[wide$TRANSFER=="t17",5:18] ~ COMMUNITY, 
#         data = wide[wide$TRANSFER=="t17",1:4], method = "bray") 
# 
# #by community
# adonis2(wide[wide$COMMUNITY=="full",5:18] ~ TRANSFER, 
#         data = wide[wide$COMMUNITY=="full",1:4], method = "bray") 
# 
# adonis2(wide[wide$COMMUNITY=="med",5:18] ~ TRANSFER, 
#         data = wide[wide$COMMUNITY=="med",1:4], method = "bray") 
# 
# adonis2(wide[wide$COMMUNITY=="low",5:18] ~ TRANSFER, 
#         data = wide[wide$COMMUNITY=="low",1:4], method = "bray") 
# 
# adonis2(wide[wide$COMMUNITY=="synt",5:18] ~ TRANSFER, 
#         data = wide[wide$COMMUNITY=="synt",1:4], method = "bray") 
