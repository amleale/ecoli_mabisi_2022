# v3 started in august2023, continued with some fixes in november 2023
# to do: compare t01 vs. t19 of control/ecoli/recovery ???
# dec2023: fixing plots, maybe some stats 
# feb 2023: pcas of all data (i.e., not separate for each transfer)

#input data is still the same file
setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/2022_fall_exp_leale/data/gcms")

library(readxl) #needed to read in excel file
library(dplyr) #needed for data manipulation
library(plyr) #needed for data manipulation
library(tidyverse) #needed for data manipulation
library(ggfortify) #needed for ggplot2 (i.e., pca plots)
library(heatmaply) #needed for heatmaps
library(matrixStats)
library(factoextra) #needed for 


#### data INPUT & quick initial changes ########

#### SEE EXCEL FILE TAB "read.me" for VARIABLE EXPLANATIONS
# zeros were replaced with 0.001 to allow PCA analysis (check in chromeleon that these are true zero value peaks)
aroma <- readxl::read_xlsx(path="gcms_raw_dec2022.xlsx", sheet ="forR.001") 
aroma <- aroma %>%  #create new variable of community x transfer
  unite("treat_tr", treatment:transfer, sep= "_", remove = FALSE)

### specify order of factors for later plotting (legends will make more sense)
aroma$transfer <- factor(aroma$transfer, levels = c("t01", "t09", "t12", "t19"))
aroma$treatment <- factor(aroma$treatment, levels = c("con", "top", "bot", "mix", "inv","rec"))
aroma$space <- factor(aroma$space, levels = c("all", "up", "low"))


###NORMALISE BY AROMA (ie. column) #####
# first put aroma data frame into new dataframe "aroma_norm" 
aroma_norm <- aroma 

#replace "A01-A29" column names with actual compound names
new_names <-  read.csv("aroma_names.csv", sep=(",")) #rename the groups (i.e., not species name)
colnames(aroma_norm)[8:36] <- new_names$aroma

#if want to see SE only for Invasion related:
for_SE <- aroma_norm %>%
  filter(sample != "milk") %>%
  filter(sample != "kefir") %>%
  filter(sample != "banana") %>%
  filter(treatment != "top") %>%
  filter(treatment != "bot") %>%
  filter(space != "up") %>%
  filter(space != "low") %>%
  filter(treatment != "mix") 

# the aroma values are in columns 7 to 25, "2" sets calculation by column, calculate median
# stores as list of values called "col_med"
col_med = apply(for_SE[8:36], 2, median) 

# see which to remove 
col_sd = apply(for_SE[8:36], 2, sd )

col_se = col_sd/sqrt(187)


# replace values in all rows, for columns with aromas with [ value/col_med ] 
# need to TRANSPOSE OF THE TRANSPOSE since R automatically reads by row...crazy confusing...but it works
aroma_norm[,8:36] <- t(t(aroma_norm[,8:36])/col_med) 

# replace now updated values in all rows, for columns 7:25 log2(value)
# now data is normalised by aroma (i.e., by column)
aroma_norm[,8:36] <- log2(aroma_norm[,8:36])

# # check that column normalisation works [don't use first row value to check!]
# aroma[3,8] # f1_t1, A1 = 38648139 (takes value in 3rd row, 7th column)
# x=110
# # column median of A1 from above = 2148.36
# med <- median(aroma$A01) # yes the same...
# aroma_norm[3,8]# value from above normalisation = -4.29
# log2(x/med) # yaaay the same :)

##### PCA plots ######
# remove aromas with little variation (as calculated above) 
aroma_norm <- aroma_norm %>% 
  select(-c("1-Propanol, 2-methyl-", "hexanoic acid, ethyl ester","Benzene, 1-methyl-2-(1-methylethyl)-",
            "2-Heptanol", "1-Hexene, 3,5-dimethyl-", "Butanoic acid", 
            "Nonanoic acid, 5-methyl-, ethyl ester", "Nonanoic acid", "n-Decanoic acid",
            "Hexane, 2,4-dimethyl-", "Octanoic acid, ethyl ester", "Benzene, 1,3-bis(1,1-dimethylethyl)-",
            "1-Butanol, 2-methyl-, acetate")) 


# exclude milk & kefir,  select only time point of interest
# creates new data frame called aroma_norm2
aroma_norm2 <- aroma_norm %>%
  filter(sample != "milk") %>%
  filter(sample != "kefir") %>%
  filter(sample != "banana") %>%
  filter(treatment != "top") %>%
  filter(treatment != "bot") %>%
  filter(treatment != "mix") %>%
  filter(space != "up") %>%
  filter(space != "low") %>%
  filter(transfer == "t19")

###### PCA PLOT - transfer 
# autoplot uses ggplot2 (need ggplot2 installed)
# create new dataframe called df2 with only aroma columns (i.e., 8-27).
# look at df2 to CONFIRM CHOOSE ONLY the 20 aromas
df2 <- aroma_norm2[8:23] 

### PCA analysis using prcomp() function 
# stores as matrix "pca_res". this will be used for making plots
# scale = TRUE standardizes the input data so mean=0 and variance=1 before doing PCA
pca_res <- prcomp(na.omit(df2), scale. = TRUE)

### SUMMARY STATS of PCA analysis (transfer)
summary(pca_res)
# fviz_eig(pca_res)  ### scree plot of dimension contributions
# rotation <- pca_res[["rotation"]] ### aroma contributions to each PC
# write.csv(rotation, "rot_t19.csv") # save output in spreadsheet, will go to source file location (i.e., working directory)
# pca_res[["x"]] ### sample contributions to each PC (sorry not working...)

### PCA plot + arrows 
aroma_pca <- autoplot(pca_res, data = aroma_norm2,
                      colour = "treatment", 
                      loadings = TRUE, loadings.colour = 'grey20',
                      loadings.label = TRUE, loadings.label.colour = 'grey20',
                      loadings.label.size = 3, loadings.label.repel=T
) +
  stat_ellipse(geom = "polygon", level = 0.95, aes(fill = treatment), alpha = 0.25) +
  scale_colour_manual(values = c("grey", "purple", "orange"),
                      labels = c("control", "introduction", "recovery")) +
  scale_fill_manual(values = c("grey", "purple", "orange"),
                    labels = c("control", "introduction", "recovery")) +
  theme_bw(base_size = 14) +
  labs(
    title = "B: transfer 09",
  )


aroma_pca$layers <- aroma_pca$layers[c(4, 1,2,3)] # only needed when loadings shown
aroma_pca
# ggsave("aroma_pca.svg",width=9,height=6.5)



#### PCA for all data together 
aroma_norm2 <- aroma_norm %>%
  filter(sample != "milk") %>%
  filter(sample != "kefir") %>%
  filter(sample != "banana") %>%
  filter(treatment != "top") %>%
  filter(treatment != "bot") %>%
  filter(treatment != "mix") %>%
  filter(space != "up") %>%
  filter(space != "low") 

df2 <- aroma_norm2[8:23] 
pca_res <- prcomp(na.omit(df2), scale. = TRUE)
summary(pca_res)

aroma_pca <- autoplot(pca_res, data = aroma_norm2,
                      colour = "treatment", 
                      loadings = TRUE, loadings.colour = 'grey20',
                      loadings.label = TRUE, loadings.label.colour = 'grey20',
                      loadings.label.size = 3, loadings.label.repel=T
) +
  stat_ellipse(geom = "polygon", level = 0.95, aes(fill = treatment), alpha = 0.25) +
  scale_colour_manual(values = c("grey", "purple", "orange"),
                      labels = c("control", "introduction", "recovery")) +
  scale_fill_manual(values = c("grey", "purple", "orange"),
                    labels = c("control", "introduction", "recovery")) +
  facet_wrap(~transfer) +
  theme_bw(base_size = 14) +
  labs(
    title = "all together",
  )

aroma_pca$layers <- aroma_pca$layers[c(4, 1,2,3)] # only needed when loadings shown
aroma_pca













