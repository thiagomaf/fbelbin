#read in data from excel file
data1 <- "Sampling_1_data_2.xlsx"
dat1 <- read_xlsx(data1)


#to sort x axis by genotype in my chosen order
dat1$Genotype <- factor(dat1$Genotype, levels=c("A1", "U", "8", "9", "10", "11"))


#######################################

#CCA1

# filter for CCA1
g_CCA1 <- dat1 %>% filter(Gene == "CCA1") 

#to sort x axis by genotype in my chosen order
g_CCA1$Genotype <- factor(g_CCA1$Genotype, levels=c("A1", "U", "8", "9", "10", "11"))


# ANOVA
model <- lm( g_CCA1$Ct ~ g_CCA1$Genotype )
CCA1ANOVA <- aov(model)
CCA1ANOVA

# Tukey test
CCA1TUKEY <- TukeyHSD(x=CCA1ANOVA, 'g_CCA1$Genotype', conf.level=0.95)
CCA1TUKEY

# Group the treatments that are not different each other together (?)-- Taken from https://www.r-graph-gallery.com/84-tukey-test
CCA1generate_label_df <- function(CCA1TUKEY, variable){
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- CCA1TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  #Putting labels in same order as in the boxplot (?) 
  Tukey.labels$Genotype=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$Genotype) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset --> making dataframe with lettering
CCA1LABELS <- CCA1generate_label_df(CCA1TUKEY , "g_CCA1$Genotype")

# re-order to match re-ordering of x (genotype)
CCA1LABELS <- CCA1LABELS[order(levels=c("A1", "U", "8", "9", "10", "11")),]


my_y_title <- expression(paste("Transcript abundance relative to ", italic("IPP2")))

# Draw the boxplot

PCCA1 <- g_CCA1 %>% filter(Gene == "CCA1") %>% ggplot(aes(Genotype, Ct, fill = Genotype)) +
  geom_boxplot(outlier.color = NA, alpha = 0.5) +
  geom_point(aes(color = Genotype), alpha = 0.8) + 
  labs(x= "Genotype", y=my_y_title) + #axis labels
  theme_minimal() + 
  scale_fill_brewer(palette="Set2") + 
  scale_color_brewer(palette = "Set2") + 
  ggtitle("CCA1") + 
  theme(plot.title = element_text(face = "bold.italic", hjust = 0.9, size = 14)) + #graph title
  geom_text(data=CCA1LABELS, aes(x=Genotype, y = c(1.25, 0.9, 0.95, 0.7, 0.55, 0.6), label = Letters), size=4)  #adding in lettering 
 
PCCA1 
