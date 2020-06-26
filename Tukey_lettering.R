library(multcompView)
library(ggplot2)
library(dplyr)
library(plyr)

# #read in data from excel file
# data1 <- "Sampling_1_data_2.xlsx"
# dat1 <- read_xlsx(data1)
# 
# #to sort x axis by genotype in my chosen order
# dat1$Genotype <- factor(dat1$Genotype, levels=c("A1", "U", "8", "9", "10", "11"))
# 
# # filter for Gene1
# g_G1 <- dat1 %>% filter(Gene == "Gene1")
# 
# #to sort x axis by genotype in my chosen order
# g_G1$Genotype <- factor(g_G1$Genotype, levels=c("A1", "U", "8", "9", "10", "11"))

dat1 <- data.frame(Gene     = "Gene1",
                   Genotype = factor(rep(c("8", "9", "10", "11", "A1", "U"), 3), levels = c("A1", "U", "8", "9", "10", "11")),
                   Ct       = randu[1:(6*3),1]) %>%
  filter(Gene == "Gene1")

# Group the treatments that are not different each other together (?)-- Taken from https://www.r-graph-gallery.com/84-tukey-test
#G1generate_label_df <- function(G1TUKEY, variable) {
G1generate_label_df <- function(.data, variable) {
  # ANOVA
  #model <- lm( g_G1$Ct ~ g_G1$Genotype )
  model <- lm(formula = Ct ~ Genotype, data = .data)
  G1ANOVA <- aov(model)
  #G1ANOVA
  
  # Tukey test
  G1TUKEY <- TukeyHSD(x = G1ANOVA, 'Genotype', conf.level = 0.95)
  #G1TUKEY

  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels              <- G1TUKEY[[variable]][,4]
  Tukey.labels              <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  Tukey.labels[,"Genotype"] <- rownames(Tukey.labels) #Putting labels in same order as in the boxplot (?)
  
  Tukey.labels        <- join(Tukey.labels, 
                              .data %>% ddply(.(Genotype), function(each) {data.frame(y = max(each$Ct))}),
                              by = variable) #### THIS join() IS SUUUUPER IMPORTANT! YOU WERE LIKELY TO RETURN MIXED RESULTS IN SOME CASES WHERE THE INPUT DATA IS NOT WELL ORGANIZED.
  #Tukey.labels              <- Tukey.labels[order(Tukey.labels$Genotype) , ]
  
  #return(Tukey.labels)
  Tukey.labels
}

# Apply the function on my dataset --> making dataframe with lettering
#G1LABELS <- G1generate_label_df(G1TUKEY , "Genotype")
G1LABELS <- dat1 %>% G1generate_label_df("Genotype")

# re-order to match re-ordering of x (genotype)
#G1LABELS <- G1LABELS[order(levels = c("A1", "U", "8", "9", "10", "11")),] #


my_y_title <- expression(paste("Transcript abundance relative to ", italic("IPP2")))

# Draw the boxplot

#PG1 <- g_G1 %>% filter(Gene == "G1") %>% ggplot(aes(Genotype, Ct, fill = Genotype)) +
PG1 <- dat1 %>%
  ggplot(aes(x = Genotype, y = Ct, fill = Genotype)) +
    #geom_boxplot(outlier.color = NA, alpha = 0.5) +
    geom_point(aes(color = Genotype), alpha = 0.8) + 
    labs(x= "Genotype", y=my_y_title) + #axis labels
    theme_minimal() + 
    scale_fill_brewer(palette="Set2") + 
    scale_color_brewer(palette = "Set2") + 
    ggtitle("G1") + 
    theme(plot.title = element_text(face = "bold.italic", hjust = 0.9, size = 14)) + #graph title
    #geom_text(data=G1LABELS, aes(x=Genotype, y = c(1.25, 0.9, 0.95, 0.7, 0.55, 0.6), label = Letters), size=4)  #adding in lettering
    geom_text(aes(x = Genotype, y = y + 0.1*(max(y, na.rm = TRUE) - min(y, na.rm = TRUE)), label = Letters), G1LABELS, size=4)

PG1
