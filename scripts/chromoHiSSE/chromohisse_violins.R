library(RevGadgets)
library(ggplot2)
setwd("~/Desktop")
# process data

carex <- readTrace("chromohisse_hiddenrates_run1.log")
df <- carex[[1]]
cols <- c("chi", 
          "clado_fission_a", "clado_fusion_a", "clado_no_change_a",
          "clado_fission_b", "clado_fusion_b", "clado_no_change_b")

# change column names to more informative parameters 
df <- df[,cols]
colnames(df) <- c("Cladogenetic Change in Hidden",
                  "Cladogenetic Fission A",
                  "Cladogenetic Fusion A",
                  "No Cladogenetic Change A",
                  "Cladogenetic Fission B",
                  "Cladogenetic Fusion B",
                  "No Cladogenetic Change B")
df %>%
tidyr::gather(key = "grp", 
              value = "val",
              factor_key = TRUE) -> df
  


df$grp2 <- car::recode(df$grp, " 'Cladogenetic Change in Hidden' = 'none'; c('Cladogenetic Fission A', 'Cladogenetic Fusion A', 'No Cladogenetic Change A') = 'A'; c('Cladogenetic Fission B', 'Cladogenetic Fusion B','No Cladogenetic Change B') = 'B' ")

# set up colors
colors <- c("#3c53a4",
            "#5ac4be80", "#f26b6c80", "#1a535d80",
            "#5ac4be80", "#f26b6c80", "#1a535d80")

names(colors) <- levels(df$grp)

g <- ggplot(df) +
  geom_violin(data = df,
              aes(x = grp, 
                  y = val, 
                  group = grp, 
                  fill = grp,
                  linetype = grp2),
              color = "black",
              lwd = 1,
              scale = "width",
              show.legend = FALSE) +
  geom_hline(yintercept = 0.0, color = "grey") + 
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = c("solid","dotted","blank")) +
  scale_fill_manual(values = colors) +
  scale_x_discrete(name = "",
                   labels = c("Hidden State Change",
                              "Fission",
                              "Fusion",
                              "No Change",
                              "Fission",
                              "Fusion",
                              "No Change")) +
  ylab("Rate") +
  ggthemes::theme_few() +
  theme(axis.text.x = element_text(face="bold", 
                                   size=14, 
                                   #angle=45,
                                   hjust = .5),
        axis.title.y = element_text(face = "bold", 
                                    size = 20))

  

