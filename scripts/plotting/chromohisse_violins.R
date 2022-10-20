library(RevGadgets)
library(ggplot2)
setwd("~/Documents/carex_chromo/")
# process data

carex <- readTrace(paths = c("output/chromohisse/chromohisse_ana_hidden_mrm_run1.log",
                             "output/chromohisse/chromohisse_ana_hidden_mrm_run2.log"))

carex <- combineTraces(carex)
df <- carex[[1]]
clado_cols <- c("chi", 
                "clado_fission_a", "clado_fission_b",
                "clado_fusion_a", "clado_fusion_b", 
                "clado_no_change_a", "clado_no_change_b")
ana_cols <- c(
              #"alpha",
              "gamma_a", "gamma_b",
              "delta_a", "delta_b")

# change column names to more informative parameters 
df_clado <- df[,clado_cols]
df_clado$clado_no_change_new_a <- df_clado$chi + df_clado$clado_no_change_a
df_clado$clado_no_change_new_b <- df_clado$chi + df_clado$clado_no_change_b

df_clado <- df_clado[ ,c(2:5, 8,9)]
colnames(df_clado) <- c("Cladogenetic Fission A",
                        "Cladogenetic Fission B",
                        "Cladogenetic Fusion A",
                        "Cladogenetic Fusion B",
                        "No Cladogenetic Change A",
                        "No Cladogenetic Change B")

df_ana <- df[,ana_cols]
colnames(df_ana) <- c(
                      #"Anagenetic Change in Hidden",
                      "Anagenetic Fission A",
                      "Anagenetic Fission B",
                      "Anagenetic Fusion A",
                      "Anagenetic Fusion B")

df_clado %>%
tidyr::gather(key = "grp", 
              value = "val",
              factor_key = TRUE) -> df_clado
  
df_ana %>%
  tidyr::gather(key = "grp", 
                value = "val",
                factor_key = TRUE) -> df_ana

df_clado$grp2 <- car::recode(df_clado$grp, " 'Cladogenetic Change in Hidden' = 'none'; c('Cladogenetic Fission A', 'Cladogenetic Fusion A', 'No Cladogenetic Change A') = 'A'; c('Cladogenetic Fission B', 'Cladogenetic Fusion B','No Cladogenetic Change B') = 'B' ")
df_ana$grp2 <- car::recode(df_ana$grp, " 'Anagenetic Change in Hidden' = 'none'; c('Anagenetic Fission A', 'Anagenetic Fusion A') = 'A'; c('Anagenetic Fission B', 'Anagenetic Fusion B') = 'B' ")

# set up colors
colors <- c(#"#3c53a4",
            "#5ac4be80", "#5ac4be80", 
            "#f26b6c80", "#f26b6c80", 
            "#1a535d80", "#1a535d80" )

names(colors) <- levels(df_clado$grp)

g <- ggplot(df_clado) +
  geom_violin(data = df_clado,
              aes(x = grp, 
                  y = val, 
                  group = grp, 
                  fill = grp,
                  linetype = grp2),
              color = "black",
              lwd = 1,
              scale = "width",
              show.legend = F) +
  stat_summary(fun=mean, aes(x = grp, y = val), geom="point", size=2, color="black") +
  #geom_hline(yintercept = 0.0, color = "grey") + 
  scale_color_manual(values = colors) +
  #scale_linetype_manual(values = c("solid","dotted","blank")) +
  scale_linetype_manual(values = c("solid","dotted")) +
  scale_fill_manual(values = colors) +
  scale_x_discrete(name = "Type of cladogenetic event",
                   labels = c(
                              #"Hidden State Change",
                              "Fission",
                              "Fission",
                              "Fusion",
                              "Fusion",
                              "No Change",
                              "No Change")) +
  ylab("Posterior density") +
  ggthemes::theme_few() +
  theme(axis.text.x = element_text(face="bold", 
                                   size=14, 
                                   #angle=45,
                                   hjust = .5),
        axis.title.y = element_text(face = "bold", 
                                    size = 20),
        axis.title.x = element_text(face = "bold", 
                                    size = 20))

colors <- c(
            # "#3c53a4",
            "#5ac4be80", "#5ac4be80", 
            "#f26b6c80", "#f26b6c80")

h <- ggplot(df_ana) +
  geom_violin(data = df_ana,
              aes(x = grp, 
                  y = val, 
                  group = grp, 
                  fill = grp,
                  linetype = grp2),
              color = "black",
              lwd = 1,
              scale = "width",
              show.legend = F) +
  stat_summary(fun=mean, aes(x = grp, y = val), geom="point", size=2, color="black") +
  #geom_hline(yintercept = 0.0, color = "grey") + 
  scale_color_manual(values = colors) +
  # scale_linetype_manual(values = c("solid","dotted","blank")) +
  scale_linetype_manual(values = c("solid","dotted")) +
  scale_fill_manual(values = colors) +
  scale_x_discrete(name = "Type of anagenetic event",
                   labels = c(
                              #"Hidden State Change",
                              "Fission",
                              "Fission",
                              "Fusion",
                              "Fusion")) +
  ylab("Posterior density") +
  ggthemes::theme_few() +
  theme(axis.text.x = element_text(face="bold", 
                                   size=14, 
                                   #angle=45,
                                   hjust = .5),
        axis.title.y = element_text(face = "bold", 
                                    size = 20),
        axis.title.x = element_text(face = "bold", 
                                    size = 20))
  
pdf("figures/chromohisse_violins_clado.pdf", height = 7, width = 12)
g
dev.off()
pdf("figures/chromohisse_violin_ana.pdf", height = 7, width = 10)
h
dev.off()


