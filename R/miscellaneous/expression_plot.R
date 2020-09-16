library(tidyverse)
library(ggsci)
tissues <- c("Heart", "Kidney", "Lung", "Brain", "Spleen", "Limb")
Pcmt1 <- c(6.118, 5.581, 3.866, 8.519, 2.509, 7.713)
Pard3 <- c(2.334, 3.684, 5.319, 3.993, 0.381, 5.993)
Ccnd3 <- c(18.287, 40.127, 60.731, 12.366, 58.173, 87.571)
Ripk4 <- c(0.169, 4.923, 6.648, 0.305, 0.073, 2.488)
Actb <- c(330.761, 700.096, 2057.37, 1065.63, 2803.45, 823.242)

df_Pcmt1 <- data.frame(tissues, gene = rep("Pcmt1", 6), expression = log10(Pcmt1))
df_Pard3 <- data.frame(tissues, gene = rep("Pard3", 6), expression = log10(Pard3))
df_Ccnd3 <- data.frame(tissues, gene = rep("Ccnd3", 6), expression = log10(Ccnd3))
df_Ripk4 <- data.frame(tissues, gene = rep("Ripk4", 6), expression = log10(Ripk4))
df_Actb <- data.frame(tissues, gene = rep("Actb", 6), expression = log10(Actb))
df_plot <- do.call(rbind, list(df_Pcmt1, df_Pard3, df_Ccnd3, df_Ripk4, df_Actb))
df_plot$gene <- as.factor(df_plot$gene)

df_plot$expression <- sapply(df_plot$expression, function(x){if(x<0){return(0)}else{return(x)}})

df_plot %>% filter(gene %in% c("Pcmt1", "Actb")) %>% 
  arrange(gene) %>% 
  ggplot(aes(tissues, expression, fill = gene)) +
    geom_bar(stat = "identity", position = position_dodge(width = .8), width = .7, size = .25) +
    scale_fill_npg() +s
    scale_x_discrete(limits = c("Heart", "Kidney", "Brain", "Lung", "Spleen", "Limb")) +
    scale_y_continuous(expand = c(0, 0), labels = c(1, 10, 100,1000)) +
    #labs(x = "", y = expression(log[10]~of~RPKM)) +
    #labs(x = "", y = bquote(log[10]~of~RPKM)) +
    labs(x = "", y = "RPKM") +
    theme_classic() +
    annotation_logticks(sides = "l", outside = T, 
                        short = unit(.8, "mm"),
                        mid = unit(1.5, "mm"),
                        long = unit(2, "mm")) +
    coord_cartesian(clip = "off") +
    theme(legend.title = element_blank(),
          legend.position = c(0.1, 0.9),
          legend.text = element_text(size = 10, face = "bold", color = "black"),
          axis.text = element_text(size = 12, face = "bold", color = "black"),
          axis.title.y = element_text(size = 14, face = "bold", color = "black")
          ) 
ggsave("figures/expression_Pcmt1.pdf", width = 6, height = 4, dpi = 1000)


df_plot %>% filter(gene %in% c("Pard3", "Actb")) %>% 
  arrange(gene) %>% 
  ggplot(aes(tissues, expression, fill = gene)) +
  geom_bar(stat = "identity", position = position_dodge(width = .8), width = .7, size = .25) +
  scale_fill_npg() +
  scale_x_discrete(limits = c("Heart", "Kidney", "Brain", "Lung", "Spleen", "Limb")) +
  scale_y_continuous(expand = c(0, 0), labels = c(1, 10, 100,1000)) +
  #labs(x = "", y = expression(log[10]~of~RPKM)) +
  #labs(x = "", y = bquote(log[10]~of~RPKM)) +
  labs(x = "", y = "RPKM") +
  theme_classic() +
  annotation_logticks(sides = "l", outside = T, 
                      short = unit(.8, "mm"),
                      mid = unit(1.5, "mm"),
                      long = unit(2, "mm")) +
  coord_cartesian(clip = "off") +
  theme(legend.title = element_blank(),
        legend.position = c(0.1, 0.9),
        legend.text = element_text(size = 10, face = "bold", color = "black"),
        axis.text = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 14, face = "bold", color = "black")
  ) 
ggsave("figures/expression_Pard3.pdf", width = 6, height = 4, dpi = 1000)

df_plot %>% filter(gene %in% c("Ccnd3", "Actb")) %>% 
  arrange(gene) %>% 
  ggplot(aes(tissues, expression, fill = gene)) +
  geom_bar(stat = "identity", position = position_dodge(width = .8), width = .7, size = .25) +
  scale_fill_npg() +
  scale_x_discrete(limits = c("Heart", "Kidney", "Brain", "Lung", "Spleen", "Limb")) +
  scale_y_continuous(expand = c(0, 0), labels = c(1, 10, 100,1000)) +
  #labs(x = "", y = expression(log[10]~of~RPKM)) +
  #labs(x = "", y = bquote(log[10]~of~RPKM)) +
  labs(x = "", y = "RPKM") +
  theme_classic() +
  annotation_logticks(sides = "l", outside = T, 
                      short = unit(.8, "mm"),
                      mid = unit(1.5, "mm"),
                      long = unit(2, "mm")) +
  coord_cartesian(clip = "off") +
  theme(legend.title = element_blank(),
        legend.position = c(0.1, 0.9),
        legend.text = element_text(size = 10, face = "bold", color = "black"),
        axis.text = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 14, face = "bold", color = "black")
  ) 
ggsave("figures/expression_Ccnd3.pdf", width = 6, height = 4, dpi = 1000)

df_plot %>% filter(gene %in% c("Ripk4", "Actb")) %>% 
  arrange(gene) %>% 
  ggplot(aes(tissues, expression, fill = gene)) +
  geom_bar(stat = "identity", position = position_dodge(width = .8), width = .7, size = .25) +
  scale_fill_npg() +
  scale_x_discrete(limits = c("Heart", "Kidney", "Brain", "Lung", "Spleen", "Limb")) +
  scale_y_continuous(expand = c(0, 0), labels = c(1, 10, 100,1000)) +
  #labs(x = "", y = expression(log[10]~of~RPKM)) +
  #labs(x = "", y = bquote(log[10]~of~RPKM)) +
  labs(x = "", y = "RPKM") +
  theme_classic() +
  annotation_logticks(sides = "l", outside = T, 
                      short = unit(.8, "mm"),
                      mid = unit(1.5, "mm"),
                      long = unit(2, "mm")) +
  coord_cartesian(clip = "off") +
  theme(legend.title = element_blank(),
        legend.position = c(0.1, 0.9),
        legend.text = element_text(size = 10, face = "bold", color = "black"),
        axis.text = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 14, face = "bold", color = "black")
  ) 
ggsave("figures/expression_Ripk4.pdf", width = 6, height = 4, dpi = 1000)
