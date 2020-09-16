install.packages("ggpubr")
install.packages("ggsci")
library(tidyverse)
library(ggpubr)
library(ggsci)
tandem <- read_delim("./data/tandem.txt", delim = "\t")
line <- read_delim("./data/regression_line.txt", delim = "\t")
colnames(line) <- c("Tandem", "Total")
NBS <- read_delim("./data/NBS.txt", delim = "\t")

lm_eqn <- function(df){
  m <- lm(Tandem ~ Total, df)
  eq <- substitute(italic(y) == b %.% italic(x) + a *","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 4),
                        b = format(unname(coef(m)[2]), digits = 4),
                        r2 = format(summary(m)$r.squared, digits = 4)))
  as.character(as.expression(eq));
}

lm.res <- lm(Tandem ~ Total, data = line)
ggplot(data = line, aes(x = Total, y = Tandem)) +
  geom_point(color = "grey30", size = 2.5) + 
  geom_smooth(method = "lm", se = F, color = "red")+
  annotate("text", x = 500, y = 600, label = lm_eqn(line), parse = T, size = 4) +
  scale_x_continuous(expand = c(0,0.5), limits = c(0,2550), breaks = seq(500, 2500, 500)) + 
  scale_y_continuous(expand = c(0,0.5), limits = c(0,715), breaks = seq(0, 700, 100)) +
  labs(x = "Total", y = "Species with tandem counts") +
  theme_classic() + 
  theme(axis.line = element_line(size = .8),
        axis.text = element_text(size = 8, face = "bold", color = "black"))
ggsave("figures/scatter_plot_2.pdf", width = 6, height = 4, dpi = 1000)

NBS_colnames <- colnames(NBS)
boxplot_df <- na.omit(NBS[, 1])
colnames(boxplot_df) <- "Value"
boxplot_df <- boxplot_df %>% mutate(Class = NBS_colnames[1])

for (i in 2:4){
  df <- na.omit(NBS[, i])
  colnames(df) <- "Value"
  df <- df %>% mutate(Class = NBS_colnames[i])
  boxplot_df <- rbind(boxplot_df, df)
}

boxplot_df %>% 
  mutate(Class = factor(Class, levels = c("Bsal angiosperms", 
                                             "Magnoliidea",
                                             "Monocotyledoneae",
                                             "Eudicotyledoneae"))) %>% 
ggplot(aes(x = Class, y = Value)) +
  geom_boxplot(aes(fill = Class), size = 1, fatten = 1.2, outlier.size = 2) + 
  scale_fill_npg() +
  scale_y_continuous(expand = c(0,2), limits = c(0,2500), breaks = seq(0, 2500, 500)) +
  labs(y = "Protein with an NB-ARC domain") + 
  theme_classic() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, face = "bold", color = "black"),
        axis.line = element_line(size = .8),
        axis.ticks = element_line(size = .8),
        axis.text = element_text(size = 8, face = "bold", color = "black"),
        axis.text.x = element_text(angle = 35, hjust = 1))
ggsave("figures/boxplot.pdf", width = 6, height = 4, dpi = 1000)


compaired <- list(c("Bsal angiosperms", "Magnoliidea"), c("Bsal angiosperms", "Monocotyledoneae"),
                  c("Bsal angiosperms", "Eudicotyledoneae"), c("Magnoliidea", "Monocotyledoneae"),
                  c("Magnoliidea", "Eudicotyledoneae"), c("Monocotyledoneae", "Eudicotyledoneae"))
ggboxplot(data = boxplot_df, x = "Class", y = "Value", fill = "Class") +
  stat_compare_means(comparisons = compaired, method = "wilcox.test") +
  labs(y = "Protein with an NB-ARC domain") + 
  theme_light() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, face = "bold", color = "black"),
        axis.line = element_line(size = .8),
        axis.ticks = element_line(size = .8),
        axis.text = element_text(size = 8, face = "bold", color = "black"),
        axis.text.x = element_text(angle = 35, hjust = 1))
ggsave("figures/boxplot_with_sign.pdf", width = 6, height = 4, dpi = 1000)


reg_eqn <- function(m){
  eq <- substitute(italic(y) == b %.% italic(x) + a *","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 4),
                        b = format(unname(coef(m)[2]), digits = 4),
                        r2 = format(summary(m)$r.squared, digits = 4)))
  as.character(as.expression(eq));
}

line_minus <- line %>% filter(Total < 2000)
colnames(line_minus) <- c("tandem_minus", "total_minus")
line_minus_res <- rbind(line_minus, c(0, 0))
line_res <- cbind(line, line_minus_res)
ggplot(data = line_res) +
  geom_point(aes(x = Total, y = Tandem), color = "grey30", size = 2.5) + 
  geom_smooth(aes(x = Total, y = Tandem), method = "lm", se = F, color = "red")+
  geom_smooth(aes(x = total_minus, y = tandem_minus), method = "lm", se = F, color = "blue") + 
  annotate("text", x = 1500, y = 300, label = reg_eqn(lm(Tandem ~ Total, data = line)), parse = T, size = 4, color = "red") +
  annotate("text", x = 600, y = 600, label = reg_eqn(lm(tandem_minus ~ total_minus, data = line_minus)), parse = T, size = 4, color = "blue") +
  scale_x_continuous(expand = c(0,0.5), limits = c(0,2550), breaks = seq(500, 2500, 500)) + 
  scale_y_continuous(expand = c(0,0.5), limits = c(0,715), breaks = seq(0, 700, 100)) +
  labs(x = "Total", y = "Species with tandem counts") +
  theme_classic() + 
  theme(axis.line = element_line(size = .8),
        axis.text = element_text(size = 8, face = "bold", color = "black"))
ggsave("figures/scatter_plot_3.pdf", width = 6, height = 4, dpi = 1000)

