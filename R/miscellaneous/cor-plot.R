install.packages("PerformanceAnalytics")
install.packages("GGally")
library(PerformanceAnalytics)
library(GGally)
all <- read.csv("./data/all.csv", header = T)
all_without_d <- read.csv("./data/all_without_d.csv", header = T)

cor(all, method = "spearman")
cor(all_without_d, method = "spearman")

chart.Correlation(all, histogram = T, pch = 19)
ggpairs(all,
        lower = list(continuous = wrap("smooth", alpha = .3), combo = "facethist"),
        diag = list(continuous = "barDiag"),
        axisLabels = "none",
        labeller = "label_parsed") + 
  theme_bw()
ggsave("./figures/corplot_classic.pdf", width = 6, height = 4, dpi = 1000)

ggpairs(all_without_d,
        lower = list(continuous = wrap("smooth", alpha = .3), combo = "facethist"),
        diag = list(continuous = "barDiag"),
        axisLabels = "none",
        labeller = "label_parsed") + 
  theme_classic()
ggsave("./figures/corplot_without_d.pdf", width = 6, height = 4, dpi = 1000)
