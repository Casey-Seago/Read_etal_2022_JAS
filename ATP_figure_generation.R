# This code is to generate the ATP figure

# Libraries needed:
library(ggplot2)
library(gridExtra)
library(grid)
library(extrafont)



# Dataset used:
oocInfo = read.csv(file = "transformed_data/final_dataset_mito_out_na.csv")


# ATP model figure generation

g2e2Grob = grobTree(textGrob("p = 0.014", x = 0.9, y = 0.05, gp = gpar(fontsize = 17, fontface = "bold", fontfamily = "Times New Roman")))
atpXg2e2Plot = ggplot(oocInfo, aes(x = g2e2, y = atp_pg, shape = as.factor(year))) + 
  geom_point(color = "black", size = 3) + 
  stat_smooth(method = "lm", col = "#FF8200", aes(group = 1)) + 
  xlab("Serum estradiol at GnRH2, pg/ml") + 
  ylab("Intraoocyte ATP, pg") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black", size = 1.0)) + 
  theme(legend.position = "none") + 
  theme(axis.title.x = element_text(face = "bold", size = 17, margin = margin(t = 8))) + 
  theme(axis.title.y = element_text(face = "bold", size = 17, margin = margin(r = 10))) + 
  theme(axis.text.x = element_text(face = "bold", size = 17, color = "black")) + 
  theme(axis.text.y = element_text(face = "bold", size = 17, color = "black")) + 
  ggtitle("A") + 
  theme(plot.title = element_text(hjust = -.1, face = "bold", size = 17)) + 
  annotation_custom(g2e2Grob) + 
  theme(text = element_text(family = "Times New Roman", face = "bold"))
  
timeGrob = grobTree(textGrob("p = 0.004", x = 0.9, y = 0.053, gp = gpar(fontsize = 17, fontface = "bold", fontfamily = "Times New Roman")))
atpXtimePlot = ggplot(oocInfo, aes(x = timeG2toFA, y = atp_pg, shape = as.factor(year))) + 
  geom_point(color = "black", size = 3) + 
  stat_smooth(method = "lm", col = "#FF8200", aes(group = 1)) + 
  xlab("Time between GnRH2 and FA, hr") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black", size = 1.0)) + 
  theme(legend.position = "bottom") + 
  theme(axis.title.x = element_text(face = "bold", size = 17, margin = margin(t = 7))) + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.text.x = element_text(face = "bold", size = 17, color = "black")) + 
  theme(axis.text.y = element_text(face = "bold", size = 17, color = "black")) + 
  ggtitle("B") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = -.1, face = "bold", size = 17)) + 
  labs(shape = "Year") + 
  annotation_custom(timeGrob) + 
  theme(text = element_text(family = "Times New Roman", face = "bold"))

g2e2Grob2 = grobTree(textGrob("p = 0.014", x = 0.14, y = 0.95, gp = gpar(fontsize = 17, fontface = "bold", fontfamily = "Times New Roman")))
atpXg2e2Plot2 = ggplot(oocInfo, aes(x = g2e2, y = atp_pg, shape = as.factor(year))) + 
  geom_point(color = "black", size = 3) + 
  stat_smooth(method = "lm", col = "#FF8200") + 
  xlab("Serum estradiol at GnRH2, pg/ml") + 
  ylab("Intraoocyte ATP, pg") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black", size = 1.0)) + 
  theme(legend.position = "none") + 
  theme(axis.title.x = element_text(face = "bold", size = 17, margin = margin(t = 8))) + 
  theme(axis.title.y = element_text(face = "bold", size = 17, margin = margin(r = 10))) + 
  theme(axis.text.x = element_text(face = "bold", size = 17, color = "black")) + 
  theme(axis.text.y = element_text(face = "bold", size = 17, color = "black")) + 
  ggtitle("C") + 
  theme(plot.title = element_text(hjust = -.1, face = "bold", size = 17)) + 
  theme(text = element_text(family = "Times New Roman", face = "bold"))

timeGrob2 = grobTree(textGrob("p = 0.004", x = 0.13, y = 0.95, gp = gpar(fontsize = 17, fontface = "bold", fontfamily = "Times New Roman")))
atpXtimePlot2 = ggplot(oocInfo, aes(x = timeG2toFA, y = atp_pg, shape = as.factor(year))) + 
  geom_point(color = "black", size = 3) + 
  stat_smooth(method = "lm", col = "#FF8200") + 
  xlab("Time between GnRH2 and FA, hr") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black", size = 1.0)) + 
  theme(legend.position = "bottom") + 
  theme(axis.title.x = element_text(face = "bold", size = 17, margin = margin(t = 7))) + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.text.x = element_text(face = "bold", size = 17, color = "black")) + 
  theme(axis.text.y = element_text(face = "bold", size = 17, color = "black")) + 
  ggtitle("D") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = -.1, face = "bold", size = 17)) + 
  labs(shape = "Year") + 
  theme(text = element_text(family = "Times New Roman", face = "bold"))

grid.arrange(atpXg2e2Plot, atpXtimePlot, atpXg2e2Plot2, atpXtimePlot2, ncol = 2)


# To get the legend

timeGrob2 = grobTree(textGrob("p = 0.004", x = 0.13, y = 0.95, gp = gpar(fontsize = 17, fontface = "bold")))
atpXtimePlot3 = ggplot(oocInfo, aes(x = timeG2toFA, y = atp_pg, shape = as.factor(year))) + 
  geom_point(color = "black", size = 3) + 
  stat_smooth(method = "lm", col = "#FF8200") + 
  xlab("Time between GnRH2 and FA, hr") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black", size = 1.0)) + 
  theme(legend.position = "bottom") + 
  theme(axis.title.x = element_text(face = "bold", size = 17, margin = margin(t = 7))) + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.text.x = element_text(face = "bold", size = 17, color = "black")) + 
  theme(axis.text.y = element_text(face = "bold", size = 17, color = "black")) + 
  ggtitle("D") + 
  theme(plot.title = element_text(hjust = -.1, face = "bold", size = 17)) + 
  labs(shape = "Year") + 
  annotation_custom(timeGrob2) + 
  theme(legend.background = element_rect(size = 1.0, linetype = "solid", colour = "black"), legend.title = element_text(face = "bold", size = 17, family = "Times New Roman"), legend.text = element_text(face = "bold", size = 17)) +
  guides(shape = guide_legend(override.aes = list(size = 4), label.vjust = 1.1, title.vjust = 1.1)) + 
  theme(text = element_text(family = "Times New Roman", face = "bold"))

