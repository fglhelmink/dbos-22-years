library(ggplot2)
library(paletteer)
library(forcats)
library(extrafont)
font_import()

#create df based on percentages
data <- data.frame(
  disorder = factor(c("Any disorder", 
           "Any mood disorder", 
           "Major depressive disorder",
           "Bipolar disorder",
           "Chornic depression",
           "Anxiety disorder",
           "ADHD",
           "Substance abuse disorder",
           "Other disorders"
           ),
           levels = c("Any disorder", 
                      "Any mood disorder", 
                      "Major depressive disorder",
                      "Bipolar disorder",
                      "Chornic depression",
                      "Anxiety disorder",
                      "ADHD",
                      "Substance abuse disorder",
                      "Other disorders"
           )),
  percentage   = c(80,65,36,11,8,35,3,24,28),
  bar_clr = paletteer::paletteer_d("trekcolors::borg"),
  bar_clr_text = "white",
  bar_face= "plain"
)

#change the colours and fonts of the highlighted bars
data$bar_clr[1] <- "#e7eb8b"
data$bar_clr_text[1] <-"gray26"
data$bar_clr[2] <- "#b7daee"
data$bar_clr_text[2] <- "gray26"
data$bar_clr[3] <- "#5b86bb"
data$bar_clr[4] <- "#ec8380"
data$bar_clr_text[4] <- "gray26"
data$bar_face[1:4] <- "bold"

#reverse the order for the plot to be in the correct order
data$disorder_reversed <- fct_rev(data$disorder)

#plot
p1 <- ggplot(data,aes(x=fct_rev(disorder), y=percentage, fill=disorder)) + 
  geom_bar(stat = "identity")+
  scale_y_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100))+
  coord_flip()+
  theme_minimal()+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major.y = element_blank())+
  theme(axis.text.y = element_text(size=30, 
                                   family="Montserrat",
                                   ifelse(levels(data$disorder_reversed) %in% data$disorder[1:4], "bold", "plain")))+
  theme(axis.text.x = element_text(size=30,
                                   family="Montserrat"))+
  theme(axis.title.x=element_text(size=30,
                                family="Montserrat",
                                face="bold",
                                vjust=-2),
        plot.margin = margin(10, 10, 30, 10))+
  xlab(NULL)+
  ylab("Percentage")+
  theme(legend.position = "none")+
  scale_fill_manual(values=data$bar_clr)+
  geom_text(aes(label = percentage),hjust = 1.5,
            size = 10, fontface = "bold", colour=data$bar_clr_text)

p1

#save
ggsave("figures/prevalence_plot.png", plot=p1, bg="white", width=20, height=10, units="in", dpi=300)

