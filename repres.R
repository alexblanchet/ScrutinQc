data <- data.frame(region = c("Gaspésie-Ile-de-la-Madeleine", 
                              "Bas-Saint-Laurent", 
                              "Chaudière-Appalaches",
                              "Centre-du-Québec", 
                              "Estrie", "Montérégie", 
                              "Montréal", 
                              "Laval", 
                              "Saguenay-Lac-Saint-Jean",
                              "Côte-Nord", 
                              "Capitale-Nationale", 
                              "Mauricie", 
                              "Lanaudière", 
                              "Laurentides", 
                              "Outaouais",
                              "Abitibi-Témiscamingue", 
                              "Nord-du-Québec"),
                   prop_elec = c(1.2, 2.6, 5.4, 3.1, 4, 
                                 18.9, 21.5, 5, 3.6, 1.1, 
                                 9.2, 3.5, 6.3, 7.6, 4.6,  1.9, 0.5),
                   prop_sieges = c(2.4, 2.4, 5.6, 4, 4,
                                   17.6, 19.2, 4.8, 4, 1.6, 
                                   8.8, 4, 6.4, 7.2, 4.8, 2.4, 0.8)
)

data$unvote <- (data$prop_sieges / data$prop_elec) -1
data$situation <- ifelse(data$unvote > 0 , "Surreprésentation", 
                         ifelse(data$unvote == 0, "Équilibre (1 personne = 1 vote)",
                                "Sous-représentation"))

library(ggplot2)

fig <- ggplot(data=data, aes(y=unvote, x=region)) +
          geom_bar(stat = "identity", aes(fill = situation)) + 
          scale_x_discrete(limits = rev(levels(data$region))) +
          coord_flip() +
          ggtitle("Représentation des régions du Québec suivant le projet de loi actuel") +
          ylab("Écart de représentation par rapport à 1 personne = 1 vote") +
          xlab("Région") +
          geom_hline(aes(yintercept = 0), colour = "grey") +
          scale_fill_manual("Situation", values=c("grey", "firebrick1", "steelblue")) +
          ylim(-0.5, 1.25) +
          theme_minimal()

ggsave("fig.jpg", fig, width = 20, height = 10, units = "cm")
