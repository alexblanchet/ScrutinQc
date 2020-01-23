# Les données viennent de https://ici.radio-canada.ca/nouvelle/1484171/reforme-mode-scrutin-montreal-regions-quebec-politique
# Les données sur la densité de population viennent de : http://www.stat.gouv.qc.ca/statistiques/recensement/2011/recens2011_reg/population/poptot_superficie_reg.htm

data <- data.frame(region = c("Gaspésie-Ile-de-la-Madeleine", 
                              "Bas-Saint-Laurent", 
                              "Chaudière-Appalaches",
                              "Centre-du-Québec", 
                              "Estrie", 
                              "Montérégie", 
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
                                   8.8, 4, 6.4, 7.2, 4.8, 2.4, 0.8),
                   densite_pop = c(4.6, 9, 27.2, 33.8, 30.4, 129.6, 
                                   3779.1, 1625.1, 2.8, 0.4, 37.3,
                                   7.3, 38, 26.9, 12, 2.5, 0.1),
                   nb_electeurs = c(75657, 158112, 328897, 189514, 245160, 
                                    1151446, 1312927, 307058, 217279, 69550, 
                                    561253, 214612, 385945, 464134, 278135, 
                                    113711, 28575 )
)

data$unvote <- (data$prop_sieges / data$prop_elec) -1
data$situation <- ifelse(data$unvote > 0 , "Surreprésentation", 
                         ifelse(data$unvote == 0, "Équilibre (1 personne = 1 vote)",
                                "Sous-représentation"))

data$densite_pop_gp <- factor( ifelse(data$densite_pop < 10, "Moins de 10", ifelse(
  data$densite_pop >= 10 & data$densite_pop < 20, "10 à 19", ifelse(
    data$densite_pop >= 20 & data$densite_pop < 30, "20 à 29", ifelse(
      data$densite_pop >= 30 & data$densite_pop < 40, "30 à 39", ifelse(
        data$densite_pop >= 40 & data$densite_pop < 50, "40 à 49", ifelse(
          data$densite_pop >= 50 & data$densite_pop < 200, "100 à 200", ifelse(
            data$densite_pop > 200, "Plus de 1000", NA
          )
        ) 
      )
    )
  )
)
)
)

data$densite_pop_gp <- factor(data$densite_pop_gp,levels(data$densite_pop_gp)[c(5, 1, 3, 4, 2, 6)])


library(ggplot2)
library(RColorBrewer)

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


fig2 <- ggplot(data=data, aes(y=unvote, x=region)) +
  geom_bar(stat = "identity", aes(fill = densite_pop_gp)) + 
  scale_x_discrete(limits = rev(levels(data$region))) +
  coord_flip() +
  ggtitle("Représentation des régions du Québec suivant le projet de loi actuel") +
  ylab("Écart de représentation par rapport à 1 personne = 1 vote") +
  xlab("Région") +
  geom_hline(aes(yintercept = 0), colour = "grey") +
  scale_fill_brewer("Habitant / km^2", palette = "OrRd") +
  ylim(-0.5, 1.25) +
  theme_minimal()

ggsave("fig2.jpg", fig2, width = 20, height = 10, units = "cm")

ggplot(data=data, aes(y=unvote, x=region)) +
  geom_bar(stat = "identity", aes(fill = nb_electeurs)) + 
  scale_x_discrete(limits = rev(levels(data$region))) +
  coord_flip() +
  ggtitle("Représentation des régions du Québec suivant le projet de loi actuel") +
  ylab("Écart de représentation par rapport à 1 personne = 1 vote") +
  xlab("Région") +
  geom_hline(aes(yintercept = 0), colour = "grey") +
  scale_fill_continuous("Nombre d'électeurs") +
  ylim(-0.5, 1.25) +
  theme_minimal()


