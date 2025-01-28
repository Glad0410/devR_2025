paste("L'abstention moyenne est de ", round(moyenneabs, digits=2), "%")
[1] "L'abstention moyenne est de  24.12 %"
voteJadot2040 <- resultats_pres2022 [resultats_pres2022$Part_Jadot > 20 & resultats_pres2022$Part_Jadot < 40 , ]
View (voteJadot2040) # permet de visualiser la nouvelle extraction
voteSaintes <- resultats_pres2022 [resultats_pres2022$Libelle =='Saintes' & resultats_pres2022$Code_dep == '17' ,]
View (voteSaintes) # permet de visualiser la nouvelle extraction
write.csv(voteSaintes, "voteSaintes.csv", row.names = FALSE, fileEncoding = "UTF-8")
voteSaintes_Candidats1_3 <- voteSaintes [ , c("Part_Macron", "Part_LePen", "Part_Melenchon")]
View (voteSaintes_Candidats1_3) # permet de visualiser la nouvelle extraction
ecartType <- sd(voteSaintes_Candidats1_3$Part_Macron Part_LePen Part_Melenchon)
ecartTypeMacron <- sd(voteSaintes_Candidats1_3$Part_Macron)
ecartTypeLePen <- sd(voteSaintes_Candidats1_3$Part_LePen)
ecartTypeMelenchon <- sd(voteSaintes_Candidats1_3$Part_Melenchon)
voteSaintes_Candidats1_3$Macron30 <- NA
voteSaintes_Candidats1_3$Macron30 <- ifelse(voteSaintes_Candidats1_3$Part_Macron > 30,
+                                        print ("MacronSup30"),
+                                        NA)
voteArthaudVaucluse <- resultats_pres2022 [resultats_pres2022$Code_dep == '84' ,]
tapply(voteArthaudVaucluse$Part_Arthaud,voteArthaudVaucluse$Libelle, max )

View(voteArthaudVaucluse)
voteLaRochelle <- resultats_pres2022 [resultats_pres2022$Libelle =='La Rochelle' & resultats_pres2022$Code_dep == '17' ,]
View(voteLaRochelle)
voteLaRochelle_Candidats1_3 <- voteLaRochelle [ , c("Part_Hidalgo", "Part_Jadot", "Part_Melenchon")]
View(voteLaRochelle_Candidats1_3)
col1 <- c("Hidalgo", "Jadot", "Melenchon")

col2 <- c(mean(voteLaRochelle_Candidats1_3$Part_Hidalgo), mean(voteLaRochelle_Candidats1_3$Part_Jadot), mean(voteLaRochelle_Candidats1_3$Part_Melenchon))

data <- data.frame(group=col1, value=col2)

ggplot(data, aes(x="", y=value , fill=group)) +
    geom_bar(stat="identity", width=1) +
    geom_col() +
    coord_polar("y", start=0) +
    
     geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("#f7ff00", "#17a442", "#ffa0fb"))
voteRoyan1_2 <- voteRoyan [ , c("Code_BV", "Part_Macron", "Part_LePen", "Part_Melenchon")]
 
# On reformatte le tableau de données (nécessite le package tidyr)
reformat <-
     voteRoyan %>% pivot_longer(cols=c("Part_Macron", "Part_LePen", "Part_Melenchon"),
                                                names_to='candidats', values_to='parts')

# On réalise autant de graphs qu’il existe de Bureaux de vote (« facet_wrap »)
ggplot(data = reformat, aes(x = candidats, y = parts, fill = candidats)) +
geom_bar(stat = "identity") +geom_col() +
coord_polar("y", start=0) +
facet_wrap(~Code_BV) +
ggtitle("Parts de vote dans les BV de Royan") +
theme_minimal() +
theme(
+   plot.title = element_text(family = "Arial", face = "bold", size = 20, hjust = 0.5, color = "#555555"),
+   axis.text.x = element_blank(), # Supprime le texte de l'axe x
+   axis.ticks.x = element_blank(), # Supprime les ticks de l'axe x
+   axis.text.y = element_blank(), # Supprime le texte de l'axe y
    axis.ticks.y = element_blank(), # Supprime les ticks de l'axe y
    panel.background = element_rect(fill = "lightgrey", color = NA) # Ajoute un fond gris clair
) +
scale_fill_manual(values = c("#0a3895", "#b831f3", "#f33157"))
> library(DBI)
> library(RPostgres)
> library(RPostgreSQL)
> # Paramétrage de la connexion PostGreSQL
> db <- "postgres"
> db_host <- "localhost"
> db_port <- "5432"
> db_user <- "postgres"
> db_pass <- "allezleTFC1"
> conn <- dbConnect(RPostgres::Postgres(),dbname = db,host = db_host,port = db_port,user = db_user,password = db_pass)
> conn # doit renvoyer <PqConnection> postgres@localhost:5432
<PqConnection> postgres@localhost:5432
> # Requête de sélection
> requete <- dbGetQuery(conn, 'SELECT * from php.form;')
> view(requete)
 requete_sql <- "DELETE FROM php.form WHERE typedemande = '1'"