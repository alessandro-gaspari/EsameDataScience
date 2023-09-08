
# Bandiere tricolori per continente
library(ggplot2)   # Inserisco le varie librerie che mi serviranno
library(dplyr)
library(tidyverse)
library(tidyr)
flag.data<-read_csv("flag_data.csv")   # Leggo dal file CSV i dati che mi serviranno per le analisi

tricolori <- filter(flag.data, V10 == 3)  # Filtro il dataset, tenendo solo i valori nella colonnna V10 che saranno = a 3

# Conto il numero di bandiere tricolori per continente e paese
conteggi_tricolori <- count(tricolori, V2, V1)

# Ordino i continenti in base al numero di bandiere tricolori
conteggi_tricolori <- conteggi_tricolori %>%
  arrange(V2)

# Creo il grafico a barre suddivise con ggplot
grafico_tricolori <- ggplot(data = conteggi_tricolori) +
  geom_bar(mapping = aes(x = factor(V2, levels = c(1, 2, 3, 4, 5, 6)), y = n, fill = factor(V2, levels = c(1, 2, 3, 4, 5, 6))), stat = "identity")  +
  # Metto quindi sulla x il nome dei continenti e sulla y n (variabile)
  scale_x_discrete(labels = c("N.America", "S.America", "Europe", "Africa",   "Asia", "Oceania")) +  # Creo le etichette per i continenti
  scale_fill_manual(values = c("#ff0051", "#ffa200", "#0091ff", "#000000", "#2bff00", "#6f00ff")) +  # Associo dei colori ai continenti
  xlab("Continente") +  # Nome dell'asse delle x
  ylab("Numero di bandiere tricolori") +  # Nome dell'asse delle y
  theme(axis.text.x = element_text(size = 14, color = "red", face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 14, color = "black", face ="bold") ,
        strip.text = element_text(size = 16, face = "bold", color = "red"),
        strip.background = element_blank()) +
  guides(fill=FALSE)  # Tolgo la legenda a destra per non creare disordine

print(grafico_tricolori)

#-------------------------------------------------------------------------------------------------------------------------------------------------------

# I due colori principali per ogni religione

library(ggplot2)  # Inserisco le librerie
library(dplyr)
library(tidyr)

flag.data <- read.csv("flag_data.csv")  # Leggo i dati dal CSV

conteggio_colori <- flag.data %>%   # Uso dplyr per manipolare i dati da flag.data
  group_by(V7, V18) %>%  # Raggruppo e conto le linee di ciascun gruppo creando una colonna per il conteggio n
  count() %>% 
  arrange(V7, desc(n)) %>%  # Ordino V7 in modo crescente e n in modo decrescente
  group_by(V7) %>%  # Raggruppo di nuovo solo per V7
  top_n(2, n)  # Mantengo solo i due colori più presenti per ogni religione (valori più alti di n)

grafico_conteggio <- ggplot(conteggio_colori, aes(x = factor(V7), y = n, fill = V18)) +  # Uso il dataframe conteggio_colori V7 sulle x, n sulle y e V18 per riempire
  geom_bar(stat = "identity", position = "dodge", color="black", size=1.1) +  # metto le barre una di fianco all'altra senza sovrapporle
  scale_x_discrete(labels = c("0" = "Cattolici", "1" = "Altri cristiani", "2" = "Mussulmani", 
                              "3" = "Buddhisti", "4" = "Hindu", "5" = "Etnici",   # creo le etichette per le religioni
                              "6" = "Marxisti", "7" = "Altro")) +
  scale_fill_manual(values = c("#006FFF", "#643C00", "#FFCC00", "#076000", "orange", "red", "white", "gray")) + #assegno i colori esatti alle lore descrizioni
  scale_y_continuous(breaks=seq(0, (max(conteggio_colori$n)+1), 1), expand=c(0, 1)) +  # faccio vedere tutti i valori sulla y fino ad un massimo di n
  xlab("Religioni") + # Nome dell'asse delle x 
  ylab("Numero di bandiere") + # Nome dell'asse delle y
  theme_light() +   # Imposto un tema scuro per poter vedere bene anche il bianco
  theme(axis.text.x = element_text(size = 12, color = "red", face = "bold", hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold", color = "red"),
        axis.title.x=element_blank(), 
        axis.text.y = element_text(size=10, color="black", face ="bold") ,
        strip.background = element_blank()) +
  labs(fill="Colori: ")  # Cambio il titolo nella legenda

print(grafico_conteggio)
#-------------------------------------------------------------------------------------------------------------------------------------------------------

# Continente con bandiere con più colori

flag.data <- read.csv("flag_data.csv")  # Leggo i dati dal CSV

colori_personalizzati <- c("red", "#ffa200", "#0091ff", "#f758fc", "#2bff00", "#6f00ff")  # Creo i miei colori personalizzati
etichette_continente <- c("N.America", "S.America", "Europa", "Africa", "Asia", "Oceania") # Creo le etichette dei continenti

stati_continenti <- flag.data %>%
  group_by(V2) %>%
  count() %>%
  ungroup() %>%
  complete(V2, fill = list(n = 0)) %>%
  left_join(flag.data) %>% 
  ungroup()

stati_continenti$V2 <- as.factor(stati_continenti$V2)


grafico_boxplot<-ggplot(stati_continenti, aes(x=V2, y=V10)) +
  geom_boxplot(fill=colori_personalizzati) +
  scale_x_discrete(labels = etichette_continente) +
  scale_y_continuous(breaks=seq(0, 8), "NUMERO DI COLORI") +
  theme(axis.text.x=element_text(size = 15, color = "red", face = "bold", hjust = 0.5),
       strip.text = element_text(size = 10, face = "bold", color = "red"),
       axis.title.x=element_blank(),
       axis.text.y=element_text(size=10, color="black", face="bold"),
       strip.background = element_blank())
  
  
print(grafico_boxplot)  # stampo il grafico

#-------------------------------------------------------------------------------------------------------------------------------------------------------

# Presenza di simboli e i due colori principali in paesi con lingua parlata uguale

library(ggplot2)  # Inserisco le librerie
library(dplyr)
library(tidyr)
flag_data <- read.csv("flag_data.csv")  # Leggo i dati dal CSV

# Definisco le etichette delle lingue
lingue_etichette <- c("Inglese", "Europeo", "Francese", "Tedesco", "Slavo", "Altro indo-europeo", "Cinese", "Arabo", "Giapponese/Turco/Finlandese", "Altro")

# Aggiungo etichette alle lingue
flag_data$V6 <- factor(flag_data$V6, levels = 1:10, labels = lingue_etichette)

# Calcolo la presenza o l'assenza di simboli per ogni bandiera (mi basterà che una di quelle colonne abbia un valore di 1)
flag_data$presenza_simboli <- ifelse(rowSums(flag_data[, c("V23", "V24", "V25", "V26", "V27")]) > 0, 1, 0)

# Calcolo il numero di bandiere con e senza simboli per ogni lingua
num_bandiere_simboli <- aggregate(cbind(con_simboli = presenza_simboli) ~ V6, flag_data, sum) # creo una somma che indica il conteggio delle bandiere con simboli
num_bandiere_totale <- aggregate(rep(1, nrow(flag_data)) ~ V6, flag_data, sum) # creo la somma delle bandiere totali
num_bandiere <- merge(num_bandiere_simboli, num_bandiere_totale, by = "V6") # unisco i due dataframe precedenti con V6 come chiave di unione
num_bandiere$senza_simboli <- num_bandiere$`rep(1, nrow(flag_data))` - num_bandiere$con_simboli  # calcolo il conteggio delle bandiere senza simboli togliendo da quelle totali quelle con simboli

# Calcolo la percentuale di bandiere con simboli e senza simboli per ogni lingua
num_bandiere$percentuale_senza_simboli <- num_bandiere$senza_simboli / num_bandiere$`rep(1, nrow(flag_data))` * 100
num_bandiere$percentuale_con_simboli <- num_bandiere$con_simboli / num_bandiere$`rep(1, nrow(flag_data))` * 100

# Creo un dataframe per le barre grigie (nere in realtà nel grafico)
barre_grigie <- data.frame(
  y = num_bandiere$V6, # nelle y ci metto i valori della colonna V6 di num_bandiere
  x = num_bandiere$percentuale_con_simboli, # sulle x la percentuale con simboli in num_bandiere
  percentuale_senza_simboli = 100 - num_bandiere$percentuale_con_simboli # qui calcolo la percentuale senza simboli
) 

# Creo il grafico a barre per la percentuale di bandiere con e senza simboli
grafico_simboli <- ggplot() +
  geom_bar(data = barre_grigie, aes(x = -percentuale_senza_simboli, y = reorder(y, -percentuale_senza_simboli), fill = "Senza Simboli"), width = 0.4, stat = "identity") + #rappresento la percentuale senza simboli
  geom_bar(data = num_bandiere, aes(x = percentuale_con_simboli, y = reorder(V6, percentuale_con_simboli), fill = "Con Simboli"), width = 0.4, stat = "identity") + #rappresento la percentuale con simboli
  geom_text(data = barre_grigie, aes(x = -percentuale_senza_simboli, y = reorder(y, -percentuale_senza_simboli), label = paste0(round(percentuale_senza_simboli, digits = 0), "%")), vjust = 0.5, hjust = -0.2, size = 4, color="yellow", face="bold") + 
  geom_text(data = num_bandiere, aes(x = percentuale_con_simboli, y = V6, label = paste0(" (", round(percentuale_con_simboli, digits = 0), "%)")), vjust = 0.5, hjust = 1.2, size = 4, face="bold") + #creo i due testi delle percentuali
  labs(x = "Presenza/assenza di simboli", y = "Lingua parlata", fill = "Legenda") + # nomi degli assi
  scale_fill_manual(values = c("Con Simboli" = "yellow", "Senza Simboli" = "black"), guide = "legend") + #coloro le barre di giallo e nero
  scale_x_continuous(limits = c(-100, 100), expand = c(0, 0), labels = abs) + #imposto i limiti degli assi per evitare disordini
  scale_y_discrete(labels = num_bandiere$V6[order(num_bandiere$percentuale_con_simboli)]) + #metto le etichette sull'asse delle y
  theme_minimal() + # uso il tema minimal
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size = 10, color = "red", face = "bold", hjust = 0.5),
        strip.text = element_text(size = 10, face = "bold", color = "red"),
        axis.title.x=element_blank())

# Visualizzo il grafico delle bandiere
print(grafico_simboli)

#-------------------------------------------------------------------------------------------------------------------------------------------------------


flag.data <- read.csv("flag_data.csv")  #leggo i dati dal CSV

# Creo i miei colori da usare dopo
colori_pers <- c("blue", "green", "#FFFFFF", "red", "black", "gold", "orange","#643C00" )  #creo i miei colori 

# Creo il conteggio del numero di bandiere per colore predominante per continente
conteggio_colori <- flag.data %>% 
  group_by(V2, V18) %>%   # raggruppo per V2 e V18, continente e colore predominante
  summarize(n_bandiere = n()) %>%   # conto il numero di bandiere
  arrange(V2, desc(n_bandiere)) # ordino in modo crescente secondo V2

# Converto V18 in fattore in modo che i colori siano ordinati
conteggio_colori$V18 <- factor(conteggio_colori$V18, levels = unique(conteggio_colori$V18), ordered = TRUE)

# Creo il grafico a barre per ogni continente
grafico <- ggplot(conteggio_colori, aes(x = V18, y = n_bandiere, fill = V18)) + # x sarà il colore predominante, y il numero di bandiere e riempio col colore predominante
  geom_bar(stat = "identity", color="black") +
  facet_wrap(~ V2, nrow = 2, labeller = labeller(V2 = c("1" = "N.America", "2" = "S.America", "3" ="Europa", 
                                                        "4" = "Africa", "5" = "Asia", "6" = "Oceania"))) +   #etichette per i continenti
  # Unisco i vari grafici con wrap
  
  ylab("Numero di Bandiere") +  #nome asse y
  scale_fill_manual(values = colori_pers) +  #imposto i colori con quelli personalizzati
  scale_y_continuous(breaks = seq(0, max(conteggio_colori$n_bandiere), 4)) +  #imposto la y di 1 in 1 
  theme_light() + 
  labs(x="")+   # tolgo il nome delle x essendo che non mi serve
  theme(axis.text.x=element_blank(), #tolgo i nomi delle x
        strip.text = element_text(size = 16, face = "bold", color = "red"),
        axis.text.y=element_text(size=10, color="black", face="bold"),
        strip.background = element_blank(),
        legend.position = "none")  # tolgo la legenda

print(grafico)

