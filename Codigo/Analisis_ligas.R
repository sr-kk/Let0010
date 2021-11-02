require(tidyverse)
require(GGally)
library(dplyr)
library(sqldf)

events = read.csv(file = 'Datos/events.csv')
matches = read.csv(file = 'Datos/ginf.csv')

# creamos una copia de los datos
eventos = events

# Combinaremos los datos para asi obtener informacion mas detallada de cada evento. Anadiremos la liga a la que corresponde cada evento y la temporada

eventos <- left_join(eventos, matches[,c('id_odsp','league','season')], by = "id_odsp")

# A continuacion reescribiremos las variables con el fin de que sean mas explicitas 

eventos$event_type = case_when(
  eventos$event_type == 0 ~ 'Announcement',
  eventos$event_type == 1 ~ 'Attempt',
  eventos$event_type == 2 ~ 'Corner',
  eventos$event_type == 3 ~ 'Foul',
  eventos$event_type == 4 ~ 'Yellow card',
  eventos$event_type == 5 ~ 'Second yellow card',
  eventos$event_type == 6 ~ 'Red card',
  eventos$event_type == 7 ~ 'Substitution',
  eventos$event_type == 8 ~ 'Free kick won',
  eventos$event_type == 9 ~ 'Offside',
  eventos$event_type == 10 ~ 'Hand ball',
  eventos$event_type == 11 ~ 'Penalty conceded',
  TRUE ~ as.character(eventos$event_type)
)

eventos$event_type2 = case_when(
  eventos$event_type2 == 12 ~ 'Key Pass',
  eventos$event_type2 == 13 ~ 'Failed through ball',
  eventos$event_type2 == 14 ~ 'Sending off',
  eventos$event_type2 == 15 ~ 'Own goal',
  TRUE ~ as.character(eventos$event_type2)
)

eventos$side <- ifelse(eventos$side == 1, 'Home', 'Away')

eventos$shot_place = case_when(
  eventos$shot_place == 1 ~ 'Bit too high',
  eventos$shot_place == 2 ~ 'Blocked',
  eventos$shot_place == 3 ~ 'Bottom left corner',
  eventos$shot_place == 4 ~ 'Bottom right corner',
  eventos$shot_place == 5 ~ 'Centre of the goal',
  eventos$shot_place == 6 ~ 'High and wide',
  eventos$shot_place == 7 ~ 'Hits the bar',
  eventos$shot_place == 8 ~ 'Misses to the left',
  eventos$shot_place == 9 ~ 'Misses to the right',
  eventos$shot_place == 10 ~ 'Too high',
  eventos$shot_place == 11 ~ 'Top centre of the goal',
  eventos$shot_place == 12 ~ 'Top left corner',
  eventos$shot_place == 13 ~ 'Top right corner',
  TRUE ~ as.character(eventos$shot_place)
)

eventos$shot_outcome = case_when(
  eventos$shot_outcome == 1 ~ 'On target',
  eventos$shot_outcome == 2 ~ 'Off target',
  eventos$shot_outcome == 3 ~ 'Blocked',
  eventos$shot_outcome == 4 ~ 'Hit the bar',
  TRUE ~ as.character(eventos$shot_outcome)
)

eventos$location = case_when(
  eventos$location == 1 ~ 'Attacking half',
  eventos$location == 2 ~ 'Defensive half',
  eventos$location == 3 ~ 'Centre of the box',
  eventos$location == 4 ~ 'Left wing',
  eventos$location == 5 ~ 'Right wing',
  eventos$location == 6 ~ 'Difficult angle and long range',
  eventos$location == 7 ~ 'Difficult angle on the left',
  eventos$location == 8 ~ 'Difficult angle on the right',
  eventos$location == 9 ~ 'Left side of the box',
  eventos$location == 10 ~ 'Left side of the six yard box',
  eventos$location == 11 ~ 'Right side of the box',
  eventos$location == 12 ~ 'Right side of the six yard box',
  eventos$location == 13 ~ 'Very close range',
  eventos$location == 14 ~ 'Penalty spot',
  eventos$location == 15 ~ 'Outside the box',
  eventos$location == 16 ~ 'Long range',
  eventos$location == 17 ~ 'More than 35 yards',
  eventos$location == 18 ~ 'More than 40 yards',
  eventos$location == 19 ~ 'Not recorded',
  TRUE ~ as.character(eventos$location)
)

eventos$bodypart = case_when(
  eventos$bodypart == 1 ~ 'right foot',
  eventos$bodypart == 2 ~ 'left foot',
  eventos$bodypart == 3 ~ 'head',
  TRUE ~ as.character(eventos$bodypart)
)

eventos$assist_method = case_when(
  eventos$assist_method == 0 ~ 'None',
  eventos$assist_method == 1 ~ 'Pass',
  eventos$assist_method == 2 ~ 'Cross',
  eventos$assist_method == 3 ~ 'Headed pass',
  eventos$assist_method == 4 ~ 'Through ball',
  TRUE ~ as.character(eventos$assist_method)
)

eventos$situation = case_when(
  eventos$situation == 1 ~ 'Open play',
  eventos$situation == 2 ~ 'Set piece',
  eventos$situation == 3 ~ 'Corner',
  eventos$situation == 4 ~ 'Free kick',
  TRUE ~ as.character(eventos$situation)
)

eventos$league =  case_when(
  eventos$league == 'D1' ~ 'Bundesliga', 
  eventos$league == 'F1' ~ 'Ligue 1', 
  eventos$league == 'E0' ~ 'Premier League',
  eventos$league == 'SP1' ~ 'La Liga',
  eventos$league == 'I1' ~ 'Serie A',
  TRUE ~ as.character(eventos$league)
)

eventos$side = as.factor(eventos$side)
eventos$bodypart = as.factor(eventos$bodypart)
eventos$event_type2 = as.factor(eventos$event_type2)
eventos$event_type = as.factor(eventos$event_type)
eventos$shot_place = as.factor(eventos$shot_place)
eventos$shot_outcome = as.factor(eventos$shot_outcome)
eventos$location = as.factor(eventos$location)
eventos$assist_method = as.factor(eventos$assist_method)
eventos$situation = as.factor(eventos$situation)
eventos$time = as.factor(eventos$time)
eventos$is_goal = as.factor(eventos$is_goal)


# Haremos uso de sql para obtener separar la informacion por ligas 

Bundesliga = sqldf("select * from eventos 
                   where league = 'Bundesliga'")

Ligue1 = sqldf("select * from eventos 
                   where league = 'Ligue 1'")

PremierLeague = sqldf("select * from eventos 
                   where league = 'Premier League'")

LaLiga = sqldf("select * from eventos 
                   where league = 'La Liga'")

SerieA = sqldf("select * from eventos 
                   where league = 'Serie A'")


# A continuacion haremos una serie de comparativas entre las 5 ligas desde la temporada 2014. Esto debido a que faltan datos de la Premier League en las temporadas anteriores, de este modo estaremos siendo mas justos con el analisis.

eventos %>% 
  filter(season >= 2014) %>% 
  ggplot(aes(x = reorder(event_type, -table(event_type)[event_type]), fill = league)) +
  geom_bar(position = 'dodge') + 
  theme_minimal() +
  scale_fill_manual(values = c('#ff0000', '#ff4d00', '#ff7400', '#ff9a00', '#ffc100')) +
  labs(title = "Top events in each league (since 2014 season)",
       x = "Main event",
       y = "Frecuency") +
  scale_y_continuous(limits = c(0, 40000), breaks = seq(0,40000, by =5000)) + 
  theme(axis.text.x = element_text(angle=45, vjust = 0.8)) 
  

subset(eventos,!is.na(event_type2)) %>% 
  filter(season >= 2014) %>% 
  ggplot(aes(x = reorder(event_type2, -table(event_type2)[event_type2]), fill = league)) +
  geom_bar(position = 'dodge') + 
  theme_minimal() +
  scale_fill_manual(values = c('#ff0000', '#ff4d00', '#ff7400', '#ff9a00', '#ffc100')) +
  labs(title = "Secondary events in each league (since 2014 season)",
       x = "Event",
       y = "Frecuency") +
  theme(axis.text.x = element_text(angle=45, vjust = 0.8)) 

# Aqui no es necesario filtrar por la temporada, puesto que la informacion faltante no es de gran importancia para hacer esta comparativa

subset(eventos,!is.na(shot_place)) %>% 
  ggplot(aes(x = reorder(shot_place, -table(shot_place)[shot_place]), fill = league)) +
  geom_bar(position = 'dodge') + 
  theme_minimal() +
  scale_fill_manual(values = c('#ff0000', '#ff4d00', '#ff7400', '#ff9a00', '#ffc100')) +
  labs(title = "Shot place in each league",
       x = "Event",
       y = "Frecuency") +
  theme(axis.text.x = element_text(angle=45, vjust = 0.8)) +
  scale_y_continuous(limits = c(0, 15000), breaks = seq(0,15000, by =5000)) 

