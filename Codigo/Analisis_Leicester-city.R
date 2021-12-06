require(tidyverse)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(gghighlight)
library(patchwork)
require(vcd)

source(here::here('Codigo/Cargar_datos.R'), encoding = 'UTF-8')

# La temporada 2015-2016 empieza el 8 de agosto del 2015 y termina el 15 de mayo del 2016

#Filtramos
PL_season_2014_2015 = eventos %>% 
  filter(league == 'Premier League', date >= "2014-08-16" & date <= '2015-05-24') 

PL_season_2015_2016 = eventos %>% 
  filter(league == 'Premier League', date >= "2015-08-08" & date <= '2016-05-15') 


LeicesterCity = PL_season_2015_2016 %>% 
  filter(event_team == 'Leicester City' | opponent == 'Leicester City')


# Mas goles por tiro PL 2015-2016 -----------------------------------------


shots = PL_season_2015_2016[PL_season_2015_2016$event_type=='Intento',] %>% 
  count(event_team)
goals = PL_season_2015_2016[PL_season_2015_2016$is_goal==1,] %>% 
  count(event_team)
goals_table_2015_2016 = left_join(shots,goals, by = 'event_team')
goals_table_2015_2016$goals_per_shot = round(goals_table_2015_2016$n.y / goals_table_2015_2016$n.x,2)
goals_table_2015_2016 = goals_table[order(desc(goals_table_2015_2016$goals_per_shot)),]
goals_table_2015_2016$event_team = factor(goals_table_2015_2016$event_team, levels = goals_table_2015_2016$event_team[order(desc(goals_table_2015_2016$goals_per_shot))])

ggplot(goals_table_2015_2016, aes(x = event_team, y = goals_per_shot, fill = event_team)) +
  geom_bar(stat = "identity",   width = 0.5, show.legend = F) + 
  theme_minimal() +
  gghighlight(event_team == "Leicester City" | event_team == 'Chelsea') +
  labs(title = "Ratio de goles por tiro", y='Goles por tiro', x=NULL,
       subtitle = ' Premier League temporada 2015-2016') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4),
        plot.title.position = "plot") +
  geom_text(aes(label = goals_per_shot),vjust = -1, size = 3, color = 'black')


# Mas goles por tiro PL 2014-2015 -----------------------------------------
shots = PL_season_2014_2015[PL_season_2014_2015$event_type=='Intento',] %>% 
  count(event_team)
goals = PL_season_2014_2015[PL_season_2014_2015$is_goal==1,] %>% 
  count(event_team)
goals_table_2014_2015 = left_join(shots,goals, by = 'event_team')
goals_table_2014_2015$goals_per_shot = round(goals_table_2014_2015$n.y / goals_table_2014_2015$n.x,2)
goals_table_2014_2015 = goals_table_2014_2015[order(desc(goals_table_2014_2015$goals_per_shot)),]
goals_table_2014_2015$event_team = factor(goals_table_2014_2015$event_team, levels = goals_table_2014_2015$event_team[order(desc(goals_table_2014_2015$goals_per_shot))])

ggplot(goals_table_2014_2015, aes(x = event_team, y = goals_per_shot, fill = event_team)) +
  geom_bar(stat = "identity",   width = 0.5, show.legend = F) + 
  theme_minimal() +
  gghighlight(event_team == "Leicester City" | event_team == 'Chelsea') +
  labs(title = "Ratio de goles por tiro", y='Goles por tiro', x=NULL,
       subtitle = ' Premier League temporada 2014-2015') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4),
        plot.title.position = "plot") +
  geom_text(aes(label = goals_per_shot),vjust = -1, size = 3, color = 'black')



# Goles por partido -------------------------------------------------------


# Goles por partido temporada 2014-2015 -----------------------------------

total_matches = count(PL_season_2014_2015 %>% 
                         filter(event_team=='Leicester City') %>% 
                         distinct(id_odsp)) 

goals_table_2014_2015$matches = rep(as.numeric(total_matches), length(goals_table_2014_2015[,1]))
goals_table_2014_2015$goals_per_match = round(goals_table_2014_2015$n.y / goals_table_2014_2015$matches, 2)
goals_table_2014_2015 = goals_table_2014_2015[order(desc(goals_table_2014_2015$goals_per_match)),]
goals_table_2014_2015$event_team = factor(goals_table_2014_2015$event_team, levels = goals_table_2014_2015$event_team[order(desc(goals_table_2014_2015$goals_per_match))])

ggplot(goals_table_2014_2015, aes(x = event_team, y = goals_per_match, fill = event_team)) +
  geom_bar(stat = "identity",   width = 0.5, show.legend = F) + 
  theme_minimal() +
  gghighlight(event_team == "Leicester City" | event_team == 'Chelsea') +
  labs(title = "Promedio de goles por partido", y='Goles por partido', x=NULL,
       subtitle = 'Premier League temporada 2014-2015') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4),
        plot.title.position = "plot") +
  geom_text(aes(label = goals_per_match),vjust = -1, size = 3, color = 'black')


# Goles por partido temporada 2015-2016 -----------------------------------
total_matches = count(PL_season_2015_2016 %>% 
                        filter(event_team=='Leicester City') %>% 
                        distinct(id_odsp)) 

goals_table_2015_2016$matches = rep(as.numeric(total_matches), length(goals_table_2015_2016[,1]))
goals_table_2015_2016$goals_per_match = round(goals_table_2015_2016$n.y / goals_table_2015_2016$matches, 2)
goals_table_2015_2016 = goals_table_2015_2016[order(desc(goals_table_2015_2016$goals_per_match)),]
goals_table_2015_2016$event_team = factor(goals_table_2015_2016$event_team, levels = goals_table_2015_2016$event_team[order(desc(goals_table_2015_2016$goals_per_match))])

ggplot(goals_table_2015_2016, aes(x = event_team, y = goals_per_match, fill = event_team)) +
  geom_bar(stat = "identity",   width = 0.5, show.legend = F) + 
  theme_minimal() +
  gghighlight(event_team == "Leicester City" | event_team == 'Chelsea') +
  labs(title = "Promedio de goles por partido", y='Goles por partido', x=NULL,
       subtitle = 'Premier League temporada 2015-2016') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4),
        plot.title.position = "plot") +
  geom_text(aes(label = goals_per_match),vjust = -1, size = 3, color = 'black') 

# Promedio de goles por partido
mean(goals_table$goals_per_match)

# Test de hipotesis goles por partido -------------------------------------------------

archivo = PL_season_2015_2016 %>%
  filter(event_team == 'Leicester City') %>% 
  select(id_odsp, event_team, opponent, is_goal)

archivo$id_odsp = as.character(archivo$id_odsp)
archivo$event_team = as.character(archivo$event_team)
archivo$opponent = as.character(archivo$opponent)
archivo$is_goal = as.numeric(archivo$is_goal)
write.csv(archivo, file = 'PL_2015.csv')
########################################################


goles_por_partido = read.csv(file = 'PL_2014_ok.csv', encoding = 'UTF-8')

n = nrow(goles_por_partido)

LC_2014 = goles_por_partido %>% filter(equipo == 'Leicester City')


dist_emp = LC_2014 %>% 
  ggplot(aes(goles)) +
  geom_histogram(aes(y = stat(count)/sum(count)), position="identity",
                 bins = 17, fill = 'coral2') +
  labs(title = 'Distribución de goles convertidos por Leicester City', 
       subtitle = 'Temporada 2014-2015',
       x = 'Goles en partidos', y = 'Probabilidad') +
  theme(plot.title.position = "plot") 


k = sort(LC_2014$goles)
gf<-goodfit(k, type = "poisson", method = "MinChisq")
gf$par
summary(gf)

pois = data.frame(x = rpois(100,  as.integer(gf$par)))

dist_teo = ggplot(pois, aes(x)) +
  geom_histogram(aes(y = stat(count)/sum(count)), bins = 17, fill = 'blue') +
  labs(title = 'Distribución teórica poisson(1.22)', x = 'Goles en partidos', 
       y = 'Probabilidad') +
  scale_x_continuous(breaks=seq(1,6,by=1))

dist_emp + dist_teo

LC_2014 %>% 
  ggplot(aes(goles)) +
  geom_histogram(aes(y = stat(count)/sum(count)), position="identity",
                 bins = 17, fill = 'coral2') +
  labs(title = 'Distribucion teórica vs empírica', 
       subtitle = 'Goles Leicester temporada 2014-2015', 
       x = 'Goles en partidos', y = 'Probabilidad') + 
  theme(plot.title.position = "plot") +
  geom_histogram(data = pois, aes(x= x, y = stat(count)/sum(count)), 
                 bins = 17, fill = 'blue',
                 alpha = 0.5) 


n = length(k)
i = 1:n
pi = (i - 0.5)/n
quantiles = qpois(p = pi, lambda = as.integer(gf$par))
plot(k, quantiles)
abline(a = 0, b = 1, col = 2)


# No se rechaza hipotesis nula, falta evidencia. 

xempp <- seq(min(k), max(k), by=0.0001)
plot(xempp, ppois(xempp, lambda=as.integer(gf$par)), type="l", col="red", xlab="Número de usuarios",ylab="ppois(usuarios, lambda)")
plot(ecdf(k), add=TRUE)


##################################################

goles_por_partido = read.csv(file = 'PL_2015_ok.csv', encoding = 'UTF-8')

n = nrow(goles_por_partido)

LC_2015 = goles_por_partido %>% filter(equipo == 'Leicester City')


dist_emp = LC_2015 %>% 
  ggplot(aes(goles)) +
  geom_histogram(aes(y = stat(count)/sum(count)), position="identity",
                 bins = 17, fill = 'coral2') +
  labs(title = 'Distribución de goles convertidos por Leicester City', 
       subtitle = 'Temporada 2015-2016',
       x = 'Goles en partidos', y = 'Probabilidad') +
  theme(plot.title.position = "plot") 


k = sort(LC_2015$goles)
gf<-goodfit(k, type = "poisson", method = "MinChisq")
gf$par
summary(gf)

# No se rechaza hipotesis nula, falta evidencia.

pois = data.frame(x = rpois(100,  as.integer(gf$par)))

dist_teo = ggplot(pois, aes(x)) +
  geom_histogram(aes(y = stat(count)/sum(count)), bins = 17, fill = 'blue') +
  labs(title = 'Distribución teórica poisson(1.85)', x = 'Goles en partidos', 
       y = 'Probabilidad') +
  scale_x_continuous(breaks=seq(1,6,by=1))

dist_emp + dist_teo

LC_2015 %>% 
  ggplot(aes(goles)) +
  geom_histogram(aes(y = stat(count)/sum(count)), position="identity",
                 bins = 17, fill = 'coral2') +
  labs(title = 'Distribucion teórica vs empírica', 
       subtitle = 'Goles Leicester temporada 2014-2015', 
       x = 'Goles en partidos', y = 'Probabilidad') + 
  theme(plot.title.position = "plot") +
  geom_histogram(data = pois, aes(x= x, y = stat(count)/sum(count)), 
                 bins = 17, fill = 'blue',
                 alpha = 0.5)
  
  
  n = length(k)
i = 1:n
pi = (i - 0.5)/n
quantiles = qpois(p = pi, lambda = as.integer(gf$par))
plot(k, quantiles)
abline(a = 0, b = 1, col = 2)

xempp <- seq(min(k), max(k), by=0.0001)
plot(xempp, ppois(xempp, lambda=as.integer(gf$par)), type="l", col="red", xlab="Número de usuarios",ylab="ppois(usuarios, lambda)")
plot(ecdf(k), add=TRUE)


# Sector de gol -----------------------------------------------------------
goal_location = subset(LeicesterCity,!is.na(shot_place)) %>% 
  filter(shot_place %in% c('Esquina inferior izquierda',
                           'Esquina inferior derecha',
                           'Centro de la portería',
                           'Esquina superior izquierda',
                           'Esquina superior derecha'),
         event_team == 'Leicester City') %>% 
  select(shot_place, is_goal)

goal_location$is_goal = ifelse(goal_location$is_goal == 1, 'Gol', 'No gol')

goal_location %>% 
  ggplot(aes(x = shot_place, fill = is_goal)) +
  geom_bar(position = 'dodge', aes( y = prop.table(..count..) * 100)) + 
  theme_minimal() +
  scale_color_colorblind() +
  labs(title = "Porcentaje de goles anotados por sector de portería",
       subtitle = "Resultados Leicester City temporada 2015-2016",
       x = "Zona de la portería",
       y = "Probabilidad de gol") +
  scale_fill_discrete("Resultado")+
  theme(axis.text.x = element_text(angle=45),
        plot.title.position = "plot")+
  geom_text(aes(y = prop.table(..count..) * 100, 
                label = scales::percent((..count..)/sum(..count..))),
            stat = 'count', 
            position = position_dodge(.9), 
            hjust = -.5,
            size = 3) +
  coord_flip()



# Características de los intentos de gol del Leicester City ---------------

LC_attempts = PL_season_2015_2016 %>% 
  filter(event_team=='Leicester City' & 
           event_type=='Intento' & 
           is_goal==1 & 
           location!='No registrado') %>% 
  select(event_team, location, situation, assist_method) %>% 
  group_by(location, situation, assist_method) %>% 
  summarize(num_goals=n()) %>%
  arrange(desc(num_goals))
colnames(LC_attempts) = c('Ubicación del campo', 'Situación del juego', 
                          'Método de asistencia', 'Cantidad de goles')
knitr::kable(LC_attempts[1:10,])
         


# Goleadores --------------------------------------------------------------

goleadores = LeicesterCity[LeicesterCity$is_goal==1 & 
                             LeicesterCity$event_team=='Leicester City',] %>% 
  count(player)
goleadores$player = factor(goleadores$player, levels = goleadores$player[order(goleadores$n)])

ggplot(goleadores, aes(n, player)) +
  geom_point(size = 7, aes( colour= 'f15bb5'), show.legend = F) +
  labs(title = 'Goleadores Leicester City', subtitle = 'Temporada 2015-2016',
       y = 'Jugador', x = 'Cantidad de goles') +
  geom_text(aes(label = n), size = 3, color = 'white') +
  theme(plot.title.position = "plot")







## IDEAS

# Analizar los goles. Ver cual es el metodo que mas se repite. Hint: contraataques
# Analizar al jugador Vardy, UN CRACK de aquella temporada




# Graficamos los intentos a porteria
  
LC_shot_place = subset(LeicesterCity,!is.na(shot_place)) %>% 
  filter(event_team == 'Leicester City') %>% 
  ggplot(aes(x = reorder(shot_place, -table(shot_place)[shot_place]))) +
  geom_bar(position = 'dodge') + 
  theme_minimal() +
  scale_color_colorblind() +
  labs(title = "How does the shots on goal end",
       subtitle = "Leicester City Attemps",
       x = "Event",
       y = "Frecuency") +
  theme(axis.text.x = element_text(angle=45, vjust = 0.8), plot.title.position = "plot") 

LC_shot_place

LC_shot_outcome = subset(LeicesterCity,!is.na(shot_outcome)) %>% 
  filter(event_team == 'Leicester City') %>% 
  ggplot(aes(x = reorder(shot_outcome, -table(shot_outcome)[shot_outcome]))) +
  geom_bar(position = 'dodge') + 
  theme_minimal() +
  scale_color_colorblind() +
  labs(title = "How does the shots on goal end",
       subtitle = "Leicester City Attemps",
       x = "Event",
       y = "Frecuency") +
  theme(axis.text.x = element_text(angle=45, vjust = 0.8)) 

LC_shot_outcome





# Pruebas

  
archivo = eventos %>%
  filter(league == 'Premier League') %>% 
  select(id_odsp, event_team, opponent, is_goal)

archivo$id_odsp = as.character(archivo$id_odsp)
archivo$event_team = as.character(archivo$event_team)
archivo$opponent = as.character(archivo$opponent)
archivo$is_goal = as.numeric(archivo$is_goal)
write.csv(archivo, file = 'Premier_league.csv')

