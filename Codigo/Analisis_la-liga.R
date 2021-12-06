require(tidyverse)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(gghighlight)

source(here::here('Codigo/Cargar_datos.R'), encoding = 'UTF-8')

# Temporada 2011-2012 inicia 27-agosto-2011 hasta 13 de mayo 2012

La_liga_2011 = eventos %>% 
  filter(league == 'La Liga', date >= "2011-08-27" & date <= '2012-05-13') 


# Temporada 2012-2013 inicia 18 agosto 2012 hasta 1 junio 2013

La_liga_2012 = eventos %>% 
  filter(league == 'La Liga', date >= "2012-08-18" & date <= '2013-06-1') 


# Temporada 2013-2014 inicia 17 de agosto de 2013 hasta 18 de mayo de 2014

La_liga_2013 = eventos %>% 
  filter(league == 'La Liga', date >= "2013-08-17" & date <= '2014-05-18') 


# Temporada 2014-2015 inicia 	23 de agosto de 2014 hasta 23 de mayo de 2015

La_liga_2014 = eventos %>% 
  filter(league == 'La Liga', date >= "2014-08-23" & date <= '2015-05-23') 


# Temporada 2015-2016 inicia 21 de agosto de 2015 hasta 15 de mayo de 2016

La_liga_2015 = eventos %>% 
  filter(league == 'La Liga', date >= "2015-08-21" & date <= '2016-05-15') 




# Mas goles por tiro la liga 2011-2012 -----------------------------------------


shots_2011 = La_liga_2011[La_liga_2011$event_type=='Intento',] %>% 
  count(event_team)
goals_2011 = La_liga_2011[La_liga_2011$is_goal==1,] %>% 
  count(event_team)
table_2011 = left_join(shots_2011,goals_2011, by = 'event_team')
table_2011$goals_per_shot = round(table_2011$n.y / table_2011$n.x,2)
table_2011 = table_2011[order(desc(table_2011$goals_per_shot)),]
table_2011$event_team = factor(table_2011$event_team, levels = table_2011$event_team[order(desc(table_2011$goals_per_shot))])
table_2011$temp = rep(2011, 20)
table_2011 = as.data.frame(table_2011)


ggplot(table_2011, aes(x = event_team, y = goals_per_shot, fill = event_team)) +
  geom_bar(stat = "identity",   width = 0.5, show.legend = F) + 
  theme_minimal() +
  gghighlight(event_team == "Barcelona" | event_team == 'Real Madrid') +
  labs(title = "Ratio de goles por tiro", y='Goles por tiro', x=NULL,
       subtitle = 'La liga, temporada 2011-2012') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4),
        plot.title.position = "plot") +
  geom_text(aes(label = goals_per_shot),vjust = -1, size = 3, color = 'black')



# Mas goles por tiro la liga 2012-2013 ------------------------------------

shots_2012 = La_liga_2012[La_liga_2012$event_type=='Intento',] %>% 
  count(event_team)
goals_2012 = La_liga_2012[La_liga_2012$is_goal==1,] %>% 
  count(event_team)
table_2012 = left_join(shots_2012,goals_2012, by = 'event_team')
table_2012$goals_per_shot = round(table_2012$n.y / table_2012$n.x,2)
table_2012 = table_2012[order(desc(table_2012$goals_per_shot)),]
table_2012$event_team = factor(table_2012$event_team, levels = table_2012$event_team[order(desc(table_2012$goals_per_shot))])
table_2012$temp = rep(2012, 20)
table_2012 = as.data.frame(table_2012)

ggplot(table_2012, aes(x = event_team, y = goals_per_shot, fill = event_team)) +
  geom_bar(stat = "identity",   width = 0.5, show.legend = F) + 
  theme_minimal() +
  gghighlight(event_team == "Barcelona" | event_team == 'Real Madrid') +
  labs(title = "Ratio de goles por tiro", y='Goles por tiro', x=NULL,
       subtitle = 'La liga, temporada 2012-2013') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4),
        plot.title.position = "plot") +
  geom_text(aes(label = goals_per_shot),vjust = -1, size = 3, color = 'black')


# Mas goles por tiro la liga 2013-2014 ------------------------------------

shots_2013 = La_liga_2013[La_liga_2013$event_type=='Intento',] %>% 
  count(event_team)
goals_2013 = La_liga_2013[La_liga_2013$is_goal==1,] %>% 
  count(event_team)
table_2013 = left_join(shots_2013,goals_2013, by = 'event_team')
table_2013$goals_per_shot = round(table_2013$n.y / table_2013$n.x,2)
table_2013 = table_2013[order(desc(table_2013$goals_per_shot)),]
table_2013$event_team = factor(table_2013$event_team, levels = table_2013$event_team[order(desc(table_2013$goals_per_shot))])
table_2013$temp = rep(2013, 20)
table_2013 = as.data.frame(table_2013)

ggplot(table_2013, aes(x = event_team, y = goals_per_shot, fill = event_team)) +
  geom_bar(stat = "identity",   width = 0.5, show.legend = F) + 
  theme_minimal() +
  gghighlight(event_team == "Barcelona" | event_team == 'Real Madrid') +
  labs(title = "Ratio de goles por tiro", y='Goles por tiro', x=NULL,
       subtitle = 'La liga, temporada 2012-2013') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4),
        plot.title.position = "plot") +
  geom_text(aes(label = goals_per_shot),vjust = -1, size = 3, color = 'black')


# Mas goles por tiro la liga 2014-2015 ------------------------------------

shots_2014 = La_liga_2014[La_liga_2014$event_type=='Intento',] %>% 
  count(event_team)
goals_2014 = La_liga_2014[La_liga_2014$is_goal==1,] %>% 
  count(event_team)
table_2014 = left_join(shots_2014,goals_2014, by = 'event_team')
table_2014$goals_per_shot = round(table_2014$n.y / table_2014$n.x,2)
table_2014 = table_2014[order(desc(table_2014$goals_per_shot)),]
table_2014$event_team = factor(table_2014$event_team, levels = table_2014$event_team[order(desc(table_2014$goals_per_shot))])
table_2014$temp = rep(2014, 20)
table_2014 = as.data.frame(table_2014)

ggplot(table_2014, aes(x = event_team, y = goals_per_shot, fill = event_team)) +
  geom_bar(stat = "identity",   width = 0.5, show.legend = F) + 
  theme_minimal() +
  gghighlight(event_team == "Barcelona" | event_team == 'Real Madrid') +
  labs(title = "Ratio de goles por tiro", y='Goles por tiro', x=NULL,
       subtitle = 'La liga, temporada 2014-2015') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4),
        plot.title.position = "plot") +
  geom_text(aes(label = goals_per_shot),vjust = -1, size = 3, color = 'black')


# Mas goles por tiro la liga 2015-2016 ------------------------------------

shots_2015 = La_liga_2015[La_liga_2015$event_type=='Intento',] %>% 
  count(event_team)
goals_2015 = La_liga_2015[La_liga_2015$is_goal==1,] %>% 
  count(event_team)
table_2015 = left_join(shots_2015,goals_2015, by = 'event_team')
table_2015$goals_per_shot = round(table_2015$n.y / table_2015$n.x,2)
table_2015 = table_2015[order(desc(table_2015$goals_per_shot)),]
table_2015$event_team = factor(table_2015$event_team, levels = table_2015$event_team[order(desc(table_2015$goals_per_shot))])
table_2015$temp = rep(2015, 20)
table_2015 = as.data.frame(table_2015)

ggplot(table_2015, aes(x = event_team, y = goals_per_shot, fill = event_team)) +
  geom_bar(stat = "identity",   width = 0.5, show.legend = F) + 
  theme_minimal() +
  gghighlight(event_team == "Barcelona" | event_team == 'Real Madrid') +
  labs(title = "Ratio de goles por tiro", y='Goles por tiro', x=NULL,
       subtitle = 'La liga, temporada 2015-2016') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4),
        plot.title.position = "plot") +
  geom_text(aes(label = goals_per_shot),vjust = -1, size = 3, color = 'black')



# Goles por partido temporada 2011-2012 -----------------------------------

total_matches_2011 = count(La_liga_2011 %>% 
                        filter(event_team=='Barcelona') %>% 
                        distinct(id_odsp)) 
table_2011$matches = rep(as.numeric(total_matches_2011), length(table_2011[,1]))
table_2011$goals_per_mach = round(table_2011$n.y / table_2011$matches, 2)


# Goles por partido temporada 2012-2013 -----------------------------------

total_matches_2012 = count(La_liga_2012 %>% 
                             filter(event_team=='Barcelona') %>% 
                             distinct(id_odsp)) 
table_2012$matches = rep(as.numeric(total_matches_2012), length(table_2012[,1]))
table_2012$goals_per_mach = round(table_2012$n.y / table_2012$matches, 2)


# Goles por partido temporada 2013-2014 -----------------------------------

total_matches_2013 = count(La_liga_2013 %>% 
                             filter(event_team=='Barcelona') %>% 
                             distinct(id_odsp)) 
table_2013$matches = rep(as.numeric(total_matches_2013), length(table_2013[,1]))
table_2013$goals_per_mach = round(table_2013$n.y / table_2013$matches, 2)


# Goles por partido temporada 2011-2012 -----------------------------------

total_matches_2014 = count(La_liga_2014 %>% 
                             filter(event_team=='Barcelona') %>% 
                             distinct(id_odsp)) 
table_2014$matches = rep(as.numeric(total_matches_2014), length(table_2014[,1]))
table_2014$goals_per_mach = round(table_2014$n.y / table_2014$matches, 2)


# Goles por partido temporada 2011-2012 -----------------------------------

total_matches_2015 = count(La_liga_2015 %>% 
                             filter(event_team=='Barcelona') %>% 
                             distinct(id_odsp)) 
table_2015$matches = rep(as.numeric(total_matches_2015), length(table_2015[,1]))
table_2015$goals_per_mach = round(table_2015$n.y / table_2015$matches, 2)



# Juntamos toda la informacion en una sola tabla --------------------------

main_table = rbind(table_2011, table_2012)
main_table = rbind(main_table, table_2013)
main_table = rbind(main_table, table_2014)
main_table = rbind(main_table, table_2015)



# Grafico evolucion de tasas de gol por tiro ------------------------------

main_table %>% 
  #filter(event_team == 'Barcelona' | event_team == 'Real Madrid') %>% 
  #arrange(event_team) %>% 
  ggplot(aes(temp, goals_per_shot, color = event_team)) + 
  geom_line(size = 1) + 
  labs(x = 'Temporada',
       y = "Índice de gol por tiro",
       title = "Evolución de las tasas de gol por tiro realizado",
       subtitle = "Desde la temporada 2011-2012 hasta la 2015-2016") +
  theme(plot.title.position = "plot") +
  gghighlight(event_team == 'Barcelona' | event_team == 'Real Madrid') +




# Grafico evolutivo de goles por partido ----------------------------------

main_table %>% 
  #filter(event_team == 'Barcelona' | event_team == 'Real Madrid') %>% 
  #arrange(event_team) %>% 
  ggplot(aes(temp, goals_per_mach, color = event_team)) + 
  geom_line(size = 1) + 
  labs(x = 'Temporada',
       y = "Índice de gol por partido",
       title = "Evolución de goles por partido",
       subtitle = "Desde la temporada 2011-2012 hasta la 2015-2016") +
  theme(plot.title.position = "plot") +
  gghighlight(event_team == 'Barcelona' | event_team == 'Real Madrid')



# Test de hipotesis goles por partido -------------------------------------------------

archivo = eventos %>%
  filter(league == 'La Liga' & (event_team == 'Barcelona' | event_team == 'Real Madrid')
         & date <= '2016-05-15') %>% 
  select(id_odsp, event_team, opponent, is_goal)

archivo$id_odsp = as.character(archivo$id_odsp)
archivo$event_team = as.character(archivo$event_team)
archivo$opponent = as.character(archivo$opponent)
archivo$is_goal = as.numeric(archivo$is_goal)
write.csv(archivo, file = 'La_Liga.csv')
########################################################


goles_equipos= read.csv(file = 'La_liga_goal_per_mach.csv', encoding = 'UTF-8')


barca = goles_equipos %>% filter(equipo == 'Barcelona')
real_madrid = goles_equipos %>% filter(equipo == 'Real Madrid')

pois_barca = data.frame(x = rpois(nrow(barca), mean(barca$goles)))
pois_rm = data.frame(x = rpois(nrow(real_madrid), mean(real_madrid$goles)))

barca %>% 
  ggplot(aes(goles)) +
  geom_histogram(aes(y = stat(count)/sum(count)), position="identity",
                 bins = 17, fill = 'coral2') +
  labs(title = 'Distribucion de goles convertidos Barcelona', 
       subtitle = 'Desde la temporada 2011-2012 hasta la 2015-2016',
       x = 'Goles en partidos', y = 'Probabilidad') +
  theme(plot.title.position = "plot") +
  scale_x_continuous(breaks=seq(0,8,by=1))

real_madrid %>% 
  ggplot(aes(goles)) +
  geom_histogram(aes(y = stat(count)/sum(count)), position="identity",
                 bins = 27, fill = 'coral2') +
  labs(title = 'Distribucion goles convertidos Real Madrid', 
       subtitle = 'Desde la temporada 2011-2012 hasta la 2015-2016', 
       x = 'Goles en partidos', y = 'Probabilidad') + 
  theme(plot.title.position = "plot") +
  scale_x_continuous(breaks=seq(0,10,by=1))

ggplot(pois_rm, aes(x)) +
  geom_histogram(aes(y = stat(count)/sum(count)), bins = 22, fill = 'blue') +
  labs(title = 'Distribucion teorica poisson(1.35)', x = 'Goles en partidos', 
       y = 'Probabilidad') +
  scale_x_continuous(breaks=seq(1,6,by=1))


real_madrid %>% 
  ggplot(aes(goles)) +
  geom_histogram(aes(y = stat(count)/sum(count)), position="identity",
                 bins = 22, fill = 'coral2') +
  labs(title = 'Distribucion goles convertidos Real Madrid', 
       subtitle = 'Desde la temporada 2011-2012 hasta la 2015-2016', 
       x = 'Goles en partidos', y = 'Probabilidad') + 
  theme(plot.title.position = "plot") +
  geom_histogram(data = pois_rm, aes(x= x, y = stat(count)/sum(count)), 
                 bins = 22, fill = 'blue',
                 alpha = 0.5)

## real madrid
k = sort(real_madrid$goles)
n = length(k)
i = 1:n
pi = (i - 0.5)/n
quantiles = qpois(p = pi, lambda = mean(real_madrid$goles))
plot(k, quantiles)
abline(a = 0, b = 1, col = 2)

require(vcd)
gf<-goodfit(k, type = "poisson", method = "MinChisq")
gf$par
summary(gf)
# No se rechaza la hipotesis nula valor-p > 0.05. No hay evidencia en contra de los datos de que se ajusten a una poisson(2.971262)

xempp <- seq(min(k), max(k), by=0.0001)
plot(xempp, ppois(xempp, lambda=2.971262), type="l", col="red", xlab="Número de usuarios",ylab="ppois(usuarios, lambda)")
plot(ecdf(k), add=TRUE)



# barcelona

k = sort(barca$goles)
n = length(k)
i = 1:n
pi = (i - 0.5)/n
quantiles = qpois(p = pi, lambda = mean(barca$goles))
plot(k, quantiles)
abline(a = 0, b = 1, col = 2)


gf<-goodfit(k, type = "poisson", method = "MinChisq")
gf$par
summary(gf)

# se rechaza la hipotesis nula

xempp <- seq(min(k), max(k), by=0.0001)
plot(xempp, ppois(xempp, lambda=mean(barca$goles)), type="l", col="red", xlab="Número de usuarios",ylab="ppois(usuarios, lambda)")
plot(ecdf(k), add=TRUE)



# Sector de gol -----------------------------------------------------------
# Valores modificables
temporada = La_liga_2015
equipo = 'Barcelona'

# no modificar, solo correr
goal_location = subset(temporada,!is.na(shot_place)) %>% 
  filter(shot_place %in% c('Esquina inferior izquierda',
                           'Esquina inferior derecha',
                           'Centro de la portería',
                           'Esquina superior izquierda',
                           'Esquina superior derecha'),
         event_team == equipo) %>% 
  select(shot_place, is_goal)

goal_location$is_goal = ifelse(goal_location$is_goal == 1, 'Gol', 'No gol')

goal_location %>% 
  ggplot(aes(x = shot_place, fill = is_goal)) +
  geom_bar(position = 'dodge', aes( y = prop.table(..count..) * 100)) + 
  theme_minimal() +
  scale_color_colorblind() +
  labs(title = "Porcentaje de goles anotados por sector de portería",
       subtitle = "Resultados en la temporada",
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


# Caracteristicas de los intentos de gol ----------------------------------

# Modificable
periodo = eventos %>%
  filter(league == 'La Liga' & (event_team == 'Barcelona' | event_team == 'Real Madrid')
         & date <= '2016-05-15') 

equipo = 'Barcelona'

# No modificar, solo correr
attempts = periodo %>% 
  filter(event_team== equipo & 
           is_goal==1 & 
           location!='No registrado') %>% 
  select(event_team, location, situation, assist_method, is_goal) %>% 
  group_by(location, situation, assist_method) %>% 
  summarize(num_goals=n()) %>%
  arrange(desc(num_goals))
colnames(attempts) = c('Ubicación del campo', 'Situación del juego', 
                          'Método de asistencia', 'Cantidad de goles')
knitr::kable(attempts[1:10,])

sum(attempts$`Cantidad de goles`)

# Goleadores --------------------------------------------------------------
La_liga = eventos %>%
  filter(league == 'La Liga' & (event_team == 'Barcelona' | event_team == 'Real Madrid')
         & date <= '2016-05-15') 


goleadores = La_liga[La_liga$is_goal==1 & La_liga$event_team=='Barcelona',] %>% 
  count(player)
goleadores$player = factor(goleadores$player, levels = goleadores$player[order(goleadores$n)])
goleadores = goleadores[order(desc(goleadores$n)),]




plot_rm = ggplot(goleadores[1:5,], aes(n, player)) +
  geom_point(size = 7, aes( colour= 'green'), show.legend = F) +
  labs(title = 'Top 5 goleadores Real Madrid', 
       subtitle = 'Temporadas 2011-2012 hasta la 2015-2016',
       y = 'Jugador', x = 'Cantidad de goles') +
  geom_text(aes(label = n), size = 3, color = 'white') +
  theme(plot.title.position = "plot")

plot_barca = ggplot(goleadores_barca[1:5,], aes(n, player)) +
  geom_point(size = 7, aes( colour= 'green'), show.legend = F) +
  labs(title = 'Top 5 goleadores Barcelona', 
       subtitle = 'Temporadas 2011-2012 hasta la 2015-2016',
       y = 'Jugador', x = 'Cantidad de goles') +
  geom_text(aes(label = n), size = 3, color = 'white') +
  theme(plot.title.position = "plot")

plot_rm + plot_barca

## CREAR UN GRAFICO QUE MUESTRE LOS TOP 3 GOLEADORES DE CADA EQUIPO (BARCA Y RM) POR TEMPORADA



# Cristiano Ronaldo vs Lionel Messi ---------------------------------------


La_liga$player = as.factor(La_liga$player)

ronaldo = La_liga %>% 
  filter(player=='cristiano ronaldo', 
        event_type=='Intento', 
        location!='No registrado') %>% 
  select(location, is_goal) %>% 
  group_by(location) %>% 
  summarize(num_intentos=n(), goles = sum(is_goal==1)) %>% 
  mutate(ratio = round(goles / num_intentos, 2))
knitr::kable(ronaldo)

messi = La_liga %>% 
  filter(player=='lionel messi', 
         event_type=='Intento', 
         location!='No registrado') %>% 
  select(location, is_goal) %>% 
  group_by(location) %>% 
  summarize(num_intentos=n(), goles = sum(is_goal==1)) %>% 
  mutate(ratio = round(goles / num_intentos, 2))
knitr::kable(messi)



# IDEAS

# Igual que goleadores, buscar a los asistidores