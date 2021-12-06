library(dplyr)

eventos = read.csv(file = 'Datos/events.csv', encoding = 'UTF-8')
matches = read.csv(file = 'Datos/ginf.csv')
 

# Combinaremos los datos para asi obtener informacion mas detallada de cada evento. Anadiremos la liga a la que corresponde cada evento y la temporada

eventos <- left_join(eventos, matches[,c('id_odsp','league','season', 'date')], by = "id_odsp")

# A continuacion reescribiremos las variables con el fin de que sean mas explicitas 

eventos$event_type = case_when(
  eventos$event_type == 0 ~ 'Anuncio',
  eventos$event_type == 1 ~ 'Intento',
  eventos$event_type == 2 ~ 'Tiro de esquina',
  eventos$event_type == 3 ~ 'Falta',
  eventos$event_type == 4 ~ 'Tarjeta amarilla',
  eventos$event_type == 5 ~ 'Segunda tarjeta amarilla',
  eventos$event_type == 6 ~ 'Tarjeta roja',
  eventos$event_type == 7 ~ 'Sustitución',
  eventos$event_type == 8 ~ 'Tiro libre obtenido',
  eventos$event_type == 9 ~ 'Fuera de juego',
  eventos$event_type == 10 ~ 'Mano',
  eventos$event_type == 11 ~ 'Penalti concedido',
  TRUE ~ as.character(eventos$event_type)
)

eventos$event_type2 = case_when(
  eventos$event_type2 == 12 ~ 'Pase clave',
  eventos$event_type2 == 13 ~ 'Balón fallido',
  eventos$event_type2 == 14 ~ 'Expulsión',
  eventos$event_type2 == 15 ~ 'Autogol',
  TRUE ~ as.character(eventos$event_type2)
)

eventos$side <- ifelse(eventos$side == 1, 'Local', 'Visita')

eventos$shot_place = case_when(
  eventos$shot_place == 1 ~ 'Un poco alto',
  eventos$shot_place == 2 ~ 'Bloqueado',
  eventos$shot_place == 3 ~ 'Esquina inferior izquierda',
  eventos$shot_place == 4 ~ 'Esquina inferior derecha',
  eventos$shot_place == 5 ~ 'Centro de la portería',
  eventos$shot_place == 6 ~ 'Alto y ancho',
  eventos$shot_place == 7 ~ 'Pega en el palo',
  eventos$shot_place == 8 ~ 'Falla a la izquierda',
  eventos$shot_place == 9 ~ 'Falla a la derecha',
  eventos$shot_place == 10 ~ 'Muy alto',
  eventos$shot_place == 11 ~ 'Centro superior del arco',
  eventos$shot_place == 12 ~ 'Esquina superior izquierda',
  eventos$shot_place == 13 ~ 'Esquina superior derecha',
  TRUE ~ as.character(eventos$shot_place)
)

eventos$shot_outcome = case_when(
  eventos$shot_outcome == 1 ~ 'A portería',
  eventos$shot_outcome == 2 ~ 'Afuera',
  eventos$shot_outcome == 3 ~ 'Bloqueado',
  eventos$shot_outcome == 4 ~ 'Pega en el palo',
  TRUE ~ as.character(eventos$shot_outcome)
)

eventos$location = case_when(
  eventos$location == 1 ~ 'Campo contrario',
  eventos$location == 2 ~ 'Campo propio',
  eventos$location == 3 ~ 'Centro del área',
  eventos$location == 4 ~ 'Banda izquierda',
  eventos$location == 5 ~ 'Banda derecha',
  eventos$location == 6 ~ 'Ángulo difícil y largo alcance',
  eventos$location == 7 ~ 'Ángulo difícil a la izquierda',
  eventos$location == 8 ~ 'Ángulo difícil a la derecha',
  eventos$location == 9 ~ 'Lado izquierdo del área',
  eventos$location == 10 ~ 'Lado izquierdo del área chica',
  eventos$location == 11 ~ 'Lado derecho del área',
  eventos$location == 12 ~ 'Lado derecho del área chica',
  eventos$location == 13 ~ 'Rango muy cercano',
  eventos$location == 14 ~ 'Punto penal',
  eventos$location == 15 ~ 'Fuera del área',
  eventos$location == 16 ~ 'Largo alcance',
  eventos$location == 17 ~ 'Más de 35 yardas',
  eventos$location == 18 ~ 'Más de 40 yardas',
  eventos$location == 19 ~ 'No registrado',
  TRUE ~ as.character(eventos$location)
)

eventos$bodypart = case_when(
  eventos$bodypart == 1 ~ 'Pie derecho',
  eventos$bodypart == 2 ~ 'Pie izquierdo',
  eventos$bodypart == 3 ~ 'Cabeza',
  TRUE ~ as.character(eventos$bodypart)
)

eventos$assist_method = case_when(
  eventos$assist_method == 0 ~ 'Ninguno',
  eventos$assist_method == 1 ~ 'Pase',
  eventos$assist_method == 2 ~ 'Centro',
  eventos$assist_method == 3 ~ 'Pase de cabeza',
  eventos$assist_method == 4 ~ 'Pase en profundidad',
  TRUE ~ as.character(eventos$assist_method)
)

eventos$situation = case_when(
  eventos$situation == 1 ~ 'Jugada abierta',
  eventos$situation == 2 ~ 'Balón parado',
  eventos$situation == 3 ~ 'Tiro de esquina',
  eventos$situation == 4 ~ 'Tiro libre',
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
eventos$date = as.Date(eventos$date)




