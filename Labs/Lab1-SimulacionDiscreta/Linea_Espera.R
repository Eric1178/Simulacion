programa_principal <- function(lambdaA = 15, lambdaS = 10, n = 100){
  rutina_inicializacion(lambdaA = lambdaA, lambdaS = lambdaS, n = n) # inicializa simulaci�n
  # ejecuta la simulaci�n mientras se cumple la condici�n de que 
  # los clientes en espera alcanzan el valor fijo de clientes que pasan por el sistema
  while (clientes_enespera < n){
    rutina_temporizador() # determina el siguiente evento y avanza los relojes
    actualiza_estadisticas() # actualiza los acumuladores
    if(sig_tipo_evento == 1) llegadas() else salidas() # llama la funci�n de evento
    next
  }
  return(reporte()) # Genera reporte al final de la simulaci�n.
}

#..................................................................
rutina_inicializacion  <- function(lambdaA, lambdaS, n){
  lambdaA  <<- lambdaA    # tiempo promedio de llegadas
  lambdaS  <<- lambdaS    # tiempo promedio de servicio
  n        <<- n          # El n�mero de clientes en el sistema que se consideran antes de detener la simulaci�n
  num_eventos <<- 2       # n�mero de eventos distintos (llegadas y salidas)
  lt_A     <<- vector(mode="numeric", length = 1) # inicializa el vector para guardar los tiempos de arribo
  reloj    <<- 0          # inicializa reloj de simulaci�n
  servidor <<- 0          # inicializa estado del servidor 0 = libre, 1 = ocupado
  q_t      <<- 0 
  tiempo_ultimo_evento <<- 0 # tiempo del evento m�s reciente
  
  # Incializa los contadores estad�sticos
  clientes_enespera <<- 0
  total_esperas     <<- 0
  tiempo_en_sistema <<- 0   # Tarea 1
  area_q            <<- 0
  area_status_servidor <<- 0
  
  # Inicializa la lista de eventos: el primer arribo y el tiempo de salida. Se asigna un tiempo de salida muy grande, 
  # ya que no hay clientes esperando. Con esto se garantiza que el siguiente evento sea una llegada.
  tiempo_sig_evento <<- c(reloj + rexp(1, 1/lambdaA), 1e30)
  le <<- c(e=reloj,tipo=0,q=q_t)
}

#----------------------------------------------------------------------------
rutina_temporizador <- function(){
  min_tiempo_sig_evento <<- 1e29  # valor inicial del m�nimo
  sig_tipo_evento <<- 0
  
  # Determina el tipo de evento del siguiente evento a ocurrir (una llegada o una salida) de acuerdo a su tama�o
  for(i in 1:num_eventos){
    if( tiempo_sig_evento[i] < min_tiempo_sig_evento ){
      min_tiempo_sig_evento <<- tiempo_sig_evento[i]
      sig_tipo_evento <<- i
    }
  }
  
  # verifica si la lista de eventos est� vac�a
  if(sig_tipo_evento == 0) 
    stop(print(paste("La lista de eventos est� vac�a en el tiempo:", reloj, sep=" ")))
  
  # La lista de eventos no est� vac�a, avanza el reloj de simulaci�n
  reloj <<- min_tiempo_sig_evento
  le <<- rbind(le,c(reloj,sig_tipo_evento,q=q_t))
}

#..............................................................................
llegadas <- function(){
  tiempo_sig_evento[1] <<- reloj + rexp(1, 1/lambdaA) # Programa un evento de llegada
  if(servidor == 1){
    q_t <<- q_t + 1 # aumenta la cola en 1
    lt_A[q_t] <<- reloj  # guarda el tiempo de llegada de este cliente en la lista de eventos.
  } else {
    Di <<- 0
    total_esperas <<- total_esperas + Di
    clientes_enespera <<- clientes_enespera + 1
    servidor <<- 1 
    tiempo_sig_evento[2] <<- reloj + rexp(1, 1/lambdaS) # tiempo de salida
    tiempo_en_sistema <<- tiempo_en_sistema + tiempo_sig_evento[2] -reloj   # Tarea 1
  }
}

# Esta Funci�n sigue el diagrama de flujo que vimos en clase.
salidas <- function(){
  if(q_t == 0){
    servidor <<- 0
    tiempo_sig_evento[2] <<-  1e30
  } else {
    q_t <<- q_t - 1
    Di <<- reloj - lt_A[1]
    total_esperas <<- total_esperas + Di
    clientes_enespera <<- clientes_enespera + 1
    tiempo_sig_evento[2] <<- reloj + rexp(1, 1/lambdaS)
    tiempo_en_sistema <<- tiempo_en_sistema + tiempo_sig_evento[2] -reloj  #Tarea 1
    for(i in 1:q_t) lt_A[i] <<- lt_A[i+1]
  }
}

reporte <- function(){
  print(paste("Promedio de espera en la fila:", round(total_esperas/clientes_enespera, 2), "minutos", sep=" "))
  print(paste("N�mero promedio de clientes esperando en la fila:",round(area_q/reloj, 2), sep = " "))
  print(paste("Utilizaci�n del servidor:",100*round(area_status_servidor/reloj, 2), "%", sep = " "))
  print(paste("El tiempo de simulaci�n fue de:", round(reloj,2), "minutos", sep = " "))
  return(list(promedio.espera = total_esperas/clientes_enespera,
              tiempo_total_promedio_en_sistema = tiempo_en_sistema/reloj, #Tarea1
              longitud_max = max(le[,3]),   #Tarea 1
              espera_max = max(total_esperas), #Tarea 1
              longitud.promedio.fila = area_q/reloj,
              utilizacion = area_status_servidor/reloj,
              tiempo.simulacion = reloj,
              le = le))
}


# Funci�n de actualizaci�n de estad�sticas
actualiza_estadisticas <- function(){
  tiempo_desde_ultimo_evento <<- reloj - tiempo_ultimo_evento
  tiempo_ultimo_evento <<-  reloj
  area_q <<- area_q + q_t * tiempo_desde_ultimo_evento
  area_status_servidor <<- area_status_servidor + servidor * tiempo_desde_ultimo_evento
}


#EJECUCION:------------------------------------------------------------------

resultado1 <- programa_principal(lambdaA = 10, lambdaS = 20)
resultado2 <- programa_principal(lambdaA = 10, lambdaS = 10)
resultado3 <- programa_principal(lambdaA = 20, lambdaS = 10)
par(mfrow=c(1,3))
plot(le[,3] ~ le[,1], data = resultado1, xlab = "tiempo", ylab = "longitd de la fila", main = "lambdaA = 10, lambdaS = 20")
plot(le[,3] ~ le[,1], data = resultado2, xlab = "tiempo", ylab = "longitd de la fila", main = "lambdaA = 20, lambdaS = 20")
plot(le[,3] ~ le[,1], data = resultado3, xlab = "tiempo", ylab = "longitd de la fila", main = "lambdaA = 20, lambdaS = 10")

