#Borra consola, variables y funciones definidas en el entorno Global
rm(list = setdiff(ls(), lsf.str()));
rm(list=lsf.str());
cat("\014")


#Experiencia 1
r <- 0.019 #Radio de las esferas
deltar <- 0.001 #Error en el radio
d <- c(0.20,0.14,0.10,0.09,0.08,0.07,0.06,0.05) #Distancias
deltad <- 0.05 #Error en la distancia
a <- c(13.5,23.5,54.5,67.5,88.5,117.5,144.5,179.5) #Ángulos theta
deltaa <- 4 #Error en los ángulos theta
factordecorreccion <- 1 - 4 * ((r^2)/(d^2)) #Factor B de corrección
errorenelfactordecorreccion <- 12*((((r^2)*deltar)/(d^3))+(((r^3)*deltad)/(d^4))) #Error en el factor de corrección
acorregido <- a*factordecorreccion #Ángulo theta corregido
errorenacorregido <- (deltaa/factordecorreccion) + (a/factordecorreccion^2)*errorenelfactordecorreccion #Error en el ángulo theta corregido
c <- 4*pi*(8.85*10^-12)*r #Capacitancia
deltac <- 4*pi*(8.85*10^-12)*deltar #Error en la capacitancia
v <- 6000 #Voltaje
deltav <- 50 #Error en el voltaje (revisar dato)
q <- c*v #Carga eléctrica
deltaq <- deltac*v + c*deltav #Error en la carga eléctrica

plot(d^2, a, pch=20)
plot(log10(d), log10(a), pch=20)






#Experiencia 2
v2 <- c(6000, 5000, 4000, 3000, 2000, 1000) #Vector de voltajes (Exp 2)
deltav2 <- 50 #Error en los voltajes
r <- 0.019 #Radio de las esferas
deltar <- 0.001 #Error en el radio
d2 <- 0.140 #Distancia fija medida en metros
deltad2 <- 0.001 #Error en la distancia (1 mm medido en metros)
c <- 4*pi*(8.85*10^-12)*r #Capacitancia
deltac <- 4*pi*(8.85*10^-12)*deltar #Error en la capacitancia
q2 <- c*v2 #Vector de cargas debido a diferentes voltajes
deltaq2 <- deltac*v2 + c*deltav2 #Error en las cargas eléctricas (experimento 2) 
b <- 1 - 4 * ((r^2)/(0.14^2)) #Factor B de corrección
deltab <- 12*((((r^2)*deltar)/(d2^3))+(((r^3)*deltad2)/(d2^4))) #Error en el factor de corrección
a2 <- c(48.5, 31.5, 23.5, 11.5, 5, 1.5) #Ángulos en grados
deltaa2 <- 4 #Error en el ángulo medido en grados
a2corregido <- a2/b #Ángulos corregidos
deltaa2corregido <- deltaa2/b + (a2*deltab)/(b^2) #Error en los ángulos corregidos

plot(q2, a2, pch=20)
plot(q2, a2corregido, pch=20)





#Experiencia 3
alpha <- c(496.5, 455.5, 327.5, 288.5, 258.5, 160, 135, 32.5) #Ángulos alfa
deltaalpha <- 4 #Error en los ángulos alfa
m <- c(75, 70, 50, 45, 40, 25, 20, 5) #Masa en miligramos
M <- m/1000000 #Masa en kilogramos (ver conversión)
p <- M*9.8 #Peso (se toma como constante)
d <- c(0.200,0.140,0.100,0.090,0.080,0.070,0.060,0.050) #Distancias (en metros)
deltad <- 0.001 #Error en la distancia (1mm medido en metros)
a <- c(13.5,23.5,54.5,67.5,88.5,117.5,144.5,179.5) #Ángulos theta
deltaa <- 4 #Error en los ángulos theta
factordecorreccion <- 1 - 4 * ((r^2)/(d^2)) #Factor B de corrección
errorenelfactordecorreccion <- 12*((((r^2)*deltar)/(d^3))+(((r^3)*deltad)/(d^4))) #Error en el factor de corrección
acorregido <- a*factordecorreccion #Ángulo theta corregido
errorenacorregido <- (deltaa/factordecorreccion) + (a/factordecorreccion^2)*errorenelfactordecorreccion #Error en el ángulo theta corregido

kt <- p/alpha #Constante de torsión
errorenkt <- ((p*deltaalpha)/(alpha^2)) #Error en la constante de torsión
kc <- ((d^2)*kt*a)/(q^2)
deltakc <- (1/(q^2))*(2*d*deltad*kt*a + (d^2)*errorenkt*a + (d^2)*kt*deltaa + ((d^2)*kt*a*deltaq)/(q))
kccorregida <- ((d^2)*kt*acorregido)/(q^2)
(1/(q^2))*(2*d*deltad*kt*acorregido + (d^2)*errorenkt*acorregido + (d^2)*kt*errorenacorregido + ((d^2)*kt*acorregido*deltaq)/(q))