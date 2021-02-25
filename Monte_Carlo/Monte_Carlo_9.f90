! Objetivo: Calcular a integral da funcao f(x) = sin(x) no intervalo
!           [0, pi] utilizando o metodo de monte carlo amostragem por
!	    importancia

PROGRAM monte_carlo_9

   USE nrtype
   USE nrutil
   USE ran_state, ONLY: ran_seed
   USE nr, ONLY: ran1

   IMPLICIT none
   INTEGER :: iseed = 1234, intervalo, sub_intervalo
   REAL :: aleatorio, func, func_elev2, soma_func = 0., integral_func, N_chamadas
   REAL :: media = 0., media_elev2 = 0., erro, soma_func_elev2 = 0.
   REAL :: x_y, theta_y
   REAL, PARAMETER :: div_picubo_6= pi**3/6.
   INTEGER, PARAMETER :: dados_monte_carlo_9 = 7, quant_pontos = 1000, quant_amostras = 10

   CALL RAN_SEED(SEQUENCE = iseed)

   OPEN( dados_monte_carlo_9,file="dados_monte_carlo_9.dat")
   DO intervalo = 10, quant_pontos, 10
      DO sub_intervalo = 1, quant_amostras
         CALL ran1(aleatorio)

         theta_y = ACOS(2.*aleatorio -1.)

         x_y =-pi*COS((theta_y-2.*pi)/3.) + pio2
         func = div_picubo_6*SIN(x_y)/(x_y*(pi-x_y))
         func_elev2 = func**2
         soma_func = soma_func + func
         soma_func_elev2 = soma_func_elev2 + func_elev2
      END DO
      N_chamadas = real(intervalo)
      media = soma_func/N_chamadas
      media_elev2 = soma_func_elev2/N_chamadas
      erro = pi*sqrt(((media_elev2-(media)**2))/N_chamadas)
      integral_func = media
! O valor exato da integral está sendo escrito manualmente em um arquivo.dat 
! separado do programa
      WRITE(dados_monte_carlo_9, *) N_chamadas, integral_func, erro
   END DO
   CLOSE(dados_monte_carlo_9)

   PRINT *,"O melhor resultado eh:", integral_func,"com erro de +-", erro

END PROGRAM monte_carlo_9
