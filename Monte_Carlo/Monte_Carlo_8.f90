! Objetivo: Calcular a integral da funcao f(x) = sin(x) no pontos
!           [0, pi] utilizando o metodo de monte carlo

PROGRAM monte_carlo_8

   USE nrtype
   USE nrutil
   USE ran_state, ONLY: ran_seed
   USE nr, ONLY: ran1

   IMPLICIT none
   INTEGER :: iseed = 1234, pontos, amostras, x_valorexato
   REAL :: aleatorio, func, func_elev2, soma_func = 0., integral_func, N_chamadas
   REAL :: media_sin = 0., media_sin_elev2 = 0., erro, soma_func_elev2 = 0.
   INTEGER, PARAMETER :: dados_monte_carlo_8 = 7, quant_pontos = 1000, quant_amostras = 10
   INTEGER, PARAMETER :: valorexato_monte_carlo = 8

   CALL RAN_SEED(SEQUENCE = iseed)

   OPEN( dados_monte_carlo_8,file="dados_monte_carlo_8.dat")
   DO pontos = 10, quant_pontos, 10
      DO amostras = 1, quant_amostras
         CALL ran1(aleatorio)
         func = sin(aleatorio*pi)
         func_elev2 = func**2
         soma_func = soma_func + func
         soma_func_elev2 = soma_func_elev2 + func_elev2
      END DO

      N_chamadas = real(pontos)
      media_sin = soma_func/N_chamadas
      media_sin_elev2 = soma_func_elev2/N_chamadas
      erro = pi*sqrt(((media_sin_elev2-(media_sin)**2))/N_chamadas)
      integral_func = pi*media_sin
! O valor exato da integral está sendo escrito manualmente em um arquivo.dat 
! separado do programa
      WRITE(dados_monte_carlo_8, *) N_chamadas, integral_func, erro
   END DO
   CLOSE(dados_monte_carlo_8)
   OPEN(valorexato_monte_carlo, file="valorexato_monte_carlo.dat")
      DO x_valorexato = 0, quant_pontos, 1000
      WRITE(valorexato_monte_carlo, *) x_valorexato, 2
      END DO
   CLOSE(valorexato_monte_carlo)

   PRINT *,"O melhor resultado eh:", integral_func,"com erro de +-", erro
END PROGRAM monte_carlo_8
