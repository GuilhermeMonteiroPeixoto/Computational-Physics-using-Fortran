! Objetivo: Interpolar e extrapolar a funcao

PROGRAM interpo_extrapo

   USE nrtype
   USE nrutil
   USE nr, ONLY: polint
   IMPLICIT none
   INTEGER :: xi, cont_pontos, lim_inicial = 1, lim_final = 4
   INTEGER, PARAMETER :: dados_pontos = 7, dados_grafico = 8
   INTEGER, PARAMETER :: intervalo_x = 11, num_pontos = 1000, lim_inicial_max=8
   REAL :: x_ponto, y_funcao_original, x_in_ex, y_grau3, y_grau10, dy
   REAL, PARAMETER :: incremento_x_ponto = 0.2
   REAL :: vie_x(intervalo_x) = 0., f(intervalo_x) = 0.
!vie_x significa valores igualmente espacados de x
   REAL, PARAMETER :: x_inicial = -1., x_final = 1.
   REAL, PARAMETER :: primeiro_x_interpo = -1.2, ultimo_x_interpo = 1.2
   REAL, PARAMETER :: incremento_x_interpo = (ultimo_x_interpo-&
                                       primeiro_x_interpo)/real(num_pontos)

!Abrir os arquivos para escrever os dados
   OPEN(dados_pontos, file="dados_pontos.dat")
   OPEN(dados_grafico,file="dados_grafico.dat")
!Calcular os valores igualmente espacados da funcao no intervalo  de -1 a 1
   x_ponto = x_inicial
   DO xi = 1, intervalo_x
      vie_x(xi) = x_ponto
      f(xi) = 0.5*(1.+tanh(6.*x_ponto))
      WRITE(dados_pontos, *) vie_x(xi), f(xi)
!Escrever cada par x e y correspondentes a funcao
      x_ponto = x_ponto + incremento_x_ponto
   END DO

!Iniciar a interpolacao e extrapolacao dos valores
   x_in_ex = primeiro_x_interpo
   DO cont_pontos = 1, num_pontos

!Determinar os intervalos para fazer a interpolacao e a extrapolacao
!com o polinomio de grau 3
      IF( x_in_ex > vie_x(lim_final-1) ) THEN
         lim_inicial = lim_inicial+1
         lim_final = lim_final+1
         IF( lim_final > intervalo_x ) THEN
            lim_inicial = lim_inicial_max
            lim_final = intervalo_x
         END IF
      END IF

!Interpolar e extrapolar com um polinomio de grau 3
      CALL polint(vie_x(lim_inicial:lim_final), f(lim_inicial:lim_final),&
                                                 x_in_ex, y_grau3, dy)
!Interpolar e extrapolar com um polinomio de grau 10
      CALL polint(vie_x, f, x_in_ex, y_grau10, dy)
!Calcular os valores da funcao original
      y_funcao_original = 0.5*(1.+tanh(6.*x_in_ex))
!Escrever os valores da funcao nos arquivos
      WRITE(dados_grafico, *) x_in_ex, y_grau10, y_grau3, y_funcao_original
      x_in_ex = x_in_ex + incremento_x_interpo
   END DO

   CLOSE(dados_pontos)
   CLOSE(dados_grafico)

END PROGRAM interpo_extrapo
