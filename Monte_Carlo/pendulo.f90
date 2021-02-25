! Objetivo: Reproduzir o grafico do periodo em funcao do angulo inicial

MODULE pendulo
!Dados globais
   REAL :: k
CONTAINS
   FUNCTION func_integral_elip(xi)
      IMPLICIT none
      REAL, DIMENSION (:), INTENT(in) :: xi
      REAL, DIMENSION(SIZE(xi)) :: func_integral_elip
      func_integral_elip = 1./sqrt(1.-(k*sin(xi))**2)
   END FUNCTION func_integral_elip
END MODULE pendulo

PROGRAM pendulo_simples

   USE pendulo
   USE nrtype
   USE nrutil
   USE nr, ONLY: qromb, trapzd, polint

   IMPLICIT none
   REAL :: theta, integral
   INTEGER :: n_repeticoes
   REAL, PARAMETER :: theta_inicial = 0., theta_final = pi
   INTEGER, PARAMETER :: n_pontos = 1000, dados_pendulo = 8
   REAL, PARAMETER :: n_pontos_real = REAL(n_pontos)
   REAL, PARAMETER :: incremento = (theta_final-theta_inicial)/n_pontos_real
   REAL, PARAMETER :: intervalo_a = 0., intervalo_b = pio2

   OPEN(dados_pendulo,file="dados_pendulo.dat")
   theta = theta_inicial
   DO n_repeticoes = 1, n_pontos
      k=sin(0.5*theta)
!Calcular a integral no intervalo de 0 a pi/2
      integral = qromb( func_integral_elip, intervalo_a, intervalo_b )
      WRITE(dados_pendulo, *) theta/pi, integral/pio2
      theta = theta +incremento
   END DO
   CLOSE(dados_pendulo)

END PROGRAM pendulo_simples
