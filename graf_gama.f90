! Objetivo: Reproduzir o gráfico da funcao gama

PROGRAM grafico_gama
	USE nrtype
	USE nrutil
	USE nr, ONLY: gammln
	IMPLICIT none
	INTEGER :: cont_pontos, variacao_x_positivo 
	INTEGER, PARAMETER :: saida_gama = 9, total_pontos = 1000
	INTEGER, PARAMETER :: x_positivo = total_pontos/2
	REAL :: x, pi_x, x_mais_1
	REAL, PARAMETER :: x_inicial = -5.25, x_final = 5.25
	REAL, PARAMETER :: incremento_x=(x_final-x_inicial)/(total_pontos)
	REAL, DIMENSION(-x_positivo:x_positivo) :: funcao_gama

	x = 0.0
	DO variacao_x_positivo = 1, x_positivo
		x = x+incremento_x
		x_mais_1 = 1.0+x
! funcao gama para abscissas positivas
		funcao_gama(variacao_x_positivo) = exp(gammln(x_mais_1))
		pi_x = pi*x
! Pela formula de reflexao para abscissas negativas
		funcao_gama (-variacao_x_positivo) = pi_x/(funcao_gama&
					(variacao_x_positivo)*sin(pi_x))
	END DO
	x = x_inicial
! Quando o argumento for igual a zero
	funcao_gama(0) = 1.0
	OPEN(unit=saida_gama, file="dados_gama.dat")
	DO cont_pontos = -x_positivo, x_positivo
		WRITE(saida_gama,fmt=*) x, funcao_gama(cont_pontos)
		x = x+incremento_x
	END DO
	CLOSE(unit=saida_gama)  
END PROGRAM grafico_gama
