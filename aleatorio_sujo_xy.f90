! Objetivo: Usar pares ordenados a partir de um gerador aleatorio do
!           tipo rapido e "sujo" para obter coordenadas de pontos em
!	    	um espaço bidimensional.

PROGRAM aleatorio_sujo_xy
	IMPLICIT none
	INTEGER, PARAMETER :: im = 6075, ia = 106, ic = 1283
	REAL, PARAMETER :: im_real = REAL(im)
	REAL :: ran_x, ran_y
	INTEGER, PARAMETER :: dados_aleatorio_xy = 7
	INTEGER :: num_repeticoes, jran = 1
! Abrir arquivo.dat de saida de dados
	OPEN(dados_aleatorio_xy,FILE="dados_aleatorio_xy.dat")
! Essas constantes dao um periodo de comprimento máximo im. Por isso im eh a
! quantidade de numeros aleatorios gerados diferentes. O gerador produz 2*im numeros
! aleatorios pois im eh impar. O primeiro numero a repetir sera na coordenada y, pois
! im eh impar e estamos distribuindo aos pares. O primeiro numero a se repetir na
! coordenada x sera apos 2im numeros gerados.
	DO num_repeticoes = 1, im
		jran = MOD(jran*ia+ic,im)
		ran_x = REAL(jran)/im_real

		jran = MOD(jran*ia+ic,im)
		ran_y = REAL(jran)/im_real
! Escrever as coordenadas no arquivo de dados
		WRITE(dados_aleatorio_xy,*) ran_x,ran_y
	END DO
! Fechar arquivo.dat de saida de dados
	CLOSE(dados_aleatorio_xy)
END PROGRAM aleatorio_sujo_xy
