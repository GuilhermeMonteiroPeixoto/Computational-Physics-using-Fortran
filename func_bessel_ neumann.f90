! Objetivo: Reproduzir o grafico da funcao de Bessel e Neumann de Ordem inteira.

PROGRAM func_bessel_neumann
	USE nrtype
	USE nr, ONLY: bessj0,bessj1,bessj,bessy0,bessy1,bessy
	IMPLICIT none
	INTEGER :: cont_pontos
	INTEGER, PARAMETER :: pontos=1000,saida_bess=9, saida_neum=8
	REAL :: x,bessel_ordem0,bessel_ordem1,bessel_ordem2,bessel_ordem3
	REAL :: neumann_ordem0,neumann_ordem1,neumann_ordem2
	REAL, PARAMETER :: x_inicial=0.0,x_final=10.0
	REAL, PARAMETER :: dx=(x_final-x_inicial)/real(pontos-1)

	OPEN(unit=saida_bess,file="bessel.dat")
	OPEN(unit=saida_neum,file="neumann.dat")
! Calculando os valores das funcoes de Bessel de ordem 0, 1, 2 e 3
! Calculando os valores das funcoes de Neumann de ordem 0, 1 e 2 para x > 0.
	x=x_inicial
   bessel_ordem0=bessj0(x)
	bessel_ordem1=bessj1(x)
	bessel_ordem2=bessj(2,x)
	bessel_ordem3=bessj(3,x)
	WRITE(unit=saida_bess,fmt=*) x,bessel_ordem0,bessel_ordem1,&
					bessel_ordem2,bessel_ordem3
	DO cont_pontos=1,pontos
		x=x+dx
		bessel_ordem0=bessj0(x)
		bessel_ordem1=bessj1(x)
		bessel_ordem2=bessj(2,x)
		bessel_ordem3=bessj(3,x)
		WRITE(unit=saida_bess,fmt=*) x,bessel_ordem0,bessel_ordem1,&
					bessel_ordem2,bessel_ordem3
		neumann_ordem0=bessy0(x)
		neumann_ordem1=bessy1(x)
		neumann_ordem2=bessy(2,x)
		WRITE(unit=saida_neum,fmt=*) x,neumann_ordem0,neumann_ordem1,&
							neumann_ordem2
	END DO
END PROGRAM func_bessel_neumann
