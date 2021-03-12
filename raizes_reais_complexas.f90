! Objetivo: Obter as raizes reais e complexas da equação 
! (terceiro grau) variando o termo independente dentro
! do intervalo de 5 a 7. Em seguida, verificar resultado.

PROGRAM raizes_reais_complexas
	USE nrtype
! Declaracao de variaveis
	IMPLICIT none
	REAL, PARAMETER :: coefic_a = -6., coefic_b = 11., coefic_a_div = coefic_a/3.
	REAL, PARAMETER :: coefic_c_inicial = 5., coefic_c_final = 7.
	REAL :: raiz_1, raiz_2, raiz_3, raiz_real1, &
		raiz_real2, raiz_real3, coefic_c = coefic_c_inicial
	REAL :: raiz_img2, raiz_img3
	REAL :: R, A, B, R_exp2, soma_ab, dif_ab
	REAL :: theta
	INTEGER, PARAMETER :: dados_reais = 7, dados_complexos = 8, &
		num_pontos = 1000
	INTEGER, PARAMETER :: dados_cx = 9
	INTEGER :: num_repetir
	REAL, PARAMETER :: incremento_c = (coefic_c_final - &
		coefic_c_inicial)/REAL(num_pontos)
	REAL :: x_da_curva, c_da_curva
	REAL, PARAMETER :: Q = coefic_a_div**2 - coefic_b/3., Q_exp3 = Q**3
	REAL, PARAMETER :: parte_R = (coefic_a_div**3) - coefic_a*coefic_b/6.
	REAL, PARAMETER :: doisraiz_q = -2.*SQRT(Q), div_1_3 = 1./3., &
				raiz3_div2 = 0.5*SQRT(3.)
				
! Os valores (iniciais e finais) de x foram calculados
! por uma calculadora grafica
	REAL, PARAMETER :: x_inicial = 0.48, x_final = 3.52, incremento_x &
		= (x_final - x_inicial)/REAL(num_pontos)
! Abrindo os arquivos de saida (fora do loop)
	OPEN(dados_reais, FILE="dados_reais.dat")
   	OPEN(dados_complexos, FILE="dados_complexos.dat")
! Separando a parte dependente de C para otimizar o loop	
	DO num_repetir = 0, num_pontos
		R = parte_R - 0.5*coefic_c
		R_exp2 = R**2
! Primeiro Caso
		IF (R_exp2 <= Q_exp3) THEN
			theta = ACOS(R/SQRT(Q_exp3))
			raiz_1 = doisraiz_q*COS(theta/3.) - (coefic_a_div)
			raiz_2 = doisraiz_q*COS((theta + TWOPI)/3.) - &
				(coefic_a_div)
			raiz_3 = doisraiz_q*COS((theta - TWOPI)/3.) - &
				(coefic_a_div)
! Escrevendo dentro dos arquivos de saida
			WRITE(dados_reais,*) coefic_c, raiz_1
		 	WRITE(dados_reais,*) coefic_c, raiz_2
		 	WRITE(dados_reais,*) coefic_c, raiz_3
! Segundo Caso
		ELSE
			A = -SIGN(1.0,R)*(ABS(R) + SQRT(R_exp2 - Q_exp3))**div_1_3		
			IF (A /= 0.) THEN
				B = Q/A
			ELSE
				B = 0.
			END IF
! Determinando as partes imaginarias e reais de cada raiz
			soma_ab = A + B
			dif_ab = A - B
			raiz_real1 = soma_ab - coefic_a_div
			raiz_real2 = -0.5*soma_ab - coefic_a_div
			raiz_img2 = raiz3_div2*dif_ab
			raiz_real3 = raiz_real2
			raiz_img3 = -raiz_img2
! Escrevendo dentro dos arquivos de saida
			WRITE(dados_reais,*) coefic_c, raiz_real1
			WRITE(dados_complexos,*) raiz_real2, raiz_img2
		 	WRITE(dados_complexos,*) raiz_real3, raiz_img3
		END IF
! Modificando o valor de c no intervalo proposto
		coefic_c = coefic_c + incremento_c
	END DO
! Fechando os arquivos de saida
	CLOSE(dados_reais)
	CLOSE(dados_complexos)
! Abrindo arquivo de saida
	OPEN(dados_cx, FILE="dados_cx.dat")
	x_da_curva = x_inicial 
	DO num_repetir = 1, num_pontos
		c_da_curva = x_da_curva*(coefic_b + (x_da_curva* &
			(x_da_curva + coefic_a)))
! Modificando o valor de x no intervalo proposto
		x_da_curva = x_da_curva + incremento_x
! Escrevendo dentro do arquivo de saida
		WRITE(dados_cx,*) c_da_curva , x_da_curva 
	END DO
! Fechando arquivo de saida
	CLOSE(dados_cx)
END PROGRAM raizes_reais_complexas
