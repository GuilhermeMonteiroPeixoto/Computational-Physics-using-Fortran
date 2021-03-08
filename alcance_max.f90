! Objetivo: Determinar o angulo que resulta no alcance maximo de um projetil
! sujeito a resistencia do ar para k no intervalo de 0 a 0,1.

MODULE arremesso_projetil
	REAL, PARAMETER :: precisao=1.e-5, g=9.8, v0=600.0
	REAL :: k, g_div_k, v0_sin_theta
	LOGICAL :: succes
CONTAINS
	SUBROUTINE funcd(t,altura_funct,deriv_funct)
		USE nrtype
		IMPLICIT none
		REAL, INTENT(IN) :: T
		REAL, INTENT(OUT) :: altura_funct,deriv_funct
		REAL :: exp_menoskt     
		exp_menoskt= EXP(-k*T)
		altura_funct=-g_div_k*T+(v0_sin_theta + g_div_k)/k*&
						(1.0-exp_menoskt)
		deriv_funct=-g_div_k+(v0_sin_theta + g_div_k)*exp_menoskt  
	END SUBROUTINE funcd
	FUNCTION altura_funct(t)
		USE nrtype
		REAL, INTENT(IN) :: T
		exp_menoskt= EXP(-k*T)
		altura_funct=-g_div_k*t+(v0_sin_theta+g_div_k)/k*&
						(1.0-exp_menoskt)
	END FUNCTION
	FUNCTION alcance(theta)
		USE nrtype
		USE nr, ONLY:rtsafe
		IMPLICIT none
		REAL, INTENT(IN) :: theta
		REAL :: T, t_1, t_2, alcance
		v0_sin_theta=v0*sin(theta)
		g_div_k=g/k
! A funcao altura_funct foi derivada e igualada a zero para encontrar o
! ponto de maximo (t_1, f(t_1)). Desse modo, t_1 eh o ponto de altura maxima
! para qualquer valor de k.
		t_1= REAL(-LOG(g/(k*v0_sin_theta+g))/k)
		t_2= REAL(1.0/k+v0_sin_theta/g)
! Agora T estah entre t_1 e t_2, usando a rtsafe encontramos o valor de T
		T = rtsafe(funcd,t_1,t_2,precisao)
		alcance=-(v0*cos(theta)*(1.0-exp(-k*T))/k)
	END FUNCTION
END MODULE arremesso_projetil
PROGRAM alcance_maximo
	USE arremesso_projetil
	USE nrtype
	USE nr, ONLY: rtsafe, brent
	IMPLICIT none 
	REAL :: R,v0_sob_k, R_max
	INTEGER :: contador_ponto
	INTEGER, PARAMETER :: r_theta_vs_k=8,pontos=300,pontos_menos1=pontos-1
	REAL :: theta_inicial=0.0,theta_intermed=0.5*PIO2
	REAL :: theta_final=PIO2,theta_max
	REAL, PARAMETER :: k_inicial=0.0,k_final=0.1
	REAL, PARAMETER :: v0_quad_g=(v0**2)/g
	REAL, PARAMETER :: variacao_k=(k_final-k_inicial)/REAL(pontos_menos1)
	OPEN(unit=r_theta_vs_k,file="R_theta_x_k.dat")
	k=k_inicial
	R_max=v0_quad_g
	theta_max = theta_intermed
	WRITE(unit=r_theta_vs_k,fmt=*) k, R_max, theta_max
	k=k+variacao_k
! Para outros valores de k
	DO contador_ponto=1, pontos_menos1
! Dada a funcao alcance e um trio de abscissas theta_inicial=0.0, theta_intermed
! e theta_final=PIO2 que delimitam um minimo (tal que theta_intermed esta entre
! theta_inicial e theta_final, e o alcance(theta_intermed) eh menor que o
! alcance(0.0) e alcance(PIO2)). A funcao brent calcula o alcance minimo, 
! multiplicando por -1 encontramos os valores maximos do alcance.
		R_max = -brent(theta_inicial,theta_intermed,theta_final,alcance,&
							precisao,theta_max)
		WRITE(unit=r_theta_vs_k, fmt=*) k, R_max, theta_max
		k=k+variacao_k
	END DO 
	CLOSE(unit=r_theta_vs_k)
END PROGRAM alcance_maximo
