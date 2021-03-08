! Objetivo: Resolver a Eq. -g_div_k*t+(v0_sin_theta+g_div_k)/k*&
! (1.0-exp_menoskt) usando rtsafe para determinar o alcance.

MODULE altura_projetil 
	REAL :: exp_menoskt,v0_sin_theta,k,g_div_k
CONTAINS
	SUBROUTINE funcd(t,altura_funct,deriv_funct)
		USE nrtype
		IMPLICIT none
		REAL, INTENT(IN) :: t
		REAL, INTENT(OUT) :: altura_funct,deriv_funct
		exp_menoskt= exp(-k*t)
		altura_funct=-g_div_k*t+(v0_sin_theta+g_div_k)/k*(1.0-exp_menoskt)
		deriv_funct=-g_div_k+(v0_sin_theta+g_div_k)*exp_menoskt
	END SUBROUTINE funcd
	FUNCTION altura_funct(t)
		USE nrtype
		REAL, INTENT(IN) :: t
		exp_menoskt= exp(-k*t)
		altura_funct=-g_div_k*t+(v0_sin_theta+g_div_k)/k*(1.0-exp_menoskt)
	END FUNCTION
END MODULE 
PROGRAM arremesso
	USE altura_projetil
	USE nrtype
	USE nr, ONLY: rtsafe
	IMPLICIT none
	INTEGER,PARAMETER :: theta_x_R=8,k_x_R=9,pontos=300,pt_menos_1=pontos-1
	REAL, PARAMETER :: g=9.81,v0=600.0
	INTEGER :: theta_1,theta_2,theta_3,k1,k2
	REAL :: theta, t,t_1,t_2, R,v0_sob_k
	INTEGER,PARAMETER :: val_interv_k=10,interv_theta=7,interv_theta_mais1=&
				interv_theta+1
	REAL, PARAMETER :: v0_quad_g=(v0**2)/g,theta0=0.0,theta_final=PIO2,&
				k_inicial=0.0, k_final=0.1
	REAL, PARAMETER :: variacao_theta=theta_final-theta0,variacao_k=&
			k_final-k_inicial,variacao_theta_1=variacao_theta/REAL(pt_menos_1)
	REAL, PARAMETER :: variacao_k1=variacao_k/REAL(val_interv_k),precisao=1.0e-5
	REAL, PARAMETER :: variacao_k2=variacao_k/REAL(pt_menos_1),variacao_theta_2=&
						variacao_theta/REAL(interv_theta_mais1)
	LOGICAL :: succes
	
	OPEN(unit=theta_x_R,file="Theta_vs_R.dat")
	OPEN(unit=k_x_R,file="k_vs_R.dat")
! R vs theta, com k no intervalo [0, 0,1] (11 valores equidistantes)
! Inicialmente para k igual a zero
	theta=theta0
	DO theta_1=1,pontos
		v0_sin_theta=v0*sin(theta)
		R=v0_quad_g*sin(2.0*theta)
		WRITE(unit=theta_x_R,fmt=*) theta/PIO2,R
		theta=theta+variacao_theta_1
	END DO
	WRITE(unit=theta_x_R,fmt=*)
! Agora para k diferente de zero
	k=k_inicial
	DO k1=1,val_interv_k
		k=k+variacao_k1
		g_div_k=g/k
		v0_sob_k=v0/k     
		theta=theta0
		R=0.0
		WRITE(unit=theta_x_R,fmt=*) theta/PIO2,R
		theta=theta+variacao_theta_1
		v0_sin_theta = v0*sin(theta)
		t_1=v0_sin_theta/g
		t_2=2.0*t_1
		DO theta_2=1,pontos
			CALL zbrac(altura_funct,t_1,t_2,succes)
			T=rtsafe(funcd,t_1,t_2,precisao)
			R=v0_sob_k*cos(theta)*(1.0-exp(-k*T))
			WRITE(unit=theta_x_R,fmt=*) theta/PIO2,R,T,t_1,t_2
			theta=theta+variacao_theta_1
			v0_sin_theta = v0*sin(theta)
			t_1=T-0.1
			t_2=T+0.1
		END DO
		WRITE(unit=theta_x_R,fmt=*)
	END DO
! R vs k[0, 0,1], com theta no intervalo (0, pi/2) (7 valores equidistantes)
	theta=theta0
	DO theta_3=1,interv_theta
! Inicialmente para k igual a zero
		theta=theta+variacao_theta_2
		v0_sin_theta=v0*sin(theta)
		k=k_inicial
		R=v0_quad_g*sin(2.0*theta)
		WRITE(unit=k_x_R,fmt=*) k,R
! Agora para k diferente de zero
		t_1=v0_sin_theta/g
		t_2=2.0*t_1
		DO k2=1,pt_menos_1
			k=k+variacao_k2
			g_div_k=g/k
			v0_sob_k=v0/k
			CALL zbrac(altura_funct,t_1,t_2,succes)
			T=rtsafe(funcd,t_1,t_2,precisao)
			R=v0*cos(theta)/k*(1.0-exp(-k*T))
			WRITE(unit=k_x_R,fmt=*) k,R
			v0_sin_theta=v0*sin(theta)
			t_1=T-0.1
			t_2=T+0.1
		END DO
		WRITE(unit=k_x_R,fmt=*)
	END DO
	CLOSE(unit=theta_x_R)
	CLOSE(unit=k_x_R)
END PROGRAM arremesso