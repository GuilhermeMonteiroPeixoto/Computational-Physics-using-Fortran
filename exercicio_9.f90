! Objetivo: Integrar a equacao diferencial que descreve o oscilador de Van der Pol 
!           e analisar o espectro_de_f de frequencias de sua transformada de Fourier

MODULE derivada
	REAL, PARAMETER :: mu=1.0,omega0=1.0,y0=1.0
CONTAINS
	SUBROUTINE deriv_func_vanderpol(t,y,dydt)
		USE nrtype
      		IMPLICIT NONE
      		REAL, INTENT(IN) :: t
      		REAL, DIMENSION(:), INTENT(IN) :: y
      		REAL, DIMENSION(:), INTENT(OUT) :: dydt
      		dydt(1)= y(2)
      		dydt(2)= mu*y(2)*(y0**2-(y(1))**2)-y(1)*omega0**2
	END SUBROUTINE
END MODULE derivada
PROGRAM FFT
	USE nrtype
	USE derivada
	USE nr, ONLY : mmid,bsstep,odeint,four1,fourrow
	INTEGER, PARAMETER :: arquivo_saida1=8,arquivo_saida2=9,num_pontos=512,&
						num_pontos_sob2=num_pontos/2
	REAL, PARAMETER :: x_inicial=2.0,t_inicial=0.0,v_inicial=0.0,&
				real_num_pontos=real(num_pontos), eps=1.0e-4 
	INTEGER :: contador
	REAL :: x1,x2,hmin=0.0,h1,f,deltaf,espectro_de_f
	CHARACTER(len=80) :: x_versus_t, delta_t
	COMPLEX, DIMENSION(num_pontos) :: DATA
	REAL, DIMENSION(2) :: ystart

	PRINT *, "Digite o intervalo do tempo?"
	READ *, h1
! Criando o nome do arquivo de dados  
	WRITE(x_versus_t,'("x_versus_t=",f8.6,".dat")')h1
	WRITE(delta_t,'("delta_t=",f8.6,".dat")')h1
	OPEN(unit=arquivo_saida1,file=x_versus_t)
	OPEN(unit=arquivo_saida2,file=delta_t)  
	ystart(1)=x_inicial
	ystart(2)=v_inicial
	x1=t_inicial
	x2=x1+h1
	DO contador=1,num_pontos
		CALL odeint(ystart,x1,x2,eps,h1,hmin,deriv_func_vanderpol,bsstep)
		WRITE(arquivo_saida1,*) x2,ystart(1)
		DATA(contador)=cmplx(ystart(1))
		x1=x2
		x2=x1+h1
	END DO
	CALL four1(DATA,1)
	deltaf=1.0/(real_num_pontos*h1)
	f=0.0
	espectro_de_f=ABS(DATA(1))
	WRITE(arquivo_saida2,*) f,espectro_de_f
	f=f+deltaf
! Escrevendo o espectro_de_f de frequencia versus a frequencia
	DO contador=2,num_pontos_sob2
		espectro_de_f=ABS(DATA(contador))+ABS(DATA(num_pontos-contador+2))
		WRITE(arquivo_saida2,*) f,espectro_de_f
		f=f+deltaf
	END DO
END PROGRAM FFT
