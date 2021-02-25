! Objetivo: Integrar a equacao diferencial do oscilador de Van der Pol 
!	    de t=0 ate t=8pi para obter os graficos x, v e E/m em funcao de 
!           t/pi e investigar sua trajetoria no espaco de fase.

MODULE derivada
	REAL, PARAMETER :: omega0=1.0,mu=1.0,x0=1.0
CONTAINS
	SUBROUTINE deriv_func_vanderpol(t, x, dxdt)
		USE nrtype
		IMPLICIT none
		REAL(SP), intent(in) :: t
		REAL(SP), DIMENSION(:), intent(in) :: x
		REAL(SP), DIMENSION(:), intent(out) :: dxdt
		dxdt(1)=x(2)
		dxdt(2)=mu*x(2)*(x0**2-(x(1))**2)-x(1)*omega0**2
	END SUBROUTINE deriv_func_vanderpol
END MODULE derivada

PROGRAM oscilador_van_der_pol
	USE nrtype
	USE derivada
	USE nr, only: bsstep, odeint
	IMPLICIT none
	INTEGER :: ponto_ti, curva_in
	INTEGER, PARAMETER :: Arquivo_t_x_v_Em=1, Arquivo_espaco_fase=2
	INTEGER, PARAMETER :: pontos=800, num_curvas=6
	REAL :: t1, t2, Em
	REAL, DIMENSION(2) :: x
	REAL, PARAMETER :: pts_menos1=pontos-1, curv_menos1=num_curvas-1
	REAL, PARAMETER :: tempo_i=0.0, tempo_final=8.0*pi, variacao_t= &
					& (tempo_final-tempo_i)/pts_menos1
	REAL, PARAMETER :: posicao_i=0.5, posicao_f=3.0, variacao_posi= &
					& (posicao_f-posicao_i)/curv_menos1
	REAL, PARAMETER :: velocidade_i=0.0, eps=epsilon(posicao_i), &
						&  h1=1.e-6, hmin=0.0
	REAL, PARAMETER :: meio_omega02=0.5*omega0**2
	x(1)=posicao_i
	x(2)=velocidade_i
	t1=tempo_i

	OPEN(unit=Arquivo_t_x_v_Em, file="x_v_Em_vs_tpi.dat")
	OPEN(unit=Arquivo_espaco_fase, file="v_vs_x.dat")
! Escrever a posicao, velocidade e energia total dividida pela massa 
! em funcao do tempo/pi
	WRITE(unit=Arquivo_t_x_v_Em,fmt=*) t1/pi, x, 0.5*x(2)**2+ &
						& meio_omega02*x(1)**2
	t2=tempo_i+variacao_t
	DO ponto_ti=2, pontos
		CALL odeint(x, t1, t2, eps, h1, hmin, deriv_func_vanderpol, bsstep)
! Escrever a posicao, velocidade e energia total dividida pela massa
! em funcao do tempo/pi
		WRITE(unit=Arquivo_t_x_v_Em,fmt=*) t2/pi, x, 0.5*x(2)**2+ &
						& meio_omega02*x(1)**2
		t1=t2
		t2=t2+variacao_t
	END DO
! Investigando a trajetoria do oscilador no espaco de fase
	x(1)=posicao_i
	DO curva_in=1, num_curvas
		t1=tempo_i
		t2=tempo_i+variacao_t
		x(2)=x(1)
		WRITE(unit=Arquivo_espaco_fase,fmt=*) x
		DO ponto_ti=1,pontos
			CALL odeint(x,t1,t2,eps,h1,hmin,deriv_func_vanderpol,bsstep)
			WRITE(unit=Arquivo_espaco_fase,fmt=*) x
			t1=t2
			t2=t2+variacao_t
		END DO
		WRITE(unit=Arquivo_espaco_fase,fmt=*)
		x(1)=posicao_i+variacao_posi*REAL(curva_in)
	END DO
	CLOSE(unit=Arquivo_t_x_v_Em)
	CLOSE(unit=Arquivo_espaco_fase)
END PROGRAM oscilador_van_der_pol
