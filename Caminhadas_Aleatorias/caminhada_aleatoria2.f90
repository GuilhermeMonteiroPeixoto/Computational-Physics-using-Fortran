! Objetivo: Gerar um grafico da posicao media, da dispersao e do logaritmo
! da dispersão versus logaritmo de N.

PROGRAM caminhadas_aleatorias2

    USE nrtype
    USE ran_state, ONLY: ran_seed
    USE nr, ONLY: ran0

    IMPLICIT none
    INTEGER :: iseed = 1234
    INTEGER, PARAMETER :: passos_total = 200, total_caminhantes = 100000
    INTEGER :: passo, caminhante_n, posicao, passos_validos, posi_array
    INTEGER, PARAMETER :: dados_caminhada_aleatoria = 7
    REAL :: aleatorio
    INTEGER, PARAMETER :: dados_media = 7, dados_dispersao = 8, dados_log = 9
    INTEGER, PARAMETER :: num_pontos = 20, intervalo_passo = 10
    INTEGER, DIMENSION( num_pontos ) :: soma_posicao=0, soma_posicao_quad = 0
    REAL media, dispersao
    REAL, PARAMETER :: caminhantes_real = REAL(total_caminhantes)
! Iniciar semente
    CALL RAN_SEED (SEQUENCE = iseed)
    DO caminhante_n = 1, total_caminhantes
        posicao = 0
        DO passo = 1, num_pontos
            passos_validos = 0
            DO
                IF (passos_validos==intervalo_passo)EXIT
! Chamar numero aleatorio uniformemente distribuido entre 0. e 1.
! excluindo os extremos
                CALL ran0(aleatorio)
! A subrotina retorna um numero aleatorio uniformemente distribuido entre 0. e
! 1. (excluindo os extremos). Para termos chances iguais de movimentacao
! (direita e esquerda) assumimos que para aleatorio menor que 0.5 (passo para
! esquerda) para aleatorio maior que 0.5 (passo para direita) e para 0.5
! (nao anda).
                IF(aleatorio < 0.5)THEN
                    posicao = posicao-1
                    passos_validos = passos_validos+1
                ELSE
                    IF(aleatorio > 0.5) THEN
                        posicao = posicao+1
                        passos_validos = passos_validos+1
                    END IF
                END IF
            END DO
            soma_posicao(passo) = soma_posicao(passo) + posicao
            soma_posicao_quad(passo) = soma_posicao_quad(passo) + &
                                                            posicao*posicao
        END DO
    END DO

    OPEN(dados_media, file="dados_media.dat")
    OPEN(dados_dispersao, file="dados_dispersao.dat")
    OPEN(dados_log, file="dados_log.dat")
        DO passo=10, passos_total, intervalo_passo
            posi_array = passo/intervalo_passo
            media = soma_posicao(posi_array)/caminhantes_real
            dispersao = sqrt(soma_posicao_quad(posi_array)/&
                                    caminhantes_real - media*media)
            WRITE(dados_media, *) passo, media
            WRITE(dados_dispersao, *) passo, dispersao
            WRITE(dados_log, *) log(REAL(passo)), log(dispersao)
        END DO
    CLOSE(dados_media)
    CLOSE(dados_dispersao)
    CLOSE(dados_log)

END PROGRAM caminhadas_aleatorias2