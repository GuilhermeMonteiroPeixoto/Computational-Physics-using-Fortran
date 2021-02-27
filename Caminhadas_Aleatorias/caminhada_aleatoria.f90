! Objetivo: Calcular a probabilidade de um caminhante, apos N passos,
!           parar em determinada posicao.

PROGRAM caminhadas_aleatorias

    USE nrtype
    USE ran_state, ONLY: ran_seed
    USE nr, ONLY: ran0 
    
    IMPLICIT none
    INTEGER :: iseed = 1234
    INTEGER, PARAMETER :: passos_total = 20, total_caminhantes = 100000
    INTEGER :: passo, caminhante_n, posicao
    INTEGER, PARAMETER :: dados_caminhada_aleatoria = 7
! Quantidade_posfinais eh a quantidade de caminhantes que terminaram em
! determinadas posicoes
    INTEGER, DIMENSION (-passos_total:passos_total) :: quantidade_posfinais = 0
    REAL :: aleatorio
    REAL, PARAMETER :: total_caminhantes_real = REAL(total_caminhantes)
    REAL, DIMENSION(-passos_total:passos_total) :: probabilidade
! Iniciar semente
    CALL RAN_SEED (SEQUENCE = iseed)
    DO caminhante_n = 1, total_caminhantes
        posicao = 0
        DO passo = 1, passos_total
! Chamar numero aleatorio uniformemente distribuido entre 0. e 1.
! excluindo os extremos
            CALL ran0(aleatorio)
! A subrotina retorna um numero aleatorio uniformemente distribuido entre 0. e
! 1. (excluindo os extremos). Para termos chances iguais de movimentacao (direita
! e esquerda) assumimos que para aleatorio menor que 0.5 (passo para esquerda)
! para aleatorio maior que 0.5 (passo para direita) e para 0.5 (nao anda).
            IF(aleatorio < 0.5)THEN
                posicao = posicao-1
            ELSE
                IF(aleatorio > 0.5) THEN
                    posicao = posicao+1
                END IF
            END IF
        END DO
!Contabilizar a quantidade de caminhadas finalizadas em cada posicao
        quantidade_posfinais(posicao) = quantidade_posfinais(posicao) + 1
    END DO
! Calcular a probabilidade de um caminhante parar em cada posicao
    probabilidade = quantidade_posfinais/total_caminhantes_real
    
    OPEN(dados_caminhada_aleatoria, FILE="dados_caminhada_aleatoria.dat")
! Escrever a posicao e a probabilidade (excluindo os casos em que a
! probabilidade eh zero)
    DO posicao = -passos_total, passos_total, 2
        WRITE(dados_caminhada_aleatoria, *) posicao, probabilidade(posicao)
    END DO
    CLOSE(dados_caminhada_aleatoria)
    
END PROGRAM caminhadas_aleatorias