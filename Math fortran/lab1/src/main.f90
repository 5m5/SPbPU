program lab2
    implicit none
    Integer n, a1, a2, a3, I , J, IPVT(5)
    Real a4(4), B(4), A(5, 5), A_1(5, 5), E(5, 5), WORK(5), con, condp1, R(5, 5), tmp, norm, copyA(5, 5)
    n = 5
    a1 = 4
    a2 = 3
    a3 = 2
    a4 = (/1.5, 1.01, 1.001, 1.0001/)

    A = 1
    E = 0
    DO I = 1, N
        DO J = I, N
            if(i == j) then
               E(i, j) = 1
            end if
        END DO
    END DO
    
    A_1 = E

    B(1) = a1
    B(2) = a2
    B(3) = a3
    B(4) = a4(1)

    DO I = 1, N-1
      write(*,*) B(I)
    END DO

    DO I = 1, N-1
        DO J = N, I+1, -1
            A(i, j) = B(I)
        END DO
    END DO

    write(*,*) "Матрица A:"
    DO I = 1, N
        write(*,*) A(I, 1:N)
    END DO

    copyA = A
    CALL DECOMP(N, N, A, con, IPVT, WORK)
    condp1 = con + 1.0
    write(*,*) "Число обусловленности:", con

    DO i = 1, N
        CALL SOLVE(N, N, A, A_1(1, i), IPVT)
    END DO

    write(*,*) "Обратная матрица:"
    DO i = 1,N
        write(*,*) A_1(i,1:N)
    END DO

    R = MATMUL(copyA, A_1) - E

    write(*,*) "Матрица R:"
    DO i = 1,N
        write(*,*) R(i,1:N)
    END DO

    DO i = 1, N
        tmp=0;
        DO J = 1, N
            tmp=tmp+ABS(R(i,j))
            if (tmp > norm) then
                norm = tmp
            end if
        end do
    end do
        write(*,*) "Норма ||R|| = ", norm

end program lab2
