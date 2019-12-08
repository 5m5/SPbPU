program lab1
    implicit none
    Integer n, a1, a2, a3, I , J, IPVT(5)
    Real p(5), pp, A(8, 8), A_1(8, 8), E(8, 8), WORK(8), con, condp1, R(8, 8), tmp, norm, copyA(8, 8)
    n = 8
    p = (/1.0, 0.1, 0.01, 0.0001, 0.000001/)

    A(1, :) = (/27.000001, -6.0, -1.0, -6.0, -3.0, -4.0, -3.0, -4.0/)
    A(2, :) = (/-6.0, 35.0, -1.0, -6.0, -5.0, -6.0, -3.0, -8.0/)
    A(3, :) = (/-1.0, -1.0, 19.0, -6.0, -8.0, -2.0, 0.0, -1.0/)
    A(4, :) = (/-6.0, -6.0, -6.0, 36.0, -4.0, -3.0, -4.0, -7.0/)
    A(5, :) = (/-3.0, -5.0, -8.0, -4.0, 25.0, 0.0, -1.0, -4.0/)
    A(6, :) = (/-4.0, -6.0, -2.0, -3.0, 0.0, 28.0, -8.0, -5.0/)
    A(7, :) = (/-3.0, -3.0, 0.0, -4.0, -1.0, -8.0, 21.0, -2.0/)
    A(8, :) = (/-4.0, -8.0, -1.0, -7.0, -4.0, -5.0, -2.0, 31.0/)
    
    E = 0
    DO I = 1, N
        DO J = I, N
            if(i == j) then
               E(i, j) = 1
            end if
        END DO
    END DO
    
    A_1 = E

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

    R = E - MATMUL(A_1, copyA)

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

end program lab1
