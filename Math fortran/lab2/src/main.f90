program lab2
    implicit none
    INTEGER N,I, k
    external SEVAL
    real SEVAL
    REAL X(8),Y(8), B(8), C(8), D(8), A1, B1, C1, fC, fA, fB, res, fres
    k = 15
    N = 8
    A1 = 0
    B1 = 2
    X = (/0.0, 0.2, 0.5, 0.7, 1.0, 1.3, 1.7, 2.0/)
    Y = (/1.0, 1.1487, 1.4142, 1.6245, 2.0000, 2.4623, 3.2490, 4.0000/)
    CALL SPLINE(N,X,Y,B,C,D)
    do i = 1, k
        C1 = (A1 + B1) / 2

        fA = SEVAL(N,A1,X,Y,B,C,D) + 5 * A1 - 3

        fB = SEVAL(N,B1,X,Y,B,C,D) + 5 * B1 - 3

        fC = SEVAL(N,C1,X,Y,B,C,D) + 5 * C1 - 3

        if (fA*fC < 0.0) B1 = C1
        if (fB*fC < 0.0) A1 = C1
    end do

    res = (A1+B1)/2
    fres = SEVAL(N,res,X,Y,B,C,D) + 5 * res - 3
    WRITE (*,*) "interval [0; 2]"
    WRITE (*,*) "number of steps: ", k
    WRITE (*,*) "result interval: [", A1,";",B1,"]"
    WRITE (*,*) "X:", res
    WRITE (*,*) "residual:", fres

end program lab2
