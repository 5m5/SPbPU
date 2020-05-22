subroutine Fun(t, x, yP)

    real :: t
    real :: x(2)
    real :: yP(2)

    yP(1) = -73*x(1)-210*x(2)+log(1+t**2)
    yP(2) = x(1)+2.7182818284590451**(-t)+t**2+1

    return

end subroutine Fun

subroutine euler1(t, tPrint, x, yP)
    real xTemp(2)
    real, intent(inout) :: yP(2)
    real, intent(in) :: t, tPrint, x(2)
    call Fun(t,x,xTemp)
    yP(1) = tPrint * xTemp(1)
    yP(2) = tPrint * xTemp(2)
end subroutine euler1

subroutine euler2(t, tPrint, x, k1, yP)
    real xTemp(2),tt,xx(2)
    real, intent(inout) :: yP(2)
    real, intent(in) :: t, tPrint, x(2), k1(2)
    tt = t + tPrint/2
    xx(1) = x(1) + k1(1)
    xx(2) = x(2) + k1(2)
    call Fun(tt,xx,xTemp)
    yP(1) = tPrint * xTemp(1)
    yP(2) = tPrint * xTemp(2)
end subroutine euler2

subroutine euler3(t, tPrint, x, k2, yP)
    real xTemp(2),tt,xx(2)
    real, intent(inout) :: yP(2)
    real, intent(in) :: t, tPrint, x(2), k2(2)
    tt = t + 3*tPrint/4
    xx(1) = x(1) + 3*k2(1)/4
    xx(2) = x(2) + 3*k2(2)/4
    call Fun(tt,xx,xTemp)
    yP(1) = tPrint * xTemp(1)
    yP(2) = tPrint * xTemp(2)
end subroutine euler3

program lab3

    external Fun, euler1, euler2, euler3
    
    integer, parameter :: neqn = 2
    real t, x(neqn), tOut, relerr, abserr, tFinal, tPrint, work(15), &
         k1(neqn), k2(neqn), xTmp(neqn)
    integer iWork(5), iFlag, i
  
    t = 0
    tFinal = 1
    tPrint = 0.05
    tOut = t + tPrint
    x(1) = -3
    x(2) = 1
    reller = 1e-9 
    abserr = 0.0
    iFlag = 1

    write(*,'(a/)') "Выполнение подпрограммы RKF45:"

    do while(t <= tFinal)
        call RKF45(Fun, neqn, x, t, tOut, relerr, abserr, iFlag, work, iWork)
        write(*,'(a,f9.4,2(a, f9.6))') "t = ", t, " x1 = ", x(1)," x2 = ", x(2)
        select case(iFlag)
            case(1,8)
                exit
            case(2)
                tOut = t + tPrint
                if (t < tFinal) then
                    continue
                end if
            case(4)
                continue
            case(5)
                abserr = 1e-9
                continue
            case(6)
                reller = 10 * reller
                iFlag = 2
                continue
            case(7)
                iFlag = 2
                continue
        end select
    end do

    t = 0
    tFinal = 1
    tPrint = 0.05
    x(1) = -3
    x(2) = 1

    write(*,'(/a,f5.3,a/)') "Выполнение методом Эйлера-Коши с шагом tPrint = ", tPrint, ":"

    write(*,'(a,f9.4,2(a, f20.6))') "t = ", t, " x1 = ", x(1)," x2 = ", x(2)

    do 

        call euler1(t,tPrint,x,k1)
        call euler2(t,tPrint,x,k1,k2)

        xTmp(1) = x(1) + (k1(1)+k2(1))/2
        xTmp(2) = x(2) + (k1(2)+k2(2))/2
        t = t + tPrint

        write(*,'(a, f9.4, 2(a, f20.6))') "t = ", t, " x1 = ", xTmp(1)," x2 = ", xTmp(2)
        
        x(1) = xTmp(1)
        x(2) = xTmp(2)

        if(t > tFinal) then
            exit
        end if
    end do

    t = 0
    tFinal = 1
    tPrint = 0.005
    x(1) = -3
    x(2) = 1
    i = 0

    write(*,'(/a,f6.4,a/)') "Выполнение методом Эйлера-Коши с шагом tPrint = ", tPrint, ":"

    write(*,'(a,f9.4,2(a, f9.6))') "t = ", t, " x1 = ", x(1)," x2 = ", x(2)

    do 

        i = i + 1
        
        call euler1(t,tPrint,x,k1)
        call euler2(t,tPrint,x,k1,k2)

        xTmp(1) = x(1) + (k1(1)+k2(1))/2
        xTmp(2) = x(2) + (k1(2)+k2(2))/2
        t = t + tPrint

        if (mod(i,10) == 0) then
            write(*,'(a, f9.4, 2(a, f9.6))') "t = ", t, " x1 = ", xTmp(1)," x2 = ", xTmp(2)
        end if
        
        x(1) = xTmp(1)
        x(2) = xTmp(2)

        if(t > tFinal) then
            exit
        end if
    end do

end program lab3
