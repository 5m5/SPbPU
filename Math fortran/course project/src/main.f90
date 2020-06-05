module global_variable
    real :: f
end module global_variable

REAL FUNCTION integral(z) ! функция для расчета интегральной части выражени
    use global_variable
    REAL z
        integral = (1.6 * z)/(sin(z)**2 + 2.56 * f * cos(z)**2)
    RETURN
END

REAL FUNCTION fun(y) ! функция для нахождения корня уравнения
    use global_variable
    EXTERNAL integral
    integer :: nointegral
    real :: A = 0.0, B = 3.1415926535897932/2, RELERR = 1.E-6, ABSERR = 0.0, &
    ERREST, FLAG, RESULT
    REAL y
    f = y

    CALL QUANC8(integral, A, B, ABSERR, RELERR, RESULT, ERREST, nointegral, FLAG)
    
    fun = RESULT - y * 1.465862

    RETURN
END
  
program course
    use global_variable
    EXTERNAL fun

    real :: RELERR = 1.E-6, x_min = -1.0, x_max = 1.09, SEVAL, ZEROIN, &
    x(21), f_x(21), LAGRANGE_VALS(21), SPLINE_VALS(21), &
    f_min = 0.5, f_max = 2.0, BB(21), CC(21), DD(21), &
    Q = 0, z1 = 0, z2 = 1000000, z_min = 0, z = 0
    
    integer :: i = 1
  
    do i = 1, 6 ! поиск минимизирующего значения z*
        z = i * 0.1
        z1 = (4*z+1)**2 + 8*z*(2**z+1) + 4**z
        if(z1 <= z2) then
            z2 = z1
            z_min = z
        end if
    end do
  
    Q = 80.66811 * z_min ! Q

    i = 1
    do while (x_min <= x_max) ! Рачет значений исходной функции
        x(i) = x_min
        f_x(i) = 1 / (1 + Q * x_min**2)
        x_min = x_min + 0.1
        i = i + 1
    end do
    
    do i = 1, 21 ! Расчет значений интерполирующей функции Лагранжа
        LAGRANGE_VALS(i) = LAGRANGE(x, f_x, 21, f_x(i))
    end do

    call spline(21, x, f_x, BB, CC, DD)
    do i = 1, 21 ! Рачет значений сплайн-функции
        SPLINE_VALS(i) = SEVAL(21, f_x(i), x, f_x, BB, CC, DD)
    end do

    do i = 1, 21
        WRITE (*,*) "-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!"
        WRITE (*,*) "Исходная функция, alpha = Q"
        WRITE (*,*) "x = ", x(i)
        WRITE (*,*) "Значения функции f(x): ", f_x(i)
        WRITE (*,*) "Значения полинома Лагранжа: ", LAGRANGE_VALS(i)
        WRITE (*,*) "Значения сплайн-функции: ", SPLINE_VALS(i)
    end do

    f =  ZEROIN(f_min, f_max, fun, RELERR)
    
    x_min = -1.0
    i = 1
    do while (x_min <= x_max) ! Рачет значений исходной функции
        x(i) = x_min
        f_x(i) = 1/(1 + f * x_min**2)
        x_min = x_min + 0.1
        i = i + 1
    end do

    do i = 1, 21 ! Расчет значений интерполирующей функции Лагранжа
        LAGRANGE_VALS(i) = LAGRANGE(x, f_x, 21, f_x(i))
    end do

    call spline(21, x, f_x, BB, CC, DD)
    do i = 1, 21 ! Рачет значений сплайн-функции
        SPLINE_VALS(i) = SEVAL(21, f_x(i), x, f_x, BB, CC, DD)
    end do

    do i = 1, 21
        WRITE (*,*) "-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!"
        WRITE (*,*) "Исходная функция, alpha = F"
        WRITE (*,*) "x = ", x(i)
        WRITE (*,*) "Значения функции f(x): ", f_x(i)
        WRITE (*,*) "Значения полинома Лагранжа: ", LAGRANGE_VALS(i)
        WRITE (*,*) "Значения сплайн-функции: ", SPLINE_VALS(i)
    end do

    contains

    REAL FUNCTION LAGRANGE(xx, yy, n, x_i)
        INTEGER n, i, j
        REAL xx(n), yy(n), x_i, result, p
        result = 0.0
        do i = 1, n
            p = 1.0
            do j = 1, n
                if(i /= j) then
                    p = p*(x_i - xx(j)) / (xx(i) - xx(j))
                end if
            end do
            result = result + p * yy(i)
        end do
        LAGRANGE = result
        return
    END

end program
