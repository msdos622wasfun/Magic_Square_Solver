program magic_square_solver

    implicit none

    integer :: solutions_desired
    character :: display_pref
    logical :: okay
    integer :: solutions_found
    logical :: solved
    integer(kind = 8) :: attempts
    integer, dimension(3, 3) :: a
    integer :: x, y
    integer :: n
    real :: r
    integer, dimension(8) :: dt
    integer(kind = 8) :: time1
    integer(kind = 8) :: time2
    integer(kind = 8) :: diff
    integer :: m
    real :: s
    character :: run_again

    call random_seed()

    print *, ""
    print *, "+-------------------------------------+"
    print *, "|                                     |"
    print *, "|      Magic Square Solver v1.00      |"
    print *, "|     Copyleft 2020 by Erich Kohl     |"
    print *, "|                                     |"
    print *, "+-------------------------------------+"
    print *, ""

    run_again = 'Y'

    do while(run_again == 'Y' .or. run_again == 'y')

        okay = .false.

        print *, "Enter the number of desired solutions to find (1 - 1000):"
        print *, ""

        do while (okay .eqv. .false.)
            read *, solutions_desired
            if (solutions_desired >= 1 .and. solutions_desired <= 1000) then
                okay = .true.
            end if
        end do

        print *, ""
        print *, "Choose one of the following options:"
        print *, ""
        print *, "1. Display each square as it is generated"
        print *, "2. Display only squares that are solved"
        print *, "3. Display only stats at the end"
        print *, ""

        okay = .false.

        do while (okay .eqv. .false.)
            read *, display_pref
            if (display_pref == '1' .or. display_pref == '2' .or. display_pref == '3') then
                okay = .true.
            end if
        end do

        print *, ""
        print *, "Press Enter to begin..."
        read (*, *)

        if (display_pref == '2' .or. display_pref == '3') then
            print *, "Processing..."
            print *, ""
        end if

        solutions_found = 0
        attempts = 0

        call date_and_time(values = dt)
        time1 = dt(8) + (dt(7) * 1000) + (dt(6) * 60000) + (dt(5) * 3600000)

        do while (solutions_found < solutions_desired)
            do x = 1,3
                do y = 1,3
                    a(x, y) = 0
                end do
            end do
            do n = 1,9
                do
                    call random_number(r)
                    x = int(r * 3 + 1)
                    call random_number(r)
                    y = int(r * 3 + 1)
                    if (a(x, y) == 0) exit
                end do
                a(x, y) = n
            end do
            attempts = attempts + 1
            solved = .true.
            if(a(1,1)+a(2,1)+a(3,1) /= 15) solved = .false.
            if(a(1,2)+a(2,2)+a(3,2) /= 15) solved = .false.
            if(a(1,3)+a(2,3)+a(3,3) /= 15) solved = .false.
            if(a(1,1)+a(1,2)+a(1,3) /= 15) solved = .false.
            if(a(2,1)+a(2,2)+a(2,3) /= 15) solved = .false.
            if(a(3,1)+a(3,2)+a(3,3) /= 15) solved = .false.
            if(a(1,1)+a(2,2)+a(3,3) /= 15) solved = .false.
            if(a(3,1)+a(2,2)+a(1,3) /= 15) solved = .false.
            if (solved .eqv. .true.) then
                solutions_found = solutions_found + 1
                if (display_pref == '1' .or. display_pref == '2') then
                    do y = 1,3
                        if (y == 1 .or. y == 3) then
                            write(*, 10) "          ", a(1, y), " | ", a(2, y), " | ", a(3, y)
                            10 format(a9,i1,a3,i1,a3,i1)
                        else
                            write(*, 20) " Solved:  ", a(1, y), " | ", a(2, y), " | ", a(3, y)
                            20 format(a9,i1,a3,i1,a3,i1)
                        end if
                    end do
                    print *, ""
                end if
            else
                if (display_pref == '1') then
                    do y = 1,3
                        write(*, 30) "          ", a(1, y), " | ", a(2, y), " | ", a(3, y)
                        30 format(a9,i1,a3,i1,a3,i1)
                    end do
                    print *, ""
                end if
            end if
        end do

        call date_and_time(values = dt)
        time2 = dt(8) + (dt(7) * 1000) + (dt(6) * 60000) + (dt(5) * 3600000)

        print *, "*** TEST COMPLETE ***"
        print *, ""

        diff = time2 - time1

        s = real(diff) / 1000.
        m = 0
        if(s >= 60) m = int(s / 60)
        s = s - (m * 60)

        write(*, 40) " Total time = ", m, " minute(s), ", s, " second(s)"
        40 format(a14,i2,a12,f6.3,a10)
        write(*, 50) " Total solutions attempted = ", attempts
        50 format(a29,i10)
        write(*, 60) " Total solutions found = ", solutions_found
        60 format(a25,i4)
        write(*, 70) " Avg. attempts per solution = ", real(attempts) / real(solutions_found)
        70 format(a30,f10.2)
        write(*, 80) " Avg. time per solution = ", (real(diff) / 1000.) / real(solutions_found), " second(s)"
        80 format(a26,f6.3,a10)

        print *, ""
        print *, "Perform another run (y/n)?"
        print *, ""
        read *, run_again
        print *, ""

    end do

    print *, "Press Enter to quit..."
    read (*, *)

end program magic_square_solver
