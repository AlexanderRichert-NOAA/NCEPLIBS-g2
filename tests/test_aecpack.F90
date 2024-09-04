! This program tests the aecpack subroutine of the NCEPLIBS-g2
! project.
!
! Alyson Stahl 2024-09-04
program test_aecpack

  implicit none

  integer, parameter :: width=2, height=2, ndpts=4
  real, parameter :: delta = 0.0000001
  real :: fld(ndpts), fld2(ndpts)
  integer :: idrstmpl(8)
  character(len=1) :: cpack(80)
  integer :: lcpack = 80
  integer :: i

  print *, 'Testing aecpack()/aecunpack()...'

  ! Create the fld variable with data to pack
  fld = (/1.0, 2.0, 3.0, 4.0/)

  idrstmpl(1) = 0
  idrstmpl(2) = 1
  idrstmpl(3) = 1
  idrstmpl(4) = 0
  idrstmpl(5) = 0
  idrstmpl(6) = 0
  idrstmpl(7) = 16
  idrstmpl(8) = 128

  ! Testing aecpack
  call aecpack(fld, width, height, idrstmpl, cpack, lcpack)
  print *, 'lcpack: ', lcpack

  ! Testing aecunpack
  call aecunpack(cpack, lcpack, idrstmpl, ndpts, fld2)

  ! Compare each value to see match, reals do not compare well
  do i = 1, ndpts
     if (abs(fld(i) - fld2(i)) .ge. delta) then
        print *, fld(i), fld2(i), 'do not match'
        stop 4
     end if
  end do

  print *, 'SUCCESS!'

end program test_aecpack
