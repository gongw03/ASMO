! The two Julian day routines are from G. Liston's SnowModel, but updated to F90
! A. Wood 2016 added two date math routines

subroutine julian_day(year,month,day,jday)
! returns vector
  use nrtype

  implicit none

  ! input variables
  integer(I4B),dimension(:),intent(in) :: year
  integer(I4B),dimension(:),intent(in) :: month
  integer(I4B),dimension(:),intent(in) :: day

  ! output variables
  integer(I4B),dimension(:),intent(out) :: jday

  ! Calculate the day of year (1,...,365,366) corresponding to the date
  !   iyear-imonth-iday. 
  jday = day &
          + min(1,max(0,month-1))*31 &
          + min(1,max(0,month-2))*(28+(1-min(1,mod(year,4)))) &
          + min(1,max(0,month-3))*31 &
          + min(1,max(0,month-4))*30 &
          + min(1,max(0,month-5))*31 &
          + min(1,max(0,month-6))*30 &
          + min(1,max(0,month-7))*31 &
          + min(1,max(0,month-8))*31 &
          + min(1,max(0,month-9))*30 &
          + min(1,max(0,month-10))*31 &
          + min(1,max(0,month-11))*30 &
          + min(1,max(0,month-12))*31

  return
end subroutine julian_day

!ccccccccccccccccccccccccccccccccc

subroutine julianday_scalar(iyear,imonth,iday,jday_scalar)
  use nrtype
  implicit none

  ! input variables
  integer(I4B),intent(in)  :: iyear
  integer(I4B),intent(in)  :: imonth
  integer(I4B),intent(in)  :: iday

  ! output variables
  integer(I4B),intent(out) :: jday_scalar

  ! Calculate the day of year (1...365,366) corresponding to the date
  !   iyear-imonth-iday. 
  jday_scalar = iday &
          + min(1,max(0,imonth-1))*31 &
          + min(1,max(0,imonth-2))*(28+(1-min(1,mod(iyear,4)))) &
          + min(1,max(0,imonth-3))*31 &
          + min(1,max(0,imonth-4))*30 &
          + min(1,max(0,imonth-5))*31 &
          + min(1,max(0,imonth-6))*30 &
          + min(1,max(0,imonth-7))*31 &
          + min(1,max(0,imonth-8))*31 &
          + min(1,max(0,imonth-9))*30 &
          + min(1,max(0,imonth-10))*31 &
          + min(1,max(0,imonth-11))*30 &
          + min(1,max(0,imonth-12))*31

  return
end subroutine julianday_scalar

! AWW-2016: added date difference function
! allows for date 2 to be before date 1 (neg days)
subroutine date_diff_ndays( yr1, mo1, dy1, yr2, mo2, dy2, nday_diff )
  use nrtype
  implicit none

  integer(I4B),intent(in)  :: yr1, mo1, dy1, yr2, mo2, dy2  
  integer(I4B),intent(out) :: nday_diff

  !local variables
  integer(I4B) ::  y, jday_y1, jday_y2, tmpdays

  nday_diff = 0
  call julianday_scalar(yr1,mo1,dy1,jday_y1)
  call julianday_scalar(yr2,mo2,dy2,jday_y2)

  ! work though various cases of how the dates relate
  if(yr2 == yr1) then 

     ! dates are in same year, just need to deal with days
      nday_diff = jday_y2-jday_y1+1

  else if (yr2 > yr1) then

    ! first date year is before second
    call julianday_scalar(yr1,12,31,tmpdays)
    nday_diff = tmpdays-jday_y1+1

    do y = (yr1+1),yr2
      if(y == yr2) then
        nday_diff = nday_diff+jday_y2
      else
        call julianday_scalar(y,12,31,tmpdays)
        nday_diff = nday_diff + tmpdays
      endif
    enddo       
  
  else 

    ! second date year is before first
    call julianday_scalar(yr2,12,31,tmpdays)
    nday_diff = tmpdays-jday_y2+1

    do y = (yr2+1),yr1
      if(y == yr1) then
        nday_diff = nday_diff+jday_y1
      else
        call julianday_scalar(y,12,31,tmpdays)
        nday_diff = nday_diff + tmpdays
      endif
    enddo       

  endif    

  return

end subroutine date_diff_ndays


! AWW-2016: subtract one day from a date and return new date
!   specific for finding state file date from starting date
subroutine day_before_date(year,month,day,newyear,newmonth,newday)
  use nrtype
  implicit none

  ! input variables
  integer(I4B),intent(in)  :: year       ! starting date
  integer(I4B),intent(in)  :: month
  integer(I4B),intent(in)  :: day

  ! output variables
  integer(I4B),intent(out)  :: newyear   ! ending date
  integer(I4B),intent(out)  :: newmonth
  integer(I4B),intent(out)  :: newday

  ! set starting date
  newyear  = year
  newmonth = month
  newday   = day

  ! Calculate the date one day before the current date
  if(day==1 .and. month==1) then
    ! shift to last day of prior year
    newyear  = year-1
    newmonth = 12
    newday   = 31
  else if(day == 1) then
    ! shift to last day of prior month
    newmonth = month-1 
    ! and that last day is ...
    if(newmonth == 4 .or. newmonth == 6 .or. newmonth == 9 .or. newmonth == 11) then
      newday = 30
    else if(newmonth == 2) then
      newday = 28
      if (mod(newyear,4)   == 0) newday = 29
      if (mod(newyear,100) == 0) newday = 28
      if (mod(newyear,400) == 0) newday = 29
    else
      newday = 31
    endif
  else 
    ! just shift to prior day within month
    newday = day-1  
  endif

  return
end subroutine day_before_date
