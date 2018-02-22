! A. Wood, Aug 2016:  These subs reconfigured to work w/ multi-HRU model setup

subroutine write_snow17_state(year,month,day,hour,cs,tprev,sim_length,curr_hru_id)
  use nrtype
  use def_namelists, only: snow_state_out_root
  implicit none

  !input variables
  character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for state fname
  integer(I4B),dimension(:),intent(in)	:: year
  integer(I4B),dimension(:),intent(in)	:: month
  integer(I4B),dimension(:),intent(in)	:: day
  integer(I4B),dimension(:),intent(in)	:: hour
  real(sp),dimension(:,:),intent(in)	:: cs	    ! carry over array
  real(sp),dimension(:),intent(in)	:: tprev    ! carry over variable
  integer(I4B),intent(in)               :: sim_length   ! length of simulation

  !local variables
  integer(I4B)	:: i
  character(len = 480) :: state_outfile

  ! make state input filename
  state_outfile = trim(snow_state_out_root) // trim(curr_hru_id)

  open(unit=95,FILE=trim(state_outfile),FORM='formatted',status='replace')
  print*, 'Writing snow state file: ', trim(state_outfile)

  41 FORMAT(I0.4, 3(I0.2), 20(F20.12))  ! big enough to separate fields
  do i = 1,sim_length
    ! print*, 'tprev = ',tprev(i)  AWW debugging

    write(95,41) year(i),month(i),day(i),hour(i),tprev(i), cs(:,i)
  enddo

  close(unit=95)

  return
end subroutine write_snow17_state

! ccccccccccccccccccccccccccccccc

subroutine write_sac_state(year,month,day,hour,uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,sim_length,curr_hru_id)
  use nrtype
  use def_namelists, only: sac_state_out_root
  implicit none

  !input variables
  character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for state fname
  integer(I4B),dimension(:),intent(in)	:: year
  integer(I4B),dimension(:),intent(in)	:: month
  integer(I4B),dimension(:),intent(in)	:: day
  integer(I4B),dimension(:),intent(in)	:: hour
  real(dp),dimension(:),intent(in)	:: uztwc	!state variable
  real(dp),dimension(:),intent(in)	:: uzfwc	!state variable
  real(dp),dimension(:),intent(in)	:: lztwc        !state variable
  real(dp),dimension(:),intent(in)	:: lzfsc	!state variable
  real(dp),dimension(:),intent(in)	:: lzfpc	!state variable
  real(dp),dimension(:),intent(in)	:: adimc	!state variable
  integer(I4B),intent(in)               :: sim_length   ! length of simulation

  !local variables
  integer(I4B)	:: i
  character(len = 480) :: state_outfile

  ! make state input filename
  state_outfile = trim(sac_state_out_root) // trim(curr_hru_id)

  42 FORMAT(I0.4, 3(I0.2), 6(F20.12))  ! big enough to separate fields
  open(unit=95,FILE=trim(state_outfile),FORM='formatted',status='replace')
  print*, 'Writing sac state file: ', trim(state_outfile)

  do i = 1,sim_length
    write(95,42) year(i),month(i),day(i),hour(i),uztwc(i),uzfwc(i),&
                       lztwc(i),lzfsc(i),lzfpc(i),adimc(i)
  enddo

  close(unit=95)

  return
end subroutine write_sac_state

! cccccccccccccccccccccccccccccccccccccccccc

subroutine write_uh_state(year,month,day,hour,tci,sim_length,uh_length,curr_hru_id)
  ! A.Wood, 2016 -- this routine writes out TCI (total channel input) for the 
  !   UH_LENGTH period preceding the first timestep of the simulation
  !   When read in, it is concatenated with the simulation TCI to initialize 
  !   the routing model
  use nrtype
  use def_namelists, only: uh_state_out_root
  implicit none

  !input variables
  character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for state fname
  integer(I4B),dimension(:),intent(in)	:: year
  integer(I4B),dimension(:),intent(in)	:: month
  integer(I4B),dimension(:),intent(in)	:: day
  integer(I4B),dimension(:),intent(in)	:: hour
  real(sp), dimension(:), intent(in) 	:: tci
  integer(I4B), intent(in)		:: sim_length
  integer(I4B), intent(in)		:: uh_length

  !local variables
  integer(I4B)	:: i
  real(sp),allocatable,dimension(:)	:: out_tci  ! holds tci except for 1st uh_length records
  character(len = 480) :: state_outfile

  ! make state input filename
  state_outfile = trim(uh_state_out_root) // trim(curr_hru_id)

  ! pad uh_length-1 period prior to tci timeseries with zeros (since no other data)
  allocate(out_tci(uh_length-1+sim_length))
  out_tci(1:uh_length-1) = 0.0
  out_tci(uh_length:sim_length+uh_length-1) = tci

  44 FORMAT(I0.4, 3(I0.2), 1000(F20.12))  ! big enough to separate fields
  open(unit=95,FILE=trim(state_outfile),FORM='formatted',status='replace')
  print*, 'Writing UH state file: ', trim(state_outfile)
  print*, ' '

  ! each day, writes out uh_length-1 values preceding the current day & also the current day
  ! reasoning is that the other states are for the END of the period, so one would be initializing 
  ! a simulation or forecast starting the following period (or day)

  do i = 1,sim_length
    write(95,44) year(i),month(i),day(i),hour(i),out_tci(i:i+uh_length-1)
  enddo

  close(unit=95)
  deallocate(out_tci)

  return
end subroutine write_uh_state

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 

subroutine read_uh_state(state_date_str, prior_tci,uh_length,curr_hru_id)
  ! A.Wood, 2016 -- just read prior uh_length + current value for tci
  !   Could read whole output state timeseries if date given to select current init,
  !   but for now let's keep that as a pre-process

  use nrtype
  use def_namelists, only: uh_state_in_root
  implicit none

  !input variable
  character(len=10), intent(in) :: state_date_str  ! AWW string to match date in input states
  character(len=20), intent(in) :: curr_hru_id	! HRU extension for state fname
  integer(I4B), intent(in)	:: uh_length

  !output variables
  real(sp), dimension(:), intent(out) 	:: prior_tci

  !local variables
  integer(I4B)	       :: ios=0
  character(len = 480) :: state_infile
  character(len = 10)  :: file_state_date_str

  ! make state input filename
  state_infile = trim(uh_state_in_root) // trim(curr_hru_id)
  open(unit=95,FILE=trim(state_infile),FORM='formatted',status='old')
  print*, 'Reading UH state file: ', trim(state_infile)

  ! format for input is an unknown number of rows with uh_length+1 columns
  !   the first column is the datestring, with no other information
  do while(ios .ge. 0)

    ! read each row and check to see if the date matches the initial state date
    read (95,*,IOSTAT=ios) file_state_date_str, prior_tci(:)

    !if(file_state_date_str == state_date_str) then
    if(file_state_date_str==state_date_str .or. file_state_date_str=='PseudoDate') then
      print *, '  -- found initial UH state on ', state_date_str
      print*, ' '
      close(unit=95)
      return
    end if

  end do
  close(unit=95)

  ! if you reach here without returning, quit -- the initial state date was not found
  print*, 'ERROR:  UH init state not found in UH initial state file.  Looking for: ',state_date_str
  print*, '  -- last state read was: ', file_state_date_str
  print*, 'Stopping.  Check inputs!'
  stop

end subroutine read_uh_state

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 

subroutine read_snow17_state(state_date_str, cs,tprev,curr_hru_id)
  use nrtype
  use def_namelists, only: snow_state_in_root
  implicit none

  ! input variables
  character(len=10),intent(in)	:: state_date_str  ! AWW string to match date in input states
  character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for snow state fname

  ! output variables
  real(sp), intent(out) 		:: tprev	! carry over variable
  real(sp), dimension(:), intent(out)	:: cs		! carry over array

  !local variables
  integer(I4B)	       :: ios=0
  character(len = 480) :: state_infile
  character(len = 10)  :: file_state_date_str

  ! make state filename
  state_infile = trim(snow_state_in_root) // trim(curr_hru_id)
  open(unit=95,FILE=trim(state_infile),FORM='formatted',status='old')
  print*, 'Reading snow state file: ', trim(state_infile)

  ! format for input is an unknown number of rows with 20 data columns (1 tprev, 19 for cs)
  !   the first column is the datestring
  do while(ios .ge. 0)

    ! read each row and check to see if the date matches the initial state date
    read(95,*,IOSTAT=ios) file_state_date_str, tprev, cs(:)

    ! if(file_state_date_str == state_date_str) then
    if(file_state_date_str==state_date_str .or. file_state_date_str=='PseudoDate') then
      print *, '  -- found initial snow state on ', state_date_str
      close(unit=95)
      return
    end if

  end do
  close(unit=95)

  ! if you reach here without returning, quit -- the initial state date was not found
  print*, 'ERROR:  snow init state not found in snow initial state file.  Looking for: ',state_date_str
  print*, '  -- last state read was: ', file_state_date_str
  print*, 'Stopping.  Check inputs!'
  stop

end subroutine read_snow17_state

! cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

subroutine read_sac_state(state_date_str, uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,curr_hru_id)
  use nrtype
  use def_namelists, only: sac_state_in_root
  implicit none

  ! input variables
  character(len=10), intent(in)	:: state_date_str  ! AWW string to match date in input states
  character(len=20), intent(in) :: curr_hru_id	! HRU extension for sac state fname
  real(sp), intent(out)	:: uztwc			!state variable
  real(sp), intent(out)	:: uzfwc			!state array
  real(sp), intent(out)	:: lztwc			!state array
  real(sp), intent(out)	:: lzfsc			!state array
  real(sp), intent(out)	:: lzfpc			!state array
  real(sp), intent(out)	:: adimc

  ! local variables
  integer(I4B)	       :: ios=0
  character(len = 480) :: state_infile
  character(len = 10)  :: file_state_date_str

  ! make state filename
  state_infile = trim(sac_state_in_root) // trim(curr_hru_id)	
  open(unit=95,FILE=trim(state_infile),FORM='formatted',status='old')
  print*, 'Reading sac state file: ', trim(state_infile)

  ! format for input is an unknown number of rows with 6 data columns
  !   the first column is the datestring
  do while(ios .ge. 0)

    ! read each row and check to see if the date matches the initial state date
    read(95,*,IOSTAT=ios) file_state_date_str, uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc

    !if(file_state_date_str == state_date_str) then
    ! checks either for real date or special word identifying the state to use
    !   this functionality facilitates ESP forecast initialization
    if(file_state_date_str==state_date_str .or. file_state_date_str=='PseudoDate') then
      print *, '  -- found initial sac model state on ', state_date_str
      close(unit=95)
      return
    end if

  end do
  close(unit=95)

  ! if you reach here without returning, quit -- the initial state date was not found
  print*, 'ERROR:  sac init state not found in sac initial state file.  Looking for: ',state_date_str
  print*, '  -- last state read was: ', file_state_date_str
  print*, 'Stopping.  Check inputs!'
  stop

end subroutine read_sac_state

! ccccccccccccccccccccccccccccccc

!subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,vpd,dayl,swdown,precip)
! AWW modified to read PET instead of dayl, vpd and swdown
! AWW modified to return basin area in sq km
subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,precip,pet,curr_hru_id)
  use nrtype
  use def_namelists, only: forcing_root, start_year,start_day,start_month, &
                        end_year,end_month,end_day

  implicit none

  ! input variables
  character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for sac state fname

  ! output variables
  integer(I4B),dimension(:),intent(out)	:: year
  integer(I4B),dimension(:),intent(out)	:: month
  integer(I4B),dimension(:),intent(out)	:: day
  integer(I4B),dimension(:),intent(out)	:: hour
  real(dp),dimension(:),intent(out)	:: tmax   ! deg C
  real(dp),dimension(:),intent(out)	:: tmin    ! deg C
  real(dp),dimension(:),intent(out)	:: precip  ! mm/day
  real(dp),dimension(:),intent(out)	:: pet   ! mm/day


  ! local variables
  integer(I4B)				:: i,ios=0
  integer(I4B)				:: yr,mnth,dy,hr
  integer(I4B)				:: read_flag
  character(len = 1024)			:: dum_str
  real(DP)				:: pcp,tma,tmn,pm_pet

  character(len = 420) :: filename

  ! make filename to read
  filename = trim(forcing_root) // trim(curr_hru_id)	

  ! =========  code below  =============
  i = 1
  read_flag = 0

  ! read met file
  open (UNIT=50,file=trim(filename),form='formatted',status='old')

  ! skip header info
  read (UNIT=50,FMT='(80A)') dum_str   ! column labels

  ! read the data, keeping only forcings in simulation period
  do while(ios .ge. 0 .and. read_flag < 2)
    ! forcing could have any format, nice!
    read (UNIT=50,FMT=*,IOSTAT=ios) yr,mnth,dy,hr,pcp,tma,tmn,pm_pet

    if(yr .eq. start_year .and. mnth .eq. start_month .and. dy .eq. start_day) then
      read_flag = 1
    end if

    ! read and store data for simulation period
    if(read_flag .eq. 1) then
      year(i)	= yr
      month(i)	= mnth
      day(i)	= dy
      hour(i)	= hr
      precip(i)	= pcp
      tmax(i)	= tma
      tmin(i)	= tmn
      pet(i)	= pm_pet
      i = i + 1
    end if

    if(yr .eq. end_year .and. mnth .eq. end_month .and. dy .eq. end_day) then
      read_flag = 2
    end if

  end do

  close(unit=50)
  return
end subroutine read_areal_forcing


! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      subroutines for reading param files: sac, snow17, unit hydrograph & pet
! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

subroutine read_sac_params(param_file_name,n_hrus)
  use nrtype
  use def_namelists, only: uztwm,uzfwm,uzk,pctim,adimp,zperc,rexp, &
			lztwm,lzfsm,lzfpm,lzsk,lzpk,pfree,riva,side,rserv, &
                        hru_area, hru_id
  implicit none
 
  !input variables
  character(len=400),intent(in) :: param_file_name
  integer(I4B),intent(in) :: n_hrus
 
  !local variables
  character(len=400)             :: readline
  character(len=50)	         :: param
  integer(I4B)		         :: ios=0
  integer                        :: n_params_read  ! count number read
  integer :: pos
 
  ! open parameter file
  open(unit=50,file=trim(param_file_name),status='old')

  ! allocate parameter variables
  allocate(hru_id(n_hrus))
  allocate(hru_area(n_hrus))
  allocate(uztwm(n_hrus))
  allocate(uzfwm(n_hrus))
  allocate(uzk(n_hrus))
  allocate(pctim(n_hrus))
  allocate(adimp(n_hrus))
  allocate(zperc(n_hrus))
  allocate(rexp(n_hrus))
  allocate(lztwm(n_hrus))
  allocate(lzfsm(n_hrus))
  allocate(lzfpm(n_hrus))
  allocate(lzsk(n_hrus))  
  allocate(lzpk(n_hrus))  
  allocate(pfree(n_hrus))
  allocate(riva(n_hrus))
  allocate(side(n_hrus))
  allocate(rserv(n_hrus))

  print*, 'Reading Sac parameters'

  ! --- now loop through parameter file and assign parameters 
  n_params_read = 0
  do while(ios == 0)
    read(unit=50,FMT='(A)',IOSTAT=ios) readline

    if(ios == 0) then   ! means 'readline' was from the file
      !print*, '  ',trim(readline)

      ! Find the first instance of whitespace in line read. Split label vs data.
      pos = scan(readline, '    ')
      param = trim(readline(1:pos))
      readline = readline(pos+1:)  ! shorten readline to include only data

      ! assign line to correct parameter array & type
      ! (following http://jblevins.org/log/control-file)
      select case (param)
        case ('hru_id')
          read(readline, *, iostat=ios) hru_id
          n_params_read = n_params_read + 1
        case ('hru_area')
          read(readline, *, iostat=ios) hru_area
          n_params_read = n_params_read + 1
        case ('uztwm')
          read(readline, *, iostat=ios) uztwm
          n_params_read = n_params_read + 1
        case ('uzfwm')
          read(readline, *, iostat=ios) uzfwm
          n_params_read = n_params_read + 1
        case('uzk') 
          read(readline, *, iostat=ios) uzk
          n_params_read = n_params_read + 1
        case('pctim')
          read(readline, *, iostat=ios) pctim
          n_params_read = n_params_read + 1
        case('adimp')
          read(readline, *, iostat=ios) adimp
          n_params_read = n_params_read + 1
        case('zperc')
          read(readline, *, iostat=ios) zperc
          n_params_read = n_params_read + 1
        case('rexp')
          read(readline, *, iostat=ios) rexp
          n_params_read = n_params_read + 1
        case('lztwm')
          read(readline, *, iostat=ios) lztwm
          n_params_read = n_params_read + 1
        case('lzfsm')
          read(readline, *, iostat=ios) lzfsm
          n_params_read = n_params_read + 1
        case('lzfpm')
          read(readline, *, iostat=ios) lzfpm
          n_params_read = n_params_read + 1
        case('lzsk')
          read(readline, *, iostat=ios) lzsk
          n_params_read = n_params_read + 1
        case('lzpk')
          read(readline, *, iostat=ios) lzpk
          n_params_read = n_params_read + 1
        case('pfree')
          read(readline, *, iostat=ios) pfree
          n_params_read = n_params_read + 1
        case('riva')
          read(readline, *, iostat=ios) riva
          n_params_read = n_params_read + 1
        case('side')
          read(readline, *, iostat=ios) side
          n_params_read = n_params_read + 1
        case('rserv')
          read(readline, *, iostat=ios) rserv
          n_params_read = n_params_read + 1
        case default
          print *, 'Parameter ',param,' not recognized in soil file'
          ! something weird here...doesn't break it but somehow enters here after last real read
      end select

    end if

  end do
  close(unit=50)

  ! quick check on completeness
  if(n_params_read /= 18) then
    print *, 'Only ',n_params_read, ' SAC params read, but need 18.  Quitting...'
    stop
  end if
  !print*, '  -------------------'

  return
end subroutine read_sac_params

! ================================================
subroutine read_snow17_params(param_file_name,n_hrus)
  use nrtype
  use def_namelists, only: scf,mfmax,mfmin,uadj,si,pxtemp,nmf,&
                        tipm,mbase,plwhc,daygm,adc,latitude, elev
  implicit none
 
  !input variables
  character(len=1024),intent(in) :: param_file_name
  integer(I4B),intent(in) :: n_hrus

  !local variables
  character(len=400)            :: readline
  character(len=50)		:: param
  integer(I4B)			:: ios=0
  integer :: pos
  integer                        :: n_params_read  ! count number read

  ! open parameter file
  open(unit=51,file=trim(param_file_name),status='old')

  ! allocate parameter variables
  allocate(scf(n_hrus))
  allocate(mfmax(n_hrus))
  allocate(mfmin(n_hrus))
  allocate(uadj(n_hrus))
  allocate(si(n_hrus))
  allocate(pxtemp(n_hrus))
  allocate(nmf(n_hrus))
  allocate(tipm(n_hrus))
  allocate(mbase(n_hrus))
  allocate(plwhc(n_hrus))
  allocate(daygm(n_hrus))
  allocate(latitude(n_hrus))
  allocate(elev(n_hrus))

  print*, 'Reading Snow17 parameters'

  ! --- now loop through parameter file and assign parameters 
  n_params_read = 0
  do while(ios .eq. 0)
    read(unit=51,FMT='(A)',IOSTAT=ios) readline

    if(ios == 0) then   ! means 'readline' was from the file
      !print*, '  ',trim(readline)

      ! Find the first instance of whitespace in line read. Split label vs data.
      pos = scan(readline, '    ')
      param = trim(readline(1:pos))
      readline = readline(pos+1:)  ! shorten readline to include only data

      ! assign line to correct parameter array & type
      ! (following http://jblevins.org/log/control-file)
      select case (param)
        case ('hru_id')
          ! do nothing, already stored it
          n_params_read = n_params_read + 1
        case ('latitude')
          read(readline, *, iostat=ios) latitude
          n_params_read = n_params_read + 1
        case ('elev')
          read(readline, *, iostat=ios) elev
          n_params_read = n_params_read + 1
        case ('mfmax')
          read(readline, *, iostat=ios) mfmax
          n_params_read = n_params_read + 1
        case ('mfmin')
          read(readline, *, iostat=ios) mfmin
          n_params_read = n_params_read + 1
        case ('scf')
          read(readline, *, iostat=ios) scf
          n_params_read = n_params_read + 1
        case ('uadj')
          read(readline, *, iostat=ios) uadj
          n_params_read = n_params_read + 1
        case ('si')
          read(readline, *, iostat=ios) si
          n_params_read = n_params_read + 1
        case ('pxtemp')
          read(readline, *, iostat=ios) pxtemp
          n_params_read = n_params_read + 1
        case ('nmf')
          read(readline, *, iostat=ios) nmf
          n_params_read = n_params_read + 1
        case ('tipm')
          read(readline, *, iostat=ios) tipm
          n_params_read = n_params_read + 1
        case ('mbase')
          read(readline, *, iostat=ios) mbase
          n_params_read = n_params_read + 1
        case ('plwhc')
          read(readline, *, iostat=ios) plwhc
          n_params_read = n_params_read + 1
        case ('daygm')
          read(readline, *, iostat=ios) daygm
          n_params_read = n_params_read + 1
        case ('adc1')
          read(readline, *, iostat=ios) adc(1)
          n_params_read = n_params_read + 1
        case ('adc2')
          read(readline, *, iostat=ios) adc(2)
          n_params_read = n_params_read + 1
        case ('adc3')
          read(readline, *, iostat=ios) adc(3)
          n_params_read = n_params_read + 1
        case ('adc4')
          read(readline, *, iostat=ios) adc(4)
          n_params_read = n_params_read + 1
        case ('adc5')
          read(readline, *, iostat=ios) adc(5)
          n_params_read = n_params_read + 1
        case ('adc6')
          read(readline, *, iostat=ios) adc(6)
          n_params_read = n_params_read + 1
        case ('adc7')
          read(readline, *, iostat=ios) adc(7)
          n_params_read = n_params_read + 1
        case ('adc8')
          read(readline, *, iostat=ios) adc(8)
          n_params_read = n_params_read + 1
        case ('adc9')
          read(readline, *, iostat=ios) adc(9)
          n_params_read = n_params_read + 1
        case ('adc10')
          read(readline, *, iostat=ios) adc(10)
          n_params_read = n_params_read + 1
        case ('adc11')
          read(readline, *, iostat=ios) adc(11)
          n_params_read = n_params_read + 1
        case default
          print *, 'Parameter ',param,' not recognized in snow file'
      end select

    end if

  end do
  close(unit=51)

  ! quick check on completeness
  if(n_params_read /= 25) then
    print *, 'Only ',n_params_read, ' SNOW17 params read, but need 25.  Quitting...'
    stop
  end if
  !print*, '  -------------------'

  return
end subroutine read_snow17_params

! cccccccccccc
subroutine read_uh_params(param_file_name,n_hrus)
  use nrtype
  use def_namelists, only: unit_shape,unit_scale
  implicit none
 
  !input variables
  integer(I4B),intent(in) :: n_hrus
  character(len=1024),intent(in) :: param_file_name

  !local variables
  character(len=400)            :: readline
  character(len=50)		:: param
  integer(I4B)			:: ios=0
  integer :: pos
  integer                        :: n_params_read  ! count number read

  ! open parameter file
  open(unit=52,file=trim(param_file_name),status='old')

  ! allocate parameter variables
  allocate(unit_shape(n_hrus))
  allocate(unit_scale(n_hrus))

  print*, 'Reading UH parameters'

  ! --- now loop through parameter file and assign parameters 
  n_params_read = 0
  do while(ios .eq. 0)
    read(unit=52,FMT='(A)',IOSTAT=ios) readline

    if(ios == 0) then   ! means 'readline' was from the file
      !print*, '  ',trim(readline)

      ! Find the first instance of whitespace in line read. Split label vs data.
      pos = scan(readline, '    ')
      param = readline(1:pos)
      readline = readline(pos+1:)  ! shorten readline to include only data

      ! assign line to correct parameter array & type
      ! (following http://jblevins.org/log/control-file)
      select case (param)
        case ('hru_id')
          ! do nothing, already stored it
          n_params_read = n_params_read + 1
        case ('unit_shape')
          read(readline, *, iostat=ios) unit_shape
          n_params_read = n_params_read + 1
        case ('unit_scale')
          read(readline, *, iostat=ios) unit_scale
          n_params_read = n_params_read + 1
        case default
          print *, 'Parameter ',param,' not recognized in UH file'
      end select

    end if

  end do
  close(unit=52)

  ! quick check on completeness
  if(n_params_read /= 3) then
    print *, 'Only ',n_params_read, ' UH params read, but need 3.  Quitting...'
    stop
  end if

  return
end subroutine read_uh_params
