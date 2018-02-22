! AWW:  this code file just contains interfaces for functions & subs 
! AWW:  used to be called 'gauge_calib'

module interfaces

  interface 

    subroutine julian_day(year,month,day,jday)
      use nrtype
      !input variables
      integer(I4B),dimension(:),intent(in) :: year
      integer(I4B),dimension(:),intent(in) :: month
      integer(I4B),dimension(:),intent(in) :: day
      !output variables
      integer(I4B),dimension(:),intent(out) :: jday
    end subroutine julian_day

    subroutine julianday_scalar(iyear,imonth,iday,jday_scalar)
      use nrtype
      integer(I4B),intent(in) :: iyear
      integer(I4B),intent(in) :: imonth
      integer(I4B),intent(in) :: iday
      integer(I4B),intent(out) :: jday_scalar
    end subroutine julianday_scalar

    ! AWW (to replace get_simlength subroutine)
    subroutine date_diff_ndays ( yr1, mo1, dy1, yr2, mo2, dy2, nday_diff )
      use nrtype
      ! input variables
      integer(I4B),intent(in)  :: yr1, mo1, dy1, yr2, mo2, dy2  
      ! output variables
      integer(I4B),intent(out) :: nday_diff
    end subroutine date_diff_ndays

    subroutine day_before_date(year, month, day, newyear, newmonth, newday)
      use nrtype
      ! input variables
      integer(I4B),intent(in)  :: year       ! starting date
      integer(I4B),intent(in)  :: month
      integer(I4B),intent(in)  :: day
      ! output variables
      integer(I4B),intent(out)  :: newyear   ! ending date
      integer(I4B),intent(out)  :: newmonth
      integer(I4B),intent(out)  :: newday
    end subroutine day_before_date   

    subroutine read_namelist(namelist_name)
      use nrtype
      !input variable
      character(len=2000),intent(in)	:: namelist_name
    end subroutine read_namelist

    subroutine sfc_pressure(elevation, pres)
      use nrtype
      use constants, only: sfc_pres_a,sfc_pres_b,sfc_pres_c,&
                       sfc_pres_d,sfc_pres_e
      real(dp), intent(in)		:: elevation
      real(dp), intent(out)		:: pres
    end subroutine sfc_pressure

    subroutine write_snow17_state(year,month,day,hour,cs,tprev,sim_length,curr_hru_id)
      use nrtype
      use def_namelists, only: snow_state_out_root
      !input variables
      character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for state fname
      integer(I4B),dimension(:),intent(in)	:: year
      integer(I4B),dimension(:),intent(in)	:: month
      integer(I4B),dimension(:),intent(in)	:: day
      integer(I4B),dimension(:),intent(in)	:: hour
      real(sp),dimension(:,:),intent(in)  :: cs	    ! carry over array
      real(sp),dimension(:),intent(in)	  :: tprev    ! carry over variable
      integer(I4B),intent(in)             :: sim_length   ! length of simulation
    end subroutine write_snow17_state

    subroutine write_sac_state(year,month,day,hour,uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,sim_length,curr_hru_id)
      use nrtype
      use def_namelists, only: sac_state_out_root
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
      integer(I4B),intent(in)           :: sim_length   ! length of simulation
      !local variables
      integer(I4B)	:: i
    end subroutine write_sac_state

    subroutine write_uh_state(year,month,day,hour,tci,sim_length,uh_length,curr_hru_id)
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
    end subroutine write_uh_state

    subroutine read_snow17_state(state_date_str, cs,tprev,curr_hru_id)
      use nrtype
      use def_namelists, only: snow_state_in_root
      ! input variables
      character(len = 10), intent(in) :: state_date_str  ! AWW string to match date in input states
      character(len = 20), intent(in) :: curr_hru_id	! HRU extension for snow state filename
      ! output variables
      real(sp), intent(out) 			:: tprev	! carry over variable
      real(sp), dimension(:), intent(out)	:: cs		! carry over snow var array
    end subroutine read_snow17_state

    subroutine read_sac_state(state_date_str, uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,curr_hru_id)
      use nrtype
      use def_namelists, only: sac_state_in_root
      !input variables
      character(len = 10), intent(in) :: state_date_str  ! AWW string to match date in input states
      character(len = 20), intent(in) :: curr_hru_id	! HRU extension for state fname
      real(sp), intent(out)	:: uztwc		!state variable
      real(sp), intent(out)	:: uzfwc		!state array
      real(sp), intent(out)	:: lztwc		!state array
      real(sp), intent(out)	:: lzfsc		!state array
      real(sp), intent(out)	:: lzfpc		!state array
      real(sp), intent(out)	:: adimc		!state array
    end subroutine read_sac_state

    subroutine read_uh_state(state_date_str, prior_tci,uh_length,curr_hru_id)
      use nrtype
      use def_namelists, only: uh_state_in_root
      implicit none
      !input variables
      character(len = 10), intent(in) :: state_date_str  ! AWW string to match date in input states
      character(len = 20), intent(in) :: curr_hru_id	! HRU extension for state fname
      integer(I4B), intent(in)	:: uh_length
      !output variables
      real(sp), dimension(:), intent(out) 	:: prior_tci
      !local variables
      integer(I4B)	:: i
    end subroutine read_uh_state

    ! subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,vpd,dayl,swdown,precip)
    ! AWW mod to just read PET
    subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,precip,pet,curr_hru_id)
      use nrtype
      use def_namelists, only: forcing_root, start_year,start_day,start_month, &
                        end_year,end_month,end_day
      !output variables
      character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for state fname
      integer(I4B),dimension(:),intent(out)	:: year
      integer(I4B),dimension(:),intent(out)	:: month
      integer(I4B),dimension(:),intent(out)	:: day
      integer(I4B),dimension(:),intent(out)	:: hour
      real(dp),dimension(:),intent(out)	:: tmin
      real(dp),dimension(:),intent(out)	:: tmax
      real(dp),dimension(:),intent(out)	:: pet
      real(dp),dimension(:),intent(out)	:: precip    
    end subroutine read_areal_forcing

    subroutine read_sac_params(param_name,n_hrus)
      use nrtype
      use def_namelists, only: uztwm,uzfwm,uzk,pctim,adimp,zperc,rexp, &
			    lztwm,lzfsm,lzfpm,lzsk,lzpk,pfree,riva,side,rserv
      !input variables
      character(len=1024),intent(in)	:: param_name
      integer(I4B),intent(in) :: n_hrus
    end subroutine read_sac_params

    subroutine read_snow17_params(param_name,n_hrus)
      use nrtype
      use def_namelists, only: scf,mfmax,mfmin,uadj,si,pxtemp,nmf,&
                        tipm,mbase,plwhc,daygm,adc
      !input variables
      character(len=1024),intent(in)	:: param_name
      integer(I4B),intent(in) :: n_hrus
    end subroutine read_snow17_params

    subroutine read_uh_params(param_name,n_hrus)
      use nrtype
      use def_namelists, only: unit_shape,unit_scale
      !input variables
      character(len=1024),intent(in)	:: param_name
      integer(I4B),intent(in) :: n_hrus
    end subroutine read_uh_params

  end interface
end module interfaces

