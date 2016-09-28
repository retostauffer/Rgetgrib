!
!
!subroutine readgrib(GRBFILE,NMESSAGES,NSTATIONS,LONS,LATS,PARAM,RES) 
!
!   use grib_api
!   implicit none
!   integer                                      :: infile
!   integer                                      :: igrib, ios, i
!
!   real(8), dimension(:), allocatable  :: nearest_lats, nearest_lons
!   real(8), dimension(:), allocatable  :: distances, values, lsm_values
!   integer(kind=kindOfInt), dimension(:), allocatable  :: indexes
!   real(kind=8)                        :: value
! 
!   integer :: msg, nmessages
!
!   ! I/O ARGUMENTS
!   integer, intent(in) :: NSTATIONS ! Number of stations, required to allocate the vectors
!   character(len=255), intent(in) :: GRBFILE
!
!   real(8), intent(inout), dimension(NSTATIONS) :: LONS, LATS ! Vector of longitudes and latitudes
!   real(8), intent(inout), dimension(NMESSAGES,NSTATIONS) :: RES  ! Return values
!   integer, intent(inout), dimension(NMESSAGES,7) :: PARAM
!   integer :: paramId, dataDate, dataTime, startStep, endStep
!
!   ! Allocating variables needed inside this script 
!   allocate(nearest_lats(NSTATIONS))
!   allocate(nearest_lons(NSTATIONS))
!   allocate(distances(NSTATIONS))
!   allocate(lsm_values(NSTATIONS))
!   allocate(values(NSTATIONS))
!   allocate(indexes(NSTATIONS))
!
!   ! Open grib file. If not readable or not found: exit with exit code 9
!   call grib_open_file(infile, GRBFILE,'r',ios)
!   if ( ios .ne. 0 ) then
!      print *, 'Problems reading the input file. Not found or not readable'
!      stop
!   endif
! 
!   ! Counting number of messages inside the grib file
!   call grib_count_in_file(infile,nmessages,ios)
!   if ( ios .ne. 0 ) then
!      print *, "Problems counting the messages in the grib file. Stop."
!      stop
!   endif
! 
!   ! Some output
!   do msg=1,nmessages,1
!
!
!      ! Extracting information
!      call grib_new_from_file(infile,igrib)
!
!      ! Getting meta information
!      call grib_get_int(igrib,'indicatorOfParameter',   PARAM(msg,1))
!      call grib_get_int(igrib,'indicatorOfTypeOfLevel', PARAM(msg,2))
!      call grib_get_int(igrib,'level',                  PARAM(msg,3))
!      call grib_get_int(igrib,'dataDate',               PARAM(msg,4))
!      call grib_get_int(igrib,'dataTime',               PARAM(msg,5))
!      call grib_get_int(igrib,'startStep',              PARAM(msg,6))
!      call grib_get_int(igrib,'endStep',                PARAM(msg,7))
!
!      !PARAM(msg,1) = paramId
!      !PARAM(msg,2) = dataDate
!      !PARAM(msg,3) = dataTime
!      !PARAM(msg,4) = startStep
!      !PARAM(msg,5) = endStep
!
!      ! Getting the data itself
!      call grib_find_nearest(igrib, .false., LATS, LONS, &
!               nearest_lats, nearest_lons,lsm_values, distances, indexes, ios)
!      call grib_release(igrib)
!
!      ! Write results onto the INTENT(INOUT) objects
!      do i=1,NSTATIONS
!         RES(msg,i) = lsm_values(i)
!         if ( msg .eq. 1 ) then
!            LONS(i)    = nearest_lons(i)
!            LATS(i)    = nearest_lats(i)
!         endif
!         !print*,LATS(i), LONS(i), nearest_lats(i), nearest_lons(i), distances(i), lsm_values(i), values(i)
!      end do
!   end do
!   call grib_close_file(infile)
!  
!   deallocate(nearest_lats)
!   deallocate(nearest_lons)
!   deallocate(distances)
!   deallocate(lsm_values)
!   deallocate(values)
!   deallocate(indexes)
!
!end subroutine readgrib


! -------------------------------------------------------------------
! Returns distinct number of elements given 'hash' and grib file
! index 'idx'. GIGS stands for grib_index_get_size.
! -------------------------------------------------------------------
integer function GIGS(idx,hash)
   use grib_api
   implicit none
   integer :: idx
   character(len=*) :: hash
   call grib_index_get_size(idx,trim(hash),GIGS)
end function

! -------------------------------------------------------------------
! Is calling GIGS (see above), but stops if number of distinct
! different values is not equal to 1. Else return 0.
! -------------------------------------------------------------------
subroutine GIGSandExit(idx,hash)
   implicit none
   integer, intent(in) :: idx
   character(len=*), intent(in) :: hash
   integer :: GIGS
   if ( GIGS(idx,trim(hash)) .ne. 1 ) then
      write(*,'(a a a)') "[!] ERROR: different",trim(hash),"found in metadata. Stop."
      stop 9
   endif
end subroutine
      


! -------------------------------------------------------------------
! Getting information about the grid in the grib file.
! -------------------------------------------------------------------
subroutine getgridinfo(GRBFILE, IINFO)

   use grib_api
   implicit none
   integer :: idx, ios, infile
   integer, dimension(1) :: itmp

   ! I/O ARGUMENTS
   character(len=255), intent(in) :: GRBFILE
   integer, intent(inout), dimension(3) :: IINFO

   ! Function
   integer :: GIGS

   ! Open grib file. If not readable or not found: exit with exit code 9
   call grib_open_file(infile, GRBFILE,'r',ios)
   if ( ios .ne. 0 ) then
      print *, 'Problems reading the input file. Not found or not readable'
      stop 9
   endif
   call grib_close_file(infile)

   ! Create index
   call grib_index_create(idx,GRBFILE,'Ni,Nj,latitudeOfFirstGridPointInDegrees,&
           longitudeOfFirstGridPointInDegrees,&
           latitudeOfLastGridPointInDegrees,longitudeOfLastGridPointInDegrees')

   ! whenever an element exists more than once (that would mean
   ! that there are different grid definitions in the file) the
   ! system stops and returns an error.
   ! grib_index_get_size() returns number of distinct values
   ! grib_get_index_xxx() returns the values itself (we only expect
   ! one!)

   ! Check if we do have the same grid definition
   call GIGSandExit(idx,'latitudeOfFirstGridPointInDegrees')
   call GIGSandExit(idx,'latitudeOfLastGridPointInDegrees')
   call GIGSandExit(idx,'longitudeOfFirstGridPointInDegrees')
   call GIGSandExit(idx,'longitudeOfLastGridPointInDegrees')

   ! center information
   if ( GIGS(idx,'Ni') .ne. 1 ) then
      write(*,'(a a a)') "[!] ERROR: different","Ni","found in metadata. Stop."
      stop 9
   endif
   call grib_index_get_int(idx,'Ni',itmp); IINFO(1) = itmp(1)
   ! grid width information
   if ( GIGS(idx,'Nj') .ne. 1 ) then
      write(*,'(a a a)') "[!] ERROR: different","Nj","found in metadata. Stop."
      stop 9
   endif
   call grib_index_get_int(idx,'Nj',itmp); IINFO(2) = itmp(1)

   ! Count number of different forecast steps in the grib file 
   call grib_index_create(idx,GRBFILE,'step')
   IINFO(3) = GIGS(idx,'step')

end subroutine getgridinfo

! -------------------------------------------------------------------
! Returns latitude and longitude vector for all grids
! -------------------------------------------------------------------
subroutine getgridll(GRBFILE,NELEM,LATS,LONS)
   
   use grib_api

   implicit none

   integer :: infile, igrib, ios

   ! I/O variables
   integer, intent(in) :: NELEM
   real(8), intent(inout), dimension(NELEM) :: LONS, LATS
   real(8), dimension(NELEM) :: values

   character(len=255), intent(in) :: GRBFILE
   

   ! Open grib file. If not readable or not found: exit with exit code 9
   call grib_open_file(infile, GRBFILE,'r',ios)
   if ( ios .ne. 0 ) then
      print *, 'Problems reading the input file. Not found or not readable'
      stop
   endif

   ! Getting first grib message
   call grib_new_from_file(infile,igrib)

   ! Getting longitude and latitude from this field
   call grib_get_data_real8(igrib, LATS, LONS, values, ios)

   ! Release and close.
   call grib_release(igrib)
   call grib_close_file(infile)

end subroutine getgridll

! -------------------------------------------------------------------
! Returns latitude and longitude vector for all grids
! -------------------------------------------------------------------
subroutine getgriddata(GRBFILE,VALUES,NELEM,NSTEP)

   use grib_api

   implicit none

   integer :: infile, igrib, ios, iret
   integer :: count, idx, currow 
   integer :: currstep
   integer :: nsteps

   character(len=20) :: currshortName
   integer, dimension(:), allocatable :: steps
   real(8), dimension(:), allocatable :: lats, lons

   ! I/O variables
   integer, intent(in) :: NELEM, NSTEP
   real(8), intent(inout), dimension(NSTEP,NELEM) :: VALUES

   character(len=255), intent(in) :: GRBFILE

   ! Function values
   integer :: arrayPositionInt

   ! Open grib file. If not readable or not found: exit with exit code 9
   call grib_open_file(infile, GRBFILE,'r',ios)
   if ( ios .ne. 0 ) then
      print *, 'Problems reading the input file. Not found or not readable'
      stop
   endif

   ! Getting all different steps in the file
   call grib_index_create(idx,GRBFILE,'step')
   call grib_index_get_size(idx,'step',nsteps)
   allocate(steps(nsteps))
   call grib_index_get(idx,'step',steps)

   ! Getting first grib message
   call grib_index_create(idx,GRBFILE,'shortName')
   call grib_index_select(idx,'shortName','2t')

   ! Allocate two dummy vectors (required for loading data)
   allocate(lats(NELEM))
   allocate(lons(NELEM))

   ! Open/calling grib file
   call grib_new_from_file(infile,igrib)
   call grib_new_from_index(idx,igrib, iret)
   do while (iret /= GRIB_END_OF_INDEX)
      count=count+1
      call grib_get(igrib,'shortName',currshortName)
      call grib_get(igrib,'step',currstep)
      write(*,'(A,A,A,i3,A,i4,A,i3)') 'shortName=',trim(currshortName),&
              '   step='  ,currstep
   
      ! Looking for position of step of the current message in 'steps'
      ! which corresponds to the row of the output data matrix.
      currow = arrayPositionInt(currstep,steps,size(steps))
      if ( currow .lt. 1 ) then
         print *, "[!] Could not find step position. Stop."; stop 8
      end if

      ! Reading data
      call grib_get_data_real8(igrib, lats, lons, VALUES(currow,:), ios)

      ! Release and take next message
      call grib_release(igrib)
      call grib_new_from_index(idx,igrib, iret)
   end do

   !!call grib_get_data_real8(igrib, LATS, LONS, values, ios)

   ! Release and close.
   call grib_release(igrib)
   call grib_close_file(infile)

end subroutine getgriddata

integer function arrayPositionInt(needle,haystack,n)
   implicit none
   integer :: needle, i, n
   integer, dimension(n) :: haystack
   arrayPositionInt = -9
   do i=1,size(haystack,1)
      if ( haystack(i) .eq. needle ) then
         arrayPositionInt = i
         return
      end if
   enddo
end function














