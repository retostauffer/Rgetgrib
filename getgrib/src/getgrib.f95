!
!  Description: how to use grib_find_nearest and grib_get_element
!
!
!
!
!
subroutine getgrib(GRBFILE,NMESSAGES,NSTATIONS,LONS,LATS,PARAM,RES) 

   use grib_api
   implicit none
   integer                                      :: infile
   integer                                      :: igrib, ios, i

   real(8), dimension(:), allocatable  :: nearest_lats, nearest_lons
   real(8), dimension(:), allocatable  :: distances, values, lsm_values
   integer(kind=kindOfInt), dimension(:), allocatable  :: indexes
   real(kind=8)                        :: value
 
   integer :: msg, nmessages

   ! I/O ARGUMENTS
   integer, intent(in) :: NSTATIONS ! Number of stations, required to allocate the vectors
   character(len=255), intent(in) :: GRBFILE

   real(8), intent(inout), dimension(NSTATIONS) :: LONS, LATS ! Vector of longitudes and latitudes
   real(8), intent(inout), dimension(NMESSAGES,NSTATIONS) :: RES  ! Return values
   integer, intent(inout), dimension(NMESSAGES,7) :: PARAM
   integer :: paramId, dataDate, dataTime, startStep, endStep

   ! Allocating variables needed inside this script 
   allocate(nearest_lats(NSTATIONS))
   allocate(nearest_lons(NSTATIONS))
   allocate(distances(NSTATIONS))
   allocate(lsm_values(NSTATIONS))
   allocate(values(NSTATIONS))
   allocate(indexes(NSTATIONS))

   ! Open grib file. If not readable or not found: exit with exit code 9
   call grib_open_file(infile, GRBFILE,'r',ios)
   if ( ios .ne. 0 ) then
      print *, 'Problems reading the input file. Not found or not readable'
      stop
   endif
 
   ! Counting number of messages inside the grib file
   call grib_count_in_file(infile,nmessages,ios)
   if ( ios .ne. 0 ) then
      print *, "Problems counting the messages in the grib file. Stop."
      stop
   endif
 
   ! Some output
   do msg=1,nmessages,1


      ! Extracting information
      call grib_new_from_file(infile,igrib)

      ! Getting meta information
      call grib_get_int(igrib,'indicatorOfParameter',   PARAM(msg,1))
      call grib_get_int(igrib,'indicatorOfTypeOfLevel', PARAM(msg,2))
      call grib_get_int(igrib,'level',                  PARAM(msg,3))
      call grib_get_int(igrib,'dataDate',               PARAM(msg,4))
      call grib_get_int(igrib,'dataTime',               PARAM(msg,5))
      call grib_get_int(igrib,'startStep',              PARAM(msg,6))
      call grib_get_int(igrib,'endStep',                PARAM(msg,7))

      !PARAM(msg,1) = paramId
      !PARAM(msg,2) = dataDate
      !PARAM(msg,3) = dataTime
      !PARAM(msg,4) = startStep
      !PARAM(msg,5) = endStep

      ! Getting the data itself
      call grib_find_nearest(igrib, .false., LATS, LONS, &
               nearest_lats, nearest_lons,lsm_values, distances, indexes, ios)
      call grib_release(igrib)

      ! Write results onto the INTENT(INOUT) objects
      do i=1,NSTATIONS
         RES(msg,i) = lsm_values(i)
         if ( msg .eq. 1 ) then
            LONS(i)    = nearest_lons(i)
            LATS(i)    = nearest_lats(i)
         endif
         !print*,LATS(i), LONS(i), nearest_lats(i), nearest_lons(i), distances(i), lsm_values(i), values(i)
      end do
   end do
   call grib_close_file(infile)
  
   deallocate(nearest_lats)
   deallocate(nearest_lons)
   deallocate(distances)
   deallocate(lsm_values)
   deallocate(values)
   deallocate(indexes)

end subroutine getgrib


subroutine messagecount(GRBFILE, nmessages)

   use grib_api
   implicit none
   integer :: infile, ios
   integer :: nmessages

   ! I/O ARGUMENTS
   character(len=255), intent(in) :: GRBFILE

   ! Open grib file. If not readable or not found: exit with exit code 9
   call grib_open_file(infile, GRBFILE,'r',ios)
   if ( ios .ne. 0 ) then
      print *, 'Problems reading the input file. Not found or not readable'
      stop
   endif
 
   ! Counting number of messages inside the grib file
   call grib_count_in_file(infile,nmessages,ios)
   if ( ios .ne. 0 ) then
      print *, "Problems counting the messages in the grib file. Stop."
      stop
   endif
 
end subroutine messagecount
