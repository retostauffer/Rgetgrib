!
!  Description: how to use grib_find_nearest and grib_get_element
!
!
!
!
!
program find

   use grib_api
   implicit none
   integer                                      :: npoints
   integer                                      :: infile
   integer                                      :: igrib, ios, i
   real(8), dimension(:), allocatable  :: lats, lons
   real(8), dimension(:), allocatable  :: nearest_lats, nearest_lons
   real(8), dimension(:), allocatable  :: distances, values, lsm_values
   integer(kind=kindOfInt), dimension(:), allocatable  :: indexes
   real(kind=8)                        :: value
 
   integer :: msg, nmessages
  
   ! initialization
   open( unit=1, file="list_points",form="formatted",action="read")
   read(unit=1,fmt=*) npoints
   allocate(lats(npoints))
   allocate(lons(npoints))
   allocate(nearest_lats(npoints))
   allocate(nearest_lons(npoints))
   allocate(distances(npoints))
   allocate(lsm_values(npoints))
   allocate(values(npoints))
   allocate(indexes(npoints))
   do i=1,npoints
      read(unit=1,fmt=*, iostat=ios) lats(i), lons(i)
      if (ios /= 0) then
         npoints = i - 1
         exit
      end if
   end do
   close(unit=1)
   call grib_open_file(infile, &
        'all.grib1','r')
        !/home/retos/Dropbox/Transfer-RetoJakob/COSMO2_surfaceheight.grb1','r')
        !'/home/retos/Dropbox/GrADS_SHARED/2011-08-11_ECMWF_topo_0.125deg.grib','r')
    
   !     a new grib message is loaded from file
   !     igrib is the grib id to be used in subsequent calls
 
   call grib_count_in_file(infile,nmessages,ios)
   if ( ios .ne. 0 ) then
      print *, "Problems counting the messages in the grib file. Stop."
      stop(9)
   endif
 
   write(*,"(Ax1I5x1A)") '# Totally found', nmessages, 'messages in grib file'
 
   nmessages = 1
   do msg=1,nmessages,1
      call grib_new_from_file(infile,igrib)
  
      call grib_find_nearest(igrib, .false., lats, lons, nearest_lats, nearest_lons,lsm_values, distances, indexes)
      call grib_release(igrib)
      do i=1, npoints
         print*, lats(i), lons(i), values(i)
         !print*,lats(i), lons(i), nearest_lats(i), nearest_lons(i), distances(i), lsm_values(i), values(i)
      end do
   end do
    
   call grib_close_file(infile)
  
   stop

  
   deallocate(lats)
   deallocate(lons)
   deallocate(nearest_lats)
   deallocate(nearest_lons)
   deallocate(distances)
   deallocate(lsm_values)
   deallocate(values)
   deallocate(indexes)
 
end program find
