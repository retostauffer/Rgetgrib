! -------------------------------------------------------------------
! - NAME:        getdata.f95
! - AUTHOR:      Reto Stauffer
! - DATE:        2016-09-29
! -------------------------------------------------------------------
! - DESCRIPTION: Fortran subroutines and functions to load the data
!                from a grib file and return them to R.
! - REQUIRES:    requires grib_api library
! -------------------------------------------------------------------
! - EDITORIAL:   2016-09-29, RS: Created file on thinkreto.
! -------------------------------------------------------------------
! - L@ST MODIFIED: 2014-09-13 15:12 on thinkreto
! -------------------------------------------------------------------

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
   ! Using:
   ! (1) for Ni (number of rows)
   ! (2) for Nj (number of cols)
   ! (3) for step (number of different forecast lead times)
   ! (4) for perturbations (number of different members)
   integer, intent(inout), dimension(6) :: IINFO

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
           latitudeOfLastGridPointInDegrees,longitudeOfLastGridPointInDegrees,&
           dataDate,dataTime')

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

   ! Count number of different perturbations (ensemble)
   call grib_index_create(idx,GRBFILE,'perturbationNumber')
   IINFO(4) = GIGS(idx,'perturbationNumber')

   ! Count number of different perturbations (ensemble)
   call grib_index_create(idx,GRBFILE,'dataDate')
   IINFO(5) = GIGS(idx,'dataDate')

   ! Count number of different perturbations (ensemble)
   call grib_index_create(idx,GRBFILE,'dataTime')
   IINFO(6) = GIGS(idx,'dataTime')

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
! Selector on shortName at the moment.
! -------------------------------------------------------------------
subroutine getgriddataByShortName(GRBFILE,SHORTNAME,META,VALUES,NELEM,NROWS)

   use grib_api

   implicit none

   integer :: infile, igrib, ios, iret, i
   integer :: count, idx, currow 
   integer :: curdate, curtime, curstep, curpert
   integer :: ndates, ntimes, nsteps, nperturbations

   character(len=20) :: currshortName
   integer, dimension(:), allocatable   :: dates, times, steps, perturbations
   real(8), dimension(:), allocatable   :: lats, lons
   integer, dimension(:,:), allocatable :: spgrid ! steps/perturbations grid

   ! I/O variables
   ! SHORTNAME: string, short name to select.
   ! META:      integer matrix to store date, hour, step, and member
   ! VALUES:    real matrix to store data.
   ! NELEM:     number of grid points (Ni times Nj)
   ! NROWS:     number of 'rows' for VALUES/META. Number of steps * number of perturbations
   integer, intent(in)                            :: NELEM, NROWS
   real(8), intent(inout), dimension(NROWS,NELEM) :: VALUES
   integer, intent(inout), dimension(NROWS,4)     :: META
   character(len=255), intent(in) :: GRBFILE
   character(len=20), intent(in) :: SHORTNAME

   ! Function values
   integer :: arrayPositionInt
   integer :: matrixPositionInt4

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

   ! Getting all different perturbations in the file
   call grib_index_create(idx,GRBFILE,'perturbationNumber')
   call grib_index_get_size(idx,'perturbationNumber',nperturbations)
   allocate(perturbations(nperturbations))
   call grib_index_get(idx,'perturbationNumber',perturbations)

   ! Getting all different dataDates 
   call grib_index_create(idx,GRBFILE,'dataDate')
   call grib_index_get_size(idx,'dataDate',ndates)
   allocate(dates(ndates))
   call grib_index_get(idx,'dataDate',dates)

   ! Getting all different dataTimes 
   call grib_index_create(idx,GRBFILE,'dataTime')
   call grib_index_get_size(idx,'dataTime',ntimes)
   allocate(times(ntimes))
   call grib_index_get(idx,'dataTime',times)

   ! Create steps/perturbations grid
   allocate(spgrid(ndates*ntimes*nperturbations*nsteps,4))
   call expandGrid4(spgrid,dates,ndates,times,ntimes,steps,nsteps,perturbations,nperturbations)

   ! Getting first grib message
   call grib_index_create(idx,GRBFILE,'shortName')
   call grib_index_select(idx,'shortName',trim(shortName))

   ! Allocate two dummy vectors (required for loading data)
   allocate(lats(NELEM))
   allocate(lons(NELEM))

   ! Open/calling grib file
   call grib_new_from_file(infile,igrib)
   call grib_new_from_index(idx,igrib, iret)
   count = 0 ! Init value
   do while (iret /= GRIB_END_OF_INDEX)
      count=count+1
      call grib_get_string(igrib,'shortName',currshortName)
      call grib_get_int(igrib,'step',curstep)
      call grib_get_int(igrib,'perturbationNumber',curpert)
      call grib_get_int(igrib,'dataDate',curdate)
      call grib_get_int(igrib,'dataTime',curtime)
      !write(*,'(A,A,A,i3,A,i3)') 'shortName=',trim(currshortName),&
      !        '   step='  ,curstep, '   member=', curpert
   
      ! Looking for position of step of the current message in 'steps'
      ! which corresponds to the row of the output data matrix.
      currow = matrixPositionInt4(curdate,curtime,curstep,curpert,spgrid,size(spgrid,1),size(spgrid,2))
      if ( currow .lt. 1 ) then
         print *, "[!] Could not find step position. Stop."; stop 8
      end if

      ! Store meta data
      META(currow,:) = (/curdate,curtime,curstep,curpert/)

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

end subroutine getgriddataByShortName


! -------------------------------------------------------------------
! Returns latitude and longitude vector for one grib message
! specified by the MESSAGENUMBER. 
! -------------------------------------------------------------------
subroutine getgriddataByMessageNumber(GRBFILE,MESSAGENUMBER,SHORTNAME,META,VALUES,LATS,LONS,NELEM)

   use grib_api

   implicit none

   integer :: infile, igrib, ios, iret, i
   integer :: count, idx
   integer :: curdate, curtime, curstep, curpert

   ! I/O variables
   ! SHORTNAME: string, short name to select.
   ! META:      integer matrix to store date, hour, step, and member
   ! VALUES:    real matrix to store data.
   ! NELEM:     number of grid points (Ni times Nj)
   ! NROWS:     number of 'rows' for VALUES/META. Number of steps * number of perturbations
   integer, intent(in)                       :: NELEM, MESSAGENUMBER
   real(8), intent(inout), dimension(NELEM)  :: VALUES, LATS, LONS
   integer, intent(inout), dimension(4)      :: META
   character(len=255), intent(in) :: GRBFILE
   character(len=20), intent(inout) :: SHORTNAME

   ! Open grib file. If not readable or not found: exit with exit code 9
   call grib_open_file(infile, GRBFILE,'r',ios)
   if ( ios .ne. 0 ) then
      print *, 'Problems reading the input file. Not found or not readable'
      stop
   endif

   ! Open/calling grib file
   count = 0 ! Init value

   do while (iret /= GRIB_END_OF_INDEX)
      count=count+1
      call grib_new_from_file(infile,igrib)
      if ( count .ne. MESSAGENUMBER ) cycle

      ! Getting required meta information
      call grib_get_string(igrib,'shortName',SHORTNAME)
      call grib_get_int(igrib,'step',curstep)
      call grib_get_int(igrib,'perturbationNumber',curpert)
      call grib_get_int(igrib,'dataDate',curdate)
      call grib_get_int(igrib,'dataTime',curtime)

      ! Store meta data
      META = (/curdate,curtime,curstep,curpert/)

      ! Reading data
      call grib_get_data_real8(igrib, LATS, LONS, VALUES, ios)

      ! Release and take next message
      call grib_release(igrib)
      call grib_new_from_index(idx,igrib, iret)

      exit
   end do

   !!call grib_get_data_real8(igrib, LATS, LONS, values, ios)

   ! Release and close.
   call grib_release(igrib)
   call grib_close_file(infile)

end subroutine getgriddataByMessageNumber


! -------------------------------------------------------------------
! Returns the position of the integer value 'neelde' in the integer
! array 'haystack' - or a negative value.
! -------------------------------------------------------------------
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


! -------------------------------------------------------------------
! Two-dimensional version of arrayPositionInt
! Similar to arrayPositionInt, but returns the position of the row
! where the first row of 'grid' corresponds to 'A', and the second
! row of 'grid' corresponds to 'B'. Or a negative value if the 
! combination cannot be found.
! -------------------------------------------------------------------
integer function matrixPositionInt(A,B,grid,gridi,gridj)
   implicit none
   integer :: A, B, gridi, gridj
   integer :: i, j, row
   integer, dimension(gridi,gridj) :: grid
   matrixPositionInt = -9
   do i=1,gridI
   do j=1,gridJ
      row = (i-1)*j+j
      if ( grid(row,1) .eq. A .and. grid(row,2) .eq. B ) then
         matrixPositionInt = row
         return
      end if
   enddo
   enddo
end function


! -------------------------------------------------------------------
! Expanding a grid from two integer vectors. The grid contains
! each dimA/dimB combination available.
! -------------------------------------------------------------------
subroutine expandGrid(grid,dimA,nA,dimB,nB)
   implicit none

   integer :: i, j
   
   ! I/O variables
   integer, intent(in)                        :: nA, nB
   integer, intent(inout), dimension(nA)      :: dimA
   integer, intent(inout), dimension(nB)      :: dimB
   integer, intent(inout), dimension(nA*nB,2) :: grid

   do i = 1, nA
   do j = 1, nB
      grid( (i-1)*j+j, 1)    = dimA(i)
      grid( (i-1)*j+j, 2)    = dimB(j)
   end do
   end do
end subroutine expandGrid


! -------------------------------------------------------------------
! Four-dimensional version of matrixPositionInt
! Similar to arrayPositionInt, but returns the position of the row
! where the first row of 'grid' corresponds to 'A', and the second
! row of 'grid' corresponds to 'B', the third to 'C', and the last
! one for 'D'. Or a negative value if the 
! combination cannot be found.
! -------------------------------------------------------------------
integer function matrixPositionInt4(A,B,C,D,grid,gridi,gridj)
   implicit none
   integer :: A, B, C, D, gridi, gridj
   integer :: i, j, row
   integer, dimension(gridi,gridj) :: grid
   matrixPositionInt4 = -9
   do i=1,gridI
   do j=1,gridJ
      row = (i-1)*j+j
      if ( grid(row,1) .eq. A .and. &
           grid(row,2) .eq. B .and. &
           grid(row,3) .eq. C .and. &
           grid(row,4) .eq. D ) then
         matrixPositionInt4 = row
         return
      end if
   enddo
   enddo
end function


! -------------------------------------------------------------------
! Expanding a grid from two integer vectors. The grid contains
! each dimA/dimB/dimC/dimD combination available.
! -------------------------------------------------------------------
subroutine expandGrid4(grid,dimA,nA,dimB,nB,dimC,nC,dimD,nD)
   implicit none

   integer :: a, b, c, d, row
   
   ! I/O variables
   integer, intent(in)                   :: nA, nB, nC, nD
   integer, intent(inout), dimension(nA) :: dimA
   integer, intent(inout), dimension(nB) :: dimB
   integer, intent(inout), dimension(nC) :: dimC
   integer, intent(inout), dimension(nD) :: dimD
   integer, intent(inout), dimension(nA*nB*nC*nD,4) :: grid

   do a = 1, nA
   do b = 1, nB
   do c = 1, nC
   do d = 1, nD
      row = (a-1)*nB*nC*nD + (b-1)*nC*nD + (c-1)*nD + d
      grid( row, 1)    = dimA(a)
      grid( row, 2)    = dimB(b)
      grid( row, 3)    = dimC(c)
      grid( row, 4)    = dimD(d)
   end do
   end do
   end do
   end do
end subroutine expandGrid4
