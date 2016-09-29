

subroutine speed(GRBFILE,SHORTNAME,NSTEPS,NPERTURBATIONS,NI,NJ)

   use grib_api

   ! I/O variables
   character(len=255), intent(in) :: GRBFILE
   character(len=20),  intent(in) :: SHORTNAME
   integer, intent(inout)         :: NSTEPS, NPERTURBATIONS, NI, NJ

   integer :: idx, i, nmessages
   character(len=1000) :: idxString

   character(len=40), dimension(:), allocatable :: idxKeywords
   integer, dimension(1) :: itmp
   integer :: GIGS

   allocate(idxKeywords(8))
   idxKeywords(1:8)  = (/'longitudeOfFirstGridPointInDegrees      ', &
                         'latitudeOfLastGridPointInDegrees        ', &
                         'longitudeOfLastGridPointInDegrees       ', &
                         'perturbationNumber                      ', &
                         'shortName                               ', &
                         'Ni                                      ', &
                         'Nj                                      ', &
                         'step                                    '/)

   ! Create index string
   idxString = idxKeywords(1)
   do i = 2, size(idxKeywords,1)
      write(idxString,'(aaa)') trim(idxString), ",", trim(idxKeywords(i))
   enddo

   ! Open grib file. If not readable or not found: exit with exit code 9
   call grib_open_file(infile, GRBFILE,'r',ios)
   if ( ios .ne. 0 ) then
      print *, 'Problems reading the input file. Not found or not readable'
      stop
   endif

   ! Create index from grib file
   call grib_index_create(idx,GRBFILE,trim(idxString))
   ! Filtering messages regarding input SHORTNAME
   call grib_index_select(idx,'shortName',trim(SHORTNAME))

   ! Checking grid
   call GIGSandExit(idx,'longitudeOfFirstGridPointInDegrees')
   call GIGSandExit(idx,'latitudeOfLastGridPointInDegrees')
   call GIGSandExit(idx,'longitudeOfLastGridPointInDegrees')
   call GIGSandExit(idx,'shortName')
   call GIGSandExit(idx,'Ni')
   call GIGSandExit(idx,'Nj')

   ! Getting some parameters, take them from first grib message
   ! as we have already checked that all grids are defined the
   ! exact same way.
   NSTEPS         = GIGS(idx,'step')
   NPERTURBATIONS = GIGS(idx,'perturbationNumber')
   call grib_index_get_int(idx,'Ni',itmp);   NI = itmp(1)
   call grib_index_get_int(idx,'Nj',itmp);   NJ = itmp(1)

   !write(*,"(3x,a20,i5)") 'NPERTURBATIONS', NPERTURBATIONS
   !write(*,"(3x,a20,i5)") 'NSTEPS', NSTEPS
   !write(*,"(3x,a20,i5)") 'NI', NI
   !write(*,"(3x,a20,i5)") 'NJ', NJ
   call grib_close_file(infile)

end subroutine speed

!! -------------------------------------------------------------------
!! Returns distinct number of elements given 'hash' and grib file
!! index 'idx'. GIGS stands for grib_index_get_size.
!! -------------------------------------------------------------------
!integer function GIGS(idx,hash)
!   use grib_api
!   implicit none
!   integer :: idx
!   character(len=*) :: hash
!   call grib_index_get_size(idx,trim(hash),GIGS)
!end function
!
!! -------------------------------------------------------------------
!! Is calling GIGS (see above), but stops if number of distinct
!! different values is not equal to 1. Else return 0.
!! -------------------------------------------------------------------
!subroutine GIGSandExit(idx,hash)
!   implicit none
!   integer, intent(in) :: idx
!   character(len=*), intent(in) :: hash
!   integer :: GIGS
!   if ( GIGS(idx,trim(hash)) .ne. 1 ) then
!      write(*,'(a a a)') "[!] ERROR: different",trim(hash),"found in metadata. Stop."
!      stop 9
!   endif
!end subroutine
!      
!
!
!! -------------------------------------------------------------------
!! Getting information about the grid in the grib file.
!! -------------------------------------------------------------------
!subroutine getgridinfo(GRBFILE, IINFO)
!
!   use grib_api
!   implicit none
!   integer :: idx, ios, infile
!   integer, dimension(1) :: itmp
!
!   ! I/O ARGUMENTS
!   character(len=255), intent(in) :: GRBFILE
!   ! Using:
!   ! (1) for Ni (number of rows)
!   ! (2) for Nj (number of cols)
!   ! (3) for step (number of different forecast lead times)
!   ! (4) for perturbations (number of different members)
!   integer, intent(inout), dimension(4) :: IINFO
!
!   ! Function
!   integer :: GIGS
!
!   ! Open grib file. If not readable or not found: exit with exit code 9
!   call grib_open_file(infile, GRBFILE,'r',ios)
!   if ( ios .ne. 0 ) then
!      print *, 'Problems reading the input file. Not found or not readable'
!      stop 9
!   endif
!   call grib_close_file(infile)
!
!   ! Create index
!   call grib_index_create(idx,GRBFILE,'Ni,Nj,latitudeOfFirstGridPointInDegrees,&
!           longitudeOfFirstGridPointInDegrees,&
!           latitudeOfLastGridPointInDegrees,longitudeOfLastGridPointInDegrees')
!
!   ! whenever an element exists more than once (that would mean
!   ! that there are different grid definitions in the file) the
!   ! system stops and returns an error.
!   ! grib_index_get_size() returns number of distinct values
!   ! grib_get_index_xxx() returns the values itself (we only expect
!   ! one!)
!
!   ! Check if we do have the same grid definition
!   call GIGSandExit(idx,'latitudeOfFirstGridPointInDegrees')
!   call GIGSandExit(idx,'latitudeOfLastGridPointInDegrees')
!   call GIGSandExit(idx,'longitudeOfFirstGridPointInDegrees')
!   call GIGSandExit(idx,'longitudeOfLastGridPointInDegrees')
!
!   ! center information
!   if ( GIGS(idx,'Ni') .ne. 1 ) then
!      write(*,'(a a a)') "[!] ERROR: different","Ni","found in metadata. Stop."
!      stop 9
!   endif
!   call grib_index_get_int(idx,'Ni',itmp); IINFO(1) = itmp(1)
!   ! grid width information
!   if ( GIGS(idx,'Nj') .ne. 1 ) then
!      write(*,'(a a a)') "[!] ERROR: different","Nj","found in metadata. Stop."
!      stop 9
!   endif
!   call grib_index_get_int(idx,'Nj',itmp); IINFO(2) = itmp(1)
!
!   ! Count number of different forecast steps in the grib file 
!   call grib_index_create(idx,GRBFILE,'step')
!   IINFO(3) = GIGS(idx,'step')
!
!   ! Count number of different perturbations (ensemble)
!   call grib_index_create(idx,GRBFILE,'perturbationNumber')
!   IINFO(4) = GIGS(idx,'perturbationNumber')
!
!end subroutine getgridinfo
!
!! -------------------------------------------------------------------
!! Returns latitude and longitude vector for all grids
!! -------------------------------------------------------------------
!subroutine getgridll(GRBFILE,NELEM,LATS,LONS)
!   
!   use grib_api
!
!   implicit none
!
!   integer :: infile, igrib, ios
!
!   ! I/O variables
!   integer, intent(in) :: NELEM
!   real(8), intent(inout), dimension(NELEM) :: LONS, LATS
!   real(8), dimension(NELEM) :: values
!
!   character(len=255), intent(in) :: GRBFILE
!   
!
!   ! Open grib file. If not readable or not found: exit with exit code 9
!   call grib_open_file(infile, GRBFILE,'r',ios)
!   if ( ios .ne. 0 ) then
!      print *, 'Problems reading the input file. Not found or not readable'
!      stop
!   endif
!
!   ! Getting first grib message
!   call grib_new_from_file(infile,igrib)
!
!   ! Getting longitude and latitude from this field
!   call grib_get_data_real8(igrib, LATS, LONS, values, ios)
!
!   ! Release and close.
!   call grib_release(igrib)
!   call grib_close_file(infile)
!
!end subroutine getgridll
!
! -------------------------------------------------------------------
! Returns latitude and longitude vector for all grids
! -------------------------------------------------------------------
subroutine speedgetgriddata(GRBFILE,SHORTNAME,LATS,LONS,META,VALUES,NELEM,NROWS)

   use grib_api

   implicit none

   integer :: infile, igrib, ios, iret
   integer :: count, idx, currow 
   integer :: curstep, curpert, curperm
   integer :: nsteps, nperturbations

   character(len=20) :: currshortName
   integer, dimension(:), allocatable   :: steps, perturbations
   integer, dimension(:,:), allocatable :: spgrid ! steps/perturbations grid

   ! I/O variables
   ! NELEM:  number of grid points (Ni times Nj)
   ! NROWS:  number of 'rows' for VALUES/META. Number of steps * number of perturbations
   ! VALUES: real matrix to store the values (one message per line)
   ! META:   integer matrix to store meta information (step and perturbation)
   integer, intent(in)                            :: NELEM, NROWS
   real(8), intent(inout), dimension(NROWS,NELEM) :: VALUES
   integer, intent(inout), dimension(NROWS,2)     :: META
   real(8), intent(inout), dimension(NELEM)       :: LATS, LONS

   character(len=50), intent(in)  :: SHORTNAME
   character(len=255), intent(in) :: GRBFILE

   ! Function values
   integer :: arrayPositionInt
   integer :: matrixPositionInt

   ! Open grib file. If not readable or not found: exit with exit code 9
   call grib_open_file(infile, GRBFILE,'r',ios)
   if ( ios .ne. 0 ) then
      print *, 'Problems reading the input file. Not found or not readable'
      stop
   endif


   ! Index to get steps/permutations for the given shorntame
   call grib_index_create(idx,GRBFILE,'shortName,step,perturbationNumber')
   call grib_index_select(idx,'shortName',trim(SHORTNAME))

   ! Getting all different steps in the file
   call grib_index_get_size(idx,'step',nsteps)
   allocate(steps(nsteps))
   call grib_index_get(idx,'step',steps)

   ! Getting all different perturbations in the file
   call grib_index_get_size(idx,'perturbationNumber',nperturbations)
   allocate(perturbations(nperturbations))
   call grib_index_get(idx,'perturbationNumber',perturbations)

   ! Create index with the required keywords and subset (select)
   ! with respect to input SHORTNAME
   !call grib_index_create(idx,GRBFILE,'step,perturbationNumber,shortName')
   call grib_index_create(idx,GRBFILE,'shortName')
   call grib_index_select(idx,'shortName',trim(SHORTNAME))

   ! Create steps/perturbations grid
   allocate(spgrid(nperturbations*nsteps,2))
   call expandGrid(spgrid,steps,nsteps,perturbations,nperturbations)

   do i = 1,size(spgrid,1)
      print *, spgrid(i,:)
   enddo

   ! Open/calling grib file
   call grib_new_from_file(infile,igrib)
   call grib_new_from_index(idx,igrib, iret)
   count = 0 ! Init value
   do while (iret /= GRIB_END_OF_INDEX)
      print *, count, ' <<---------'
      count=count+1
      call grib_get(igrib,'shortName',currshortName)
      call grib_get(igrib,'step',curstep)
      call grib_get(igrib,'perturbationNumber',curperm)
      call grib_get(igrib,'perturbationNumber',curpert)
      write(*,'(A,A,A,i3,A,i3)') 'shortName=',trim(currshortName),&
              '   step='  ,curstep, '   member=', curpert
   
      ! Looking for position of step of the current message in 'steps'
      ! which corresponds to the row of the output data matrix.
      print *, "     -> ",curstep,curperm
      currow = matrixPositionInt(curstep,curperm,spgrid,size(spgrid,1),size(spgrid,2))
      if ( currow .lt. 1 ) then
         print *, "[!] Could not find step position. Stop."; stop 8
      end if

      call grib_get_int(igrib, 'step', META(currow,1))
      call grib_get_int(igrib, 'perturbationNumber', META(currow,2))

      ! Reading data
      call grib_get_data_real8(igrib, lats, lons, VALUES(currow,:), ios)

      ! Release and take next message
      call grib_release(igrib)
      call grib_new_from_index(idx,igrib, iret)
   end do

   ! Release and close.
   call grib_release(igrib)
   call grib_close_file(infile)

end subroutine speedgetgriddata















