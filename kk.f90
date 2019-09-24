program kk
  implicit none

  integer, parameter :: maxNameLen = 16
  integer, parameter :: baseYear = 2016
  character(len=7), parameter :: kkBasis = 'kkNames'

  logical :: uniqueTotal, uniqueGift
  integer :: iters
  integer :: ii, jj, years
  integer :: thisYear, pastYears
  integer :: totalAllNames, lenNew, lenSpouse
  integer :: tempInt
  character(len=0) :: tempChar
  integer, dimension(8) :: timeDate
  integer, dimension(:), allocatable :: lenOld
  integer, dimension(:), allocatable :: tempIntArr
  integer, dimension(:), allocatable :: orderingArr
  logical, dimension(:), allocatable :: uniqueOld, uniqueOldSpouse
  character(len=11), dimension(:), allocatable :: filesOld
  logical, dimension(:), allocatable :: isParticipant
  logical, dimension(:,:), allocatable :: giftMatrix
  character(len=maxNameLen), dimension(:), allocatable :: allNames
  character(len=maxNameLen), dimension(:), allocatable :: names, namesNew
  character(len=maxNameLen), dimension(:,:), allocatable :: spouseList

  real :: t_start, t_end, t_elapsed
  integer :: t_hr, t_min, t_sec

  ! Need a construct which allows a char array of different length arrays
  type char2D
     character(len=maxNameLen), dimension(:), allocatable :: char1D
  end type char2D

  type(char2D), dimension(:), allocatable :: namesOld

  call random_seed()
  call date_and_time(VALUES=timeDate)
  thisYear = timeDate(1)
  pastYears = thisYear - baseYear
  allocate(filesOld(pastYears), lenOld(pastYears) &
       & , namesOld(pastYears), uniqueOld(pastYears) &
       & , uniqueOldSpouse(pastYears))

  ! -----------------------Previous years' kk data----------------------

  do years=1,pastYears
     write(filesOld(years),'(a7,i4)') kkBasis, baseYear+years-1
     ! write(*,*) filesOld(years)

     open(101, file=filesOld(years), action='read')
     read(101,*) lenOld(years)
     allocate(namesOld(years)%char1D(lenOld(years)))
     do ii=1,lenOld(years)
        read(101,*) namesOld(years)%char1D(ii)
     end do
     close(101)
  end do

  ! -------------------------This year's kk data------------------------

  open(101, file='kkData', action='read')
  read(101,*) totalAllNames
  allocate(giftMatrix(totalAllNames,totalAllNames) &
       &, tempIntArr(totalAllNames), allNames(totalAllNames) &
       &, isParticipant(totalAllNames), orderingArr(totalAllNames))

  do ii = 1,totalAllNames
     read(101,*) tempInt, allNames(ii), tempIntArr(:)
     isParticipant(ii) = (tempInt .eq. 1)
     giftMatrix(ii,:) = (tempIntArr(:) .eq. 1)
  end do
  close(101)

  lenNew = count(isParticipant)
  allocate(names(lenNew))
  names(:) = pack(allNames, isParticipant)
  orderingArr(:) = (/ (ii,ii=1,totalAllNames) /)

  ! write(*,*) 'Participants'
  ! write(*,*) names
  ! write(*,*)

  ! --------------------------Read spouse list--------------------------

  open(101, file='spouseList', action='read')
  read(101,*) lenSpouse
  allocate(spouseList(lenSpouse,2))
  do ii = 1,lenSpouse
     read(101,*) spouseList(ii,:)
  end do
  close(101)

  ! ---------------------------praise RNGesus---------------------------

  ! Randomise array until a unique config is found
  iters = 0
  uniqueTotal = .false.
  call cpu_time(t_start)
  do while ((.not. uniqueTotal))! .and. (iters .le. 1000))
     iters = iters + 1
     namesNew = randomiseCharArray(names, lenNew)
     ! Previous year check
     do years = 1,pastYears
        uniqueOld(years) = isUnique(namesOld(years)%char1D &
             & , namesNew, lenOld(years), lenNew &
             & , spouseList, lenSpouse)
     end do

     ! Close relative check
     uniqueGift = canGift(namesNew, lenNew, giftMatrix &
          & , allNames, totalAllNames)
     uniqueTotal = all(uniqueOld) .and. uniqueGift
     ! write(*,*) iters
  end do
  call cpu_time(t_end)

  ! --------------------------------------------------------------------

  call convertTime(t_start-t_end, t_hr, t_min, t_sec)
  write(*,'(a,i4,a,i2,a,i2,a)') 'Time taken:', t_hr, 'hrs,', t_min, 'min,', t_sec, 'sec'
  write(*,*) 'Total attempts: ', iters
  write(*,'(a)') namesNew

  ! Write the new names to file
  open(101, file='new_names', action='write')
  write(101,*) lenNew
  do ii = 1,lenNew
     write(101,*) namesNew(ii)
  end do
  close(101)

  do years=1,pastYears
     deallocate(namesOld(years)%char1D)
  end do
  deallocate(names, allNames, filesOld, tempIntArr &
       &, namesOld, uniqueOld, isParticipant &
       &, giftMatrix, spouseList, uniqueOldSpouse)

contains

  subroutine convertTime(t, t_h, t_m, t_s)
    implicit none
    real, intent(in)     :: t
    integer, intent(out) :: t_h, t_m, t_s

    t_h = int(t/3600)
    t_m = int((t - t_h*3600)/60)
    t_s = int(t - t_h*3600 - t_m*60)

  end subroutine convertTime


  function canGift(arrIn, lenIn, giftMat, namesAll, lenAll)
    implicit none
    integer :: lenIn, lenAll
    character(len=maxNameLen), dimension(lenIn) :: arrIn
    character(len=maxNameLen), dimension(lenAll) :: namesAll
    logical, dimension(lenAll,lenAll) :: giftMat
    logical canGift
    !
    integer :: i, j, ip1
    integer :: nLoc, np1Loc
    character(len=maxNameLen) :: name, namep1
    ! integer, dimension(lenGift) :: orderArr

    canGift = .true.
    do i = 1,lenIn
       name = arrIn(i)
       ip1 = i+1
       if (ip1 .eq. lenIn+1) ip1 = 1
       namep1 = arrIn(ip1)

       do j = 1,lenAll
          if (namesAll(j) .eq. name) then
             nLoc = j
             cycle
          end if
          if (namesAll(j) .eq. namep1) then
             np1Loc = j
             cycle
          end if
       end do

       canGift = giftMat(nLoc,np1Loc)
       ! write(*,*) name, namep1, canGift
       if (canGift .eq. .false.) exit
    end do

  end function canGift


  function randomiseCharArray(arrayIn, lenIn) result(arrOut)
    implicit none
    integer :: lenIn
    character(len=maxNameLen), dimension(lenIn) :: arrayIn
    ! Locals
    character(len=maxNameLen), dimension(lenIn) :: arrIn
    character(len=maxNameLen), dimension(lenIn) :: arrOut
    integer :: count
    real :: randNum
    integer :: randInt

    count = 1
    call random_seed()
    arrIn(:) = arrayIn(:)

    do while (count .le. lenIn)
       call random_number(randNum)
       randInt = int(lenIn*randNum + 1)
       if (arrIn(randInt) .ne. 'null') then

          arrOut(count) = arrIn(randInt)
          arrIn(randInt) = 'null'
          count = count + 1

       end if
    end do


  end function randomiseCharArray


  function isUnique(nOld, nNew, lOld, lNew, nSps, lSps)
    implicit none
    integer :: lOld, lNew, lSps
    character(len=maxNameLen), dimension(lOld)   :: nOld ! Old names
    character(len=maxNameLen), dimension(lNew)   :: nNew ! New names
    character(len=maxNameLen), dimension(lSps,2) :: nSps ! Spouse list
    logical :: isUnique
    ! Locals
    integer :: i, j, k, ip1, jp1
    character(len=maxNameLen) :: oldSpouse

    isUnique = .true.

    do i = 1,lNew
       ip1 = i + 1
       if (ip1 .eq. lNew+1) ip1 = 1
       do j = 1,lOld
          jp1 = j + 1
          if (jp1 .eq. lOld+1) jp1 = 1

          do k = 1,lSps
             if (nOld(j) .eq. nSps(k,1)) then
                oldSpouse = nSps(k,2)
                ! exit
             end if
          end do

          if (((nOld(j) .eq. nNew(i)) .or. (oldSpouse .eq. nNew(i))) &
               & .and. (nOld(jp1) .eq. nNew(ip1))) then
             isUnique = .false.
             ! exit
          end if

       end do
       if (.not. isUnique) exit
    end do

  end function isUnique


  ! function isUnique(nOld, nNew, lOld, lNew)
  !   implicit none
  !   integer :: lOld, lNew
  !   character(len=maxNameLen), dimension(lOld) :: nOld
  !   character(len=maxNameLen), dimension(lNew) :: nNew
  !   logical :: isUnique

  !   integer :: i, j, ip1, jp1

  !   isUnique = .true.

  !   do i = 1,lNew
  !      ip1 = i + 1
  !      if (ip1 .eq. lNew+1) ip1 = 1
  !      do j = 1,lOld
  !         jp1 = j + 1
  !         if (jp1 .eq. lOld+1) jp1 = 1

  !         if ((nOld(j) .eq. nNew(i)) .and. (nOld(jp1) .eq. nNew(ip1))) then
  !            isUnique = .false.
  !            exit
  !         end if

  !      end do
  !      if (.not. isUnique) exit
  !   end do

  ! end function isUnique

end program kk
