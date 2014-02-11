main directory
==============

CMakeLists.txt
--------------

::

  set(MAIN_SRC_FILES
     ${CMAKE_CURRENT_SOURCE_DIR}/main.F90
     ${CMAKE_CURRENT_SOURCE_DIR}/SimulationSetup.F90
     ${CMAKE_CURRENT_SOURCE_DIR}/SimulationVars.F90
     ${CMAKE_CURRENT_SOURCE_DIR}/GridSetup.F90
     ${CMAKE_CURRENT_SOURCE_DIR}/GridTransform.F90
     ${CMAKE_CURRENT_SOURCE_DIR}/GridTransformSetup.F90
     ${CMAKE_CURRENT_SOURCE_DIR}/Parameters.F90 CACHE INTERNAL "" FORCE)


main.F90
--------

::

  !> \file: main.F90
  !> \author: Sayop Kim 

  PROGRAM main
     USE SimulationSetup_m, ONLY: InitializeCommunication
     USE GridSetup_m, ONLY: InitializeGrid
     USE GridTransform_m, ONLY: GridTransform
     USE io_m, ONLY: WriteTecPlot, filenameLength
     USE Parameters_m, ONLY: wp
  
     IMPLICIT NONE
  
     CHARACTER(LEN=filenameLength) :: outputfile = 'output.tec'

     CALL InitializeCommunication
     ! Make initial condition for grid point alignment
     ! Using Algebraic method
     CALL InitializeGrid
     ! Use Elliptic grid points
     CALL GridTransform
     CALL WriteTecPlot(outputfile,'"I","J","K","Jacobian","Pi","Psi"')
  END PROGRAM main


SimulationVars.F90
------------------

::

  !> \file: SimulationVars.F90
  !> \author: Sayop Kim

  MODULE SimulationVars_m
     USE parameters_m, ONLY : wp
     IMPLICIT NONE

     INTEGER :: imax, jmax, kmax, nmax
     REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: xp
     REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:) :: BOTedge
     REAL(KIND=wp) :: cy1, cy2, cy3, cy4, cy5, cy6
     REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: inverseJacobian
     REAL(KIND=wp), DIMENSION(3,8) :: xblkV    ! x,y,z points at 8 vertices of block
  END MODULE SimulationVars_m


parameters.F90
--------------

::

  !> \file parameters.F90
  !> \author Sayop Kim
  !> \brief Provides parameters and physical constants for use throughout the
  !! code.
  MODULE Parameters_m
     INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(8)
  
     CHARACTER(LEN=10), PARAMETER :: CODE_VER_STRING = "V.001.001"
     REAL(KIND=wp), PARAMETER :: PI = 3.14159265358979323846264338_wp

  END MODULE Parameters_m


GridSetup.F90
-------------

::

  !> \file: GridSetup.F90
  !> \author: Sayop Kim
  
  MODULE GridSetup_m
     USE Parameters_m, ONLY: wp
     USE SimulationSetup_m, ONLY: GridStretching
     IMPLICIT NONE
  
     PUBLIC :: InitializeGrid, GenerateInteriorPoints

  CONTAINS
  !-----------------------------------------------------------------------------!
     SUBROUTINE InitializeGrid()
  !-----------------------------------------------------------------------------!
     USE io_m, ONLY: ReadGridInput
     IMPLICIT NONE

     ! Create Bottom Edge coordinate values
     CALL ReadGridInput
     CALL InitializeGridArrays
     CALL CreateBottomEdge
     CALL SetEdgePnts
     CALL GridPntsAlgbra
     CALL GenerateInteriorPoints

     END SUBROUTINE

  !-----------------------------------------------------------------------------!
     SUBROUTINE InitializeGridArrays()
  !-----------------------------------------------------------------------------!
        ! imax: number of grid points in i-drection
        ! jmax: number of grid points in j-direction
        ! kmax: number of grid points in k-direction
        ! xp(3,imax,jmax,kmax): curvilinear coordinates in physical space
        USE SimulationVars_m, ONLY: imax, jmax, kmax, &
                                    xp, inverseJacobian
        IMPLICIT NONE

        WRITE(*,'(a)') ""
        WRITE(*,'(a)') "Initializing data arrays..."
        ALLOCATE(xp(3,imax,jmax,kmax))
        ALLOCATE(inverseJacobian(imax,jmax,kmax))
        xp = 0.0_wp
        inverseJacobian = 0.0_wp
     END SUBROUTINE


  !-----------------------------------------------------------------------------!
     SUBROUTINE CreateBottomEdge()
  !-----------------------------------------------------------------------------!
     USE io_m, ONLY: width, FEsize, GeoSize, DCsize, &
                     Gpnts
     USE SimulationVars_m, ONLY: imax, jmax, kmax,&
                                 xblkV, cy4, cy5, cy6
     USE SimulationVars_m, ONLY: BOTedge
     USE SimulationSetup_m, ONLY: UniformSpacing
     IMPLICIT NONE
     INTEGER :: i

     ALLOCATE(BOTedge(3,imax))
     WRITE(*,*) ""
     WRITE(*,*) "Creating Bottome edge point values with Airfoil geometry"
     DO i = 2, FEsize
        BOTedge(1,i) = GridStretching(xblkV(1,1), Gpnts(1,1), i, FEsize, cy4)
        !BOTedge(2,i) = UniformSpacing(xblkV(2,1), Gpnts(2,1), i, FEsize)
        BOTedge(3,i) = GridStretching(xblkV(3,1), Gpnts(3,1), i, FEsize, cy4)
     ENDDO
     DO i = FEsize + 1, FEsize + GeoSize - 1
        BOTedge(1,i) = GridStretching(Gpnts(1,1), Gpnts(1,2), i-FEsize+1, GeoSize, cy5)
        !BOTedge(2,i) = UniformSpacing(Gpnts(2,1), Gpnts(2,2), i-FEsize+1, GeoSize)
        !BOTedge(3,i) = UniformSpacing(Gpnts(3,1), Gpnts(3,2), i-FEsize+1, GeoSize)
        BOTedge(3,i) = Airfoil(BOTedge(1,i))
     ENDDO
     DO i = FEsize + GeoSize, imax - 1
        BOTedge(1,i) = GridStretching(Gpnts(1,2), xblkV(1,2), i-FEsize-GeoSize+2, &
                                      DCsize, cy6)
        !BOTedge(2,i) = UniformSpacing(Gpnts(2,2), xblkV(2,2), i-FEsize-GeoSize+2, DCsize)
        BOTedge(3,i) = GridStretching(Gpnts(3,2), xblkV(3,2), i-FEsize-GeoSize+2, &
                                      DCsize, cy6)
     ENDDO

     END SUBROUTINE


  !-----------------------------------------------------------------------------!
     FUNCTION Airfoil(xx) RESULT(yx)
  !-----------------------------------------------------------------------------!
     IMPLICIT NONE
     REAL(KIND=wp) xint, thick, xx, yx
     xint = 1.008930411365_wp
     thick = 0.15_wp
     yx = 0.2969_wp * sqrt(xint * xx) - 0.126_wp * xint * xx - 0.3516_wp * &
          (xint * xx)**2 + 0.2843_wp * (xint * xx)**3 - 0.1015_wp * (xint * xx)**4
     yx = 5.0_wp * thick * yx

     END FUNCTION Airfoil


  !-----------------------------------------------------------------------------!
     SUBROUTINE SetEdgePnts()
  !-----------------------------------------------------------------------------!
        USE SimulationVars_m, ONLY: imax, jmax, kmax, &
                                    xp, xblkV, BOTedge, cy1
        USE SimulationSetup_m, ONLY: UniformSpacing
        IMPLICIT NONE
        INTEGER :: i
   
        WRITE(*,'(a)') ""
        WRITE(*,'(a)') "Setting Boundary Conditions..."
        !+++++++++++++++++++++++++++++++++++++++++++++++++++!
        ! Assign coordinates value in xblkV(8,3)            !
        ! Below shows 8 vertices defined in one single block!
        !                                                   !
        !         7--------------8                          !
        !        /|             /|                          !
        !       / |            / |                          !
        !      3--------------4  |    z  y                  !
        !      |  |           |  |    | /                   !
        !      |  5-----------|--6    |/                    !
        !      | /            | /     --- x                 !
        !      |/             |/                            !
        !      1--------------2                             !
        !                                                   !
        !+++++++++++++++++++++++++++++++++++++++++++++++++++!
        ! Vertex (1)
        !xblkV(1,1) = 0.0
        !xblkV(2,1) = 0.0
        !xblkV(3,1) = 0.0
        DO i = 1, 3
           xp(i,1,1,1) = xblkV(i,1)
        ENDDO
        ! Vertex (2)
        !xblkV(1,2) = 0.0
        !xblkV(2,2) = 0.0
        !xblkV(3,2) = 0.0
        DO i = 1, 3
           xp(i,imax,1,1) = xblkV(i,2)
        ENDDO
        ! Vertex (3)
        !xblkV(1,3) = 0.0
        !xblkV(2,3) = 0.0
        !xblkV(3,3) = 0.0
        DO i = 1, 3
           xp(i,1,1,kmax) = xblkV(i,3)
        ENDDO

        ! Vertex (4)
        !xblkV(1,4) = 0.0
        !xblkV(2,4) = 0.0
        !xblkV(3,4) = 0.0
        DO i = 1, 3
           xp(i,imax,1,kmax) = xblkV(i,4)
        ENDDO
        ! Vertex (5)
        !xblkV(1,5) = 0.0
        !xblkV(2,5) = 0.0
        !xblkV(3,5) = 0.0
        DO i = 1, 3
           xp(i,1,jmax,1) = xblkV(i,5)
        ENDDO
        ! Vertex (6)
        !xblkV(1,6) = 0.0
        !xblkV(2,6) = 0.0
        !xblkV(3,6) = 0.0
        DO i = 1, 3
           xp(i,imax,jmax,1) = xblkV(i,6)
        ENDDO
        ! Vertex (7)
        !xblkV(1,7) = 0.0
        !xblkV(2,7) = 0.0
        !xblkV(3,7) = 0.0
        DO i = 1, 3
           xp(i,1,jmax,kmax) = xblkV(i,7)
        ENDDO
        ! Vertex (8)
        !xblkV(1,8) = 0.0
        !xblkV(2,8) = 0.0
        !xblkV(3,8) = 0.0
        DO i = 1, 3
           xp(i,imax,jmax,kmax) = xblkV(i,8)
        ENDDO
        !+++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Set up boundary point coordinates at every edge
        !
        !                 
        !         +--------(8)---------+
        !        /|                   /|
        !     (11)|                (12)|
        !      /  |                 /  |
        !     +---------(4)--------+  (6)
        !     |  (5)               |   |
        !     |   |                |   |     z  y
        !     |   |                |   |     | /
        !    (1)  +-------(7)------|---+     |/
        !     |  /                (2) /      ---x
        !     |(9)                 |(10)    
        !     |/                   |/      
        !     +--------(3)---------+
        !
        !+++++++++++++++++++++++++++++++++++++++++++++++++++
        ! edge (1)
        DO i = 2, kmax - 1
           xp(1,1,1,i) = UniformSpacing(xblkV(1,1), xblkV(1,3), i, kmax)
           xp(2,1,1,i) = UniformSpacing(xblkV(2,1), xblkV(2,3), i, kmax)
           xp(3,1,1,i) = GridStretching(xblkV(3,1), xblkV(3,3), i, kmax, cy1)
        ENDDO
        ! edge (2)
        DO i = 2, kmax - 1
           xp(1,imax,1,i) = UniformSpacing(xblkV(1,2), xblkV(1,4), i, kmax)
           xp(2,imax,1,i) = UniformSpacing(xblkV(2,2), xblkV(2,4), i, kmax)
           xp(3,imax,1,i) = GridStretching(xblkV(3,2), xblkV(3,4), i, kmax, cy1)
        ENDDO
        ! edige (3)
        DO i = 2, imax - 1
           !xp(1,i,1,1) = UniformSpacing(xblkV(1,1), xblkV(1,2), i, imax)
           xp(2,i,1,1) = UniformSpacing(xblkV(2,1), xblkV(2,2), i, imax)
           !xp(3,i,1,1) = UniformSpacing(xblkV(3,1), xblkV(3,2), i, imax)
           xp(1,i,1,1) = BOTedge(1,i)
           xp(3,i,1,1) = BOTedge(3,i)
        ENDDO
        ! edge (4)
        DO i = 2, imax - 1
           xp(1,i,1,kmax) = UniformSpacing(xblkV(1,3), xblkV(1,4), i, imax)
           xp(2,i,1,kmax) = UniformSpacing(xblkV(2,3), xblkV(2,4), i, imax)
           xp(3,i,1,kmax) = UniformSpacing(xblkV(3,3), xblkV(3,4), i, imax)
        ENDDO
        ! edge (5)
        DO i = 2, kmax - 1
           xp(1,1,jmax,i) = xp(1,1,1,i)
           xp(2,1,jmax,i) = UniformSpacing(xblkV(2,5), xblkV(2,7), i, kmax)
           xp(3,1,jmax,i) = xp(3,1,1,i)
        ENDDO
        ! edge (6)
        DO i = 2, kmax - 1
           xp(1,imax,jmax,i) = xp(1,imax,1,i)
           xp(2,imax,jmax,i) = UniformSpacing(xblkV(2,6), xblkV(2,8), i, kmax)
           xp(3,imax,jmax,i) = xp(3,imax,1,i)
        ENDDO
        ! edge (7)
        DO i = 2, imax - 1
           !xp(1,i,jmax,1) = UniformSpacing(xblkV(1,5), xblkV(1,6), i, imax)
           xp(2,i,jmax,1) = UniformSpacing(xblkV(2,5), xblkV(2,6), i, imax)
           !xp(3,i,jmax,1) = UniformSpacing(xblkV(3,5), xblkV(3,6), i, imax)
           xp(1,i,jmax,1) = BOTedge(1,i)
           xp(3,i,jmax,1) = BOTedge(3,i)
        ENDDO
        ! edge (8) 
        DO i = 2, imax - 1
           xp(1,i,jmax,kmax) = xp(1,i,1,kmax)
           xp(2,i,jmax,kmax) = UniformSpacing(xblkV(2,7), xblkV(2,8), i, imax)
           xp(3,i,jmax,kmax) = xp(3,i,1,kmax)
        ENDDO
        ! edge (9)
        DO i = 2, jmax - 1
           xp(1,1,i,1) = UniformSpacing(xblkV(1,1), xblkV(1,5), i, jmax)
           xp(2,1,i,1) = UniformSpacing(xblkV(2,1), xblkV(2,5), i, jmax)
           xp(3,1,i,1) = UniformSpacing(xblkV(3,1), xblkV(3,5), i, jmax)
        ENDDO
        ! edge (10)
        DO i = 2, jmax - 1
           xp(1,imax,i,1) = UniformSpacing(xblkV(1,2), xblkV(1,6), i, jmax)
           xp(2,imax,i,1) = UniformSpacing(xblkV(2,2), xblkV(2,6), i, jmax)
           xp(3,imax,i,1) = UniformSpacing(xblkV(3,2), xblkV(3,6), i, jmax)
        ENDDO
        ! edge (11)
        DO i = 2, jmax - 1
           xp(1,1,i,kmax) = UniformSpacing(xblkV(1,3), xblkV(1,7), i, jmax)
           xp(2,1,i,kmax) = UniformSpacing(xblkV(2,3), xblkV(2,7), i, jmax)
           xp(3,1,i,kmax) = UniformSpacing(xblkV(3,3), xblkV(3,7), i, jmax)
        ENDDO
        ! edge (12)
        DO i = 2, jmax - 1
           xp(1,imax,i,kmax) = UniformSpacing(xblkV(1,4), xblkV(1,8), i, jmax)
           xp(2,imax,i,kmax) = UniformSpacing(xblkV(2,4), xblkV(2,8), i, jmax)
           xp(3,imax,i,kmax) = UniformSpacing(xblkV(3,4), xblkV(3,8), i, jmax)
        ENDDO
     END SUBROUTINE


  !-----------------------------------------------------------------------------!
     SUBROUTINE GridPntsAlgbra()
  !-----------------------------------------------------------------------------!
        USE SimulationVars_m, ONLY: imax, jmax, kmax, &
                                    xp, xblkV, cy1
        USE SimulationSetup_m, ONLY: UniformSpacing
        IMPLICIT NONE
        INTEGER :: i, j, k
  
        WRITE(*,'(a)') ""
        WRITE(*,'(a)') "Writing grid points on block surface..."

        !+++++++++++++++++++++++++++++++++++++++++
        ! "front plane"
        !     +------------+
        !     |            |   z(k)
        !     | i-k plane  |    |
        !     |  (j = 1)   |    |
        !     1------------+    ---- x(i)
        !
        !k=kmax @---@---@---@---@
        !       |   |   |   |   |  @: edge points (known)
        !       @---o---o---o---@  o: interior points (unknown)
        !       |   |   |   |   |
        !       @---o---o---o---@
        !       |   |   |   |   |
        !   k=1 @---@---@---@---@
        !      i=1             i=imax
        ! x-coordinate is determined along the i=const lines
        ! y-coordinate is same as y of corner (1)
        ! z-coordinate is determined along the k=const lines
        !+++++++++++++++++++++++++++++++++++++++++
        DO i = 2, imax - 1
           DO k = 2, kmax - 1
              xp(1,i,1,k) = UniformSpacing(xp(1,i,1,1), xp(1,i,1,kmax), k, kmax)
              xp(2,i,1,k) = UniformSpacing(xp(2,i,1,1), xp(2,i,1,kmax), k, kmax)
              xp(3,i,1,k) = GridStretching(xp(3,i,1,1), xp(3,i,1,kmax), k, kmax, cy1)
           ENDDO
        ENDDO
        !+++++++++++++++++++++++++++++++++++++++++
        ! "back plane"
        !     +------------+
        !     |            |   z(k)
        !     | i-k plane  |    |
        !     | (j = jmax) |    |
        !     5------------+    ---- x(i)
        ! x-coordinate is determined along the i=const lines
        ! y-coordinate is same as y of corner (5)
        ! z-coordinate is determined along the k=const lines
        !+++++++++++++++++++++++++++++++++++++++++
        DO i = 2, imax - 1
           DO k = 2, kmax - 1
              xp(1,i,jmax,k) = xp(1,i,1,k)
              xp(2,i,jmax,k) = UniformSpacing(xp(2,i,jmax,1), xp(2,i,jmax,kmax), k, kmax)
              xp(3,i,jmax,k) = xp(3,i,1,k)
           ENDDO
        ENDDO
        !+++++++++++++++++++++++++++++++++++++++++
        ! "left plane"
        !                 +
        !                /|
        !               / |   j-k plane (i = 1)
        !              /  |
        !             +   +
        !             |  /  z(k) y(j)
        !             | /     |  /
        !             |/      | /
        !             1       |/ 
        ! x-coordinate is same as x of corner (1)
        ! y-coordinate is determined along the j=const lines
        ! z-coordinate is determined along the k=const lines
        !+++++++++++++++++++++++++++++++++++++++++
        DO j = 2, jmax - 1
           DO k = 2, kmax - 1
              xp(1,1,j,k) = UniformSpacing(xp(1,1,j,1), xp(1,1,j,kmax), k, kmax)
              xp(2,1,j,k) = UniformSpacing(xp(2,1,j,1), xp(2,1,j,kmax), k, kmax)
              xp(3,1,j,k) = GridStretching(xp(3,1,j,1), xp(3,1,j,kmax), k, kmax, cy1)
           ENDDO
        ENDDO
        !+++++++++++++++++++++++++++++++++++++++++
        ! "right plane"
        !                 +
        !                /|
        !               / |   j-k plane (i = imax)
        !              /  |
        !             +   +
        !             |  /  z(k) y(j)
        !             | /     |  /
        !             |/      | /
        !             2       |/ 
        ! x-coordinate is same as x of corner (2)
        ! y-coordinate is determined along the j=const lines
        ! z-coordinate is determined along the k=const lines
        !+++++++++++++++++++++++++++++++++++++++++
        DO j = 2, jmax - 1
           DO k = 2, kmax - 1
              xp(1,imax,j,k) = UniformSpacing(xp(1,imax,j,1), xp(1,imax,j,kmax), k, kmax)
              xp(2,imax,j,k) = xp(2,1,j,k)
              xp(3,imax,j,k) = xp(3,1,j,k)
           ENDDO
        ENDDO
  
        !+++++++++++++++++++++++++++++++++++++++++
        ! "bottom plane"
        !           +-------------+
        !          /             /   y(j)
        !         /  i-j plane  /   /
        !        /  (k = 1)    /   /
        !       1-------------+    ---->x(i)
        ! x-coordinate is determined along the i=const lines
        ! y-coordinate is determined along the j=const lines
        ! z-coordinate is same as z of corner (1)
        !+++++++++++++++++++++++++++++++++++++++++
        DO i = 2, imax - 1
           DO j = 2, jmax - 1
              xp(1,i,j,1) = UniformSpacing(xp(1,i,1,1), xp(1,i,jmax,1), j, jmax)
              xp(2,i,j,1) = UniformSpacing(xp(2,i,1,1), xp(2,i,jmax,1), j, jmax)
              xp(3,i,j,1) = xp(3,i,1,1)
           ENDDO
        ENDDO
        !+++++++++++++++++++++++++++++++++++++++++
        ! "top plane"
        !           +-------------+
        !          /             /   y(j)
        !         /  i-j plane  /   /
        !        /  (k = kmax) /   /
        !       3-------------+    ---->x(i)
        ! x-coordinate is determined along the i=const lines
        ! y-coordinate is determined along the j=const lines
        ! z-coordinate is same as z of corner (3)
        !+++++++++++++++++++++++++++++++++++++++++
        DO i = 2, imax - 1
           DO j = 2, jmax - 1
              xp(1,i,j,kmax) = xp(1,i,1,kmax)
              xp(2,i,j,kmax) = xp(2,i,j,1)
              xp(3,i,j,kmax) = UniformSpacing(xp(3,i,1,kmax), xp(3,i,jmax,kmax), j, jmax)
           ENDDO
        ENDDO
     END SUBROUTINE
  

  !-----------------------------------------------------------------------------!
     SUBROUTINE GenerateInteriorPoints()
  !-----------------------------------------------------------------------------!
        USE SimulationVars_m, ONLY: imax, jmax, kmax, &
                                    xp, xblkV, cy1
        USE SimulationSetup_m, ONLY: UniformSpacing
        IMPLICIT NONE
        INTEGER :: i, j, k
  
        WRITE(*,'(a)') ""
        WRITE(*,'(a)') "Writing interior grid points..."
        DO i = 2, imax -1
           DO k = 2, kmax - 1
              DO j = 2, jmax - 1
                 xp(1,i,j,k) = UniformSpacing(xp(1,i,1,k), xp(1,i,jmax,k), j, jmax)
                 xp(2,i,j,k) = UniformSpacing(xp(2,i,1,k), xp(2,i,jmax,k), j, jmax)
                 xp(3,i,j,k) = GridStretching(xp(3,i,1,k), xp(3,i,jmax,k), j, jmax, cy1)
              ENDDO
           ENDDO
        ENDDO
  
     END SUBROUTINE

  END MODULE GridSetup_m


GridTransform.F90
-----------------

::

  !> \file GridTransform.F90
  !> \author Sayop Kim

  MODULE GridTransform_m
     USE Parameters_m, ONLY: wp
     USE io_m, ONLY: iControl, WriteRMSlog, filenameLength
     USE SimulationVars_m, ONLY: nmax
     USE GridTransformSetup_m, ONLY: InitializeArrays, CalculateA123, &
                                     CalculatePiPsi, ThomasLoop, &
                                     CopyFrontTOBack, CalculateGridJacobian, &
                                     RMSres, RMScrit
     USE GridSetup_m, ONLY: GenerateInteriorPoints
     IMPLICIT NONE
     CHARACTER(LEN=filenameLength) :: RMSlogfile = 'RMSlog.dat'
  CONTAINS

  !-----------------------------------------------------------------------------!
     SUBROUTINE GridTransform()
  !-----------------------------------------------------------------------------!
     IMPLICIT NONE
     INTEGER :: n

     CALL InitializeArrays
     IF ( iControl == 1) CALL CalculatePiPsi
     DO n = 1, nmax
        CALL CalculateA123
        CALL ThomasLoop
        CALL WriteRMSlog(n,RMSlogfile)
        IF (RMSres <= RMScrit) EXIT
     ENDDO
     CALL CopyFrontTOBack
     CALL GenerateInteriorPoints
     CALL CalculateGridJacobian
     END SUBROUTINE GridTransform
  
  END MODULE


GridTransformSetup.F90
----------------------

::

  !> \file GridTransformSetup.F90
  !> \author Sayop Kim

  MODULE GridTransformSetup_m
     USE Parameters_m, ONLY: wp
     USE SimulationVars_m, ONLY: imax, jmax, kmax, &
                                 xp, cy2, cy3
     IMPLICIT NONE

     PUBLIC CalculateA123, CalculatePiPsi, ThomasLoop, &
            RMScrit, RMSres

     REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: InverseGridMetrics
     REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: A1, A2, A3, Pi, Psi
     REAL(KIND=wp) :: RMScrit, RMSres
  CONTAINS

  !-----------------------------------------------------------------------------!
     SUBROUTINE InitializeArrays()
  !-----------------------------------------------------------------------------!
     IMPLICIT NONE

     ALLOCATE(A1(imax,1,kmax))
     ALLOCATE(A2(imax,1,kmax))
     ALLOCATE(A3(imax,1,kmax))
     A1 = 0.0_wp
     A2 = 0.0_wp
     A3 = 0.0_wp

     ALLOCATE(Pi(imax,1,kmax))
     ALLOCATE(Psi(imax,1,kmax))
     Pi = 0.0_wp
     Psi = 0.0_wp

     END SUBROUTINE InitializeArrays


  !-----------------------------------------------------------------------------!
     SUBROUTINE CalculateA123()
  !-----------------------------------------------------------------------------!
  ! Evaluate A1, A2, A3 coefficients before looping Thomas method
  ! A1 = (x_k)^2 + (z_k)^2
  ! A2 = (x_i)*(x_k) + (z_i)*(z_k)
  ! A3 = (x_i)^2 + (z_i)^2

     IMPLICIT NONE
     INTEGER :: i, j, k
     ! _i: derivative w.r.t ksi
     ! _k: derivative w.r.t zeta
     REAL(KIND=wp) :: x_i, z_i, x_k, z_k

     ! Evaluate only on j=1 surface (2D i-k front plane)
     j = 1

     !WRITE(*,'(a)') ""
     !WRITE(*,'(a)') "Calculating A1, A2, A3 coefficients for the governing equation..."
     DO i = 2, imax - 1
        DO k = 2, kmax - 1
           x_i = 0.5_wp * (xp(1,i+1,j,k) - xp(1,i-1,j,k))
           z_i = 0.5_wp * (xp(3,i+1,j,k) - xp(3,i-1,j,k))
           x_k = 0.5_wp * (xp(1,i,j,k+1) - xp(1,i,j,k-1))
           z_k = 0.5_wp * (xp(3,i,j,k+1) - xp(3,i,j,k-1))
           A1(i,j,k) = x_k**2 + z_k**2
           A2(i,j,k) = x_i*x_k + z_i*z_k
           A3(i,j,k) = x_i**2 + z_i**2
        ENDDO
     ENDDO

     END SUBROUTINE CalculateA123

  !-----------------------------------------------------------------------------!
     SUBROUTINE CalculatePiPsi()
  !-----------------------------------------------------------------------------!
  ! Initialize Pi and Psy value before moving into pseudo time loop.
     USE SimulationSetup_m, ONLY: UniformSpacing, GridStretching
  
     IMPLICIT NONE
     INTEGER :: i, j, k
     ! _i: derivative w.r.t ksi
     ! _k: derivative w.r.t zeta
     REAL(KIND=wp) :: x_i, z_i, x_ii, z_ii, &
                      x_k, z_k, x_kk, z_kk
  
     ! Evaluate only on j=1 surface (2D i-k front plane)
     j = 1

     WRITE(*,'(a)') ""
     WRITE(*,'(a)') "Calculating Pi and Psi variables for controling elliptic grid..."
     ! Evaluate Psi on the boundaries (i=1, i=imax)
     DO i = 1, imax, imax - 1
        DO k = 2, kmax - 1
           x_k = 0.5_wp * (xp(1,i,j,k+1) - xp(1,i,j,k-1))
           z_k = 0.5_wp * (xp(3,i,j,k+1) - xp(3,i,j,k-1))
           x_kk = xp(1,i,j,k+1) - 2.0_wp * xp(1,i,j,k) + xp(1,i,j,k-1)
           z_kk = xp(3,i,j,k+1) - 2.0_wp * xp(3,i,j,k) + xp(3,i,j,k-1)
           IF(abs(x_k) > abs(z_k)) THEN
              Psi(i,j,k) = -x_kk / x_k
           ELSE
              Psi(i,j,k) = -z_kk / z_k
           ENDIF
        ENDDO
     ENDDO
     ! Evaluate Pi on the boundaries (k=1, k=kmax)
     DO k = 1, kmax, kmax - 1
        DO i = 2, imax - 1
           x_i = 0.5_wp * (xp(1,i+1,j,k) - xp(1,i-1,j,k))
           z_i = 0.5_wp * (xp(3,i+1,j,k) - xp(3,i-1,j,k))
           x_ii = xp(1,i+1,j,k) - 2.0_wp * xp(1,i,j,k) + xp(1,i-1,j,k)
           z_ii = xp(3,i+1,j,k) - 2.0_wp * xp(3,i,j,k) + xp(3,i-1,j,k)
           IF(abs(x_i) > abs(z_i)) THEN
              Pi(i,j,k) = -x_ii / x_i
           ELSE
              Pi(i,j,k) = -z_ii / z_i
           ENDIF
        ENDDO
     ENDDO

     ! Evaluate Pi and Psi at interior points
     DO i = 2, imax - 1
        DO k = 2, kmax - 1
           !Psi(i,j,k) = UniformSpacing(Psi(1,j,k), Psi(imax,j,k), i, imax)
           Psi(i,j,k) = GridStretching(Psi(1,j,k), Psi(imax,j,k), i, imax, cy3)
           !Pi(i,j,k) = UniformSpacing(Pi(i,j,1), Pi(i,j,kmax), k, kmax)
           Pi(i,j,k) = GridStretching(Pi(i,j,1), Pi(i,j,kmax), k, kmax, cy2)
        ENDDO
     ENDDO
     END SUBROUTINE CalculatePiPsi


  !-----------------------------------------------------------------------------!
     SUBROUTINE ThomasLoop()
  !-----------------------------------------------------------------------------!
  ! Thomas method for solving tridiagonal matrix system
  ! This subroutine should be run in a pseudo time loop
     IMPLICIT NONE
     INTEGER :: i, j, k

     REAL(KIND=wp), DIMENSION(imax) :: a, b, c, d
     REAL(KIND=wp) :: x_ik, x_k, z_ik, z_k
     RMSres = 0.0_wp
     j = 1

     DO k = 2, kmax - 1
        ! Calculate governing equation w.r.t x-coordinate
        DO i = 1, imax
           IF( i == 1 .or. i == imax ) THEN
               a(i) = 0.0_wp
               b(i) = 1.0_wp
               c(i) = 0.0_wp
               d(i) = xp(1,i,j,k)
           ELSE
               a(i) = A1(i,j,k) * (1.0_wp - 0.5_wp * Pi(i,j,k))
               b(i) = -2.0_wp * (A1(i,j,k) + A3(i,j,k))
               c(i) = A1(i,j,k) * (1.0_wp + 0.5_wp * Pi(i,j,k))
               x_k = 0.5_wp * (xp(1,i,j,k+1) - xp(1,i,j,k-1))
               x_ik = 0.25_wp * ( xp(1,i+1,j,k+1) - xp(1,i+1,j,k-1) &
                                 -xp(1,i-1,j,k+1) + xp(1,i-1,j,k-1) )
               d(i) = 2.0_wp * A2(i,j,k) * x_ik - A3(i,j,k) * ( xp(1,i,j,k+1) + &
                                                                xp(1,i,j,k-1) + &
                                                                Psi(i,j,k) * x_k )
           ENDIF
        ENDDO
        ! Call Thomas method solver
        CALL SY(1, imax, a, b, c, d)
        ! Update values at n+1 pseudo time
        DO i = 1, imax
           RMSres = RMSres + (d(i) - xp(1,i,j,k)) ** 2
           xp(1,i,j,k) = d(i)
        ENDDO
  
        ! Calculate governing equation w.r.t x-coordinate
         DO i =1, imax
           IF( i == 1 .or. i == imax ) THEN
               a(i) = 0.0_wp
               b(i) = 1.0_wp
               c(i) = 0.0_wp
               d(i) = xp(3,i,j,k)
           ELSE
               a(i) = A1(i,j,k) * (1.0_wp - 0.5_wp * Pi(i,j,k))
               b(i) = -2.0_wp * (A1(i,j,k) + A3(i,j,k))
               c(i) = A1(i,j,k) * (1.0_wp + 0.5_wp * Pi(i,j,k))
               z_k = 0.5_wp * (xp(3,i,j,k+1) - xp(3,i,j,k-1))
               z_ik = 0.25_wp * ( xp(3,i+1,j,k+1) - xp(3,i+1,j,k-1) &
                                 -xp(3,i-1,j,k+1) + xp(3,i-1,j,k-1) )
               d(i) = 2.0_wp * A2(i,j,k) * z_ik - A3(i,j,k) * ( xp(3,i,j,k+1) + &
                                                                xp(3,i,j,k-1) + &
                                                                Psi(i,j,k) * z_k )
           ENDIF
        ENDDO
        ! Call Thomas method solver
        CALL SY(1, imax, a, b, c, d)
        ! Update values at n+1 pseudo time
        DO i = 1, imax
           RMSres = RMSres + (d(i) - xp(3,i,j,k)) ** 2
           xp(3,i,j,k) = d(i)
        ENDDO
     ENDDO
     END SUBROUTINE ThomasLoop
  

  !-----------------------------------------------------------------------------!
     SUBROUTINE SY(IL,IU,BB,DD,AA,CC)
  !-----------------------------------------------------------------------------!
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: IL, IU
     REAL(KIND=wp), DIMENSION(IL:IU), INTENT(IN) :: AA, BB
     REAL(KIND=wp), DIMENSION(IL:IU), INTENT(INOUT) :: CC, DD
  
     INTEGER :: LP, I, J
     REAL(KIND=wp) :: R
  
     LP = IL + 1

     DO I = LP, IU
        R = BB(I) / DD(I-1)
        DD(I) = DD(I) - R*AA(I-1)
        CC(I) = CC(I) - R*CC(I-1)
     ENDDO

     CC(IU) = CC(IU)/DD(IU)
     DO I = LP, IU
        J = IU - I + IL
        CC(J) = (CC(J) - AA(J)*CC(J+1))/DD(J)
     ENDDO
     END SUBROUTINE SY  


  !-----------------------------------------------------------------------------!
     SUBROUTINE CopyFrontTOBack()
  !-----------------------------------------------------------------------------!
     USE SimulationVars_m, ONLY: imax, jmax, kmax, xp
     USE SimulationSetup_m, ONLY: UniformSpacing
  
     IMPLICIT NONE
     INTEGER :: i, k
  
     DO i = 2, imax - 1
        DO k = 2, kmax - 1
           xp(1,i,jmax,k) = xp(1,i,1,k)
           xp(2,i,jmax,k) = UniformSpacing(xp(2,i,jmax,1), xp(2,i,jmax,kmax), k, kmax)
           xp(3,i,jmax,k) = xp(3,i,1,k)
        ENDDO
     ENDDO

     END SUBROUTINE CopyFrontTOBack


  !-----------------------------------------------------------------------------!
     SUBROUTINE CalculateGridJacobian()
  !-----------------------------------------------------------------------------!
     USE SimulationVars_m, ONLY: imax, jmax, kmax, xp, inverseJacobian

     IMPLICIT NONE
     INTEGER :: i, j, k
     ! xgst, ygst, zgst: arbitrary ghost cell points
     REAL(KIND=wp) :: x_i, y_i, z_i, x_j, y_j, z_j, x_k, y_k, z_k, &
                      xgst, ygst, zgst

     DO i = 1, imax
        DO j = 1, jmax
           DO k = 1, kmax
              ! calculate x_i, y_i, z_i
              IF ( i == 1 ) THEN
                 xgst = xp(1,i,j,k) - (xp(1,i+1,j,k) - xp(1,i,j,k))
                 ygst = xp(2,i,j,k) - (xp(2,i+1,j,k) - xp(2,i,j,k))
                 zgst = xp(3,i,j,k) - (xp(3,i+1,j,k) - xp(3,i,j,k))
                 x_i = 0.5_wp * (xp(1,i+1,j,k) - xgst)
                 y_i = 0.5_wp * (xp(2,i+1,j,k) - ygst)
                 z_i = 0.5_wp * (xp(3,i+1,j,k) - zgst)
              ELSEIF ( i == imax ) THEN
                 xgst = xp(1,i,j,k) + (xp(1,i,j,k) - xp(1,i-1,j,k))
                 ygst = xp(2,i,j,k) + (xp(2,i,j,k) - xp(2,i-1,j,k))
                 zgst = xp(3,i,j,k) + (xp(3,i,j,k) - xp(3,i-1,j,k))
                 x_i = 0.5_wp * (xgst - xp(1,i-1,j,k))
                 y_i = 0.5_wp * (ygst - xp(2,i-1,j,k))
                 z_i = 0.5_wp * (zgst - xp(3,i-1,j,k))
              ELSE
                 x_i = 0.5_wp * (xp(1,i+1,j,k) - xp(1,i-1,j,k))
                 y_i = 0.5_wp * (xp(2,i+1,j,k) - xp(2,i-1,j,k))
                 z_i = 0.5_wp * (xp(3,i+1,j,k) - xp(3,i-1,j,k))
              ENDIF
              ! calculate x_j, y_j, z_j
              IF ( j == 1 ) THEN
                 xgst = xp(1,i,j,k) - (xp(1,i,j+1,k) - xp(1,i,j,k))
                 ygst = xp(2,i,j,k) - (xp(2,i,j+1,k) - xp(2,i,j,k))
                 zgst = xp(3,i,j,k) - (xp(3,i,j+1,k) - xp(3,i,j,k))
                 x_j = 0.5_wp * (xp(1,i,j+1,k) - xgst)
                 y_j = 0.5_wp * (xp(2,i,j+1,k) - ygst)
                 z_j = 0.5_wp * (xp(3,i,j+1,k) - zgst)
              ELSEIF ( j == jmax ) THEN
                 xgst = xp(1,i,j,k) + (xp(1,i,j,k) - xp(1,i,j-1,k))
                 ygst = xp(2,i,j,k) + (xp(2,i,j,k) - xp(2,i,j-1,k))
                 zgst = xp(3,i,j,k) + (xp(3,i,j,k) - xp(3,i,j-1,k))
                 x_j = 0.5_wp * (xgst - xp(1,i,j-1,k))
                 y_j = 0.5_wp * (ygst - xp(2,i,j-1,k))
                 z_j = 0.5_wp * (zgst - xp(3,i,j-1,k))
              ELSE
                 x_j = 0.5_wp * (xp(1,i,j+1,k) - xp(1,i,j-1,k))
                 y_j = 0.5_wp * (xp(2,i,j+1,k) - xp(2,i,j-1,k))
                 z_j = 0.5_wp * (xp(3,i,j+1,k) - xp(3,i,j-1,k))
              ENDIF
              ! calculate x_k, y_k, z_k
              IF ( k == 1 ) THEN
                 xgst = xp(1,i,j,k) - (xp(1,i,j,k+1) - xp(1,i,j,k))
                 ygst = xp(2,i,j,k) - (xp(2,i,j,k+1) - xp(2,i,j,k))
                 zgst = xp(3,i,j,k) - (xp(3,i,j,k+1) - xp(3,i,j,k))
                 x_k = 0.5_wp * (xp(1,i,j,k+1) - xgst)
                 y_k = 0.5_wp * (xp(2,i,j,k+1) - ygst)
                 z_k = 0.5_wp * (xp(3,i,j,k+1) - zgst)
              ELSEIF ( k == kmax ) THEN
                 xgst = xp(1,i,j,k) + (xp(1,i,j,k) - xp(1,i,j,k-1))
                 ygst = xp(2,i,j,k) + (xp(2,i,j,k) - xp(2,i,j,k-1))
                 zgst = xp(3,i,j,k) + (xp(3,i,j,k) - xp(3,i,j,k-1))
                 x_k = 0.5_wp * (xgst - xp(1,i,j,k-1))
                 y_k = 0.5_wp * (xp(2,i,j,k+1) - ygst)
                 z_k = 0.5_wp * (xp(3,i,j,k+1) - zgst)
              ELSEIF ( k == kmax ) THEN
                 xgst = xp(1,i,j,k) + (xp(1,i,j,k) - xp(1,i,j,k-1))
                 ygst = xp(2,i,j,k) + (xp(2,i,j,k) - xp(2,i,j,k-1))
                 zgst = xp(3,i,j,k) + (xp(3,i,j,k) - xp(3,i,j,k-1))
                 x_k = 0.5_wp * (xgst - xp(1,i,j,k-1))
                 y_k = 0.5_wp * (ygst - xp(2,i,j,k-1))
                 z_k = 0.5_wp * (zgst - xp(3,i,j,k-1))
              ELSE
                 x_k = 0.5_wp * (xp(1,i,j,k+1) - xp(1,i,j,k-1))
                 y_k = 0.5_wp * (xp(2,i,j,k+1) - xp(2,i,j,k-1))
                 z_k = 0.5_wp * (xp(3,i,j,k+1) - xp(3,i,j,k-1))
              ENDIF
              ! Calculate 1/J: Inverse of grid Jacobian           
              inverseJacobian(i,j,k) = x_i * (y_j * z_k - y_k * z_j) - &
                                       x_j * (y_i * z_k - y_k * z_i) + &
                                       x_k * (y_i * z_j - y_j * z_i)
           ENDDO
        ENDDO
     ENDDO

     END SUBROUTINE CalculateGridJacobian


  END MODULE


SimulationSetup.F90
-------------------

::

  !> \file SimulationSetup.F90
  !> \author Sayop Kim
  
  MODULE SimulationSetup_m
     USE Parameters_m, ONLY: wp
     IMPLICIT NONE

     PUBLIC :: InitializeCommunication, UniformSpacing, GridStretching

  CONTAINS

  !-----------------------------------------------------------------------------!
     SUBROUTINE InitializeCommunication()
  !-----------------------------------------------------------------------------!
        USE Parameters_m, ONLY: CODE_VER_STRING
        IMPLICIT NONE
  
        WRITE(*,'(a)') ""
        WRITE(*,'(a)') "CFD code Version: ", CODE_VER_STRING
     END SUBROUTINE InitializeCommunication


  !-----------------------------------------------------------------------------!
     FUNCTION UniformSpacing(xmin,xmax,indx,indxMax) RESULT(outcome)
  !-----------------------------------------------------------------------------!
        !Distribute interior grid points based on edge points' coordinates.
        !Linear Interpolateion is made by referring to (i,j,k) indices
        IMPLICIT NONE
        REAL(KIND=wp), INTENT(IN) :: xmin, xmax
        INTEGER, INTENT(IN) :: indx, indxMax
        REAL(KIND=wp) :: outcome, coef
        coef = REAL(indx - 1) / REAL(indxMax - 1)
        outcome = xmin + coef * (xmax - xmin)
     END FUNCTION UniformSpacing

  !-----------------------------------------------------------------------------!
     FUNCTION GridStretching(xmin,xmax,indx,indxMax,cy) RESULT(outcome)
  !-----------------------------------------------------------------------------!
        !Distribute interior grid points based on stretching coefficient
        !Interpolateion is made by referring to (i,j,k) indices

        IMPLICIT NONE
        REAL(KIND=wp) :: cy
        REAL(KIND=wp), INTENT(IN) :: xmin, xmax
        INTEGER, INTENT(IN) :: indx, indxMax
        REAL(KIND=wp) :: outcome, coef
        coef = log(1.0_wp + (exp(-cy) - 1.0_wp) * REAL(indx - 1) / REAL(indxMax - 1))
        outcome = xmin - coef * (xmax - xmin) / cy
     END FUNCTION GridStretching

  END MODULE SimulationSetup_m
                                            
