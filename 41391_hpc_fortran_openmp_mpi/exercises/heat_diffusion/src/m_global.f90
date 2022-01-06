MODULE m_global

    IMPLICIT none
 
    ! ------------------------------------------------- !
    ! INPUT PARAMETERS                                  !
    ! ------------------------------------------------- !
    INTEGER, PARAMETER   :: Nx = 21, Ny = 21                ! nx: amount of cells in x-dir, ny: amount of cells in y-dir
    INTEGER, PARAMETER   :: Lx = 1, Ly = 1                  ! box dimensions (Lx: length in x-dir, Ly: length in y-dir)
    INTEGER, PARAMETER   :: D = 1                           ! D: diffusion constant
    INTEGER, PARAMETER   :: nsteps = 200                    ! nsteps: amount of time steps made

    ! ------------------------------------------------- !
    ! DATA EXTRACT PARAMETERS                           !
    ! ------------------------------------------------- !
    CHARACTER(LEN = 12) :: output_name = 'Tfield'

    ! ------------------------------------------------- !
    ! DEPENDENT PARAMETERS                              !
    ! ------------------------------------------------- !
    REAL, PARAMETER      :: dx = Lx / REAL(Nx - 1)          ! dx: cell width in the x-dir
    REAL, PARAMETER      :: dy = Ly / REAL(Ny - 1)          ! dy: cell width in the y-dir
    REAL, PARAMETER      :: rdx2 = 1/(dx ** 2)              ! rdx2: 1/(dx ** 2), to aid setting up the equations
    REAL, PARAMETER      :: rdy2 = 1/(dy ** 2)              ! rdy2: see above
    REAL, PARAMETER      :: dt = MIN(dx, dy) ** 2 / (4*D)   ! dt: time-step size, satisfying the Fourier limit

    ! ------------------------------------------------- !
    ! SHARED FIELD ARRAYS                               !
    ! ------------------------------------------------- !
    REAL, DIMENSION(:,:), ALLOCATABLE :: tfield             ! tfield: empty array to contain the temperature field
    
 END MODULE
 