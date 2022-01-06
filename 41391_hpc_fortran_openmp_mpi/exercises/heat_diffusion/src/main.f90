PROGRAM main
   
   ! ------------------------------------------------- !
   ! MODULES                                           !
   ! ------------------------------------------------- !
   USE m_global         ! contains global-scoped parameters
   USE m_arrays         ! contains array manipulation functions
   USE m_io             ! contains input / output functions
   USE m_heat_diffusion ! contains heat diffusion time simulation and field initialization


   ! ------------------------------------------------- !
   ! INITIALIZATION                                    !
   ! ------------------------------------------------- !
   IMPLICIT NONE

   ! initialize time and indices
   INTEGER :: info

   ! allocate memory to the global field
   CALL alloc(tfield, Nx, Ny, info)

   ! initialize the temperature field according to BC
   CALL init_tfield(tfield, initial_temperature = 0.0)


   ! ------------------------------------------------- !
   ! HEAT DIFFUSION                                    !
   ! ------------------------------------------------- !
   CALL simulate_diffusion(tfield, nsteps, verbose = 0)


   ! ------------------------------------------------- !
   ! EXTRACT FIELD(S)                                  !
   ! ------------------------------------------------- !
   CALL extract_field(tfield, output_name)

END PROGRAM main
