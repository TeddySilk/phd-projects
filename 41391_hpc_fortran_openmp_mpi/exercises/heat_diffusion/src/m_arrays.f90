MODULE m_arrays
   IMPLICIT NONE

   REAL, DIMENSION(:, :), POINTER :: w

CONTAINS

   ! alloc() (re)allocates a 2D field array
   SUBROUTINE alloc(c, n, m, info)

      REAL, DIMENSION(:, :) :: c
      INTEGER, INTENT(IN)  :: n, m
      INTEGER, INTENT(OUT) :: info

      IF (.NOT.ASSOCIATED(c)) THEN
         ALLOCATE(c(n, m))
      ELSE
         ALLOCATE(w(n, m))

         DISALLOCATE
      ENDIF

   END SUBROUTINE alloc

   FUNCTION copy(old_array) RESULT(new_array)


   END FUNCTION copy

   SUBROUTINE cp_field(field1, field2)
      REAL, DIMENSION(:,:) :: field1, field2
      INTEGER :: i, j
      INTEGER, DIMENSION(2) :: array_size

      array_size = shape(field1)

      DO j = 1, array_size(2)
         DO i = 1, array_size(1)
            field2(i, j) = field1(i, j)
         ENDDO
      ENDDO

   END SUBROUTINE cp_field
END MODULE
