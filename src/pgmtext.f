C*PGMTEXT -- non-standard alias for PGMTXT
C+
      SUBROUTINE PGMTEXT (SIDE, DISP, COORD, FJUST, TEXT)
      CHARACTER*(*) SIDE, TEXT
      REAL DISP, COORD, FJUST
C
C See description of PGMTXT.
C--
      CALL PGMTXT (SIDE, DISP, COORD, FJUST, TEXT)
      END
