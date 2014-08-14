C*GREXEC -- PGPLOT device handler dispatch routine
C+
      SUBROUTINE GREXEC(IDEV,IFUNC,RBUF,NBUF,CHR,LCHR)
      INTEGER IDEV, IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C DO NOT MODIFY THIS ROUTINE.
C You should always create a new version by re-executing
C the command file NEWEXEC.COM.
C---
      INTEGER NDEV
      PARAMETER (NDEV=6)
      CHARACTER*10 MSG
C---
      GOTO(1,2,3,4,5,6) IDEV
      IF (IDEV.EQ.0) THEN
          RBUF(1) = NDEV
          NBUF = 1
      ELSE
          WRITE (MSG,'(I10)') IDEV
          CALL GRQUIT('Unknown device code in GREXEC: '//MSG)
      END IF
      RETURN
C---
    1 CALL SSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    2 CALL NUDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    3 CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,1)
      RETURN
    4 CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,2)
      RETURN
    5 CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,3)
      RETURN
    6 CALL GLDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,1)
      RETURN
C
      END
