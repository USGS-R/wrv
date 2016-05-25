C----------------------------------------------------------------------------------------
      SUBROUTINE SCLN2COND1RP
C     ******************************************************************
C      ALLOCATE SPACE AND READ PROPERTIES FOR CONDUIT TYPE CLNs
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CLN1MODULE
      USE GLOBAL, ONLY: IUNIT,IOUT,NEQS,NODES,NROW,NCOL,IFREFM,IUNSTR,
     *                  INCLN
      CHARACTER*200 LINE
C----------------------------------------------------------------------------------------
C12------ALLOCATE SPACE FOR CONDUIT TYPE CLNs AND PREPARE TO REFLECT INPUT TO LISTING FILE
      ALLOCATE (ACLNCOND(NCONDUITYP,3))
      WRITE(IOUT,23)
23    FORMAT(/20X,' CONDUIT NODE INFORMATION'/
     1  20X,40('-')/5X,'CONDUIT NODE',8X,'RADIUS',3X,'CONDUIT SAT K',
     1  /5X,12('-'),8X,6('-'),3X,13('-'))
C13------READ CONDUIT PROPERTIES FOR EACH CONDUIT TYPE
      DO I=1,NCONDUITYP
        CALL URDCOM(INCLN,IOUT,LINE)
        IF(IFREFM.EQ.0) THEN
          READ(LINE,'(I10,2F10.3)') IFNO,FRAD,CONDUITK
          LLOC=71
        ELSE
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSRAD,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,CONDUITK,IOUT,INCLN)
          FRAD = FSRAD
        END IF
C
C14--------FILL PROPERTY ARRAYS WITH READ AND PREPARE INFORMATION
        ACLNCOND(I,1) = IFNO
        ACLNCOND(I,2) = FRAD
        ACLNCOND(I,3) = CONDUITK
        WRITE(IOUT,24)IFNO,FRAD,CONDUITK
24      FORMAT(5X,I10,2(1X,E15.6))
      ENDDO
C--------RETURN
      RETURN
      END
C----------------------------------------------------------------------------      
CADD      ALLOCATE SPACE AND READ PROPERTIES FOR OTHER TYPES OF CLNs HERE
C----------------------------------------------------------------------------
      SUBROUTINE CLNA(IC,AREAF)
C--------COMPUTE X-SECTIONAL FLOW AREA FOR NODE
      USE CLN1MODULE, ONLY:  ACLNCOND,NCONDUITYP
      DOUBLE PRECISION AREAF,RADFSQ
C--------------------------------------------------------------------------------------
      PI = 3.1415926
      IF(IC.LE.NCONDUITYP)THEN
C1-------CLN NODE IS A CONDUIT
        RADFSQ = ACLNCOND(IC,2)**2
        AREAF = PI * RADFSQ
      ELSEIF(IC.GT.NCONDUITYP)THEN
C2------ADD COMPUTATION FOR AREA FOR OTHER CLN TYPES HERE
CADD      ADD COMPUTATION FOR AREA FOR OTHER TYPES OF CLNs HERE
      ENDIF
C7------RETURN
      RETURN
      END
C--------------------------------------------------------------------------------------
      SUBROUTINE CLNK(IC,FK)
C--------COMPUTE EFFECTIVE HYDRAULIC CONDUCTIVITY FOR NODE
      USE CLN1MODULE, ONLY:  ACLNCOND,NCONDUITYP
      DOUBLE PRECISION FK,RADFSQ
C--------------------------------------------------------------------------------------
      IF(IC.LE.NCONDUITYP)THEN
C1-------CLN NODE IS A CONDUIT
        RADFSQ = ACLNCOND(IC,2)**2
        CONDUITK = ACLNCOND(IC,3)
        FK = CONDUITK * RADFSQ
      ELSEIF(IC.GT.NCONDUITYP)THEN
C2------ADD COMPUTATION FOR K FOR OTHER CLN TYPES HERE
CADD     ADD COMPUTATION FOR K FOR OTHER CLN TYPES HERE
      ENDIF
C7------RETURN
      RETURN
      END
C--------------------------------------------------------------------------------------
      SUBROUTINE CLNR(IC,FRAD)
C--------COMPUTE RADIUS FOR CONNECTION OF CLN SEGMENT TO 3-D GRID WITH THEIM EQUATION
      USE CLN1MODULE, ONLY: ACLNCOND,NCONDUITYP
      DOUBLE PRECISION FRAD
C--------------------------------------------------------------------------------------
      IF(IC.LE.NCONDUITYP)THEN
C1-------CLN NODE IS A CONDUIT
        FRAD = ACLNCOND(IC,2)
      ELSEIF(IC.GT.NCONDUITYP)THEN
C2------ADD COMPUTATION FOR EFFECTIVE RADIUS FOR OTHER CLN TYPES HERE
CADD     ADD COMPUTATION FOR RADIUS FOR OTHER CLN TYPES HERE
      ENDIF
C7------RETURN
      RETURN
      END
C--------------------------------------------------------------------------------------
      SUBROUTINE CLNP(IC,FPER)
C--------COMPUTE EFFECTIVE PERIMETER FOR CONNECTION OF CLN SEGMENT TO 3-D GRID
      USE CLN1MODULE, ONLY: ACLNCOND,NCONDUITYP
      DOUBLE PRECISION FPER
C--------------------------------------------------------------------------------------
      PI = 3.1415926
      IF(IC.LE.NCONDUITYP)THEN
C1-------CLN NODE IS A CONDUIT
        FPER = 2 * PI * ACLNCOND(IC,2)
      ELSEIF(IC.GT.NCONDUITYP)THEN
C2------ADD COMPUTATION FOR PERIMETER FOR OTHER CLN TYPES HERE
CADD      ADD COMPUTATION FOR PERIMETER FOR OTHER CLN TYPES HERE      
      ENDIF
C7------RETURN
      RETURN
      END
C--------------------------------------------------------------------------------
      SUBROUTINE CLNPW(ICLN,HD,PERIW)
C--------COMPUTE WETTED X-SECTIONAL PERIMETER FOR NODE
      USE CLN1MODULE, ONLY:  ACLNCOND,NCONDUITYP,ACLNNDS
      USE GLOBAL, ONLY: NODES,HNEW
      DOUBLE PRECISION PERIW,RADFSQ,HD,FRAD,BBOT,DEPTH
C--------------------------------------------------------------------------------------
      N = ACLNNDS(ICLN,1)
      IC = ACLNNDS(ICLN,2)
      IF(IC.LE.NCONDUITYP)THEN
C1-------CLN NODE IS A CONDUIT
        PI = 3.1415926
        FRAD = ACLNCOND(IC,2)
        BBOT = ACLNNDS(ICLN,5)
        DEPTH = HD - BBOT
        IF(DEPTH.LE.0)THEN
          PERIW = 0.0
        ELSEIF(DEPTH.LE.FRAD)THEN
          PERIW = 2.0*FRAD*ACOS((FRAD-DEPTH)/FRAD)
        ELSEIF(DEPTH.LE.2.0*FRAD)THEN
          PERIW = 2.0*FRAD*(PI - ACOS((DEPTH-FRAD)/FRAD))
        ELSE
          PERIW = 2* PI *FRAD
        ENDIF
      ELSEIF(IC.GT.NCONDUITYP)THEN
C2------ADD COMPUTATION FOR WETTED X-SECTIONAL PERIMETER FOR OTHER CLN TYPES HERE
CADD      ADD COMPUTATION FOR WETTED X-SECTIONAL PERIMETER FOR OTHER CLN TYPES HERE    
      ENDIF
C
C5------RETURN.
      RETURN
      END
C-------------------------------------------------------------------------------
      SUBROUTINE CLNAW(ICLN,HD,AREAW)
C--------COMPUTE WETTED X-SECTIONAL FLOW AREA FOR NODE
      USE CLN1MODULE, ONLY:  ACLNCOND,NCONDUITYP,ACLNNDS
      USE GLOBAL, ONLY: NODES,HNEW
      DOUBLE PRECISION AREAW,RADFSQ,HD,FRAD,BBOT,DEPTH
C--------------------------------------------------------------------------------------
      N = ACLNNDS(ICLN,1)
      IC = ACLNNDS(ICLN,2)
      IF(IC.LE.NCONDUITYP)THEN
C1-------CLN NODE IS A CONDUIT
        PI = 3.1415926
        FRAD = ACLNCOND(IC,2)
        BBOT = ACLNNDS(ICLN,5)
        DEPTH = HD - BBOT
        IF(DEPTH.LE.0)THEN
          AREAW = 0.0
        ELSEIF(DEPTH.LE.FRAD)THEN
          AREAW = FRAD*FRAD*ACOS((FRAD-DEPTH)/FRAD) - (FRAD-DEPTH)*
     1         SQRT(FRAD*FRAD - (FRAD-DEPTH)**2)
        ELSEIF(DEPTH.LE.2.0*FRAD)THEN
          AREAW = FRAD*FRAD*(PI - ACOS((DEPTH-FRAD)/FRAD))
     1    - (FRAD-DEPTH) * SQRT(FRAD*FRAD - (FRAD-DEPTH)**2)
        ELSE
          AREAW = PI *FRAD*FRAD
        ENDIF
      ELSEIF(IC.GT.NCONDUITYP)THEN
C2------ADD COMPUTATION FOR WETTED X-SECTIONAL FLOW AREA FOR OTHER CLN TYPES HERE
CADD      ADD COMPUTATION FOR WETTED X-SECTIONAL FLOW AREA FOR OTHER CLN TYPES HERE
      ENDIF
C
C5------RETURN.
      RETURN
      END
C-----------------------------------------------------------------------

