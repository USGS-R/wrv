      SUBROUTINE SDIS2GLO8AR (IUDIS,IOUT)
C     *****************************************************************
C     READ GLOBAL DATA ALLOCATE SPACE FOR 3-D DOMAIN PARAMETERS, 
C     AND READ CONFINING BED INFORMATION ARRAY, LAYCBD
C     *****************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,IXSEC,
     1            LENUNI,IUNIT,ITRSS,NODES,NODLAY,LAYCBD,INBAS,IVC,
     2            PERLEN,NSTP,TSMULT,ISSFLG,BOT,TOP,IUNSTR,AMAT,AREA,
     3            IA,JA,JAS,ISYM,NJA,NJAG,IVSD,DELC,DELR,IPRCONN,
     4            IBOUND,MXNODLAY,ICONCV,NOCVCO,NEQS,IFREFM,IDSYMRD,
     5            IATMP,NJATMP,NOVFC 
      CHARACTER*200 LINE      
C
C1------Check for existence of discretization file
      INDIS=IUNIT(IUDIS)
      IF(INDIS.LE.0) THEN
         WRITE(IOUT,*) ' DIS file must be specified for MODFLOW to run'
         CALL USTOP(' ')
      END IF
C2-------IDENTIFY PACKAGE
      WRITE(IOUT,11) INDIS
   11 FORMAT(1X,/1X,'DIS -- UNSTRUCTURED GRID DISCRETIZATION PACKAGE,',
     1  ' VERSION 1 : 5/17/2010 - INPUT READ FROM UNIT ',I4)
C
C
C3------Read comments and the first line following the comments.
      CALL URDCOM(INDIS,IOUT,LINE)
C
C4------Get the grid size, stress periods, and options like
C4------ITMUNI, and LENUNI from first line.
      LLOC=1
      IVSD=0
      IF(IUNSTR.EQ.0)IVSD = -1
      IF(IUNSTR.EQ.0)THEN
C4A-----FOR STRUCTURED GRID READ NLAY, NROW AND NCOL
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLAY,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NROW,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCOL,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPER,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMUNI,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LENUNI,R,IOUT,INDIS)      
        NODES = NCOL*NROW*NLAY
C
        WRITE(IOUT,15) NLAY,NROW,NCOL
   15   FORMAT(1X,I4,' LAYERS',I10,' ROWS',I10,' COLUMNS')
      ELSE
C4B------FOR UNSTRUCTURED GRID READ NUMBER OF NODES, LAYERS AND CONNECTIVITY SIZES
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NODES,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLAY,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NJAG,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IVSD,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPER,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMUNI,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LENUNI,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDSYMRD,R,IOUT,INDIS)
C
        NJA = NJAG
        WRITE(IOUT,16) NODES,NLAY,NJAG,IVSD
   16   FORMAT(1X,I10,' NODES',I10,' NLAY',I10,' NJAG',
     *  2X,'VERT. SUBDISCRETIZATION INDEX, IVSD = ',I2)
        WRITE(IOUT,17)IDSYMRD
17      FORMAT(1X,'INDEX FOR INPUT OF UNSTRUCTURED, FINITE-VOLUME',1X,
     1   'CONNECTIVITY INFORMATION, IDSYMRD = ',I3)
      ENDIF
C
      WRITE(IOUT,20) NPER
   20 FORMAT(1X,I4,' STRESS PERIOD(S) IN SIMULATION')
C
C5------SELECT AND PRINT A MESSAGE SHOWING TIME UNIT.
      IF(ITMUNI.LT.0 .OR. ITMUNI.GT.5) ITMUNI=0
      IF(ITMUNI.EQ.0) THEN
         WRITE(IOUT,30)
   30    FORMAT(1X,'MODEL TIME UNIT IS UNDEFINED')
      ELSE IF(ITMUNI.EQ.1) THEN
         WRITE(IOUT,40)
   40    FORMAT(1X,'MODEL TIME UNIT IS SECONDS')
      ELSE IF(ITMUNI.EQ.2) THEN
         WRITE(IOUT,50)
   50    FORMAT(1X,'MODEL TIME UNIT IS MINUTES')
      ELSE IF(ITMUNI.EQ.3) THEN
         WRITE(IOUT,60)
   60    FORMAT(1X,'MODEL TIME UNIT IS HOURS')
      ELSE IF(ITMUNI.EQ.4) THEN
         WRITE(IOUT,70)
   70    FORMAT(1X,'MODEL TIME UNIT IS DAYS')
      ELSE
         WRITE(IOUT,80)
   80    FORMAT(1X,'MODEL TIME UNIT IS YEARS')
      END IF
C
C6------SELECT AND PRINT A MESSAGE SHOWING LENGTH UNIT.
      IF(LENUNI.LT.0 .OR. LENUNI.GT.3) LENUNI=0
      IF(LENUNI.EQ.0) THEN
         WRITE(IOUT,90)
   90    FORMAT(1X,'MODEL LENGTH UNIT IS UNDEFINED')
      ELSE IF(LENUNI.EQ.1) THEN
         WRITE(IOUT,91)
   91    FORMAT(1X,'MODEL LENGTH UNIT IS FEET')
      ELSE IF(LENUNI.EQ.2) THEN
         WRITE(IOUT,93)
   93    FORMAT(1X,'MODEL LENGTH UNIT IS METERS')
      ELSE IF(LENUNI.EQ.3) THEN
         WRITE(IOUT,95)
   95    FORMAT(1X,'MODEL LENGTH UNIT IS CENTIMETERS')
      END IF
C
C7----ALLOCATE SPACE FOR TEMPORAL INFORMATION AND CONFINING LAYERS
      ALLOCATE(LAYCBD(NLAY))
      ALLOCATE(BOT(NODES))
      ALLOCATE(TOP(NODES))
      ALLOCATE (PERLEN(NPER),NSTP(NPER),TSMULT(NPER),ISSFLG(NPER))
      ALLOCATE (ICONCV,NOCVCO,NOVFC)
C
C8----SET FLAGS AND CONFINING INFORMATION     
      ICONCV=1
      NOCVCO=1
      NOVFC=0
C
C9-------Read confining bed information
      READ(INDIS,*) (LAYCBD(K),K=1,NLAY)
      LAYCBD(NLAY)=0
      WRITE(IOUT,*) ' Confining bed flag for each layer:'
      WRITE(IOUT,'(20I4)') (LAYCBD(K),K=1,NLAY)
C
C10------Count confining beds and setup LAYCBD to be the confining
C10------bed number for each layer.
      NCNFBD=0
      DO 100 K=1,NLAY
      IF(LAYCBD(K).NE.0) THEN
         NCNFBD=NCNFBD+1
         LAYCBD(K)=NCNFBD
      END IF
  100 CONTINUE
      NBOTM=NLAY+NCNFBD      
C
C11------RETURN.
      RETURN
      END      
C
C---------------------------------------------------------------------------------------------
      SUBROUTINE SGWF2BAS8SR
C     ******************************************************************
C     Read IBOUND, HNOFLO and initial heads for structured grid input
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,NJA,IVSD,
     1                     LENUNI,IXSEC,ITRSS,INBAS,IFREFM,NODES,IOUT,
     2                     IUNIT,NIUNIT,HNEW,LAYHDT,LAYHDS,NODLAY,
     3                     PERLEN,NSTP,TSMULT,ISSFLG,IUNSTR,MXNODLAY,
     4                     HOLD,IBOUND,RHS,AMAT,BUFF,STRT,
     5                     IDEALLOC_LPF,IDEALLOC_HY,
     6                     ITRNSP,Sn,So,NEQS,iunsat
      USE GWFBASMODULE,ONLY:MSUM,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN,
     1                 LBHDSV,LBDDSV,LBBOSV,IBUDFL,ICBCFL,IHDDFL,ISPCFL,
     2                 IAUXSV,IBDOPT,IPRTIM,IPEROC,ITSOC,ICHFLG,IFRCNVG,
     3                 DELT,PERTIM,TOTIM,HNOFLO,CHEDFM,CDDNFM,
     4                 CBOUFM,VBVL,VBNM,ISPCFM,ISPCUN,CSPCFM
C
      CHARACTER*4 CUNIT(NIUNIT)
      CHARACTER*80 HEADNG(2)
      CHARACTER*200 LINE
C
      INTEGER, DIMENSION(:,:,:),    ALLOCATABLE  ::ITMP
      REAL, DIMENSION(:,:,:),ALLOCATABLE  ::HTMP
      CHARACTER*24 ANAME(2)
      DATA ANAME(1) /'          BOUNDARY ARRAY'/
      DATA ANAME(2) /'            INITIAL HEAD'/
C     ------------------------------------------------------------------
C
C1------READ BOUNDARY ARRAY(IBOUND).
      ALLOCATE(ITMP(NCOL,NROW,NLAY))
      IF(IXSEC.EQ.0) THEN
         DO 290 K=1,NLAY
           KK=K
           CALL U2DINT(ITMP(1,1,KK),ANAME(1),NROW,NCOL,KK,INBAS,IOUT)
  290    CONTINUE
      ELSE
         CALL U2DINT(ITMP(1,1,1),ANAME(1),NLAY,NCOL,-1,INBAS,IOUT)
      END IF
C
C2------COPY ITMP INTO IBOUND
      N=0
      DO 300 K=1,NLAY
      DO 300 I=1,NROW
      DO 300 J=1,NCOL
        N=N+1
        IBOUND(N) = ITMP(J,I,K)
  300 CONTINUE
      DEALLOCATE(ITMP)
C
C------------------------------------------------------------------------
C3------READ AND PRINT HEAD VALUE TO BE PRINTED FOR NO-FLOW CELLS.
      IF(IFREFM.EQ.0) THEN
         READ(INBAS,'(F10.0)') HNOFLO
      ELSE
         READ(INBAS,*) HNOFLO
      END IF
      WRITE(IOUT,3) HNOFLO
    3 FORMAT(1X,/1X,'AQUIFER HEAD WILL BE SET TO ',1PG12.5,
     1       ' AT ALL NO-FLOW NODES (IBOUND=0).')
C
C-----------------------------------------------------------------------
C4------READ INITIAL HEADS.
      ALLOCATE(HTMP(NCOL,NROW,NLAY))
      IF(IXSEC.EQ.0) THEN
        DO 350 K=1,NLAY
          KK=K
          CALL U2DREL(HTMP(1,1,KK),ANAME(2),NROW,NCOL,KK,INBAS,IOUT)
  350    CONTINUE
      ELSE
        CALL U2DREL(HTMP(1,1,1),ANAME(2),NLAY,NCOL,-1,INBAS,IOUT)
      END IF
C
C5------COPY INITIAL HEADS FROM HTMP TO HNEW AND TO STRT.
      N=0
      DO 400 K=1,NLAY
      DO 400 I=1,NROW
      DO 400 J=1,NCOL
        N=N+1
        HNEW(N)=HTMP(J,I,K)
        STRT(N) = HNEW(N)
        IF(IBOUND(N).EQ.0) HNEW(N)=HNOFLO
  400 CONTINUE
      DEALLOCATE(HTMP)
C---------------------------------------------------------------------
C6------RETURN.
      RETURN
      END
C
C---------------------------------------------------------------------------------------------
      SUBROUTINE SGWF2BAS8UR
C     ******************************************************************
C     Read IBOUND, HNOFLO and initial heads for unstructured grid input
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,NJA,IVSD,
     1                     LENUNI,IXSEC,ITRSS,INBAS,IFREFM,NODES,IOUT,
     2                     IUNIT,NIUNIT,HNEW,LAYHDT,LAYHDS,NODLAY,
     3                     PERLEN,NSTP,TSMULT,ISSFLG,IUNSTR,MXNODLAY,
     4                     HOLD,IBOUND,RHS,AMAT,BUFF,STRT,
     5                     IDEALLOC_LPF,IDEALLOC_HY,
     6                     ITRNSP,Sn,So,NEQS,iunsat
      USE GWFBASMODULE,ONLY:MSUM,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN,
     1                 LBHDSV,LBDDSV,LBBOSV,IBUDFL,ICBCFL,IHDDFL,ISPCFL,
     2                 IAUXSV,IBDOPT,IPRTIM,IPEROC,ITSOC,ICHFLG,IFRCNVG,
     3                 DELT,PERTIM,TOTIM,HNOFLO,CHEDFM,CDDNFM,
     4                 CBOUFM,VBVL,VBNM,ISPCFM,ISPCUN,CSPCFM
C
      CHARACTER*4 CUNIT(NIUNIT)
      CHARACTER*80 HEADNG(2)
      CHARACTER*200 LINE
C
      REAL, DIMENSION(:),ALLOCATABLE  ::HTMP1
      CHARACTER*24 ANAME(2)
      DATA ANAME(1) /'          BOUNDARY ARRAY'/
      DATA ANAME(2) /'            INITIAL HEAD'/
C     ------------------------------------------------------------------
C
C1------READ BOUNDARY ARRAY(IBOUND).
      DO K = 1,NLAY
        KK = K
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
        CALL U1DINT(IBOUND(NSTRT),ANAME(1),NDSLAY,K,INBAS,IOUT)
      ENDDO
C
C----------------------------------------------------------------------
C2------READ AND PRINT HEAD VALUE TO BE PRINTED FOR NO-FLOW CELLS.
      IF(IFREFM.EQ.0) THEN
         READ(INBAS,'(F10.0)') HNOFLO
      ELSE
         READ(INBAS,*) HNOFLO
      END IF
      WRITE(IOUT,3) HNOFLO
    3 FORMAT(1X,/1X,'AQUIFER HEAD WILL BE SET TO ',G12.5,
     1       ' AT ALL NO-FLOW NODES (IBOUND=0).')
C
C-----------------------------------------------------------------------
C3------READ INITIAL HEADS.
      ALLOCATE(HTMP1(Nodes))
      DO K=1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        CALL U1DREL(Htmp1(NSTRT),ANAME(2),NNDLAY-NSTRT+1,K,INBAS,IOUT)
      ENDDO
      DO N=1,NODES
        HNEW(N) = HTMP1(N)
        STRT(N) = HNEW(N)
        IF(IBOUND(N).EQ.0) HNEW(N)=HNOFLO
      ENDDO
      DEALLOCATE(HTMP1)
C
C----------------------------------------------------------------------
C4------RETURN.
      RETURN
      END
C---------------------------------------------------------------------
      SUBROUTINE SGWF2DIS8SR(IOUT,INDIS)
C     *****************************************************************
C     SET NODLAY ARRAY, READ/PREPARE GEOMETRIC PARAMETERS, AND COMPUTE
C     MATRIX CONNECTIVITY FOR STRUCTURED FD GRID WITH MODFLOW-2005 INPUT STRUCTURE 
C     *****************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,IXSEC,
     1              LENUNI,IUNIT,ITRSS,NODES,NODLAY,LAYCBD,INBAS,IVC,
     2              PERLEN,NSTP,TSMULT,ISSFLG,BOT,TOP,IUNSTR,AMAT,AREA,
     3              IA,JA,ISYM,NJA,NJAS,IVSD,PGF,FAHL,CL1,CL2,DELC,DELR,
     4              IBOUND,MXNODLAY,ICONCV,NOCVCO,NEQS,IFREFM,IPRCONN
C
      DOUBLE PRECISION, DIMENSION(:,:,:),ALLOCATABLE  ::BOTM
      INTEGER, DIMENSION(:),    ALLOCATABLE  ::LBOTM
      REAL, DIMENSION(:),    ALLOCATABLE  ::AREATEMP
      CHARACTER*200 LINE
      CHARACTER*24 ANAME(5)
      DATA ANAME(1) /'                    DELR'/
      DATA ANAME(2) /'                    DELC'/
      DATA ANAME(3) /'TOP ELEVATION OF LAYER 1'/
      DATA ANAME(4) /'  MODEL LAYER BOTTOM EL.'/
      DATA ANAME(5) /'BOT. EL. OF QUASI-3D BED'/
C
C     ------------------------------------------------------------------
C1------FILL NODLAY ARRAY WITH LAST NODE NUMBER FOR EACH LAYER AND SET MXNODLAY
      ALLOCATE(NODLAY(0:NLAY))
C-------COMPUTE LAST NODE NUMBER FOR EACH LAYER FOR STRUCTURED GRID
      NNDLAY = NCOL*NROW
      NODLAY(0) = 0
      DO K=1,NLAY
        NODLAY(K) = NODLAY(K-1) + NNDLAY
      ENDDO
      MXNODLAY = NNDLAY
C2---ALLOCATE AND READ DELR AND DELC FOR STRUCTURED GRID
      ALLOCATE (DELR(NCOL))
      ALLOCATE (DELC(NROW))
C
C-------Read the DELR and DELC arrays.
      K = 0
      CALL U1DREL(DELR,ANAME(1),NCOL,K,INDIS,IOUT)
      CALL U1DREL(DELC,ANAME(2),NROW,K,INDIS,IOUT)
C
C3------Read the top elevation of layer 1.
C3------because BOTM(J,I,0) contains the top elevation of layer 1.
      ALLOCATE (BOTM(NCOL,NROW,0:NBOTM))
      CALL U2DREL8(BOTM(1,1,0),ANAME(3),NROW,NCOL,0,INDIS,IOUT)
C-------PUT TOP ELEV OF LAYER 1 INTO TOP ARRAY
      N=0
      DO I=1,NROW
      DO J=1,NCOL
        N=N+1
        TOP(N) = BOTM(J,I,0)
      ENDDO
      ENDDO
C
C4-----Allocate LBOTM and set values; setup the pointer to each layer's
C4------bottom array (LBOTM)
      ALLOCATE(LBOTM(NLAY))
      NCNFBD=0
      DO 100 K=1,NLAY
      LBOTM(K)=K+NCNFBD
      IF(LAYCBD(K).NE.0) THEN
         NCNFBD=NCNFBD+1
         LAYCBD(K)=NCNFBD
      END IF
  100 CONTINUE
      NBOTM=NLAY+NCNFBD      
C
C5-----Read the bottom elevations of all layers and fill TOP and BOT arrays.
      DO 120 K=1,NLAY
        KK=K
        CALL U2DREL8
     *  (BOTM(1,1,LBOTM(K)),ANAME(4),NROW,NCOL,KK,INDIS,IOUT)
        IF(LAYCBD(K).NE.0) CALL U2DREL8(BOTM(1,1,LBOTM(K)+1),ANAME(5),
     1    NROW,NCOL,KK,INDIS,IOUT)
C
        N=0
        IF(K.NE.1)THEN
           N = NODLAY(K-1)
        ENDIF
        DO I=1,NROW
        DO J=1,NCOL
          N=N+1
          BOT(N) = BOTM(J,I,LBOTM(K))
        ENDDO
        ENDDO
C
        IF(K.NE.NLAY)THEN
          N=NODLAY(K)
          KY = K
          IF(LAYCBD(K).NE.0) KY = LBOTM(K)+1
          DO I=1,NROW
          DO J=1,NCOL
            N=N+1
            TOP(N) = BOTM(J,I,KY)
          ENDDO
          ENDDO
        ENDIF
C
  120 CONTINUE
C6------COMPUTE AND WRITE AREA FOR ALL NODES OF A LAYER
      IF(IPRCONN.NE.0) WRITE(IOUT,*)'AREA IS BELOW, 22G15.6'
      N = 0
C      ALLOCATE(AREATEMP(NCOL))
      DO I=1,NROW
      NC = 0
      DO J=1,NCOL
         N=N+1
         NC = NC+1
         AREA(N) = DELR(J)*DELC(I)
C         AREATEMP(NC) = AREA(N)
      ENDDO
C      WRITE(IOUT,155)(AREATEMP(NC),NC=1,NCOL)
      ENDDO
C      DEALLOCATE (AREATEMP)
      IF(IPRCONN.NE.0)
     *  WRITE(IOUT,155) (AREA(N),N=1,NCOL*NROW)
155   FORMAT(1P,22G15.6)
C
      DO K=2,NLAY
      DO IJ=1,NNDLAY
        N = (K-1)*NNDLAY + IJ
        AREA(N) = AREA(IJ)
      ENDDO
      ENDDO
C7------FILL IA AND NJA
      CALL FILLIA
      ALLOCATE(JA(NJA))                  !setup JA array
C
C8-----FILL JA ARRAY
      CALL FILLJA
C9-----WRITE IA AND JA ARRAYS OF GROUNDWATER CONNECTION
      IF(IPRCONN.NE.0)THEN
        WRITE(IOUT,*)'IA IS BELOW, 40I10'
        WRITE(IOUT,55)(IA(I),I=1,NEQS+1)
        WRITE(IOUT,*)'NJA = ',NJA,'NJAS =',NJA
        WRITE(IOUT,*)'JA IS BELOW, 40I10'
        WRITE(IOUT,55)(JA(J),J=1,NJA)
55      FORMAT(40I10)
      ENDIF      
C
C10-----DEALLOCATE UNNEEDED ARRAYS
      DEALLOCATE(BOTM)
      DEALLOCATE(LBOTM)
C
C----------------------------------------------------------------------
C11-----RETURN.
      RETURN
      END
C---------------------------------------------------------------------
      SUBROUTINE SGWF2DIS8UR(IOUT,INDIS)
C     *****************************************************************
C     READ AND SET NODLAY ARRAY, AND READ GEOMETRIC PARAMETERS AND
C     MATRIX CONNECTIVITY FOR UNSTRUCTURED GRID
C     *****************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,IXSEC,
     1  LENUNI,IUNIT,ITRSS,NODES,NODLAY,LAYCBD,INBAS,IVC,IDSYMRD,
     2  PERLEN,NSTP,TSMULT,ISSFLG,BOT,TOP,IUNSTR,AMAT,AREA,IAG,IA,JA,
     3  JAS,ISYM,NJA,NJAS,NJAG,IVSD,PGF,FAHL,CL1,CL2,DELC,DELR,IBOUND,
     4  MXNODLAY,ICONCV,NOCVCO,NEQS,IFREFM,INCLN,INGNC,INGNC2,INGNCn,
     5  IATMP,NJATMP,IPRCONN
C
      DOUBLE PRECISION, DIMENSION(:,:,:),ALLOCATABLE  ::BOTM
      REAL, DIMENSION(:),    ALLOCATABLE  ::TEMP      
      CHARACTER*200 LINE
      CHARACTER*24 APNAME
      CHARACTER*24 ANAME(6)
      DATA ANAME(1) /'  NO. OF NODES PER LAYER'/
      DATA ANAME(2) /'                     TOP'/
      DATA ANAME(3) /'                     BOT'/
      DATA ANAME(4) /'                    AREA'/
      DATA ANAME(5) /'                      IA'/
      DATA ANAME(6) /'                      JA'/
C
C     ------------------------------------------------------------------
C1-------FILL NODLAY ARRAY WITH LAST NODE NUMBER FOR EACH LAYER AND SET MXNODLAY
      ALLOCATE(NODLAY(0:NLAY))
C1A----READ NUMBER OF NODES FOR EACH LAYER
      K = 0
      CALL U1DINT(NODLAY(1),ANAME(1),NLAY,K,INDIS,IOUT)
C1B-----FIND MXNODLAY
      MXNODLAY = 0
      DO K=1,NLAY
        IF(NODLAY(K).GT.MXNODLAY) MXNODLAY = NODLAY(K)
      ENDDO
C1C------COMPUTE CUMULATIVE TO GIVE NODE NUMBER OF LAST NODE OF A LAYER
      NODLAY(0) = 0
      DO K=2,NLAY
        NODLAY(K) = NODLAY(K-1) + NODLAY(K)
      ENDDO
C---------------------------------------------------------------------------
C
C2------READ TOP ARRAY
      DO K = 1,NLAY
        KK = K
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
        CALL U1DREL8(TOP(NSTRT),ANAME(2),NDSLAY,K,INDIS,IOUT)
      ENDDO
C
C3------READ BOT ARRAY
      DO K = 1,NLAY
        KK = K
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
        CALL U1DREL8(BOT(NSTRT),ANAME(3),NDSLAY,K,INDIS,IOUT)
      ENDDO
C
C4-----READ HORIZONTAL AREA 
        IF(IVSD.EQ.-1)THEN
C4A-------READ AREA ONLY FOR ONE LAYER IF IVSD = -1
          CALL U1DREL(AREA,ANAME(4),NODLAY(1),1,INDIS,IOUT)
          DO K=2,NLAY
            DO IJ=1,NODLAY(1)
              NN = (K-1)*NODLAY(1) + IJ
              AREA(NN) = AREA(IJ)
            ENDDO
          ENDDO
        ELSE
C4B------IF IVSD IS NOT -1, READ AREA FOR EACH LAYER
          DO K = 1,NLAY
            KK = K
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(AREA(NSTRT),ANAME(4),NDSLAY,K,INDIS,IOUT)
          ENDDO
        ENDIF
C
C5------READ CONNECTIONS PER NODE AND CONNECTIVITY AND FILL IA AND JA ARRAYS FOR GWF DOMAIN
      K = 0
      CALL U1DINT(IA,ANAME(5),NODES,K,INDIS,IOUT)
      ALLOCATE(JA(NJA))
      CALL U1DINT(JA,ANAME(6),NJA,K,INDIS,IOUT)
C5A------ENSURE POSITIVE TERM FOR DIAGONAL OF JA
      DO IJA = 1,NJA
        IF(JA(IJA).LT.0) JA(IJA) = -JA(IJA)
      ENDDO
C5B------MAKE IA CUMULATIVE FROM CONNECTION-PER-NODE
      DO II=2,NODES+1
        IA(II) = IA(II) + IA(II-1)
      ENDDO
C---------IA(N+1) IS CUMULATIVE_IA(N) + 1
      DO II=NODES+1,2,-1
        IA(II) = IA(II-1) + 1
      ENDDO
      IA(1) = 1    
C----------------------------------------------------------------------
C15------RETURN.
      RETURN
      END
C------------------------------------------------------------------------
      SUBROUTINE FILLIA
C     ******************************************************************
C     FILL IA ARRAY FOR A STRUCTURED GRID INPUT WITH CONNECTIONS- PER-NODE
C     ******************************************************************
      USE GLOBAL, ONLY:NODES,NLAY,NROW,NCOL,IA,NEQS,INCLN,NJA,
     1  IPRCONN,IOUT
C-------------------------------------------------------------------
C
C1------INITIALIZE IA ARRAY AND NJA
      NJA = 0
      DO I=1,NEQS + 1
        IA(I) = 0
      ENDDO
C------------------------------------------------------------
C2-----COMPUTE CONNECTIONS PER ROW IN IA
C------------------------------------------------------------
      NNDLAY = NCOL*NROW
C2A-----PREVIOUS LAYER
      DO K = 1,NLAY - 1
        KDEL = (K-1)*NNDLAY
        DO IJ = 1,NNDLAY
          ND1 = IJ + KDEL
          ND2 = ND1 + NNDLAY
          IA(ND2) = IA(ND2) + 1
        ENDDO
      ENDDO
C2B-----PREVIOUS ROW
      DO K = 1,NLAY
        KDEL = (K-1)*NNDLAY
         DO I = 1,NROW - 1
          DO J = 1,NCOL
            ND1 = J + NCOL*(I-1) + KDEL
            ND2 = ND1 + NCOL
            IA(ND2) = IA(ND2) + 1
          ENDDO
        ENDDO
      ENDDO
C2C-----PREVIOUS AND NEXT COLUMN
      DO K = 1,NLAY
        KDEL = (K-1)*NNDLAY
        DO I = 1,NROW
          DO J = 1,NCOL - 1
            ND1 = J + NCOL*(I-1) + KDEL
            ND2 = ND1 + 1
            IA(ND1) = IA(ND1) + 1
            IA(ND2) = IA(ND2) + 1
          ENDDO
        ENDDO
      ENDDO
C2D-----NEXT ROW
      DO K = 1,NLAY
        KDEL = (K-1)*NNDLAY
        DO I = 1,NROW - 1
          DO J = 1,NCOL
            ND1 = J + NCOL*(I-1) + KDEL
            IA(ND1) = IA(ND1) + 1
          ENDDO
        ENDDO
      ENDDO
C2E-----NEXT LAYER
      DO K = 1,NLAY - 1
        KDEL = (K-1)*NNDLAY
        DO IJ = 1,NNDLAY
          ND1 = IJ + KDEL
          IA(ND1) = IA(ND1) + 1
        ENDDO
      ENDDO
C2G-------DIAGONAL
      DO N = 1,NODES
          IA(N) = IA(N) + 1
      ENDDO
C2H-------PRINT NUMBER OF CONNECTIONS PER NODE IF REQUESTED
      IF(IPRCONN.NE.0)THEN
        WRITE(IOUT,*)'NUMBER OF CONNECTIONS PER NODE IS BELOW, 40I10'
        WRITE(IOUT,55)(IA(I),I=1,NODES)
55      FORMAT(40I10)
      ENDIF           
C------------------------------------------------------------
C3-------NEXT COMPUTE CUMULATIVE OF CONNECTIONS PER ROW IN IA
C------------------------------------------------------------
      DO II=2,NODES+1
        IA(II) = IA(II) + IA(II-1)
      ENDDO
C-------IA(N+1) IS CUMULATIVE_IA(N) + 1
      DO II=NODES+1,2,-1
        IA(II) = IA(II-1) + 1
      ENDDO
      IA(1) = 1
      NJA = IA(NODES+1) - 1
C
C4------RETURN
      RETURN
      END
C -----------------------------------------------------------------------
      SUBROUTINE FILLJA
C     ******************************************************************
C     CREATES JA ARRAY FOR A STRUCTURED GRID INPUT.
C     ******************************************************************
      USE GLOBAL, ONLY:NODES,NLAY,NROW,NCOL,IA,JA,NJA,NEQS,INCLN
C----------------------------------------------------------
C
C1------INITIALIZE JA ARRAY
      DO I=1,NJA
        JA(I) = 0
      ENDDO
      NNDLAY = NCOL*NROW
C2------PUT DIAGONAL IN FIRST LOCATION
      DO I=1,NODES
        JA(IA(I)) = I
      ENDDO
C3-------PREVIOUS LAYER
      DO K = 1,NLAY - 1
        KDEL = (K-1)*NNDLAY
        DO IJ = 1,NNDLAY
          ND1 = IJ + KDEL
          ND2 = ND1 + NNDLAY
          CALL FINDJA(ND2,ND1)
        ENDDO
      ENDDO
C4-------PREVIOUS ROW
      DO K = 1,NLAY
        KDEL = (K-1)*NNDLAY
         DO I = 1,NROW - 1
          DO J = 1,NCOL
            ND1 = J + NCOL*(I-1) + KDEL
            ND2 = ND1 + NCOL
            CALL FINDJA(ND2,ND1)
          ENDDO
        ENDDO
      ENDDO
C5-------PREVIOUS AND NEXT COLUMN
      DO K = 1,NLAY
        KDEL = (K-1)*NNDLAY
        DO I = 1,NROW
          DO J = 1,NCOL - 1
            ND1 = J + NCOL*(I-1) + KDEL
            ND2 = ND1 + 1
            CALL FINDJA(ND1,ND2)
            CALL FINDJA(ND2,ND1)
          ENDDO
        ENDDO
      ENDDO
C6-------NEXT ROW
      DO K = 1,NLAY
        KDEL = (K-1)*NNDLAY
        DO I = 1,NROW - 1
          DO J = 1,NCOL
            ND1 = J + NCOL*(I-1) + KDEL
            ND2 = ND1 + NCOL
            CALL FINDJA(ND1,ND2)
          ENDDO
        ENDDO
      ENDDO
C7-------NEXT LAYER
      DO K = 1,NLAY - 1
        KDEL = (K-1)*NNDLAY
        DO IJ = 1,NNDLAY
          ND1 = IJ + KDEL
          ND2 = ND1 + NNDLAY
          CALL FINDJA(ND1,ND2)
        ENDDO
      ENDDO
C------------------------------------------------------------------
C9------RETURN
      RETURN
      END
C -----------------------------------------------------------------------
      SUBROUTINE FINDJA(I,J)
C     ******************************************************************
C     ADD J TO THE ADJACENCY LIST FOR I
C     ******************************************************************
      USE GLOBAL, ONLY:IA,JA,NJA
C----------------------------------------------------------
      IF(I.NE.J)THEN
        DO II=IA(I),IA(I+1)-1
          IF(JA(II).EQ.0)THEN
C1----------FILL FIRST AVAILABLE NONZERO SPACE
            JA(II) = J
            GOTO 100
          ENDIF
        ENDDO
      ENDIF
  100 CONTINUE
C2------RETURN
      RETURN
      END
C -----------------------------------------------------------------------
      SUBROUTINE FILLGFS(IOUT)
C     ******************************************************************
C     CALCULATES PGF, CL1, CL2 AND FAHL ARRAYS FOR A STRUCTURED GRID INPUT.
C     PGF IS GEOMETRY FACTOR: FOR HORIZONTAL DIRECTION, PGF IS THE INTERFACE
C     LENGTH DIVIDED BY CONNECTION LENGTH. FOR VERTICAL DIRECTION, PGF IS
C     THE INTERFACE AREA DIVIDED BY CONNECTION LENGTH
C     FAHL IS THE FACE AREA (VERTICAL DIRECTION) OR HORIZONTAL LENGTH
C     ******************************************************************
      USE GLOBAL, ONLY:DELC,DELR,IBOUND,TOP,BOT,CL1,CL2,IPRCONN,IVC,
     1    NODES,NLAY,NROW,NCOL,NJA,NJAS,IA,JA,JAS,PGF,FAHL,AMAT,ISYM
      DOUBLE PRECISION ANUM,DENOM,T1,T2,BNUM
      REAL CONFINETHK
      REAL, ALLOCATABLE,   DIMENSION(:) ::TEMPPL
C----------------------------------------------------------
C
C1------ALLOCATE SPACE AND INITIALIZE ARRAYS
      ALLOCATE(CL1(NJAS),CL2(NJAS))
C
      ALLOCATE( TEMPPL(NJA))
      TEMPPL=0.0
      CL1=0.0
      CL2=0.0
      PGF=0.0
      FAHL=0.0
      NNDLAY = NROW*NCOL
C
C2--------COMPUTE GEOMETRY FACTOR AS FLOW AREA (or length) DIVIDED BY DISTANCE AND
C2--------FILL CL1, CL2,  PGF AND FAHL ARRAYS FOR ALL POROUS MATRIX NODES
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
        N=J+(I-1)*NCOL+(K-1)*NNDLAY
C2A-------GET INDICES FOR ADJACENT NODES AND SET COLUMN POINTER TO 1
        NRN=N+NCOL
        NRL=N-NCOL
        NCN=N+1
        NCL=N-1
        NLN=N+NNDLAY
        NLL=N-NNDLAY
        JPTR = 1
C2B---------ALL SYMMETRIC MATRICES SO ADVANCE POINTER FOR PREVIOUS LAYER,
        IF(K.NE.1)THEN
          JPTR = JPTR + 1
        ENDIF
C2C-------ALL SYMMETRIC MATRICES SO ADVANCE POINTER FOR PREVIOUS ROW
        IF(I.NE.1)THEN
          JPTR = JPTR + 1
        ENDIF
C2D-------ALL SYMMETRIC MATRICES SO ADVANCE POINTER FOR PREVIOUS COLUMN
        IF(J.NE.1)THEN
          JPTR = JPTR + 1
        ENDIF                                    
C2E-------COMPUTE AND FILL FOR NEXT COLUMN
        IF(J.NE.NCOL)THEN
          ANUM = DELC(I)
          DENOM = (DELR(J) + DELR(J+1))*0.5
          ID = IA(N) + JPTR
          IDS = JAS(ID)
          PGF(IDS) = ANUM/DENOM   
          FAHL(IDS) = ANUM
          CL1(IDS)= DELR(J)*0.5
          CL2(IDS)= DELR(J+1)*0.5
          JPTR = JPTR + 1
        ENDIF
C2F-------COMPUTE AND FILL FOR NEXT ROW
        IF(I.NE.NROW)THEN
          ANUM = DELR(J)
          DENOM = (DELC(I) + DELC(I+1))*0.5
          ID = IA(N) + JPTR
          IDS = JAS(ID)
          PGF(IDS) = ANUM/DENOM  
          FAHL(IDS) = ANUM
          CL1(IDS)= DELC(I)*0.5
          CL2(IDS)= DELC(I+1)*0.5
          JPTR = JPTR + 1
        ENDIF
C2G-------COMPUTE AND FILL FOR NEXT LAYER
        IF(K.NE.NLAY)THEN
          ANUM = DELR(J) * DELC(I)
          CONFINETHK = BOT(N) - TOP(NLN)
          T1 = (TOP(N) - BOT(N))*0.5 + CONFINETHK * 0.5
          T2 = (TOP(NLN) - BOT(NLN))*0.5 + CONFINETHK * 0.5
          DENOM = T1 + T2
          ID = IA(N) + JPTR
          IDS = JAS(ID)          
          PGF(IDS) = ANUM/(DENOM  + 1.0e-20)
          FAHL(IDS) = ANUM
          CL1(IDS)= T1
          CL2(IDS)= T2
          JPTR = JPTR + 1
        ENDIF
      ENDDO
      ENDDO
      ENDDO
C3-------WRITE CL1, CL2 AND FAHL ARRAYS
      IF(IPRCONN.NE.0)THEN
        WRITE(IOUT,*)'CL1, CL2 ARE BELOW, 22G15.6, UNSYMMETRIC'
        DO N=1,NODES
          DO II = IA(N)+1,IA(N+1)-1
            JJ = JA(II)
            IF(JJ.GE.N)THEN
              IIS = JAS(II)
              TEMPPL(II) = CL1(IIS)
              TEMPPL(ISYM(II)) = CL2(IIS)
            ENDIF                  
          ENDDO
        ENDDO
        WRITE(IOUT,55)(TEMPPL(J),J=1,NJA)
C--------------            
C        WRITE(IOUT,55)(CL1(J),J=1,NJAS)
C        WRITE(IOUT,*)'CL2 IS BELOW, 22G15.6, UNSYMMETRIC'
C        WRITE(IOUT,55)(CL2(J),J=1,NJAS)
        WRITE(IOUT,*)'FAHL IS BELOW, 22G15.6, UNSYMMETRIC'
        DO N=1,NODES
          DO II = IA(N)+1,IA(N+1)-1
            JJ = JA(II)
            IF(JJ.GE.N)THEN
              IIS = JAS(II)
              IF(IVC(IIS).EQ.1)THEN
                BNUM = 1.0
              ELSE
                BNUM = 0.5*( (TOP(N) - BOT(N)) + (TOP(JJ) - BOT(JJ)))  
csp                BNUM = MIN ((TOP(N) - BOT(N)) , (TOP(JJ) - BOT(JJ))) !min is used when IVC=2
              ENDIF
              TEMPPL(II) = FAHL(IIS)*BNUM
              TEMPPL(ISYM(II)) = FAHL(IIS)*BNUM
            ENDIF                  
          ENDDO
        ENDDO
        WRITE(IOUT,55)(TEMPPL(J),J=1,NJA)

C        WRITE(IOUT,55)(FAHL(J),J=1,NJAS)
55      FORMAT(1P,22G15.6)
      ENDIF

      DEALLOCATE(TEMPPL)
C4------RETURN
      RETURN
      END
C -----------------------------------------------------------------------
      SUBROUTINE FILLGFU(INDIS,IOUT)
C     ******************************************************************
C     READ PGF, CL1, CL2 AND FAHL ARRAYS FOR UNSTRUCTURED GRID INPUT.
C     PGF IS GEOMETRY FACTOR: FOR HORIZONTAL DIRECTION, PGF IS THE INTERFACE
C     LENGTH DIVIDED BY CONNECTION LENGTH. FOR VERTICAL DIRECTION, PGF IS
C     THE INTERFACE AREA DIVIDED BY CONNECTION LENGTH
C     FAHL IS THE FACE AREA (VERTICAL DIRECTION) OR HORIZONTAL LENGTH
C     ******************************************************************   
      USE GLOBAL,   ONLY:NODES,NODLAY,LAYCBD,INBAS,IVC,AREA,IDSYMRD,
     1  IA,JA,JAS,ISYM,NJA,NJAS,IVSD,PGF,FAHL,CL1,CL2,IATMP,NJATMP,
     2  IBOUND,MXNODLAY,ICONCV,NOCVCO,NEQS,IFREFM,IPRCONN,TOP,BOT
C       
      CHARACTER*24 ANAME(4)
      DOUBLE PRECISION BNUM
      DATA ANAME(1) /'     CONNECTION LENGTH 1'/
      DATA ANAME(2) /'     CONNECTION LENGTH 2'/
      DATA ANAME(3) /'    CONNECTION LENGTH 12'/
      DATA ANAME(4) /'      PERPENDICULAR AREA'/
C12-------READ CONNECTION LENGTHS (DENOM TERM)
      ALLOCATE(CL1(NJAS),CL2(NJAS))
      CL1 = 0.0
      CL2 = 0.0
      IF(IDSYMRD.EQ.1)THEN
C12A-------CL1 AND CL2 ARE READ SEPARATELY, IF SYMMETRIC MATRICES ARE READ      
        CALL U1DRELNJA(CL1,IATMP,ANAME(1),NJATMP,INDIS,IOUT,IDSYMRD)
        CALL U1DRELNJA(CL2,IATMP,ANAME(2),NJATMP,INDIS,IOUT,IDSYMRD)
      ELSE
C12B-------CL1 AND CL2 ARE READ IN ONE UNSYMMETRIC MATRIX AND SPLIT UP. 
        CALL U1DRELNJAU(CL1,CL2,ANAME(3),IATMP,NJATMP,INDIS,IOUT)
      ENDIF      
C
C13-------READ CONNECTED PERPENDICULAR FLOW LENGTH OR AREA
C13------(AREA FOR VERTICAL CONNECTION, LENGTH FOR HORIZONTAL)
      CALL U1DRELNJA(FAHL,IATMP,ANAME(4),NJATMP,INDIS,IOUT,IDSYMRD) 
C
C14-----COMPUTE GEOMETRIC FACTOR IN PGF AND SET FAHL AS FA FOR HORIZONTAL CONNECTION
      DO N=1,NODES
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IF(JJ.LE.N.OR.JJ.GT.NODES) CYCLE
          IIS = JAS(II)
          PGF(IIS) = FAHL(IIS) / (CL1(IIS) + CL2(IIS) + 1.0E-20)
          IF(IVC(IIS).EQ.1) CYCLE !DONE IF VERTICAL DIRECTION CONNECTION
          IF(IVC(IIS).EQ.2)THEN
            BNUM = MIN((TOP(N) - BOT(N)) , (TOP(JJ) - BOT(JJ)))
          ELSE
            BNUM = 0.5*( (TOP(N) - BOT(N)) + (TOP(JJ) - BOT(JJ)))  
          ENDIF
          PGF(IIS) = PGF(IIS) / (BNUM + 1.0E-20)  ! AREA IS READ IN - CONVERT TO PL FOR HORIZONTAL FACE
          FAHL(IIS) = FAHL(IIS) / (BNUM + 1.0E-20)
        ENDDO
      ENDDO
C4------RETURN
      RETURN
      END       
C----------------------------------------------------------------------
      SUBROUTINE SGWF2BAS7D(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     CALCULATE, PRINT, AND SAVE DRAWDOWNS FOR STRUCTURED GWF GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,HNEW,STRT,NODLAY,
     1                      IBOUND,IOUT,SN,iunsat
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IDDNFM,IDDNUN,LBDDSV,
     2                      CDDNFM,IOFLG
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION SSTRT
      REAL,          SAVE,    DIMENSION(:,:,:),    ALLOCATABLE ::BUFF
C
      DATA TEXT /'        DRAWDOWN'/
C     ------------------------------------------------------------------
C
C1-------ALLOCATE TEMPORARY SPACE
      ALLOCATE(BUFF(NCOL,NROW,NLAY))
C
C2------FOR EACH LAYER CALCULATE DRAWDOWN IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C3------IS DRAWDOWN NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,2).EQ.0 .AND. IOFLG(KL,4).EQ.0) GO TO 59
C
C4------CALCULATE DRAWDOWN FOR THE LAYER.
      DO 58 I=1,NROW
      DO 58 J=1,NCOL
      N = (K-1)*NROW*NCOL + (I-1)*NCOL + J
      BUFF(J,I,K)=HNEW(N)
      SSTRT=STRT(N)
      IF(IBOUND(N).NE.0) BUFF(J,I,K)=SSTRT-HNEW(N)
      IF(IUNSAT.EQ.1) BUFF(J,I,K) = SN(N)
   58 CONTINUE
   59 CONTINUE
C
C5------FOR EACH LAYER: DETERMINE IF DRAWDOWN SHOULD BE PRINTED.
C5------IF SO THEN CALL ULAPRS OR ULAPRW TO PRINT DRAWDOWN.
      IF(ISA.NE.0) THEN
         IF(IXSEC.EQ.0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,2).EQ.0) GO TO 69
           IF(IDDNFM.LT.0) CALL ULAPRS(BUFF(1,1,K),TEXT,KSTP,KPER,
     1                  NCOL,NROW,KK,-IDDNFM,IOUT)
           IF(IDDNFM.GE.0) CALL ULAPRW(BUFF(1,1,K),TEXT,KSTP,KPER,
     1                  NCOL,NROW,KK,IDDNFM,IOUT)
           IPFLG=1
   69      CONTINUE
C
C5A-----PRINT DRAWDOWN FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,2).NE.0) THEN
             IF(IDDNFM.LT.0) CALL ULAPRS(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,-IDDNFM,IOUT)
             IF(IDDNFM.GE.0) CALL ULAPRW(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,IDDNFM,IOUT)
             IPFLG=1
           END IF
         END IF
      END IF
C
C6------FOR EACH LAYER: DETERMINE IF DRAWDOWN SHOULD BE SAVED.
C6------IF SO THEN CALL A ULASAV OR ULASV2 TO RECORD DRAWDOWN.
      IFIRST=1
      IF(IDDNUN.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        NSTRT = NODLAY(K-1)+1
        KK=K
        IF(IOFLG(K,4).EQ.0) GO TO 79
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IDDNUN,KSTP,KPER
   74   FORMAT(1X,/1X,'DRAWDOWN WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        IF(CDDNFM.EQ.' ') THEN
           CALL ULASAV(BUFF(1,1,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IDDNUN)
        ELSE
           CALL ULASV2(BUFF(1,1,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IDDNUN,CDDNFM,LBDDSV,IBOUND(NSTRT))
        END IF
   79   CONTINUE
C
C6A-----SAVE DRAWDOWN FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,4).NE.0) THEN
          WRITE(IOUT,74) IDDNUN,KSTP,KPER
          IF(CDDNFM.EQ.' ') THEN
             CALL ULASAV(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NLAY,-1,IDDNUN)
          ELSE
             CALL ULASV2(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IDDNUN,CDDNFM,LBDDSV,IBOUND)
          END IF
        END IF
      END IF
80    CONTINUE
C
C7------DEALLOCATE TEMPORARY SPACE
      DEALLOCATE(BUFF)

C
C8------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2BAS7H(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     PRINT AND RECORD HEADS FOR STRUCTURED GWF GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,HNEW,NODLAY,
     1                      IBOUND,IOUT
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IHEDFM,IHEDUN,LBHDSV,
     2                      CHEDFM,IOFLG
C
      REAL,          SAVE,    DIMENSION(:,:,:),    ALLOCATABLE ::BUFF
      CHARACTER*16 TEXT
      DATA TEXT /'            HEAD'/
C     ------------------------------------------------------------------
      ALLOCATE(BUFF(NCOL,NROW,NLAY))
C
C1------FOR EACH LAYER MOVE HNEW TO BUFF IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS HEAD NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,1).EQ.0 .AND. IOFLG(KL,3).EQ.0) GO TO 59
C
C3------MOVE HNEW TO BUFF FOR THE LAYER.
      DO 58 I=1,NROW
      DO 58 J=1,NCOL
      N = (K-1)*NROW*NCOL + (I-1)*NCOL + J
      BUFF(J,I,K)=HNEW(N)
   58 CONTINUE
   59 CONTINUE
C
C4------FOR EACH LAYER: DETERMINE IF HEAD SHOULD BE PRINTED.
C4------IF SO THEN CALL ULAPRS OR ULAPRW TO PRINT HEAD.
      IF(ISA.NE.0) THEN
         IF(IXSEC.EQ.0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,1).EQ.0) GO TO 69
           IF(IHEDFM.LT.0) CALL ULAPRS(BUFF(1,1,K),TEXT,KSTP,KPER,
     1               NCOL,NROW,KK,-IHEDFM,IOUT)
           IF(IHEDFM.GE.0) CALL ULAPRW(BUFF(1,1,K),TEXT,KSTP,KPER,
     1               NCOL,NROW,KK,IHEDFM,IOUT)
           IPFLG=1
   69      CONTINUE
C
C4A-----PRINT HEAD FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,1).NE.0) THEN
             IF(IHEDFM.LT.0) CALL ULAPRS(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,-IHEDFM,IOUT)
             IF(IHEDFM.GE.0) CALL ULAPRW(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,IHEDFM,IOUT)
             IPFLG=1
           END IF
         END IF
      END IF
C
C5------FOR EACH LAYER: DETERMINE IF HEAD SHOULD BE SAVED ON DISK.
C5------IF SO THEN CALL ULASAV OR ULASV2 TO SAVE HEAD.
      IFIRST=1
      IF(IHEDUN.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        NSTRT = NODLAY(K-1)+1
        KK=K
        IF(IOFLG(K,3).EQ.0) GO TO 79
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IHEDUN,KSTP,KPER
   74   FORMAT(1X,/1X,'HEAD WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        IF(CHEDFM.EQ.' ') THEN
           CALL ULASAV(BUFF(1,1,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IHEDUN)
        ELSE
           CALL ULASV2(BUFF(1,1,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IHEDUN,CHEDFM,LBHDSV,IBOUND(NSTRT))
        END IF
   79   CONTINUE
C
C5A-----SAVE HEAD FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,3).NE.0) THEN
          WRITE(IOUT,74) IHEDUN,KSTP,KPER
          IF(CHEDFM.EQ.' ') THEN
             CALL ULASAV(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NLAY,-1,IHEDUN)
          ELSE
             CALL ULASV2(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IHEDUN,CHEDFM,LBHDSV,IBOUND)
          END IF
        END IF
      END IF
80    CONTINUE
      DEALLOCATE(BUFF)
C
C6------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2BAS7IB(KSTP,KPER)
C     ******************************************************************
C     RECORD IBOUND FOR STRUCTURED GWF GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,IBOUND,IOUT
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IBOUUN,LBBOSV,CBOUFM,IOFLG
C
      INTEGER,  SAVE,    DIMENSION(:,:,:),    ALLOCATABLE ::ITEMP
      CHARACTER*16 TEXT
      DATA TEXT /'          IBOUND'/
C     ------------------------------------------------------------------
      IF(IBOUUN.LE.0) RETURN
C
C1------ALLOCATE TEMPORARY SPACE      
      ALLOCATE (ITEMP(NCOL,NROW,NLAY))
C
C2------FILL IBOUND IN TEMPORARY SPACE FOR OUTPUT
      N=0
      DO 58 K=1,NLAY
      DO 58 I=1,NROW
      DO 58 J=1,NCOL
      N = N+1
      ITEMP(J,I,K)=IBOUND(N)
   58 CONTINUE
C
C3------FOR EACH LAYER: SAVE IBOUND WHEN REQUESTED.
      IFIRST=1
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,7).EQ.0) GO TO 79
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IBOUUN,KSTP,KPER
   74   FORMAT(1X,/1X,'IBOUND WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        CALL ULASV3(ITEMP(1,1,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IBOUUN,CBOUFM,LBBOSV)
   79   CONTINUE
C
C3A-----SAVE IBOUND FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,7).NE.0) THEN
          WRITE(IOUT,74) IBOUUN,KSTP,KPER
          CALL ULASV3(ITEMP(1,1,1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IBOUUN,CBOUFM,LBBOSV)
        END IF
      END IF
C
C4------RETURN.
      DEALLOCATE(ITEMP)
      RETURN
      END
C
C----------------------------------------------------------------------
      SUBROUTINE SGWF2BAS7DU(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     CALCULATE, PRINT, AND SAVE DRAWDOWNS FOR UNSTRUCTURED GWF GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,HNEW,STRT,NODLAY,
     1                      IBOUND,IOUT,BUFF,NODES
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IDDNFM,IDDNUN,LBDDSV,
     2                      CDDNFM,IOFLG
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION SSTRT
C
      DATA TEXT /'       DRAWDOWNU'/
C     ------------------------------------------------------------------
C
C1------FOR EACH LAYER CALCULATE DRAWDOWN IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS DRAWDOWN NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,2).EQ.0 .AND. IOFLG(KL,4).EQ.0) GO TO 59
C
C3------CALCULATE DRAWDOWN FOR THE LAYER.
      NNDLAY = NODLAY(K)
      NSTRT = NODLAY(K-1)+1
      DO 58 N=NSTRT,NNDLAY
      BUFF(N)=HNEW(N)
      SSTRT=STRT(N)
      IF(IBOUND(N).NE.0) BUFF(N)=SSTRT-HNEW(N)
   58 CONTINUE
   59 CONTINUE
C
C4------FOR EACH LAYER: DETERMINE IF DRAWDOWN SHOULD BE PRINTED.
C4------IF SO THEN CALL ULAPRU TO PRINT DRAWDOWN.
      IF(ISA.NE.0) THEN
         IF(IXSEC.EQ.0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,2).EQ.0) GO TO 69
           NNDLAY = NODLAY(K)
           NSTRT = NODLAY(K-1)+1
           CALL ULAPRU(BUFF,TEXT,KSTP,KPER,
     1           NSTRT,NNDLAY,KK,IABS(IDDNFM),IOUT,PERTIM,TOTIM,NODES)
           IPFLG=1
   69      CONTINUE
C
C4A-----PRINT DRAWDOWN FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,2).NE.0) THEN
           CALL ULAPRU(BUFF,TEXT,KSTP,KPER,
     1           NSTRT,NNDLAY,-1,IABS(IDDNFM),IOUT,PERTIM,TOTIM,NODES)
             IPFLG=1
           END IF
         END IF
      END IF
C
C5------FOR EACH LAYER: DETERMINE IF DRAWDOWN SHOULD BE SAVED.
C5------IF SO THEN CALL A ULASAV OR ULASV2 TO RECORD DRAWDOWN.
      IFIRST=1
      IF(IDDNUN.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,4).EQ.0) GO TO 79
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IDDNUN,KSTP,KPER
   74   FORMAT(1X,/1X,'DRAWDOWN WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        IF(CDDNFM.EQ.' ') THEN
           CALL ULASAVU(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                NNDLAY,KK,IDDNUN,NODES)
        ELSE
           CALL ULASV2U(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1             NNDLAY,KK,IDDNUN,CDDNFM,LBDDSV,IBOUND(NSTRT),NODES)
        END IF
   79   CONTINUE
C
C5A-----SAVE DRAWDOWN FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,4).NE.0) THEN
          WRITE(IOUT,74) IDDNUN,KSTP,KPER
          IF(CDDNFM.EQ.' ') THEN
           CALL ULASAVU(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                NNDLAY,-1,IDDNUN,NODES)
        ELSE
           CALL ULASV2U(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1             NNDLAY,-1,IDDNUN,CDDNFM,LBDDSV,IBOUND(NSTRT),NODES)
          END IF
        END IF
      END IF
80    CONTINUE
C
C6------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2BAS7HU(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     PRINT AND RECORD HEADS FOR UNSTRUCTURED GWF GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,HNEW,NODLAY,
     1                      IBOUND,IOUT,NODES,BUFF
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IHEDFM,IHEDUN,LBHDSV,
     2                      CHEDFM,IOFLG
C
      CHARACTER*16 TEXT
      DATA TEXT /'           HEADU'/
C     ------------------------------------------------------------------
C
C1------FOR EACH LAYER MOVE HNEW TO BUFF IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS HEAD NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,1).EQ.0 .AND. IOFLG(KL,3).EQ.0) GO TO 59
C
C3------MOVE HNEW TO BUFF FOR THE LAYER.
      NNDLAY = NODLAY(K)
      NSTRT = NODLAY(K-1)+1
      DO 58 N=NSTRT,NNDLAY
      BUFF(N)=HNEW(N)
   58 CONTINUE
   59 CONTINUE
C
C4------FOR EACH LAYER: DETERMINE IF HEAD SHOULD BE PRINTED.
C4------IF SO THEN CALL ULAPRU TO PRINT HEAD.
      IF(ISA.NE.0) THEN
         IF(IXSEC.EQ.0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,1).EQ.0) GO TO 69
           NNDLAY = NODLAY(K)
           NSTRT = NODLAY(K-1)+1
           CALL ULAPRU(BUFF,TEXT,KSTP,KPER,
     1           NSTRT,NNDLAY,KK,IABS(IHEDFM),IOUT,PERTIM,TOTIM,NODES)
           IPFLG=1
   69      CONTINUE
C
C4A-----PRINT HEAD FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,1).NE.0) THEN
           CALL ULAPRU(BUFF,TEXT,KSTP,KPER,
     1           NSTRT,NNDLAY,-1,IABS(IHEDFM),IOUT,PERTIM,TOTIM,NODES)
             IPFLG=1
C
           END IF
         END IF
      END IF
C
C5------FOR EACH LAYER: DETERMINE IF HEAD SHOULD BE SAVED ON DISK.
C5------IF SO THEN CALL ULASAV OR ULASV2 TO SAVE HEAD.
      IFIRST=1
      IF(IHEDUN.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,3).EQ.0) GO TO 79
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IHEDUN,KSTP,KPER
   74   FORMAT(1X,/1X,'HEAD WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        IF(CHEDFM.EQ.' ') THEN
           CALL ULASAVU(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                NNDLAY,KK,IHEDUN,NODES)
        ELSE
           CALL ULASV2U(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1             NNDLAY,KK,IHEDUN,CHEDFM,LBHDSV,IBOUND(NSTRT),NODES)
        END IF
        IPFLG=1
   79   CONTINUE
C
C5A-----SAVE HEAD FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,3).NE.0) THEN
          WRITE(IOUT,74) IHEDUN,KSTP,KPER
          IF(CHEDFM.EQ.' ') THEN
             CALL ULASAVU(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                NNDLAY,-1,IHEDUN,NODES)
          ELSE
             CALL ULASV2U(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                  NNDLAY,-1,IHEDUN,CHEDFM,LBHDSV,IBOUND,NODES)
          END IF
          IPFLG=1
        END IF
      END IF
C
C6------RETURN.
   80 CONTINUE
      RETURN
C
      END
      SUBROUTINE SGWF2BAS7IBU(KSTP,KPER)
C     ******************************************************************
C     RECORD IBOUND FOR UNSTRUCTURED GWF GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,IXSEC,IBOUND,IOUT,NODLAY,NODES
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IBOUUN,LBBOSV,CBOUFM,IOFLG
C
      CHARACTER*16 TEXT
      DATA TEXT /'          IBOUND'/
C     ------------------------------------------------------------------
      IF(IBOUUN.LE.0) RETURN
C
C1------FOR EACH LAYER: SAVE IBOUND WHEN REQUESTED.
      IFIRST=1
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,7).EQ.0) GO TO 79
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
C
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IBOUUN,KSTP,KPER
   74   FORMAT(1X,/1X,'IBOUND WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        CALL ULASV3U(IBOUND,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                NNDLAY,KK,IBOUUN,CBOUFM,LBBOSV,NODES)
   79   CONTINUE
C
C1A-----SAVE IBOUND FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,7).NE.0) THEN
          WRITE(IOUT,74) IBOUUN,KSTP,KPER
          CALL ULASV3U(IBOUND,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                  NNDLAY,-1,IBOUUN,CBOUFM,LBBOSV,NODES)
        END IF
      END IF
C
C2------RETURN.
      RETURN
      END
