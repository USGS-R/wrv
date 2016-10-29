      MODULE GWFHFBMODULE
        INTEGER, POINTER  ::MXHFB,NHFB,IPRHFB,NHFBNP,NPHFB,IHFBPB
        REAL,    DIMENSION(:,:), ALLOCATABLE   ::HFB
      END MODULE GWFHFBMODULE


      SUBROUTINE GWF2HFB7U1AR(INHFB)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR HORIZONTAL FLOW BARRIER PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,LAYHDT,
     1                      DELR,DELC,IOUT,NODES,IUNSTR,NODLAY
      USE GWFHFBMODULE,ONLY:MXHFB,NHFB,IPRHFB,NHFBNP,NPHFB,IHFBPB,HFB
C
      INTEGER INHFB, MXACTFB
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------Allocate scalar data.
      ALLOCATE(MXHFB,NHFB,IPRHFB,NHFBNP,NPHFB,IHFBPB)
C
C2------IDENTIFY PACKAGE.
      WRITE(IOUT,1) INHFB
    1 FORMAT(1X,/1X,'HFB -- HORIZONTAL-FLOW BARRIER',
     &' PACKAGE, VERSION 7, 5/2/2005.',/,'   INPUT READ FROM UNIT ',I4)
C
C3------READ AND PRINT NPHFB, MXFB, NHFBNP
      CALL URDCOM(INHFB,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPHFB,DUM,IOUT,INHFB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXFBP,DUM,IOUT,INHFB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NHFBNP,DUM,IOUT,INHFB)
      WRITE(IOUT,500) NPHFB,MXFBP
  500 FORMAT(1X,I5,' PARAMETERS DEFINE A MAXIMUM OF ',I6,
     &       ' HORIZONTAL FLOW BARRIERS')
      WRITE(IOUT,530) NHFBNP
  530 FORMAT(1X,I6,' HORIZONTAL FLOW BARRIERS NOT DEFINED BY',
     &       ' PARAMETERS')
C
C4------LOOK FOR NOPRINT OPTION.
      IPRHFB = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INHFB)
      IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
        WRITE(IOUT,3)
    3   FORMAT(1X,
     &'LISTS OF HORIZONTAL FLOW BARRIER CELLS WILL NOT BE PRINTED')
        IPRHFB = 0
      END IF
C
C5------CALCULATE AMOUNT OF SPACE USED BY HFB PACKAGE AND ALLOCATE HFB.
      MXACTFB = NHFBNP+MXFBP
      IHFBPB = MXACTFB + 1
      MXHFB = MXACTFB + MXFBP
      ALLOCATE (HFB(7,MXHFB))
C
C6------CHECK THAT THE FLOW PACKAGE IS A KIND THAT HFB CAN SUPPORT.
C6------LAYHDT IS -1 UNLESS THE FLOW PACKAGE CHANGES IT.  IF LAYHDT
C6------IS STILL NEGATIVE, IT IS ASSUMED THAT HFB WILL NOT WORK.
      IF (LAYHDT(1).LT.0) THEN
        WRITE(IOUT,550)
  550   FORMAT(/,
     &' ERROR: SELECTED FLOW PACKAGE DOES NOT SUPPORT HFB PACKAGE',/,
     &' -- STOP EXECUTION (GWF2HFB7U1AR)')
        CALL USTOP(' ')
      ENDIF
C
C7------READ PARAMETER DEFINITIONS (ITEMS 2 AND 3)
      WRITE(IOUT,600) NPHFB
  600 FORMAT(//,1X,I5,' HFB parameters')
      IF (NPHFB.GT.0) THEN
        LSTSUM = IHFBPB
        DO 20 K = 1,NPHFB
          LSTBEG = LSTSUM
          CALL UPARLSTRP(LSTSUM,MXHFB,INHFB,IOUT,IP,'HFB ','HFB ',
     &                   1,NUMINST)
          IF(NUMINST.GT.0) THEN
            WRITE(IOUT,*) ' INSTANCES ARE NOT SUPPORTED FOR HFB'
            CALL USTOP(' ')
          END IF
          NLST=LSTSUM-LSTBEG
          IF(IUNSTR.EQ.0)THEN
            CALL SGWF2HFB7RL(NLST,HFB,LSTBEG,MXHFB,INHFB,IOUT,
     &         'BARRIER  LAYER  IROW1  ICOL1  IROW2  ICOL2     FACTOR',
     &         NCOL,NROW,NLAY,IPRHFB)
          ELSE
            CALL SGWF2HFB7RLU(NLST,HFB,LSTBEG,MXHFB,INHFB,IOUT,
     &         'BARRIER      NODE1    NODE2      FACTOR              ',
     &         NODES,NODLAY,NLAY,IPRHFB)
          ENDIF
          CALL SGWF2HFB7CKU(LSTBEG,LSTSUM-1)
   20   CONTINUE
      ENDIF
C
C8------READ BARRIERS NOT DEFINED BY PARAMETERS (ITEM 4)
      NHFB = 0
      WRITE(IOUT,610) NHFBNP
  610 FORMAT(/,1X,I6,' BARRIERS NOT DEFINED BY PARAMETERS')
      IF(NHFBNP.GT.0) THEN
        IF(IUNSTR.EQ.0)THEN
          CALL SGWF2HFB7RL(NHFBNP,HFB,1,MXHFB,INHFB,IOUT,
     1      'BARRIER  LAYER  IROW1  ICOL1  IROW2  ICOL2    HYDCHR',
     2             NCOL,NROW,NLAY,IPRHFB)
        ELSE
          CALL SGWF2HFB7RLU(NHFBNP,HFB,1,MXHFB,INHFB,IOUT,
     &         'BARRIER      NODE1    NODE2      HYDCHR              ',
     2             NODES,NODLAY,NLAY,IPRHFB)
        ENDIF
        CALL SGWF2HFB7CKU(1,NHFBNP)
        NHFB = NHFB + NHFBNP
      ENDIF
C
C9------SUBSTITUTE DATA FOR PARAMETERIZED BARRIERS INTO ACTIVE SECTION
C9------OF HFB ARRAY
      IOUTU = IOUT
      IF (IPRHFB.EQ.0) IOUTU = -IOUT
      MXACTFB= IHFBPB-1
      CALL PRESET('HFB ')
      IF(NPHFB.GT.0) THEN
C
C10-----READ NUMBER OF ACTIVE HFB PARAMETERS (ITEM 5)
        READ(INHFB,*) NACTHFB
        IF (NACTHFB.GT.0) THEN
          DO 650 I = 1,NACTHFB
C
C11-----READ AND ACTIVATE AN HFB PARAMETER (ITEM 6)
            CALL SGWF2HFB7SUB(INHFB,'HFB ',IOUTU,'HFB ',HFB,7,MXHFB,
     1                        MXACTFB,NHFB,IUNSTR,
     2       'BARRIER  LAYER  IROW1  ICOL1  IROW2  ICOL2     HYDCHR',
     3       'BARRIER   NODE1     NODE2      HYDCHR')
  650     CONTINUE
        ENDIF
      ENDIF
C
C
C12-----MODIFY HORIZONTAL BRANCH CONDUCTANCES FOR CONSTANT T LAYERS.
      CALL SGWF2HFB7MC
      WRITE (IOUT,660) NHFB
  660 FORMAT(/,1X,1I6,' HFB BARRIERS')
C
      RETURN
      END
      SUBROUTINE GWF2HFB7U1FM
C     ******************************************************************
C     MODIFY HORIZONTAL BRANCH CONDUCTANCES IN VARIABLE-TRANSMISSIVITY
C     LAYERS TO ACCOUNT FOR HORIZONTAL FLOW BARRIERS. STORE UNMODIFIED
C     HORIZONTAL CONDUCTANCE IN HFB(7,#) TO ALLOW CALCULATION OF
C     SENSITIVITIES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,HNEW,TOP,BOT,IBOUND,IVC,
     1                      AMAT,PGF,FAHL,IA,JA,JAS,ISYM
      USE GWFBCFMODULE, ONLY: LAYCON
      USE GWFHFBMODULE,ONLY:NHFB,HFB
      DOUBLE PRECISION HD1,HD2,TDW,ATERM
C     ------------------------------------------------------------------
C
C2------FOR EACH BARRIER, MODIFY HORIZONTAL BRANCH CONDUCTANCES IF LAYER
C2------IS CONVERTIBLE.
      DO 10 II=1,NHFB
        K = HFB(3,II)
C
C3-----IF LAYCON=0,2,4, THE THICKNESS AND CONDUCTANCE DO NOT VARY, AND
C3-----MODIFICATION OF CONDUCTANCE DUE TO BARRIER WAS DONE IN
C3-----SGWF1HFB7MC
        IF (LAYCON(K).EQ.3.OR.LAYCON(K).EQ.1) THEN
C
C4------CELLS N1 AND N2 ARE THE ONE WHOSE HORIZONTAL BRANCH
C4------CONDUCTANCES ARE TO BE MODIFIED.
          N1 = HFB(1,II)
          N2 = HFB(2,II)
          IF(IBOUND(N1).EQ.0.OR.IBOUND(N2).EQ.0) CYCLE
C
          HCDW = HFB(6,II)
C-------------FIND BARRIER LOCATION AND ADJUST AMAT
          DO IAJ = IA(N1)+1,IA(N1+1)-1
            JJ = JA(IAJ)
            IAJS = JAS(IAJ)
            IF(JJ.EQ.N2)THEN
C
C8----------CALCULATE AVERAGE SATURATED THICKNESS BETWEEN CELLS N1 AND N2.
              HD1 = HNEW(N1)
              HD2 = HNEW(N2)
              IF (HD1.GT.TOP(N1)) HD1 = TOP(N1)
              IF (HD2.GT.TOP(N2)) HD2 = TOP(N2)
              IF(IVC(IAJS).EQ.2)THEN
                BMAX = MAX( BOT(N1),BOT(N2))
                IF (HD1.LT.BMAX)HD1 = BMAX
                IF (HD2.LT.BMAX)HD2 = BMAX
                THKAVG = 0.5*(TH1+TH2)
              ELSE
                IF (HD1.LT.BOT(N1))HD1 = BOT(N1)
                IF (HD2.LT.BOT(N2))HD2 = BOT(N2)
                THKAVG = ((HD1-BOT(N1)) + (HD2-BOT(N2)))/2.
              ENDIF
              HFB(7,II) = PGF(IAJS)
              TDW = THKAVG*HCDW
C-------------FILL TERM AND DIAGONAL
              ATERM = AMAT(IAJ)
              AMAT(IAJ) = TDW*AMAT(IAJ)*FAHL(IAJS)/
     *       (TDW*FAHL(IAJS) + AMAT(IAJ))
              AMAT(IA(N1)) = AMAT(IA(N1)) + ATERM - AMAT(IAJ)
C-------------FILL TERM AND DIAGONAL ON OTHER NODE
              ATERM = AMAT(ISYM(IAJ))
              AMAT(ISYM(IAJ)) = TDW*AMAT(ISYM(IAJ))*FAHL(IAJS)/
     *       (TDW*FAHL(IAJS) + AMAT(ISYM(IAJ)))
              AMAT(IA(N2)) = AMAT(IA(N2)) + ATERM - AMAT(ISYM(IAJ))
              GO TO 100
            ENDIF
          ENDDO
100       CONTINUE
        ENDIF
   10 CONTINUE
C
C16-----RETURN
      RETURN
      END
      SUBROUTINE SGWF2HFB7MC
C     ******************************************************************
C     MODIFY HORIZONTAL CONDUCTANCES (IN PGF) FOR CONFINED LAYERS TO
C     ACCOUNT FOR HORIZONTAL FLOW BARRIERS.  STORE UNMODIFIED HORIZONTAL
C     CONDUCTANCES IN HFB(7,#) TO ALLOW CALCULATION OF SENSITIVITIES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,TOP,BOT,PGF,FAHL,IA,JA,JAS,ISYM,IVC
      USE GWFBCFMODULE, ONLY: LAYCON
      USE GWFHFBMODULE,ONLY:NHFB,HFB
C     ------------------------------------------------------------------
C
C1------INITIALIZE ERROR FLAG TO ZERO.
      IERFLG=0
C
C2----DO FOR EACH BARRIER IN RANGE.
      DO 10 II = 1,NHFB
        N1 = HFB(1,II)
C
C3------FIND ROW AND COLUMN NUMBERS OF THE TWO CELLS ON BOTH SIDES
C3------OF THE BARRIER.
        N2 = HFB(2,II)
        K = HFB(3,II)
        IF (LAYCON(K).EQ.3.OR.LAYCON(K).EQ.1) CYCLE
C
C ------FIND BARRIER LOCATION AND ADJUST PGF
        DO IAJ = IA(N1)+1,IA(N1+1)-1
          JJ = JA(IAJ)
          IAJS = JAS(IAJ)
          IF(JJ.EQ.N2)THEN
            TH0 = TOP(N1) - BOT(N1)
            TH1 = TOP(N2) - BOT(N2)
            IF(IVC(IAJS).EQ.2)THEN
              THKAVG = MIN(TH0,TH1)
            ELSE
              THKAVG = (TH0+TH1)/2.0
            ENDIF
            TDW = THKAVG*HFB(6,II)
            HFB(7,II) = PGF(IAJS)
            PGF(IAJS) = TDW*PGF(IAJS)*FAHL(IAJS)/
     *       (TDW*FAHL(IAJS) + PGF(IAJS))
            GO TO 100
          ENDIF
        ENDDO
100     CONTINUE
   10 CONTINUE
C
C14-----RETURN
      RETURN
      END
C
      SUBROUTINE SGWF2HFB7CKU(IB1,IB2)
C     ******************************************************************
C     CHECK HFB CELL LOCATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,IA,JA
      USE GWFHFBMODULE,ONLY:HFB
C     ------------------------------------------------------------------
C
C1----INITIALIZE ERROR FLAG TO ZERO.
      IERFLG=0
C
C2----CHECK EACH BARRIER IN RANGE.
      DO 10 II = IB1,IB2
C
C3------FIND NODE NUMBERS OF THE TWO CELLS ON BOTH SIDES
C3------OF THE BARRIER AND REARRANGE HFB ARRAY.
        N1 = MIN(HFB(1,II),HFB(2,II))
        N2 = MAX(HFB(1,II),HFB(2,II))
        HFB(1,II) = N1
        HFB(2,II) = N2
        IADJ = 0
        DO IIA = IA(N1)+1,IA(N1+1)-1
          JJ = JA(IIA)
          IF(JJ.EQ.N2)THEN
            IADJ = 1
            GO TO 100
          ENDIF
        ENDDO
100     CONTINUE
        IF(IADJ.EQ.0)THEN
C
C4------CELLS ARE NOT ADJACENT. PRINT ERROR MESSAGE AND SET ERROR FLAG.
   80     WRITE (IOUT,1) II-IB1+1
    1     FORMAT (1X,'ERROR DETECTED IN LOCATION DATA OF BARRIER NO. ',
     &            I6)
          IERFLG=1
        ENDIF
   10 CONTINUE
C
C5------HALT EXECUTION IF ERRORS ARE DETECTED.
      IF (IERFLG.EQ.1) CALL USTOP(' ')
C
C6------RETURN
      RETURN
      END
      SUBROUTINE SGWF2HFB7RL(NLIST,HFB,LSTBEG,MXHFB,INPACK,
     &                       IOUT,LABEL,NCOL,NROW,NLAY,IPRFLG)
C     ******************************************************************
C     Read and print a list of HFB barriers.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*(*) LABEL
      DIMENSION HFB(7,MXHFB)
      CHARACTER*200 LINE,FNAME
      CHARACTER*1 DASH(120)
      DATA DASH/120*'-'/
      DATA NUNOPN/99/
C
C jfisher 2016-10-20: remove dependency on 'openspec.inc', contents included here.
      CHARACTER*20 ACCESS,FORM,ACTION(2)
      DATA ACCESS/'STREAM'/
      DATA FORM/'UNFORMATTED'/
      DATA (ACTION(I),I=1,2)/'READ','READWRITE'/
C     INCLUDE 'openspec.inc'
C
C     ------------------------------------------------------------------
C
C1------Check for and decode EXTERNAL and SFAC records.
      IN = INPACK
      ICLOSE = 0
      READ(IN,'(A)') LINE
      SFAC = 1.
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         IN = I
         IF (IPRFLG.EQ.1) WRITE(IOUT,111) IN
  111    FORMAT(1X,'Reading list on unit ',I4)
         READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME = LINE(ISTART:ISTOP)
         IN = NUNOPN
         IF (IPRFLG.EQ.1) WRITE(IOUT,115) IN,FNAME
  115    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE = 1
         READ(IN,'(A)') LINE
      END IF
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
         IF (IPRFLG.EQ.1) THEN
            WRITE(IOUT,116) SFAC
  116       FORMAT(1X,'LIST SCALING FACTOR= ',1PG12.5)
         ENDIF
         READ(IN,'(A)') LINE
      END IF
C
C2------Define label for printout.
      NBUF = LEN(LABEL)+3
      IF (IPRFLG.EQ.1) THEN
         WRITE(IOUT,103) LABEL
         WRITE(IOUT,104) (DASH(J),J=1,NBUF)
  103    FORMAT(1X,/1X,A)
  104    FORMAT(1X,400A)
      ENDIF
C
C3------Loop through the number of cells to read.
      N = NLIST+LSTBEG-1
      DO 250 II=LSTBEG,N
C
C4------Read a line into the buffer.  (The first line has already been read
C4------in order to scan for EXTERNAL and SFAC records.)
      IF(II.NE.LSTBEG) READ(IN,'(A)') LINE
C
C5------Read the non-optional values from a line.
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I1,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J1,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I2,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J2,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,FACTOR,IOUT,IN)
      N1 = NROW*NCOL*(K-1) + (I1-1)*NCOL + J1
      N2 = NROW*NCOL*(K-1) + (I2-1)*NCOL + J2
      HFB(1,II) = N1
      HFB(2,II) = N2
      HFB(3,II) = K
      HFB(4,II) = 0
      HFB(5,II) = 0
      HFB(6,II) = FACTOR*SFAC
      HFB(7,II) = 0.0
C
C6------Write the values that were read.
      NN = II-LSTBEG+1
      IF (IPRFLG.EQ.1) WRITE(IOUT,205) NN,K,I1,J1,I2,J2,HFB(6,II)
205   FORMAT(1X,I6,2X,I5,1X,4(2X,I5),2X,1PG11.4)
C
C7------Check for illegal grid location.
      IF(K.LT.1 .OR. K.GT.NLAY) THEN
         WRITE(IOUT,*) ' Layer number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
      IF(I1.LT.1 .OR. I1.GT.NROW .OR. I2.LT.1 .OR. I2.GT.NROW) THEN
         WRITE(IOUT,*) ' Row number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
      IF(J1.LT.1 .OR. J1.GT.NCOL .OR. J2.LT.1 .OR. J2.GT.NCOL) THEN
         WRITE(IOUT,*) ' Column number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
  250 CONTINUE
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
C
C8------Return.
      RETURN
      END
      SUBROUTINE SGWF2HFB7RLU(NLIST,HFB,LSTBEG,MXHFB,INPACK,
     &                       IOUT,LABEL,NODES,NODLAY,NLAY,IPRFLG)
C     ******************************************************************
C     Read and print a list of HFB barriers.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*(*) LABEL
      DIMENSION HFB(7,MXHFB),NODLAY(0:NLAY)
      CHARACTER*200 LINE,FNAME
      CHARACTER*1 DASH(120)
      DATA DASH/120*'-'/
      DATA NUNOPN/99/
C
C jfisher 2016-10-20: remove dependency on 'openspec.inc', contents included here.
      CHARACTER*20 ACCESS,FORM,ACTION(2)
      DATA ACCESS/'STREAM'/
      DATA FORM/'UNFORMATTED'/
      DATA (ACTION(I),I=1,2)/'READ','READWRITE'/
C     INCLUDE 'openspec.inc'
C
C     ------------------------------------------------------------------
C
C1------Check for and decode EXTERNAL and SFAC records.
      IN = INPACK
      ICLOSE = 0
      READ(IN,'(A)') LINE
      SFAC = 1.
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         IN = I
         IF (IPRFLG.EQ.1) WRITE(IOUT,111) IN
  111    FORMAT(1X,'Reading list on unit ',I4)
         READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME = LINE(ISTART:ISTOP)
         IN = NUNOPN
         IF (IPRFLG.EQ.1) WRITE(IOUT,115) IN,FNAME
  115    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE = 1
         READ(IN,'(A)') LINE
      END IF
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
         IF (IPRFLG.EQ.1) THEN
            WRITE(IOUT,116) SFAC
  116       FORMAT(1X,'LIST SCALING FACTOR= ',1PG12.5)
         ENDIF
         READ(IN,'(A)') LINE
      END IF
C
C2------Define label for printout.
      NBUF = LEN(LABEL)+3
      IF (IPRFLG.EQ.1) THEN
         WRITE(IOUT,103) LABEL
         WRITE(IOUT,104) (DASH(J),J=1,NBUF)
  103    FORMAT(1X,/1X,A)
  104    FORMAT(1X,400A)
      ENDIF
C
C3------Loop through the number of cells to read.
      N = NLIST+LSTBEG-1
      DO 250 II=LSTBEG,N
C
C4------Read a line into the buffer.  (The first line has already been read
C4------in order to scan for EXTERNAL and SFAC records.)
      IF(II.NE.LSTBEG) READ(IN,'(A)') LINE
C
C5------Read the non-optional values from a line.
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,N1,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,N2,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,FACTOR,IOUT,IN)
C-----GET LAYER NUMBER FOR N1
      DO K=1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        IF(N1.GE.NSTRT.AND.N1.LE.NNDLAY)THEN
          KK = K
          GO TO 101
        ENDIF
      ENDDO
101   CONTINUE
      IF(.NOT.(N2.GE.NSTRT.AND.N2.LE.NNDLAY))WRITE(IOUT,105)N1,N2,KK
105   FORMAT('FHB NODES ',I9,' AND ',I9,'ARE NOT BOTH IN LAYER ',I9)
C
      HFB(1,II) = N1
      HFB(2,II) = N2
      HFB(3,II) = KK
      HFB(4,II) = 0
      HFB(5,II) = 0
      HFB(6,II) = FACTOR*SFAC
      HFB(7,II) = 0.0
C
C6------Write the values that were read.
      NN = II-LSTBEG+1
      IF (IPRFLG.EQ.1) WRITE(IOUT,205) NN,N1,N2,HFB(6,II)
205   FORMAT(1X,I7,2X,I9,2X,I9,2X,1PG13.6)
C
C7------Check for illegal grid location.
      IF(N1.LT.1 .OR. N1.GT.NODES) THEN
         WRITE(IOUT,*) ' Node number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
  250 CONTINUE
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
C
C8------Return.
      RETURN
      END
      SUBROUTINE SGWF2HFB7SUB(IN,PACK,IOUTU,PTYP,HFB,LSTVL,MXHFB,
     1                MXACTFB,NHFB,IUNSTR,LABEL,LABEL2)
C     ******************************************************************
C     Read a parameter name, look it up in the list of parameters,
C     and substitute values into active part of HFB array.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      CHARACTER*(*) PACK,PTYP
      DIMENSION HFB(LSTVL,MXHFB)
      CHARACTER*(*) LABEL,LABEL2
      CHARACTER*200 LINE
      CHARACTER*10 CTMP1,CTMP2
C     ------------------------------------------------------------------
C
C1------The Listing File file unit is the absolute value of IOUTU.
C1------Read the parameter name.
      IOUT = ABS(IOUTU)
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
      WRITE(IOUT,1) LINE(ISTART:ISTOP)
    1 FORMAT(/,' Parameter:  ',A)
      IF(LINE(ISTART:ISTOP).EQ.' ') THEN
        WRITE(IOUT,*) ' Blank parameter name in the ',PACK,' file.'
        CALL USTOP(' ')
      END IF
C
C2------Find the parameter in the list of parameters.
      CTMP1=LINE(ISTART:ISTOP)
      CALL UPCASE(CTMP1)
      DO 100 IP=1,IPSUM
        CTMP2=PARNAM(IP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
          IF(PARTYP(IP).NE.PTYP) THEN
            WRITE(IOUT,11) PARNAM(IP),PARTYP(IP),PACK,PTYP
   11       FORMAT(1X,'Parameter type conflict:',/
     1        1X,'Named parameter:',A,' was defined as type:',A,/
     2        1X,'However, this parameter is used in the ',A,
     3          ' file, so it should be type:',A)
            CALL USTOP(' ')
          END IF
C
C3------Set indices to point to the barriers that correspond to the
C3------specified parameter.
          NLST=IPLOC(2,IP)-IPLOC(1,IP)+1
          NI=1
C
C4------Check that the parameter is not already active.
          IF (IACTIVE(IP).GT.0) THEN
            WRITE(IOUT,73) PARNAM(IP)
   73       FORMAT(/,1X,'*** ERROR: PARAMETER "',A,
     &          '" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD',/,
     &          ' -- STOP EXECUTION (UPARLSTSUB)')
            CALL USTOP(' ')
          ENDIF
C
C5------Set the active flag.
          IACTIVE(IP)=NI
C
C6------Accumulate the total number of active barriers in the list.
          NHFB=NHFB+NLST
          IF(NHFB.GT.MXACTFB) THEN
            WRITE(IOUT,83) NHFB,MXACTFB
   83       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE LIST ENTRIES (',I6,
     1       ')',/1X,'IS GREATER THAN THE MAXIMUM ALLOWED (',I6,')')
            CALL USTOP(' ')
          END IF
C
C7------Write label for barrier values if IOUTU is positive.
          IF(IOUTU.GT.0) THEN
             IF(IUNSTR.EQ.0) WRITE(IOUT,'(1X,A)') LABEL
             IF(IUNSTR.NE.0) WRITE(IOUT,'(1X,A)') LABEL2
             WRITE(IOUT,84)
   84        FORMAT(1X,56('-'))
           END IF
C
C8------Copy the values from the paramter location into the front part
C8------of the list where the currently active list is kept.
          IF(IUNSTR.EQ.0)THEN
          DO 90 I=1,NLST
              II=NHFB-NLST+I
              III=I-1+IPLOC(1,IP)+(NI-1)*NLST
              DO 85 J=1,7
                HFB(J,II)=HFB(J,III)
   85         CONTINUE
C
C8A-----Scale HYDCHR by the parameter value.
              HFB(6,II)=HFB(6,II)*B(IP)
              IL=HFB(1,II)
              IR1=HFB(2,II)
              IC1=HFB(3,II)
              IR2=HFB(4,II)
              IC2=HFB(5,II)
              IF(IOUTU.GT.0) WRITE(IOUT,89) II,IL,IR1,IC1,IR2,IC2,
     &          HFB(6,II)
   89         FORMAT(1X,I6,2X,I5,1X,4(2X,I5),2X,1PG11.4)
   90       CONTINUE
          ELSE
          DO 91 I=1,NLST
              II=NHFB-NLST+I
              III=I-1+IPLOC(1,IP)+(NI-1)*NLST
              DO 86 J=1,7
                HFB(J,II)=HFB(J,III)
   86         CONTINUE
C
C8A-----Scale HYDCHR by the parameter value.
              HFB(6,II)=HFB(6,II)*B(IP)
              N1=HFB(1,II)
              N2=HFB(2,II)
              IF(IOUTU.GT.0) WRITE(IOUT,88) II,N1,N2,HFB(6,II)
   88         FORMAT(1X,I7,2X,I9,2X,I9,2X,1PG13.6)
   91       CONTINUE
          ENDIF
C
C8B------After moving the data, return.
          RETURN
        END IF
  100 CONTINUE
C
C9------All parameter names have been checked without finding the
C9------parameter. Write an error message and stop.
      WRITE(IOUT,*) ' The ',PACK,
     1   ' file specifies an undefined parameter:',LINE(ISTART:ISTOP)
      CALL USTOP(' ')
C
      END
      SUBROUTINE GWF2HFB7U1DA
C  Deallocate HFB data for a grid.
      USE GWFHFBMODULE
C
        DEALLOCATE(MXHFB)
        DEALLOCATE(NHFB)
        DEALLOCATE(IPRHFB)
        DEALLOCATE(NHFBNP)
        DEALLOCATE(NPHFB)
        DEALLOCATE(IHFBPB)
        DEALLOCATE(HFB)
C
      RETURN
      END
