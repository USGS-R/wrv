      MODULE GNCnMODULE
        INTEGER, POINTER::MXGNCn,NGNCn,IPRGNCn,NGNCNPn,NPGNCn,IGNCPBn,
     1                    I2Kn,ISYMGNCn,MXADJn,IFLALPHAn
        REAL,    DIMENSION(:,:), ALLOCATABLE   ::GNCn,SATCn
        INTEGER, DIMENSION (:), ALLOCATABLE :: LGNCn
        INTEGER, DIMENSION (:,:,:),  ALLOCATABLE :: IRGNCn
      END MODULE GNCnMODULE


      SUBROUTINE GNCn2DISU1AR(IUGNCn)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR GHOST NODE WITH n ADDITIONAL CONTRIBUTING NODES
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:LAYHDT,IOUT,NODES,IUNSTR,NODLAY,IUNIT,
     1                INGNCn,NEQS,NLAY
      USE GNCnMODULE,  ONLY:MXGNCn,NGNCn,IPRGNCn,NGNCNPn,NPGNCn,IGNCPBn,
     1    GNCn,SATCn,LGNCn,IRGNCn,I2Kn,ISYMGNCn,MXADJn,IFLALPHAn
C
      INTEGER IUGNCn, MXACTGNn
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE.
      INGNCn = IUNIT(IUGNCn)
      WRITE(IOUT,1) INGNCn
    1 FORMAT(1X,/1X,'GNCn -- GHOST NODE CORRECTION PACKAGE WITH n',
     &         1X,'ADJACENT CONTRIBUTING NODES',
     &      ' VERSION 1, 6/18/2011.',/,'   INPUT READ FROM UNIT ',I4)
C
C2------Allocate scalar data.
      ALLOCATE(MXGNCn,NGNCn,IPRGNCn,NGNCNPn,NPGNCn,IGNCPBn,
     1  I2Kn,ISYMGNCn,MXADJn,IFLALPHAn)
C
C3------READ AND PRINT maximum number of GNCn nodes and flags for GNCn package
      CALL URDCOM(INGNCn,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPGNCn,DUM,IOUT,INGNCn)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXFBPn,DUM,IOUT,INGNCn)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NGNCNPn,DUM,IOUT,INGNCn)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXADJn,DUM,IOUT,INGNCn)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I2Kn,DUM,IOUT,INGNCn)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISYMGNCn,DUM,IOUT,INGNCn)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFLALPHAn,DUM,IOUT,INGNCn)
      WRITE(IOUT,500) NPGNCn,MXFBPn
  500 FORMAT(1X,I5,' PARAMETERS DEFINE A MAXIMUM OF ',I6,
     &       ' GHOST NODE CORRECTION LOCATIONS')
      WRITE(IOUT,530) NGNCNPn
  530 FORMAT(1X,I6,' GHOST NODE CORRECTION LOCATIONS NOT DEFINED BY',
     &       ' PARAMETERS')
      WRITE(IOUT,531) MXADJn,I2Kn,ISYMGNCn,IFLALPHAn
  531 FORMAT(1X,' MAXIMUM NUMBER OF ADJACENT CONTRIBUTING NODES = ',
     &  I6/1X,' FLAG INDICATING SECOND-ORDER CORRECTION ON',
     &      ' TRANSMISSIVITY TERM FOR UNCONFINED FLOW, I2Kn = ',I6
     &     /1X,' FLAG FOR SYMMETRIC IMPLEMENTATION OF GNCn, ISYMGNCn = '
     & ,I6/1X,' FLAG FOR INPUT OF SATURATED CONDUCTANCES, IFLALPHAn = '
     & ,I6 )
C
C4------LOOK FOR NOPRINT OPTION.
      IPRGNCn = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INGNCn)
      IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
        WRITE(IOUT,3)
    3   FORMAT(1X,
     &'LISTS OF GNC CELLS WILL NOT BE PRINTED')
        IPRGNCn = 0
      END IF
C
C5------CALCULATE AMOUNT OF SPACE USED BY GNCn PACKAGE AND ALLOCATE GNCn.
      MXACTGNn = NGNCNPn+MXFBPn
      IGNCPBn = MXACTGNn + 1
      MXGNCn = MXACTGNn + MXFBPn
      ALLOCATE (GNCn(3+2*MXADJn,MXGNCn))
      IF(IFLALPHAn.EQ.1) ALLOCATE (SATCn(MXADJN+1,MXGNCn)) !STORES SATURATED CONDUCTANCES FOR COMPUTING CONTRIBUTING FACTORS
      IF(ISYMGNCn.EQ.0) ALLOCATE (IRGNCn(2,MXADJn,MXGNCn))
      IF(I2Kn.EQ.1) ALLOCATE (LGNCn(MXGNCn)) ! NEED TO ALSO STORE LAYER OF GNCn NODE AND NODAL TRANSMISSIVITY (OR S)
C
C6------READ PARAMETER DEFINITIONS (ITEMS 2 AND 3)
      WRITE(IOUT,600) NPGNCn
  600 FORMAT(//,1X,I5,' GNC parameters')
      IF (NPGNCn.GT.0) THEN
        LSTSUM = IGNCPBn
        DO 20 K = 1,NPGNCn
          LSTBEG = LSTSUM
          CALL UPARLSTRP(LSTSUM,MXGNCn,INGNCn,IOUT,IP,'GNC ','GNC ',
     &                   1,NUMINST)
          IF(NUMINST.GT.0) THEN
            WRITE(IOUT,*) ' INSTANCES ARE NOT SUPPORTED FOR GNC'
            CALL USTOP(' ')
          END IF
          NLST=LSTSUM-LSTBEG
          CALL SGNCn2DISU1RLU(NLST,GNCn,LSTBEG,MXGNCn,INGNCn,
     &       IOUT,'GNC-NODE    NODEn   NODEm    NODESj FACTORSj    ',
     &       NEQS,IPRGNCn,I2Kn,LGNCn,IRGNCn,ISYMGNCn,MXADJn,IFLALPHAn)
   20   CONTINUE
      ENDIF
C
C7------READ GHOST NODES NOT DEFINED BY PARAMETERS (ITEM 4)
      NGNCn = 0
      WRITE(IOUT,610) NGNCNPn
  610 FORMAT(/,1X,I6,' GHOST NODES NOT DEFINED BY PARAMETERS')
      IF(NGNCNPn.GT.0) THEN
        CALL SGNCn2DISU1RLU(NGNCNPn,GNCn,1,MXGNCn, INGNCn,IOUT,
     &       'GNC-NODE    NODEn   NODEm    NODESj FACTORSj    ',
     &        NEQS,IPRGNCn,I2Kn,LGNCn,IRGNCn,ISYMGNCn,MXADJn,IFLALPHAn)
        NGNCn = NGNCn + NGNCNPn
      ENDIF
C
C8------SUBSTITUTE DATA FOR PARAMETERIZED GNCn NODES INTO ACTIVE SECTION
C8------OF GNCn ARRAY
      IOUTU = IOUT
      IF (IPRGNCn.EQ.0) IOUTU = -IOUT
      MXACTGNn= IGNCPBn-1
      CALL PRESET('GNC ')
      IF(NPGNCn.GT.0) THEN
C
C9------READ NUMBER OF ACTIVE GNCn PARAMETERS (ITEM 5)
        READ(INGNCn,*) NACTGNCn
        IF (NACTGNCn.GT.0) THEN
          DO 650 I = 1,NACTGNCn
C
C10-----READ AND ACTIVATE A GNCn PARAMETER (ITEM 6)
            CALL SGNCn2DISU1SUB(INGNCn,'GNC ',IOUTU,'GNC ',GNCn,
     1             MXGNCn,MXADJn,MXACTGNn,NGNCn,IFLALPHAn,
     3         'GNC-NODE    NODEn   NODEm    NODESj FACTORSj    ')
  650     CONTINUE
        ENDIF
      ENDIF
C11-----REFLECT TOTAL GNCn CELLS IN OUTPUT LISTING FILE
      WRITE (IOUT,660) NGNCn
  660 FORMAT(/,1X,1I6,' GNC CELLS')
C
C12---RETURN
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE GNCn2DISU1FM
C     ******************************************************************
C     MODIFY EQUATION FOR GHOST NODE CONTRIBUTIONS
C     ROWS OF MATRIX FOR IMPLICIT; RHS VECTOR FOR EXPLICIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,HNEW,TOP,BOT,IBOUND,IOUT,
     1                      AMAT,PGF,FAHL,IA,JA,JAS,ISYM,RHS,HNEW
      USE GNCnMODULE,ONLY:NGNCn,GNCn,IRGNCn,ISYMGNCn,MXADJn
      DOUBLE PRECISION ALPHA,BETA,Cnm,ATERM,RTERM
C     ------------------------------------------------------------------
C
C1------FOR EACH GHOST NODE, GET CONDUCTANCE AND STORE
      DO 10 IG=1,NGNCn
C
C2------GET CELL LOCATIONS AND PROPERTIES.
          N1 = GNCn(1,IG)
          N2 = GNCn(2,IG)
C
C3-----------FIND LOCATION OF N2 IN ROW N1 FOR CONDUCTANCE TERM
          DO II = IA(N1)+1,IA(N1+1)-1
            JJ = JA(II)
            IIS = JAS(II)
            IF(JJ.EQ.N2)THEN
              GNCn(2*MXADJn+3,IG) = AMAT(II)
              GO TO 100
            ENDIF
          ENDDO
100       CONTINUE
   10 CONTINUE
C---------------------------------------------------------------------------
C1------FOR EACH GHOST NODE, MODIFY COLUMNS N AND J (GHOST NODE CONTRIBUTORS)
C1------IN ROWS N AND M FOR FLOW BETWEEN NODES N AND M.
      DO 20 IG=1,NGNCn
C
C2------GET CELL LOCATIONS AND PROPERTIES.
          N1 = GNCn(1,IG)
          N2 = GNCn(2,IG)
          IF(IBOUND(N1).EQ.0.OR.IBOUND(N2).EQ.0) CYCLE
C
C3-----------FIND LOCATION OF N2 IN ROW N1 AND ADJUST AMAT FOR ROWS N1 AND N2
          DO II = IA(N1)+1,IA(N1+1)-1 !this whole loop is to get ISYM(II). could save that in IRGNCn(3,IG)
            JJ = JA(II)
            IIS = JAS(II)
            IF(JJ.EQ.N2)THEN
              Cnm = GNCn(2*MXADJn+3,IG)
              DO IADJn = 1,MXADJn
                Jn = GNCn(2+IADJn,IG)
                IF(IBOUND(JN).EQ.0) CYCLE
                ALPHA = GNCn(MXADJn+2+IADJn,IG)
                ATERM = ALPHA * Cnm
C3A-------------UNSYMMETRIC IMPLEMENTATION
                IF(ISYMGNCn.EQ.0)THEN
                  LOC_JN = IRGNCn(1,IADJn,IG)
                  LOC_JM = IRGNCn(2,IADJn,IG)
C
C3A1----------------FOR ROW N1
C3A1---------------LESSEN N1 LOCATION BY (ALPHA-1)*Cnm IN DIAGONAL OF ROW N1
                  AMAT(IA(N1)) = AMAT(IA(N1)) + ATERM
C3A2---------------PUT -ATERM IN LOCATION J OF ROW N
                  AMAT(LOC_JN) = AMAT(LOC_JN) - ATERM
C3B1----------------FOR ROW N2
C3B1---------------LESSEN N1 LOCATION BY (ALPHA-1)*Cnm IN OFF-DIAGONAL OF ROW N2
                  AMAT(ISYM(II)) = AMAT(ISYM(II)) - ATERM
C3B2---------------PUT ATERM IN LOCATION J OF ROW M
                  AMAT(LOC_JM) = AMAT(LOC_JM) + ATERM
                ELSE
C4A---------------SYMMETRIC IMPLEMENTATION
                  RTERM = ATERM*(HNEW(N1) - HNEW(Jn))
                  RHS(N1) = RHS(N1) - RTERM
                  RHS(N2) = RHS(N2) + RTERM
                ENDIF
              ENDDO
              GO TO 200
            ENDIF
          ENDDO
200       CONTINUE
   20 CONTINUE

C
C5------RETURN
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE GNCn2DISU1N4
C     ******************************************************************
C     MODIFY ROWS OF MATRIX FOR GHOST NODE CONTRIBUTIONS TO NEWTON TERM
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,         ONLY:NCOL,NROW,HNEW,TOP,BOT,IBOUND,IVC,ICONCV,
     1                      AMAT,RHS,PGF,FAHL,IA,JA,JAS,ISYM
      USE GNCnMODULE,  ONLY:NGNCn,GNCn,IRGNCn,MXADJn
      USE SMSMODULE,      ONLY: DKDHC
      DOUBLE PRECISION ALPHA,Cnm,TERM,HJbar,CONSTERM
C     ------------------------------------------------------------------
C
C1------FOR EACH GHOST NODE, MODIFY COLUMNS N AND J (GHOST NODE CONTRIBUTORS)
C1------IN ROWS N AND M FOR FLOW BETWEEN NODES N AND M.
      DO 10 IG=1,NGNCn
C
C2------GET CELL LOCATIONS AND PROPERTIES.
          N1 = GNCn(1,IG)
          N2 = GNCn(2,IG)
          IF(IBOUND(N1).EQ.0.OR.IBOUND(N2).EQ.0) CYCLE
C
C3-----------FIND LOCATION OF N2 AN ROW N1 AND ADJUST AMAT FOR ROWS N1 AND N2
c------------***csp can store PGF in GNCn location and no need to do II loop below
          DO II = IA(N1)+1,IA(N1+1)-1
            JJ = JA(II)
            IIS = JAS(II)
            IF(JJ.EQ.N2)THEN
              IF(IVC(IIS).EQ.1.AND.ICONCV.EQ.1) GO TO 100
              IUPS = JJ
              IF(HNEW(JJ).LT.HNEW(N1)) IUPS = N1
              IPIV1 = IA(N1)
              IPIV2 = IA(JJ)
C
              DO IADJn = 1,MXADJn
                Jn = GNCn(2+IADJn,IG)
                IF(IBOUND(Jn).EQ.0) CYCLE
                ALPHA = GNCn(MXADJn+2+IADJn,IG)
                CONSTERM = - PGF(IIS)*ALPHA *(HNEW(N1) - HNEW(Jn))
                IF(IUPS.EQ.N1)THEN
                  TERM = CONSTERM*DKDHC(IIS)
                  AMAT(IPIV1) = AMAT(IPIV1) + TERM
                  IF(IBOUND(JJ).GT.0)THEN ! DO NOT FILL NEWTON TERM IN ROW OF CONSTANT HEAD
                    AMAT(ISYM(II)) = AMAT(ISYM(II)) - TERM
                  ENDIF
                  RHS(N1) = RHS(N1) + TERM* HNEW(N1)
                  RHS(JJ) = RHS(JJ) - TERM* HNEW(N1)
                ELSE
                  TERM = CONSTERM*DKDHC(IIS)
                  IF(IBOUND(N1).GT.0)THEN  ! DO NOT FILL NEWTON TERM IN ROW OF CONSTANT HEAD
                    AMAT(II) = AMAT(II) + TERM
                  ENDIF
                  AMAT(IPIV2) = AMAT(IPIV2) - TERM
                  RHS(N1) = RHS(N1) + TERM*HNEW(JJ)
                  RHS(JJ) = RHS(JJ) - TERM*HNEW(JJ)
                ENDIF
              ENDDO
C
              GO TO 100
            ENDIF
          ENDDO
100       CONTINUE
   10 CONTINUE
C
C4-----RETURN
      RETURN
      END
C--------------------------------------------------------------------------------------
      SUBROUTINE SGNCn2DISU1RLU(NLIST,GNCn,LSTBEG,MXGNCn,INPACK,IOUT,
     &  LABEL,NEQS,IPRFLG,I2Kn,LGNCn,IRGNCn,ISYMGNCn,MXADJn,IFLALPHAn)
C     ******************************************************************
C     Read and print a list of GNCn cells.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*(*) LABEL
      DIMENSION GNCn(2*MXADJn+3,MXGNCn)
      INTEGER LGNCn(MXGNCn),IRGNCn(2,MXADJn,MXGNCn)
      CHARACTER*200 LINE,FNAME
      CHARACTER*1 DASH(120)
      DATA DASH/120*'-'/
      DATA NUNOPN/99/
C
C jfisher 2015-02-23: remove dependency on 'openspec.inc'
C     INCLUDE 'openspec.inc'
      CHARACTER*20 ACCESS,FORM,ACTION(2)
      DATA ACCESS/'STREAM'/
      DATA FORM/'UNFORMATTED'/
      DATA (ACTION(I),I=1,2)/'READ','READWRITE'/
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
C5------Read the non-optional values from a line
C5------AND SET GNCn ARRAY WITH INFORMATION
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,N1,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,N2,R,IOUT,IN)
      GNCn(1,II) = N1
      GNCn(2,II) = N2
      DO IADJn = 1,MXADJn
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,N3,R,IOUT,IN)
        GNCn(2+IADJn,II) = N3
      ENDDO
      DO IADJn = 1,MXADJn
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,FACTOR1,IOUT,IN)
        GNCn(2+MXADJn+IADJn,II) = FACTOR1
      ENDDO
      IF(IFLALPHAn.EQ.1)THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,FACTOR1,IOUT,IN)
        GNCn(3+2*MXADJn,II) = FACTOR1
      ENDIF
      IF(ISYMGNCn.EQ.0)THEN
        DO IADJn = 1,MXADJn
          IRGNCn(1,IADJn,II) = 0
          IRGNCn(2,IADJn,II) = 0
        ENDDO
      ENDIF
      IF(I2Kn.NE.0) THEN
        LGNCn(II) = 0
      ENDIF
C
C6------Write the values that were read.
      NN = II-LSTBEG+1
      IF (IPRFLG.EQ.1) THEN
        IF(IFLALPHAn.EQ.0) WRITE(IOUT,205) NN,N1,N2,
     1  (GNCn(2+IADJn,II),IADJn=1,MXADJn),
     1  (GNCn(2+MXADJn+IADJn,II),IADJn=1,MXADJn)
        IF(IFLALPHAn.EQ.1) WRITE(IOUT,205) NN,N1,N2,
     1  (GNCn(2+IADJn,II),IADJn=1,MXADJn),
     1  (GNCn(2+MXADJn+IADJn,II),IADJn=1,MXADJn),GNCn(3+2*MXADJn,II)
      ENDIF
205   FORMAT(1X,I7,2X,I9,2X,I9,200(2X,1PG13.6))
C
C7------Check for illegal grid location.
      IF(N1.LT.1 .OR. N1.GT.NEQS) THEN
         WRITE(IOUT,*) ' Node number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
  250 CONTINUE
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
C
C8------Return.
      RETURN
      END
      SUBROUTINE SGNCn2DISU1SUB(IN,PACK,IOUTU,PTYP,GNCn,MXGNCn,
     1    MXADJn,MXACTGNn,NGNCn,IFLALPHAn,LABEL)
C     ******************************************************************
C     Read a parameter name, look it up in the list of parameters,
C     and substitute values into active part of GNCn array.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      CHARACTER*(*) PACK,PTYP
      DIMENSION GNCn(2*MXADJn+3,MXGNCn)
      CHARACTER*(*) LABEL
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
C3------Set indices to point to the GNCn cells that correspond to the
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
C6------Accumulate the total number of active GNCn cells in the list.
          NGNCn=NGNCn+NLST
          IF(NGNCn.GT.MXACTGNn) THEN
            WRITE(IOUT,83) NGNCn,MXACTGNn
   83       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE LIST ENTRIES (',I6,
     1       ')',/1X,'IS GREATER THAN THE MAXIMUM ALLOWED (',I6,')')
            CALL USTOP(' ')
          END IF
C
C7------Write label for GNCn cell values if IOUTU is positive.
          IF(IOUTU.GT.0) THEN
             WRITE(IOUT,'(1X,A)') LABEL
             WRITE(IOUT,84)
   84        FORMAT(1X,56('-'))
           END IF
C
C8------Copy the values from the paramter location into the front part
C8------of the list where the currently active list is kept.
          DO 91 I=1,NLST
              II=NGNCn-NLST+I
              III=I-1+IPLOC(1,IP)+(NI-1)*NLST
              DO 86 J=1,2*MXADJn+3
                GNCn(J,II)=GNCn(J,III)
   86         CONTINUE
C
C8A-----Scale AlphaGNC,  by the parameter value.
              DO IADJn = 1,MXADJn
                GNCn(2+MXADJn+IADJn,II)=GNCn(2+MXADJn+IADJn,II)*B(IP)
              ENDDO
              IF(IFLALPHAn.EQ.1) GNCn(3+2*MXADJn,II) =
     &           GNCn(3+2*MXADJn,II) * B(IP)
              N1=GNCn(1,II)
              N2=GNCn(2,II)
              IF(IOUTU.GT.0) WRITE(IOUT,88) II,N1,N2,
     1  (GNCn(2+IADJn,II),IADJn=1,MXADJn),
     1  (GNCn(2+MXADJN+IADJn,II),IADJn=1,MXADJn)
   88         FORMAT(1X,I7,2X,I9,2X,I9,200(2X,1PG13.6))
   91     CONTINUE
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
C-----------------------------------------------------------------------
      SUBROUTINE GNCn2DISU1DA
C  Deallocate GNCn data for a grid.
      USE GNCnMODULE
C
        DEALLOCATE(MXGNCn)
        DEALLOCATE(NGNCn)
        DEALLOCATE(IPRGNCn)
        DEALLOCATE(NGNCNPn)
        DEALLOCATE(NPGNCn)
        DEALLOCATE(IGNCPBn)
        DEALLOCATE(GNCn)
        DEALLOCATE(MXADJn,I2Kn)
C
      RETURN
      END
C------------------------------------------------------------------------
       SUBROUTINE ADDIAJA_GNCn(SMAT)
C     ******************************************************************
C     ADD IA AND JA OF GNCn NODES TO THE GLOBAL IA AND JA ARRAYS AND
C     PREPARE GNCn ARRAYS
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
C
      USE GLOBAL,     ONLY:NODES,NEQS,NJA,IA,JA
      USE GNCNMODULE,ONLY:NGNCN,ISYMGNCN,MXADJN,GNCN
      USE SPARSEMODULE
      TYPE(SPARSEMATRIX),INTENT(INOUT) :: SMAT
C
C     ----------------------------------------------------------------------
C1--------SKIP CHANGING MATRIX STRUCTURE IF SYMMETRIC MATRIX WITH
C1--------RHS UPDATE
      IF(ISYMGNCN.EQ.1) RETURN
C
C2------ADD THE GNC CONNECTIONS TO SMAT.  FIRST M AND J AND THEN
C2------N AND J.  THE LAST ARG IN THE ADDCONNECTION CALL MEANS
C2------DUPLICATES ARE NOT STORED
      DO IGNCN=1,NGNCN
          N = GNCN(1,IGNCN)
          M = GNCN(2,IGNCN)
          DO IADJN=1,MXADJN
              J=GNCN(2+IADJN,IGNCN)
              CALL SMAT%ADDCONNECTION(M,J,1)
              CALL SMAT%ADDCONNECTION(J,M,1)
              CALL SMAT%ADDCONNECTION(N,J,1)
              CALL SMAT%ADDCONNECTION(J,N,1)
          ENDDO
      ENDDO
C--------------------------------------------------------------------------
C9------RETURN
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE SGNCn2DISU1MC
C     ******************************************************************
C     PREPARE GNCn VARIABLES:
C     FIND AND STORE LOCATION OF NODE J IN ROWS N AND M.
C     STORE SATURATED CONDUCTIVITY FOR CONTRIBUTING FACTOR CALCULATIONS IF IFLALPHAn=1
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,IA,JA,JAS,ISYM,NODLAY,NLAY
      USE GNCnMODULE,  ONLY:NGNCn,GNCn,SATCn,LGNCn,IRGNCn,I2Kn,
     1  ISYMGNCn,MXADJn,IFLALPHAn
      DOUBLE PRECISION SUMC
C     ------------------------------------------------------------------
C
C1----DO FOR EACH GNCn CELL.
      DO 10 IG = 1,NGNCn
C
C2------FIND NODE NUMBERS FOR GHOST AND ASSOCIATED CELLS.
        N1 = GNCn(1,IG)
        N2 = GNCn(2,IG)
        DO IADJn = 1,MXADJn
          N3 = GNCn(2+IADJn,IG)
C
C---------------------------------------------------------------------------
          IF(ISYMGNCn.EQ.0)THEN
C4----------FIND LOCATION OF GHOST CELL J IN ROW N AND SAVE
            DO II = IA(N1),IA(N1+1)-1
              JJ = JA(II)
              IF(JJ.EQ.N3)THEN
                IRGNCn(1,IADJn,IG) = II
                GO TO 200
              ENDIF
            ENDDO
200         CONTINUE
C
C5----------FIND LOCATION OF GHOST CELL J IN ROW M AND SAVE
            DO II = IA(N2),IA(N2+1)-1
              JJ = JA(II)
              IF(JJ.EQ.N3)THEN
                IRGNCn(2,IADJn,IG) = II
                GO TO 201
              ENDIF
            ENDDO
201         CONTINUE
          ENDIF
        ENDDO
C--------------------------------------------------------------------------
   10 CONTINUE
C--------------------------------------------------------------------------
C
C7------FIND AND STORE LAYER NUMBER OF GNCn NODE FOR SECOND ORDER K CORRECTION
      IF(I2Kn.EQ.1)THEN
        DO IG=1,NGNCn
          N1 = GNCn(1,IG)
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            IF(N1.GE.NSTRT.AND.N1.LE.NNDLAY)THEN
              LGNCn(IG) = K
              GO TO 101
            ENDIF
          ENDDO
101        CONTINUE
        ENDDO
      ENDIF
C
C8------STORE SATURATED CONDUCTANCE OF EACH CONTRIBUTING NODE FOR IFLALPHAn=1
      IF(IFLALPHAn.EQ.1)THEN
        DO IG=1,NGNCn
          DO IADJn = 1,MXADJn+1
            SATCn(IADJn,IG) = GNCn(2+MXADJn+IADJn,IG)
          ENDDO
        ENDDO
C9------COMPUTE AND SAVE CONTRIBUTING FACTORS FROM SATURATED CONDUCTANCES
        DO IG=1,NGNCn
          SUMC = SATCn(MXADJn+1,IG)
          DO IADJn = 1,MXADJn
            SUMC = SUMC + SATCn(IADJn,IG)
          ENDDO
          DO IADJn = 1,MXADJn
            GNCn(2+MXADJn+IADJn,IG) = SATCn(IADJn,IG) / SUMC
          ENDDO
        ENDDO
      ENDIF
C
C9------RETURN
      RETURN
      END
C
C -----------------------------------------------------------------------
      SUBROUTINE SGNCn2BCFU1S
C     ******************************************************************
C     FIRST, FILL CONTRIBUTING FACTORS FROM SAT CONDUCTANCES AND S_UPS.
C     FILL SECOND ORDER CORRECTION OF K ON UNCONFINED GHOST NODE TERMS
C     ******************************************************************
      USE GLOBAL, ONLY:JA,IA,NODES,NEQS,IVC,JAS,IBOUND
      USE GNCnMODULE,ONLY:NGNCn,GNCn,LGNCn,I2Kn,IFLALPHAn,SATCn,MXADJn
      USE GWFBCFMODULE, ONLY: LAYCON
      REAL,    DIMENSION(:), ALLOCATABLE :: SATLOC
      DOUBLE PRECISION SUPS,SUMC
C-----------------------------------------------------------------------
C1----SKIP IF CONTRIBUTING FACTOR IS NOT CALCULATED BUT READ
      IF(IFLALPHAn.EQ.0) GO TO 999
      ALLOCATE(SATLOC(MXADJn+1))
C2------FOR EACH GNCn NODE
      DO IG =1,NGNCn
        ND1 = GNCn(1,IG)
C3------GET UPSTREAM SATURATION BETWEEN CONNECTING NODE AND GNC LOCATION
        DO IADJn = 1,MXADJn
          ND3 = GNCn(2+IADJn,IG)
          CALL SGNCn2SUPS(IG,ND1,ND3,SUPS)
          SATLOC(IADJn) = SUPS
        ENDDO
        CALL SGNCn2SUPS(IG,ND1,ND1,SUPS)
        SATLOC(MXADJn+1) = SUPS
C4--------COMPUTE CONTRIBUTING FACTOR FOR ADJACENT CONTRIBUTING NODES
        SUMC = 0.0
        DO IADJn = 1,MXADJn+1
          SUMC = SUMC + SATCn(IADJn,IG)*SATLOC(IADJn)
        ENDDO
        DO IADJn = 1,MXADJn
          GNCn(2+MXADJn+IADJn,IG) = SATCn(IADJn,IG)*SATLOC(IADJn)/SUMC
        ENDDO
      ENDDO
      DEALLOCATE(SATLOC)
C
C-----------------------------------------------------------------------
999   CONTINUE
C5-------RETURN IF SECOND ORDER TERM IS NOT REQUIRED
      IF(I2Kn.EQ.0) RETURN
C
C6------DO FOR EACH GNCn NODE
      DO IG=1,NGNCn
        ND1 = GNCn(1,IG)
        ND2 = GNCn(2,IG)
C7-----SKIP ADJUSTMENTS IF ANY CONTRIBUTING NODE IS INACTIVE
        IF(IBOUND(ND1).EQ.0.OR.IBOUND(ND2).EQ.0) GO TO 11
        DO IADJn = 1,MXADJn
          N3 = GNCn(2+IADJn,IG)
          IF(IBOUND(N3).EQ.0) GO TO 11
        ENDDO
C8------FIND LOCATION IN AMAT OF N2 IN ROW OF N1 AND
C8------FIND IF CONNECTION OF N TO M IS VERTICAL OR HORIZONTAL
        DO II=IA(ND1)+1,IA(ND1+1)-1
          JJ = JA(II)
          IF(JJ.EQ.ND2)THEN
            IIS = JAS(II)
            IVD = IVC(IIS)
            GO TO 10
          ENDIF
        ENDDO
10      CONTINUE
C
C9-------FOR HORIZONTAL CONNECTION
        IF(IVD.EQ.0.OR.IVD.EQ.2)THEN
C10---------COMPUTE GHOST K-VALUE AND HORIZONTAL CONDUCTANCE AS PER LAYCON
          K = LGNCn(IG)
          IF(LAYCON(K).EQ.3. OR.LAYCON(K).EQ.1) THEN
            IF(ND1.GT.NODES.OR.ND2.GT.NODES) CYCLE ! CORRECTION ON CLN ONLY FOR LAYCON=4
            CALL SGNCn2BCFU1WDS(IG,ND1,ND2,II,IIS)
          ELSEIF(LAYCON(K).EQ.4)THEN
            CALL SGNCn2BCFU1WDS4(IG,ND1,ND2,II,IIS)
          ENDIF
        ENDIF
  11    CONTINUE
      ENDDO
C
C11------RETURN
      RETURN
      END
C -----------------------------------------------------------------------
      SUBROUTINE SGNCn2BCFU1WDS(IG,N,M,II,IIS)
C     ******************************************************************
C     FILL SECOND ORDER CORRECTION OF K ON UNCONFINED GHOST NODE TERMS
C     AND COMPUTE HORIZONTAL CONDUCTANCE AND ADJUST AMAT FOR LAYCON=1 OR 3
C     ******************************************************************
      USE GLOBAL, ONLY:JA,IA,NODES,NEQS,BUFF,AMAT,HNEW,TOP,BOT,
     1   PGF,ISYM,CL1,CL2,IBOUND
      USE GNCnMODULE, ONLY:NGNCn,GNCn,LGNCn,I2Kn,MXADJn
      USE GWFBCFMODULE, ONLY: LAYCON,HK
      DOUBLE PRECISION HD,BBOT,TTOP,THCK,HTMP,ANUM,THIK1,THIK2
      DOUBLE PRECISION Cnm,CnmO,TRAN,ALPHAN
C-----------------------------------------------------------------------
C
C1------GET HEAD AT GHOST NODE AND COMPUTE SATURATED THICKNESS
      ALPHAN = 0.0
      HD = 0.0
      DO IADJn = 1,MXADJn
        N3 = GNCn(2+IADJn,IG)
        ALPHA = GNCn(2+MXADJn+IADJn,IG)
        ALPHAN = ALPHAN + ALPHA
        HD = HD + ALPHA*HNEW(N3)
      ENDDO
      HD = HD + (1.0-ALPHAN)*HNEW(N)
C
      BBOT=BOT(N)
      K = LGNCn(IG)
      IF(LAYCON(K).EQ.1) GO TO 50
      TTOP=TOP(N)
      IF(HD.GT.TTOP) HD=TTOP
   50 THCK=HD-BBOT
C
C2------SET TRANSMISSIVITY IN BUFF
      IF(THCK.LE.0.0D0) THEN
C2A-----SET TRANSMISSIVITY TO ZERO IF THICKNESS IS <0
        BUFF(N) = 0.0
      ELSE
C
C2B-----CALCULATE TRANSMISSIVITY IF SATURATED THICKNESS>0
         BUFF(N) = THCK * HK(N)
      ENDIF
C------------------------------------------------------------------------------
      ISLOC = ISYM(II)
C3------COMPUTE CONDUCTANCE TERM BETWEEN N AND M
      EL1 = CL1(IIS)
      EL2 = CL2(IIS)
      THIK1 = 1.0
      THIK2 = 1.0
      CALL CBCK12(II,N,M,K,EL1,EL2,ANUM,BUFF,
     *      THIK1,THIK2)
      Cnm = PGF(IIS) * ANUM
      CnmO = AMAT(II)
C
C4------ADJUST CONDUCTIVITY TERM FOR ROW N
      AMAT(II) = AMAT(II) -CnmO + Cnm
      AMAT(IA(N)) = AMAT(IA(N)) +CnmO - Cnm
C5------ADJUST CONDUCTIVITY TERM FOR ROW M
      AMAT(ISLOC) = AMAT(ISLOC) - CnmO + Cnm
      AMAT(IA(M)) = AMAT(IA(M)) +CnmO - Cnm
C
C6------RETURN
      RETURN
      END
C -----------------------------------------------------------------------
      SUBROUTINE SGNCn2BCFU1WDS4(IG,N,M,II,IIS)
C     ******************************************************************
C     FILL SECOND ORDER CORRECTION OF K ON UNCONFINED GHOST NODE TERMS
C     AND COMPUTE HORIZONTAL CONDUCTANCE AND ADJUST AMAT FOR LAYCON=4
C     ******************************************************************
      USE GLOBAL, ONLY:JA,IA,NODES,NEQS,AMAT,HNEW,TOP,BOT,
     1   PGF,ISYM,Sn
      USE GNCnMODULE,ONLY:NGNCn,GNCn,I2Kn,MXADJn
      DOUBLE PRECISION HD,BBOT,TTOP,THCK,TOTTHICK,SUPS,ALPHA,ALPHAN
C-----------------------------------------------------------------------
C-------CALCULATE UPSTREAM SATURATION BETWEEN GHOST NODE AND NODE M
      CALL SGNCn2SUPS(IG,N,M,SUPS)
C------------------------------------------------------------------------------
      ISLOC = ISYM(II)
      Cnm = PGF(IIS) * SUPS
      CnmO = AMAT(II)
C
C4------ADJUST CONDUCTIVITY TERM FOR ROW N
      AMAT(II) = AMAT(II) -CnmO + Cnm
      AMAT(IA(N)) = AMAT(IA(N)) +CnmO - Cnm
C5------ADJUST CONDUCTIVITY TERM FOR ROW M
      AMAT(ISLOC) = AMAT(ISLOC) - CnmO + Cnm
      AMAT(IA(M)) = AMAT(IA(M)) +CnmO - Cnm
C
C6------RETURN
      RETURN
      END
C--------------------------------------------------------------------------------
      SUBROUTINE SGNCn2SUPS(IG,N,M,SUPS)
C     ******************************************************************
C     COMPUTE UPSTREAM SATURATION BETWEEN NODE M AND GHOST NODE (WHICH SITS ON N)
C     ******************************************************************
      USE GLOBAL, ONLY:HNEW,TOP,BOT,Sn
      USE GNCnMODULE,ONLY:NGNCn,GNCn,I2Kn,MXADJn
      DOUBLE PRECISION HD,BBOT,TTOP,THCK,TOTTHICK,SUPS,ALPHA,ALPHAN
C1------GET HEAD AT GHOST NODE AND COMPUTE WATER LEVEL
      ALPHAN = 0.0
      HD = 0.0
      DO IADJn = 1,MXADJn
        N3 = GNCn(2+IADJn,IG)
        ALPHA = GNCn(2+MXADJn+IADJn,IG)
        ALPHAN = ALPHAN + ALPHA
        HD = HD + ALPHA*HNEW(N3)
      ENDDO
      HD = HD + (1.0-ALPHAN)*HNEW(N)
C2------CALCULATE SATURATED THICKNESS.
      BBOT=BOT(N)
      TTOP=TOP(N)
      TOTTHICK = TTOP - BBOT
      IF(TOTTHICK.LT.1.E-10)THEN
        TOTTHICK = 1.E-10
      ENDIF
      CALL SAT_THIK(N,HD,TOTTHICK,BBOT,THCK)
C
C3------STORE UPSTREAM SATURATION VALUE IN SUPS.
      SUPS = THCK
      IF(HNEW(M).GT.HD) SUPS = Sn(M)
C
C4----RETURN
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE SGNCn2BCFU1BDCH
C     ******************************************************************
C     ADJUST CBC FLUX FOR GHOST NODE TERMS ON CONSTANT HEAD NODES
C     ******************************************************************
      USE GLOBAL, ONLY:JA,IA,NODES,NEQS,IVC,JAS,ISYM,IBOUND,ICONCV,IOUT,
     1                 HNEW,AMAT,BUFF
      USE GNCnMODULE,ONLY:NGNCn,GNCn,I2Kn,ISYMGNCn,MXADJn
      USE GWFBCFMODULE, ONLY: LAYCON
      DOUBLE PRECISION QNJ1,QMJ1,QNJ2,QMJ2,Cnm,ATERM,ALPHA,CORRECTnm,
     1  SIGALJ,HD
C-----------------------------------------------------------------------
C
C1------DO FOR EACH GNCn NODE
      DO IG=1,NGNCn
C
C2--------GET NODE NUMBERS AND GHOST NODE PROPERTIES AND LOCATIONS
        N1 = GNCn(1,IG)
        N2 = GNCn(2,IG)
C2A------CYCLE IF NEITHER NODE IS A CONSTANT HEAD NODE
        IF(IBOUND(N1).GE.0.AND.IBOUND(N2).GE.0) CYCLE
C3------COMPUTE HEAD VALUE AT GHOST LOCATION AND ADJUSTMENT TERM
        SIGALJ = 0.0
        HD = 0.0
        DO IADJn = 1,MXADJn
          N3 = GNCn(2+IADJn,IG)
          IF(IBOUND(N3).EQ.0) CYCLE
          ALPHA = GNCn(2+MXADJn+IADJn,IG)
          SIGALJ = SIGALJ + ALPHA
          HD = HD + ALPHA*HNEW(N3)
        ENDDO
        ATERM = SIGALJ * HNEW(N1) - HD
C4--------COMPUTE AND ADJUST FLUX FROM BUFF
        Cnm = GNCn(2*MXADJn+3,IG)
        CORRECTnm = ATERM * Cnm
        BUFF(N1) = BUFF(N1) - CORRECTnm
        BUFF(N2) = BUFF(N2) + CORRECTnm
      ENDDO
C
C7------RETURN
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE SGNCn2BCFU1BDADJ
C     ******************************************************************
C     ADJUST CBC FLUX FOR GHOST NODE TERMS
C     ******************************************************************
      USE GLOBAL, ONLY:JA,IA,NODES,NEQS,IVC,JAS,ISYM,IBOUND,ICONCV,IOUT,
     1                 HNEW,AMAT,FLOWJA
      USE GNCnMODULE,ONLY:NGNCn,GNCn,I2Kn,ISYMGNCn,MXADJn
      USE GWFBCFMODULE, ONLY: LAYCON
      DOUBLE PRECISION QNJ1,QMJ1,QNJ2,QMJ2,Cnm,ATERM,ALPHA,CORRECTnm,
     1  SIGALJ,HD
C-----------------------------------------------------------------------
C
C1------DO FOR EACH GNCn NODE
      DO IG=1,NGNCn
C
C2--------GET NODE NUMBERS AND GHOST NODE PROPERTIES AND LOCATIONS
        N1 = GNCn(1,IG)
        N2 = GNCn(2,IG)
        IF(IBOUND(N1).EQ.0.OR.IBOUND(N2).EQ.0) CYCLE
C3------COMPUTE HEAD VALUE AT GHOST LOCATION AND ADJUSTMENT TERM
        SIGALJ = 0.0
        HD = 0.0
        DO IADJn = 1,MXADJn
          N3 = GNCn(2+IADJn,IG)
          IF(IBOUND(N3).EQ.0) CYCLE
          ALPHA = GNCn(2+MXADJn+IADJn,IG)
          SIGALJ = SIGALJ + ALPHA
          HD = HD + ALPHA*HNEW(N3)
        ENDDO
        ATERM = SIGALJ * HNEW(N1) - HD
C
C4---------FIND LOCATION OF N2 IN ROW N1
        DO II = IA(N1)+1,IA(N1+1)-1
          JJ = JA(II)
          IIS = JAS(II)
          IF(JJ.EQ.N2)THEN
C
C6-----------ADJUST FLUXES FROM RHS
            Cnm = AMAT(II)
            CORRECTnm = ATERM * Cnm
            FLOWJA(II) = FLOWJA(II) - CORRECTnm
            FLOWJA(ISYM(II)) = FLOWJA(ISYM(II)) + CORRECTnm
          ENDIF
        ENDDO
      ENDDO
C
C7------RETURN
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE SGNCn2BCFU1BDADJ1
C     ******************************************************************
C     RESET GHOST NODE CONTRIBUTIONS IN MATRIX FOR IMPLICIT GNCn
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,HNEW,TOP,BOT,IBOUND,IOUT,
     1                      AMAT,PGF,FAHL,IA,JA,JAS,ISYM,RHS,HNEW
      USE GNCnMODULE,ONLY:NGNCn,GNCn,IRGNCn,ISYMGNCn,MXADJn
      DOUBLE PRECISION ALPHA,BETA,Cnm,ATERM,ATERM1,ATERM2,RTERM
C     ------------------------------------------------------------------
C---------------------------------------------------------------------------
C1------RETURN IF EXPLICIT GNC
      IF(ISYMGNCn.EQ.1) RETURN
C2------FOR EACH GHOST NODE, RESET COLUMNS N AND J (GHOST NODE CONTRIBUTORS)
C2------IN ROWS N AND M FOR FLOW BETWEEN NODES N AND M.
      DO 20 IG=1,NGNCn
C
C3------GET CELL LOCATIONS AND PROPERTIES.
        N1 = GNCn(1,IG)
        N2 = GNCn(2,IG)
        IF(IBOUND(N1).EQ.0.OR.IBOUND(N2).EQ.0) CYCLE
        DO IADJn = 1,MXADJn
          N3 = GNCn(2+IADJn,IG)
          IF(IBOUND(N3).EQ.0) CYCLE
          ALPHA = GNCn(2+MXADJn+IADJn,IG)
          LOC_JN = IRGNCn(1,IADJn,IG)
          LOC_JM = IRGNCn(2,IADJn,IG)
C
C4-----------FIND LOCATION OF N2 IN ROW N1 AND ADJUST AMAT FOR ROWS N1 AND N2
          DO II = IA(N1)+1,IA(N1+1)-1
            JJ = JA(II)
            IIS = JAS(II)
            IF(JJ.EQ.N2)THEN
              Cnm = GNCn(3+2*MXADJn,IG)
              ATERM =  ALPHA * Cnm
C4A1------------FOR ROW N1
C4A1-----------LESSEN N1 LOCATION BY (ALPHA-1)*Cnm IN DIAGONAL OF ROW N1
              AMAT(IA(N1)) = AMAT(IA(N1)) - ATERM
C4A2-----------PUT -ATERM IN LOCATION J OF ROW N
              AMAT(LOC_JN) = AMAT(LOC_JN) + ATERM
C4B1------------FOR ROW N2
C4B1-----------LESSEN N1 LOCATION BY (ALPHA-1)*Cnm IN OFF-DIAGONAL OF ROW N2
              AMAT(ISYM(II)) = AMAT(ISYM(II)) + ATERM
C4B2-----------PUT ATERM IN LOCATION J OF ROW M
              AMAT(LOC_JM) = AMAT(LOC_JM) - ATERM
              GO TO 200
            ENDIF
          ENDDO
200       CONTINUE
        ENDDO
   20 CONTINUE

C
C5------RETURN
      RETURN
      END
C---------------------------------------------------------------------------
