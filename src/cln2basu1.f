      SUBROUTINE SDIS2CLN1AR(IUCLN)
C     ******************************************************************
C     ALLOCATE SPACE AND READ NODE AND CONNECTIVITY INFORMATION FOR CLN DOMAIN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CLN1MODULE
      USE GLOBAL, ONLY: IUNIT,IOUT,NEQS,NODES,NROW,NCOL,IFREFM,IUNSTR,
     *                  INCLN
      USE GWFBASMODULE, ONLY: IHEDUN,IDDNUN,IBOUUN
      CHARACTER*24 ANAME(3)
      CHARACTER*200 LINE
      DATA ANAME(1) /'   NODES PER CLN SEGMENT'/
      DATA ANAME(2) /'                      IA'/
      DATA ANAME(3) /'                      JA'/  
      DOUBLE PRECISION FRAD
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE.
        INCLN = IUNIT(IUCLN)
        WRITE(IOUT,1)INCLN
    1   FORMAT(1X,/1X,'CLN -- CONNECTED LINE NETWORK DISCRETIZATION ',
     1    'PROCESS, VERSION 1, 3/3/2012 INPUT READ FROM UNIT ',I4)
C
C2------ALLOCATE SCALAR VARIABLES AND INITIALIZE.
      ALLOCATE(NCLN,ICLNCB,ICLNHD,ICLNDD,ICLNIB,NCLNNDS,NCLNGWC,NJA_CLN)
      ALLOCATE(NCONDUITYP) !OTHER CLN TYPES CAN BE DIMENSIONED HERE
      ALLOCATE(ICLNTIB) !TRANSIENT IBOUND OPTION
      ICLNTIB=0
      NCLN = 0
C
      CALL URDCOM(INCLN,IOUT,LINE)
C
C3A-----CHECK FOR OPTIONS KEYWORD AT TOP OF FILE
      IPRCONN=0
      IOPTFOUND=0
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'OPTIONS') THEN
        IOPTFOUND=1
   70   CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'TRANSIENT') THEN
          ICLNTIB=1
          WRITE(IOUT,71)
   71     FORMAT(1X,'TRANSIENT IBOUND OPTION:',
     1     ' READ TRANSIENT IBOUND RECORDS FOR EACH STRESS PERIOD.')
        ELSEIF(LINE(ISTART:ISTOP).EQ.'PRINTIAJA') THEN
          IPRCONN=1
          WRITE(IOUT,72)
   72     FORMAT(1X,'PRINT CLN IA AND JA OPTION:',
     1     ' THE CLN IA AND JA ARRAYS WILL BE PRINTED TO LIST FILE.')
        ELSEIF(LINE(ISTART:ISTOP).EQ.' ') THEN
          CONTINUE
        ELSE
          WRITE(IOUT,79) LINE(ISTART:ISTOP)
   79     FORMAT(1X,'UNKNOWN OPTION DETECTED: ',A)
        ENDIF
        IF(LLOC.LT.200) GO TO 70
      END IF
      IF(IOPTFOUND.GT.0) CALL URDCOM(INCLN,IOUT,LINE)
C
C3B------READ MAXIMUM NUMBER OF CLN NODES AND UNIT OR FLAGS FOR CLN
C3B------DOMAIN OUTPUT OF HEAD, DRAWDOWN AND CELL-BY-CELL FLOW TERMS.
      IF(IFREFM.EQ.0) THEN
        READ(LINE,'(8I10)') NCLN,ICLNNDS,ICLNCB,ICLNHD,ICLNDD,
     1    ICLNIB,NCLNGWC,NCONDUITYP
        LLOC=81
      ELSE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCLN,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNNDS,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNCB,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNHD,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNDD,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNIB,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCLNGWC,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCONDUITYP,R,IOUT,INCLN)
CADD----ADD NUMBER OF OTHER CLN NODE TYPES HERE TO CATALOGUE THEM        
      END IF
C---------------------------------------------------------------------------
C3C-----REFLECT FLAGS IN OUTPUT LISTING FILE
      WRITE(IOUT,3) NCLN,ICLNNDS,NCLNGWC
    3 FORMAT(1X,'FLAG (0) OR MAXIMUM NUMBER OF LINEAR NODES (NCLN) =',I7
     1  /1X,'FLAG (-VE) OR NUMBER OF LINEAR NODES (+VE)',
     1  1X,'(ICLNNDS) =',I7
     1  /1X,'NUMBER OF LINEAR NODE TO MATRIX GRID CONNECTIONS',
     1  ' (NCLNGWC) =',I7/)
C
      IF(ICLNCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL',
     1   ' IS NOT 0 (FLAG ICLNCB IS LESS THAN ZERO)')
      IF(ICLNCB.GT.0) WRITE(IOUT,8) ICLNCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I5,
     1  '(FLAG ICLNCB IS GREATER THAN ZERO)')
      IF(ICLNCB.EQ.0) WRITE(IOUT,6)
    6 FORMAT(1X,'CELL-BY-CELL FLOWS WILL NOT BE SAVED OR PRINTED',
     1  1X,'(FLAG ICLNCB IS EQUAL TO ZERO)')
C
      IF(ICLNHD.LT.0) WRITE(IOUT,9)
    9 FORMAT(1X,'CLN HEAD OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
     1   'NUMBER (IHEDUN) AS USED FOR HEAD OUTPUT FOR POROUS MATRIX'
     2   1X,'(FLAG ICLNHD IS LESS THAN ZERO)')
      IF(ICLNHD.GT.0) WRITE(IOUT,10) ICLNHD
   10 FORMAT(1X,'CLN HEAD OUTPUT WILL BE SAVED ON UNIT ',I4,
     1  '(FLAG ICLNHD IS GREATER THAN ZERO)')
      IF(ICLNHD.EQ.0) WRITE(IOUT,31)
   31 FORMAT(1X,'CLN HEAD OUTPUT WILL NOT BE SAVED OR PRINTED',
     1  1X,'(FLAG ICLNHD IS EQUAL TO ZERO)')
        IF(ICLNHD.LT.0) ICLNHD = IHEDUN
C
      IF(ICLNDD.LT.0) WRITE(IOUT,12)
   12 FORMAT(1X,'CLN DDN OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
     1   'NUMBER (IDDNUN) AS USED FOR DDN OUTPUT FOR POROUS MATRIX'
     2   1X,'(FLAG ICLNDD IS LESS THAN ZERO)')
      IF(ICLNDD.GT.0) WRITE(IOUT,13) ICLNDD
   13 FORMAT(1X,'CLN DDN OUTPUT WILL BE SAVED ON UNIT ',I4,
     1  '(FLAG ICLNDD IS GREATER THAN ZERO)')
      IF(ICLNDD.EQ.0) WRITE(IOUT,14)
   14 FORMAT(1X,'CLN DDN OUTPUT WILL NOT BE SAVED OR PRINTED',
     1  1X,'(FLAG ICLNDD IS EQUAL TO ZERO)')
      IF(ICLNDD.LT.0) ICLNDD = IDDNUN
C
      IF(ICLNIB.LT.0) WRITE(IOUT,32)
   32 FORMAT(1X,'CLN IBOUND OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
     1   'NUMBER (IBOUUN) AS USED FOR DDN OUTPUT FOR POROUS MATRIX'
     2   1X,'(FLAG ICLNIB IS LESS THAN ZERO)')
      IF(ICLNIB.GT.0) WRITE(IOUT,33) ICLNIB
   33 FORMAT(1X,'CLN IBOUND OUTPUT WILL BE SAVED ON UNIT ',I4,
     1  '(FLAG ICLNIB IS GREATER THAN ZERO)')
      IF(ICLNIB.EQ.0) WRITE(IOUT,17)
   17 FORMAT(1X,'CLN IBOUND OUTPUT WILL NOT BE SAVED OR PRINTED',
     1  1X,'(FLAG ICLNIB IS EQUAL TO ZERO)')
      IF(ICLNDD.LT.0) ICLNIB = IBOUUN
C--------------------------------------------------------------------------------
C
C4------DIMENSION AND READ ARRAY THAT CONTAINS NUMBER OF NODES PER CLN SEGMENT
      IF(NCLN.GT.0)THEN      
        ALLOCATE(NNDCLN(0:NCLN))
        K = 0
        CALL U1DINT(NNDCLN(1),ANAME(1),NCLN,K,INCLN,IOUT)
        NNDCLN(0) = 0
C
C5--------MAKE NNDCLN ARRAY CUMULATIVE
        DO I = 1,NCLN
          NNDCLN(I) = NNDCLN(I) + NNDCLN(I-1)
        ENDDO
        NCLNCONS = NNDCLN(NCLN)
C------------------------------------------------------------------------------
C6--------FILL CLNCON WITH CONNECTIVITY OF ADJACENT CLN NODES
        IF(ICLNNDS.LT.0)THEN
C6A-------FILL CLN CONNECTIONS SEQUENTIALLY WITH GLOBAL NODE NUMBERS
          NCLNNDS = NNDCLN(NCLN)
          ALLOCATE(CLNCON(NCLNNDS))
          DO I=1,NCLNNDS
            CLNCON(I) =  I ! LOCAL NUMBER NOW 
          ENDDO
        ELSE
C6B-------SET NUMBER OF CLN NODES AND READ CONNECTION ARRAY FOR EACH CLN SEGMENT
          NCLNNDS = ICLNNDS
          ALLOCATE(CLNCON(NCLNCONS))
          DO I=1,NCLN
            IF(IFREFM.EQ.0) THEN
              READ(INCLN,'(200I10)') 
     1        (CLNCON(J),J=NNDCLN(I-1)+1,NNDCLN(I))
            ELSE
              READ(INCLN,*) (CLNCON(J),J=NNDCLN(I-1)+1,NNDCLN(I))
            ENDIF
          ENDDO
CSPC6C-------CONVERT CLN-NODE NUMBER TO GLOBAL NODE NUMBER
CSP        DO I=1,NCLNCONS
CSP          CLNCON(I) = NODES + CLNCON(I)
CSP        ENDDO
        ENDIF
C6D--------CONVERT TO IA_CLN AND JA_CLN
        ALLOCATE(IA_CLN(NCLNNDS+1))
        CALL FILLIAJA_CLN
C6E---------DEALLOCATE UNWANTED ARRAYS        
        DEALLOCATE (NNDCLN) ! NNDCLN NEEDED FOR WRITING BUDGET TO ASCII FILE? 
        DEALLOCATE (CLNCON)
      ELSE 
C----------------------------------------------------------------------
C7------FOR INPUT OF IA AND JAC OF CLN DOMAIN (NCLN = 0), READ DIRECTLY
        NCLNNDS = ICLNNDS
        ALLOCATE(IA_CLN(NCLNNDS+1))
C7A-------READ NJA_CLN        
        IF(IFREFM.EQ.0) THEN
          READ(INCLN,'(I10)') NJA_CLN
        ELSE
          READ(INCLN,*) NJA_CLN
        ENDIF
C7B-------READ CONNECTIONS PER NODE AND CONNECTIVITY AND FILL IA_CLN AND JA_CLN ARRAYS
        K = 0
        CALL U1DINT(IA_CLN,ANAME(2),NCLNNDS,K,INCLN,IOUT)
        ALLOCATE(JA_CLN(NJA_CLN))
        CALL U1DINT(JA_CLN,ANAME(3),NJA_CLN,K,INCLN,IOUT)
C7C--------ENSURE POSITIVE TERM FOR DIAGONAL OF JA_CLN
        DO IJA = 1,NJA_CLN
          IF(JA_CLN(IJA).LT.0) JA_CLN(IJA) = -JA_CLN(IJA)
        ENDDO
C7D--------MAKE IA_CLN CUMULATIVE FROM CONNECTION-PER-NODE
        DO II=2,NCLNNDS+1
          IA_CLN(II) = IA_CLN(II) + IA_CLN(II-1)
        ENDDO
C---------IA_CLN(N+1) IS CUMULATIVE_IA_CLN(N) + 1
        DO II=NCLNNDS+1,2,-1
          IA_CLN(II) = IA_CLN(II-1) + 1
        ENDDO
        IA_CLN(1) = 1
      ENDIF
C----------------------------------------------------------------------
C8------ALLOCATE SPACE FOR CLN PROPERTY ARRAYS
      ALLOCATE(ACLNNDS(NCLNNDS,6))
      ALLOCATE(IFLINCLN(NCLNNDS))      
      ALLOCATE(ICCWADICLN(NCLNNDS))
      ALLOCATE(ICGWADICLN(NCLNGWC))
C
C9------PREPARE TO REFLECT INPUT PROPERTIES INTO LISTING FILE
      WRITE(IOUT,21)
21    FORMAT(/20X,' CONNECTED LINE NETWORK INFORMATION'/
     1  20X,40('-')/5X,'CLN-NODE NO.',1X,'CLNTYP',1X,'ORIENTATION',2X,
     1  'CLN LENGTH',4X,'BOT ELEVATION',9X,'FANGLE',9X,'IFLIN',11X,
     1  'ICCWADI'/5X,11('-'),2X,6('-'),1X,11('-'),1X,11('-'),4X,13('-'),
     1   4X,11('-'),8X,6('-'),4X,7('-'))
C
C10-------READ BASIC PROPERTIES FOR ALL CLN NODES AND FILL ARRAYS
      DO I = 1,NCLNNDS
        CALL URDCOM(INCLN,IOUT,LINE)
        IF(IFREFM.EQ.0) THEN
          READ(LINE,'(3I10,3F10.3,2I10)') IFNO,IFTYP,IFDIR,FLENG,FELEV,
     1        FANGLE,IFLIN,ICCWADI
          LLOC=71
        ELSE
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFTYP,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFDIR,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FLENG,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FELEV,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FANGLE,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFLIN,R,IOUT,INCLN)
          IF(IFLIN.EQ.0) IFLIN = -1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICCWADI,R,IOUT,INCLN)
        END IF
C11A--------FOR ANGLED PIPE, IF DEPTH OF FLOW IS LESS THAN DIAMETER MAKE HORIZONTAL
        IF(IFDIR.EQ.2)THEN
          FDPTH = FLENG * SIN(FANGLE)
          IC=IFTYP
          CALL CLNR(IC,FRAD)
          IF(FDPTH.LT.2.0*FRAD) IFDIR = 1
        ENDIF
        WRITE(IOUT,22)IFNO,IFTYP,IFDIR,FLENG,FELEV,FANGLE,IFLIN,ICCWADI
22      FORMAT(5X,I10,1X,I6,1X,I10,3(1X,E15.6),1X,I10,1X,I10)
C11B--------FILL PROPERTY ARRAYS WITH READ AND PREPARE INFORMATION
        ACLNNDS(I,1) = IFNO + NODES ! GLOBAL NODE NUMBER FOR CLN-CELL 
        ACLNNDS(I,2) = IFTYP
        ACLNNDS(I,3) = IFDIR
        ACLNNDS(I,4) = FLENG
        ACLNNDS(I,5) = FELEV
        ACLNNDS(I,6) = FANGLE
        IFLINCLN(I) = IFLIN
        ICCWADICLN(I) = ICCWADI
      ENDDO
C----------------------------------------------------------------------------------------
C12------ALLOCATE SPACE FOR CLN TO GW PROPERTY ARRAYS
      ALLOCATE(ACLNGWC(NCLNGWC,6))
C----------------------------------------------------------------------------------------
C13------READ CONNECTING SUBSURFACE NODE AND ASSOCIATED PARAMETERS
      IF(IUNSTR.EQ.0)THEN
C
C14A-----FOR STRUCTURED GRID READ SUBSURFACE NODE IN IJK FORMATS
C14A-----AND OTHER CLN SEGMENT PROPERTY INFORMATION
        CALL SCLN2DIS1SR
      ELSE
C
C14B-----FOR UNSTRUCTURED GRID READ SUBSURFACE NODE NUMBER OF
C14B-----CONNECTION AND OTHER CLN SEGMENT PROPERTY INFORMATION
        CALL SCLN2DIS1UR
      ENDIF
C----------------------------------------------------------------------------------------
C15------ALLOCATE SPACE AND FILL PROPERTIES FOR ALL CONDUIT TYPE CLNs
      IF(NCONDUITYP.GT.0)THEN
        CALL SCLN2COND1RP
      ENDIF
C
C16------IF IPRCONN THEN WRITE CLN CONNECTIVITY TO THE OUTPUT FILE
      IF(IPRCONN.NE.0)THEN
        WRITE(IOUT,'(/,A)')' IA_CLN IS BELOW, 40I10'
        WRITE(IOUT,55)(IA_CLN(I),I=1,NCLNNDS+1)
        WRITE(IOUT,*)'NJA_CLN = ',NJA_CLN
        WRITE(IOUT,*)'JA_CLN IS BELOW, 40I10'
        WRITE(IOUT,55)(JA_CLN(J),J=1,NJA_CLN)
55      FORMAT(40I10)
      ENDIF            
C----------------------------------------------------------------------------------------
C17------ALLOCATE SPACE AND FILL PROPERTIES FOR OTHER CLN TYPES HERE
CADD------ADD OTHER CLN TYPE READ AND PREPARE INFORMATION HERE
C----------------------------------------------------------------------------------------
C18-----RETURN
      RETURN
      END
C---------------------------------------------------------------------------------------      
      SUBROUTINE FILLIAJA_CLN
C     ******************************************************************
C      FILL IA AND JA OF CLN DOMAIN IF CLN INPUT IS FOR LINEAR SEGMENTS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CLN1MODULE
      USE GLOBAL, ONLY: IUNIT,IOUT,NEQS,NODES,NROW,NCOL,IFREFM,IUNSTR,
     *                  INCLN,JA
      USE SPARSEMODULE
      TYPE(SPARSEMATRIX) :: SMAT
      INTEGER,ALLOCATABLE,DIMENSION(:) :: ROWMAXNNZ
C------------------------------------------------------------------------
C1--------FILL NUMBER OF CONNECTIONS IN ROWMAXNNZ TO INITIALIZE SMAT SIZE
      ALLOCATE(ROWMAXNNZ(NCLNNDS))
      DO N=1,NCLNNDS
          ROWMAXNNZ(N)=3 ! START WITH 3
      ENDDO
      CALL SMAT%INIT(NCLNNDS, NCLNNDS, ROWMAXNNZ)
      DEALLOCATE(ROWMAXNNZ)
C--------------------------------------------------------------------
C2--------ADD THE CLN CONNECTIONS TO SMAT (OF TYPE SPARSEMATRIX) 
C2A-------FIRST ADD SELF-CONNECTION
      DO ICLN = 1,NCLNNDS
        CALL SMAT%ADDCONNECTION(ICLN,ICLN,1)  
      ENDDO
C2B------FOR CLN-CLN CONNECTIONS      
      DO IFR = 1,NCLN
        NN = NNDCLN(IFR)
        NB = NNDCLN(IFR-1)+1
        DO N = NB,NN-1
          ND1 = CLNCON(N)
          ND2 = CLNCON(N+1)
          CALL SMAT%ADDCONNECTION(ND1,ND2,0)  ! INODUP=0 AS NO NEED TO CHECK FOR DUPLICATES 
          CALL SMAT%ADDCONNECTION(ND2,ND1,0)  ! INODUP=0 AS NO NEED TO CHECK FOR DUPLICATES
        ENDDO
      ENDDO
C--------------------------------------------------------------------
C3------ALLOCATE JA_CLN AND FILL THE IA_CLN AND JA_CLN ARRAYS
      NJA_CLN=SMAT%NNZ
      ALLOCATE(JA_CLN(NJA_CLN))
      CALL SMAT%FILLIAJA(IA_CLN,JA_CLN,IERR)
C--------------------------------------------------------------------
C4------DESTROY THE SPARSEMATRIX SMAT
      CALL SMAT%DESTROY
C----------------------------------------------------------------------------------------
C14-----RETURN
      RETURN
      END            
C ---------------------------------------------------------------------
      SUBROUTINE SCLN2DIS1SR
C     ******************************************************************
C      READ PROPERTIES FOR CLN DOMAIN FOR A STRUCTURED GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CLN1MODULE
      USE GLOBAL, ONLY: IUNIT,IOUT,NEQS,NODES,NROW,NCOL,IFREFM,IUNSTR,
     *                  INCLN
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      PI = 3.1415926
C1------PREPARE TO REFLECT INPUT INTO LISTING FILE
      WRITE(IOUT,21)
21    FORMAT(/20X,' CLN TO 3-D GRID CONNECTION INFORMATION'/
     1  20X,40('-')/5X,'F-NODE NO.',6X,'LAYER',8X,'ROW',5X,'COLUMN',
     2  2X,'EQTN. TYPE',5X,'      FSKIN',11X,'FLENG',10X,
     4  'FANISO',3X,'ICGWADI'/5X,10('-'),6X,5('-'),8X,3('-'),5X,
     5  6('-'),2X,11('-'),3X,12('-'),2X,14('-'),4X,12('-'),3X,7('-'))
C2-------READ PROPERTIES AND SUBSURFACE CONNECTION INFORMATION FOR ALL CLN NODES
      DO I = 1,NCLNGWC
        CALL URDCOM(INCLN,IOUT,LINE)
        IF(IFREFM.EQ.0) THEN
          READ(LINE,'(5I10,3F10.3,I10)') IFNO,IFLAY,IFROW,IFCOL,IFCON,
     1      FSKIN,FLENG,FANISO,ICGWADI
          LLOC=71
        ELSE
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFLAY,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFROW,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCOL,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCON,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSKIN,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FLENG,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FANISO,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICGWADI,R,IOUT,INCLN)
        END IF
C3--------SET SKIN PARAMETER AND REFLECT INPUT IN LST FILE
        IF(IFCON.EQ.0)FSKIN = 0.0
        WRITE(IOUT,22)IFNO,IFLAY,IFROW,IFCOL,IFCON,FSKIN,FLENG,FANISO,
     1        ICGWADI
22      FORMAT(5X,I10,3(1X,I10),2X,I10,3(1X,E15.6),1X,I9)
C4--------FILL CLN AND GW NODE NUMBERS AND CONNECTION PROPERTY MATRIX
        ACLNGWC(I,1) = IFNO
        IFNOD = (IFLAY-1)*NROW*NCOL + (IFROW-1)*NCOL + IFCOL
        ACLNGWC(I,2) = IFNOD
        ACLNGWC(I,3) = IFCON
        ACLNGWC(I,4) = FSKIN
        ACLNGWC(I,5) = FANISO
        ACLNGWC(I,6) = FLENG
        ICGWADICLN(I) = ICGWADI
      ENDDO
C5-----RETURN
      RETURN
      END
C ---------------------------------------------------------------------
      SUBROUTINE SCLN2DIS1UR
C     ******************************************************************
C      READ PROPERTIES FOR CLN DOMAIN FOR A UNSTRUCTURED GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CLN1MODULE
      USE GLOBAL, ONLY: IUNIT,IOUT,NEQS,NODES,NROW,NCOL,IFREFM,IUNSTR,
     *                  INCLN
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      PI = 3.1415926
C1------PREPARE TO REFLECT INPUT INTO LISTING FILE
      WRITE(IOUT,23)
23    FORMAT(/20X,' CLN TO 3-D GRID CONNECTION INFORMATION'/
     1    20X,40('-')/5X,'F-NODE NO.',1X,'GW-NODE NO',2X,
     2    'EQTN. TYPE',2X,'      FSKIN',11X,
     4    'FLENG',9X,'FANISO'3X,'ICGWADI'/5X,10('-'),1X,10('-'),
     5    1X,11('-'),5X,11('-'),1X,17('-'),1X,15('-'),3X,10('-'))
C2-------READ PROPERTIES AND SUBSURFACE CONNECTION INFORMATION FOR ALL CLN NODES
      DO I = 1,NCLNGWC
        CALL URDCOM(INCLN,IOUT,LINE)
        IF(IFREFM.EQ.0) THEN
          READ(LINE,'(3I10,3F10.3,I10)') IFNO,IFNOD,IFCON,FSKIN,FLENG,
     1          FANISO,ICGWADI
          LLOC=51
        ELSE
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNOD,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCON,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSKIN,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FLENG,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FANISO,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICGWADI,R,IOUT,INCLN)
        END IF
C3--------SET SKIN PARAMETER AND REFLECT INPUT IN LST FILE
        IF(IFCON.EQ.0)FSKIN = 0.0
        WRITE(IOUT,24)IFNO,IFNOD,IFCON,FSKIN,FLENG,FANISO,ICGWADI
24      FORMAT(5X,I10,1X,I10,2X,I10,3(1X,E15.6),1X,I9)
C4--------FILL CLN AND GW NODE NUMBERS AND CONNECTION PROPERTY MATRIX
        ACLNGWC(I,1) = IFNO
        ACLNGWC(I,2) = IFNOD
        ACLNGWC(I,3) = IFCON
        ACLNGWC(I,4) = FSKIN
        ACLNGWC(I,5) = FANISO
        ACLNGWC(I,6) = FLENG
        ICGWADICLN(I) = ICGWADI
      ENDDO
C5-----RETURN
      RETURN
      END
C
C ---------------------------------------------------------------------
      SUBROUTINE CLN2BAS1AR
C     ******************************************************************
C     READ IBOUND AND STARTING HEADS AND PREPARE KADI, Sn AND PGF ARRAYS FOR CLN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CLN1MODULE, ONLY: NCLNNDS,NCLNGWC,ACLNNDS,IFLINCLN,
     1    ICCWADICLN,ICGWADICLN
      USE GLOBAL, ONLY: IOUT,IBOUND,NODES,STRT,HNEW,Sn,So,INCLN,IWADI,
     * IWADICLN
      USE GWFBASMODULE, ONLY: HNOFLO
      REAL, DIMENSION(:),ALLOCATABLE  ::HTMP1
      CHARACTER*24 ANAME(2)
      DATA ANAME(1) /' CONDUIT BOUNDARY ARRAY'/
      DATA ANAME(2) /'   CONDUIT INITIAL HEAD'/
      DOUBLE PRECISION HD,THCK,BBOT
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE.
      WRITE(IOUT,1)INCLN
    1 FORMAT(1X,/1X,'CLN -- CONDUIT DOMAIN FLOW PACKAGE, VERSION 1,',
     1  ' 5/17/2010 INPUT READ FROM UNIT ',I4)
C
C2-------READ IBOUND FOR CLN NODES
      CALL U1DINT(IBOUND(NODES+1),ANAME(1),NCLNNDS,0,INCLN,IOUT)
C3-------READ INITIAL HEADS FOR CLN NODES
      ALLOCATE(HTMP1(NCLNNDS))
      CALL U1DREL(HTMP1,ANAME(2),NCLNNDS,0,INCLN,IOUT)
      DO N=1,NCLNNDS
        HNEW(NODES+N) = HTMP1(N)
        STRT(NODES+N) = HTMP1(N)
        IF(IBOUND(NODES+N).EQ.0) HNEW(NODES+N)=HNOFLO
      ENDDO
      DEALLOCATE(HTMP1)
C
C4-----SET VOLUMETRIC FRACTIONS FOR CLN-NODES IN SATURATION ARRAY
      DO  IFN=1,NCLNNDS
        N = ACLNNDS(IFN,1)
        IFLIN = IFLINCLN(IFN)
        IF(IBOUND(N).NE.0.AND.IFLIN.EQ.0) THEN
C---------CALCULATE INITIAL SATURATED THICKNESS.
          HD=HNEW(N)
          BBOT = ACLNNDS(IFN,5)
          CALL CLN_THIK(IFN,HD,BBOT,THCK)
          Sn(N)=THCK
          So(N) = Sn(N)
        ENDIF
      ENDDO
C--------------------------------------------------------------------------------
C5-------FILL PGF ARRAY FOR CLN FLOW AND ITS CONNECTION WITH POROUS MATRIX
      CALL SFILLPGF_CLN
C----------------------------------------------------------------------------------------
C12A------ESTABLISH WADI CONDITION FOR CLN  
        IWADICLN = 0
        DO I = 1,NCLNNDS
           IF(ICCWADICLN(I).NE.0) IWADICLN = 1 
        ENDDO    
        DO I = 1,NCLNGWC
          IF(ICGWADICLN(I).NE.0) IWADICLN = 1 
        ENDDO
        IF(IWADICLN.EQ.1) IWADI = 1
C
C6------RETURN
      RETURN
      END
C
      SUBROUTINE CLN1RP(IN)
C     ******************************************************************
C     UPDATE IBOUND IF TRANSIENT IBOUND OPTION SPECIFIED
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CLN1MODULE, ONLY: ICLNTIB,NCLNNDS
      USE GLOBAL,      ONLY:IOUT,IFREFM,IUNSTR,IBOUND,HNEW,IA,JA,NODES
      USE GWFBASMODULE, ONLY: HNOFLO
      IMPLICIT NONE
      INTEGER, DIMENSION(:),ALLOCATABLE  ::ITEMP
      CHARACTER*24 ANAME
      CHARACTER(LEN=200) line
      INTEGER,INTENT(IN) :: IN
      INTEGER :: LLOC,NIB0,NIB1,NIBM1,ISTART,ISTOP,I,ICELL,IB,IAVHEAD,
     1           IHEAD,IBOUNDKP,ISUM,JJ,NODCLN
      REAL :: R,HEAD
      DATA ANAME /'     ZEROED IBOUND CELLS'/
C     ------------------------------------------------------------------
C
C1-----RETURN IF ITIB IS 0 OR FIRST STRESS PERIOD
      IF(ICLNTIB.EQ.0) RETURN
C
C3------READ FLAGS      
      READ(IN,'(A)',END=100) LINE
      IF(IFREFM.EQ.0)THEN
        READ(LINE,'(3I10)') NIB0,NIB1,NIBM1
      ELSE
        LLOC = 1
        CALL URWORD(line, lloc, istart, istop, 2, NIB0, r, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 2, NIB1, r, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 2, NIBM1, r, Iout, In)
      END IF
C------------------------------------------------------------------------
C4------CHECK IF IBOUND IS TO BE ZEROED OUT.
      IF(NIB0.LE.0) THEN
C
C4A-----NIB0=<0, SO NO CELLS INACTIVATED.
        WRITE(IOUT,3)
    3  FORMAT(1X,/1X,'NO CLN CELLS INACTIVATED FROM LAST STRESS PERIOD')
      ELSE
C
C4B-----NIB0>0, SO READ LIST OF INACTIVATED CELLS AND SET IBOUND TO ZERO
        ALLOCATE (ITEMP(NIB0))
        CALL U1DINT(ITEMP,ANAME,NIB0,0,IN,IOUT)
        DO I=1,NIB0
          ICELL = ITEMP(I)
          IF(ICELL.LT.1 .OR. ICELL.GT.NCLNNDS) THEN
            WRITE(IOUT,*) 'ERROR IN TRANSIENT CLN NODE SPECIFICATION.'
            WRITE(IOUT,*) 'CLN NODE NUMBER NOT BETWEEN 1 AND NCLNNDS.'
            WRITE(IOUT,*) 'CLN NODE NUMBER: ', ICELL
            WRITE(IOUT,*) 'NCLNNDS: ', NCLNNDS
            CALL USTOP('')
          ENDIF
          ICELL = ICELL + NODES
          IBOUND(ICELL) = 0 
          HNEW(ICELL) = HNOFLO  
        ENDDO
        DEALLOCATE(ITEMP)
      ENDIF
C------------------------------------------------------------------------
C5------CHECK IF IBOUND IS TO BE ACTIVATED.
      IF(NIB1.LE.0) THEN
C
C5A-----NIB1=<0, SO NO CELLS ACTIVATED.
        WRITE(IOUT,4)
4       FORMAT(1X,/1X,'NO CLN CELLS ACTIVATED FROM LAST STRESS PERIOD')
      ELSE
C
C5B-----NIB1>0, SO READ LIST OF ACTIVATED CELLS AND SET IBOUND TO 1
        DO IB=1,NIB1
C5C-------READ CELL NUMBER  
          CALL URDCOM(In, Iout, line)
          LLOC = 1
          IF(IFREFM.EQ.0)THEN
            READ(LINE,'(I10)') ICELL
            LLOC=11
          ELSE
            CALL URWORD(line, lloc, istart, istop, 2, ICELL, r,Iout, In)
          END IF              
C
C5CA----CHECK FOR VALID CELL NUMBER THEN CONVERT TO GLOBAL NUMBER
          IF(ICELL.LT.1 .OR. ICELL.GT.NCLNNDS) THEN
            WRITE(IOUT,*) 'ERROR IN TRANSIENT CLN NODE SPECIFICATION.'
            WRITE(IOUT,*) 'CLN NODE NUMBER NOT BETWEEN 1 AND NCLNNDS.'
            WRITE(IOUT,*) 'CLN NODE NUMBER: ', ICELL
            WRITE(IOUT,*) 'NCLNNDS: ', NCLNNDS
            CALL USTOP('')
          ENDIF
          NODCLN = ICELL
          ICELL = ICELL + NODES
C
C5D--------GET OPTIONS 
          IAVHEAD=0
          IHEAD = 0
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
          IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
C5D1---------READ KEYWORD OPTION FOR HEAD TO BE READ.   
            IHEAD = 1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HEAD,IOUT,IN)
          ELSEIF(LINE(ISTART:ISTOP).EQ.'AVHEAD') THEN
C5D2----------READ KEYWORD OPTION FOR AVERAGE HEAD TO BE READ.          
             IAVHEAD=1
          ENDIF
C5E-----------SET IBOUND AND HEADS
          IBOUNDKP = IBOUND(ICELL)
          IBOUND(ICELL) = 1
          IF(IHEAD.EQ.1)THEN
C5E1--------HEAD IS SET TO GIVEN VALUE              
            HNEW(ICELL) = HEAD  
          ELSEIF(IAVHEAD.EQ.1)THEN
C5E2--------HEAD IS SET TO AVERAGE OF CONNECTING ACTIVE CELLS              
            HEAD = 0.0
            ISUM = 0
            DO I=IA(ICELL)+1,IA(ICELL+1)-1
              JJ = JA(I)
              IF(IBOUND(JJ).NE.0) THEN
                HEAD = HEAD + HNEW(JJ)  
                ISUM = ISUM + 1
              ENDIF
            ENDDO
            IF(ISUM.GT.0) THEN
              HEAD = HEAD / ISUM
            ELSE
              WRITE(IOUT,*) 'ERROR ACTIVATING CLN CELL: ', NODCLN
              WRITE(IOUT,*) 'CANNOT CALCULATE AN AVERAGE STARTING HEAD.'
              WRITE(IOUT,*) 'BECAUSE NO CONNECTED CELLS ARE ACTIVE.'
              WRITE(IOUT,*) 'STOPPING...'
              CALL USTOP('')              
            ENDIF
            HNEW(ICELL) = HEAD
          ELSE
C5E3--------CHECK TO SEE IF NODE WAS PREVIOUSLY INACTIVE
            IF(IBOUNDKP.EQ.0)THEN
              WRITE(IOUT,11)ICELL
11            FORMAT(1X,'*** NEED TO SET HEAD IF INACTIVE CELL IS MADE'
     1        1X,'ACTIVE FOR CELL ',I9,', STOPPING ***')
              STOP
            ENDIF  
          ENDIF
        ENDDO
      ENDIF      
C------------------------------------------------------------------------
C6------CHECK IF IBOUND IS TO BE MADE MINUS ONE (PRESCRIBED HEAD).
      IF(NIBM1.LE.0) THEN
C
C5A-----NIBM1=<0, SO NO CELLS MADE PRESCRIBED HEAD.
        WRITE(IOUT,5)
5     FORMAT(/1X,'NO CLN CELLS PRESCRIBED HEAD FROM LAST STRESS PERIOD')
      ELSE
C
C5B-----NIBM1>0, SO READ LIST OF PRESCRIBED HEAD CELLS AND SET IBOUND TO -1
        DO IB=1,NIBM1
C5C-------READ CELL NUMBER  
          CALL URDCOM(In, Iout, line)
          LLOC = 1
          IF(IFREFM.EQ.0)THEN
            READ(LINE,'(I10)') ICELL
            LLOC=11
          ELSE
            CALL URWORD(line, lloc, istart, istop, 2, ICELL, r,Iout, In)
          END IF              
C
C5CA----CHECK FOR VALID CELL NUMBER THEN CONVERT TO GLOBAL NUMBER
          IF(ICELL.LT.1 .OR. ICELL.GT.NCLNNDS) THEN
            WRITE(IOUT,*) 'ERROR IN TRANSIENT CLN NODE SPECIFICATION.'
            WRITE(IOUT,*) 'CLN NODE NUMBER NOT BETWEEN 1 AND NCLNNDS.'
            WRITE(IOUT,*) 'CLN NODE NUMBER: ', ICELL
            WRITE(IOUT,*) 'NCLNNDS: ', NCLNNDS
            CALL USTOP('')
          ENDIF
          NODCLN = ICELL
          ICELL = ICELL + NODES
C
C5D--------GET OPTIONS 
          IAVHEAD=0
          IHEAD = 0
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
          IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
C5D1---------READ KEYWORD OPTION FOR HEAD TO BE READ.   
            IHEAD = 1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HEAD,IOUT,IN)
          ELSEIF(LINE(ISTART:ISTOP).EQ.'AVHEAD') THEN
C5D2----------READ KEYWORD OPTION FOR AVERAGE HEAD TO BE READ.          
             IAVHEAD=1
          ENDIF
C5E-----------SET IBOUND AND HEADS
          IBOUNDKP = IBOUND(ICELL)
          IBOUND(ICELL) = -1
          IF(IHEAD.EQ.1)THEN
C5E1--------HEAD IS SET TO GIVEN VALUE              
            HNEW(ICELL) = HEAD  
          ELSEIF(IAVHEAD.EQ.1)THEN
C5E2--------HEAD IS SET TO AVERAGE OF CONNECTING ACTIVE CELLS              
            HEAD = 0.0
            ISUM = 0
            DO I=IA(ICELL)+1,IA(ICELL+1)-1
              JJ=JA(I)
              IF(IBOUND(JJ).NE.0) THEN
                HEAD = HEAD + HNEW(JJ)  
                ISUM = ISUM + 1
              ENDIF
            ENDDO
            IF(ISUM.GT.0) THEN
              HEAD = HEAD / ISUM
            ELSE
              WRITE(IOUT,*) 'ERROR CONVERTING CLN TO CONSTANT: ', NODCLN
              WRITE(IOUT,*) 'CANNOT CALCULATE AN AVERAGE STARTING HEAD.'
              WRITE(IOUT,*) 'BECAUSE NO CONNECTED CELLS ARE ACTIVE.'
              WRITE(IOUT,*) 'STOPPING...'
              CALL USTOP('')              
            ENDIF
            HNEW(ICELL) = HEAD
          ELSE
C5E3--------CHECK TO SEE IF NODE WAS PREVIOUSLY INACTIVE
            IF(IBOUNDKP.EQ.0)THEN
              WRITE(IOUT,12)ICELL
12            FORMAT(1X,'*** NEED TO SET HEAD IF INACTIVE CELL IS MADE'
     1        1X,'PRESCRIBED HEAD FOR CELL ',I9,', STOPPING ***')
              STOP
            ENDIF  
          ENDIF
        ENDDO
      ENDIF
      GOTO 200            
C
C6-----ERROR READING RECORD
  100 WRITE(IOUT,*) 'ERROR READING TRANSIENT IBOUND RECORD FOR CLN.'
      WRITE(IOUT,*) 'STOPPING...'
      CALL USTOP('')
  200 CONTINUE
C
C6------RETURN
      RETURN
      END
C -----------------------------------------------------------------------
      SUBROUTINE SFILLPGF_CLN
C     ******************************************************************
C     COMPUTE AND FILL CONSTANT TERMS INTO PGF ARRAY FOR CONDUIT DOMAIN FLOW
C     ALSO FILL AREA IN FAHL, FILL CL1, AND CL2 FOR CLN NODES
C     AND SET IVC = 3 OR 4 FOR CONDUIT-CONDUIT OR CONDUIT-MATRIX CONNECTIONS
C     ******************************************************************
      USE GLOBAL, ONLY:NODES,NJA,IA,PGF,FAHL,TOP,BOT,CL1,CL2,NODES,NLAY,
     1            NODLAY,IBOUND,JA,JAS,IVC,ISYM,AREA,IDEALLOC_HY
      USE CLN1MODULE, ONLY: ACLNNDS,NCLNNDS,NNDCLN,ACLNGWC,NCLNGWC,
     1                  ACLNCOND,NCLN,CLNCON,IA_CLN,JA_CLN
      USE GWFBCFMODULE,ONLY:HK,CV,LAYCON,LAYAVG,IKVFLAG
      DOUBLE PRECISION FK,AREAF,EL,RADFSQ,CWCn,RO,ROD,CWND,DEX,DEXN,DEZ,
     1  AREAF1,AREAF2,FK1,FK2,FPER,FRAD,FANISO,EL1,EL2,AKVAL
C
C--------------------------------------------------------------------------------------
C1-----CONNECT CLN NODES TO EACH OTHER
C--------------------------------------------------------------------------------------
      PI = 3.1415926
C1A---------loop over all CLN nodes
      DO NC1 = 1,NCLNNDS
C2----------loop over all connections of node NC1
        DO II_CLN = IA_CLN(NC1)+1,IA_CLN(NC1+1)-1
          NC2 = JA_CLN(II_CLN)
          IF(NC2.GT.NC1) CYCLE
          ND1 = ACLNNDS(NC1,1)   !  NC1 + NODES
          ND2 = ACLNNDS(NC2,1)   !  NC2 + NODES
          IC1 = ACLNNDS(NC1,2)     !CLN TYPE FOR NODE 1
          IC2 = ACLNNDS(NC2,2)     !CLN TYPE FOR NODE 2
C1C---------FIND LOWER ROW NUMBER TO FILL UPPER DIAGONAL OF PGF
          IF(ND2.GT.ND1)THEN
            NL = ND1
            NH = ND2
          ELSE
            NL = ND2
            NH = ND1
          ENDIF
C--------------------------------------------------------------------------------------
C2---------COMPUTE AND FILL CONDUCTANCE TERM FOR CLN-CLN CONNECTION IN PGF
          DO II = IA(NL)+1,IA(NL+1)-1
            JJ = JA(II)
            IF(JJ.NE.NH) CYCLE
            IIS = JAS(II)
C
            CALL CLNA(IC1,AREAF1)
            CALL CLNK(IC1,FK1)
            CALL CLNA(IC2,AREAF2)
            CALL CLNK(IC2,FK2)
            AREAF = MIN(AREAF1,AREAF2)             ! MIN OF AREAS
            EL1 = ACLNNDS(NC1,4)
            EL2 = ACLNNDS(NC2,4)
            EL = 0.5*(EL1+EL2)
            FK = (EL1+EL2)*(FK1 * FK2) / (FK1*EL1 + FK2*EL2)     ! LENGTH-WEIGHTED HARMONIC MEAN OF CONDUCTIVITY
            CWCn = AREAF * FK / EL
            IF(CWCn.GT.1.0E7) CWCn = 1.0E7
            PGF(IIS) = CWCn
C--------------------------------------------------------------------------------------
C3-----------ALSO FILL AREA, IVC, CL1, CL2 - NEEDED FOR TRANSPORT
            FAHL(IIS) = AREAF
            IVC(IIS) = 3
            CL1(IIS) = ACLNNDS(NC1,4)  !DIVIDE BY 2?
            CL2(IIS) = ACLNNDS(NC2,4)  !DIVIDE BY 2?
          ENDDO
        ENDDO
      ENDDO
C
C--------------------------------------------------------------------------------------
C4-----CONNECT CLN NODES WITH POROUS MATRIX
C-------------------------------------------------------------------------------------
C4A-----loop over all conduit node to GW connections
      DO IFN = 1,NCLNGWC
        IH = ACLNGWC(IFN,1)
        NH = ACLNNDS(IH,1)
        NL = ACLNGWC(IFN,2)
        DO II = IA(NL)+1,IA(NL+1)-1
          JJ = JA(II)
          IF(JJ.NE.NH) CYCLE
          IIS = JAS(II)
C--------------------------------------------------------------------------------------
C4B---------FIRST COMPUTE EFFECTIVE CELL RADIUS FOR THIS CONNECTION
          FANISO = ACLNGWC(IFN,5)
          IFNC = ACLNGWC(IFN,1)
          IFDIR = ACLNNDS(IFNC,3)
          IF(IFDIR.EQ.0)THEN
C4B1---------GET RO FOR VERTICAL WELL USING ISOTROPIC THIEM EQUATION,
C4B1--------USE ANISOTROPY FOR CONDUCTANCE (PGF)
            RO = 0.0
            DEXN = 0.0
            DO IJ = IA(NL)+1,IA(NL+1)-1
              JJ1 = JA(IJ)
              IF(JJ1.GT.NODES)CYCLE
              IJS = JAS(IJ)
              IF(IVC(IJS).EQ.1) CYCLE
              RO = RO + CL1(IJS)**2
              DEXN = DEXN + 1.0
            ENDDO
            RO = 0.28 * SQRT(2.0*RO/DEXN)
          ELSE
C4B2--------GET RO FOR HORIZONTAL WELL USING ANISOTROPIC EQUATION
            DEX = 0.0
            DEXN = 0.0
            DO IJ = IA(NL)+1,IA(NL+1)-1
              JJ = JA(IJ)
              IF(JJ.GT.NODES)CYCLE
              IJS = JAS(IJ)
              IF(IVC(IJS).EQ.1) CYCLE
              DEX = DEX + CL1(IJS)
              DEXN = DEXN + 1.0
            ENDDO
            DEX = 2.0 * DEX / DEXN
            DEZ = TOP(NL) - BOT(NL)
            RO = DEX**2 * SQRT(1.0/FANISO) + DEZ**2 * SQRT(FANISO)
            ROD = (1.0/FANISO)**0.25 + FANISO**0.25
            RO = 0.28 * SQRT(RO) / ROD
          ENDIF
C--------------------------------------------------------------------------------------
C5---------COMPUTE CONDUCTANCE TERM FOR THE DIFFERENT CLN-MATRIX CONNECTION TYPES
          IFCON = ACLNGWC(IFN,3)
          FSKIN = ACLNGWC(IFN,4)
          IFTYP = ACLNNDS(IFNC,2)
          CALL CLNP(IFTYP,FPER)
          IF(IFCON.EQ.3)THEN
C
C5A-----------CONNECTION IS ACROSS A LEAKANCE TERM LIKE CONDUIT FLOW PROCESS OF MF2K5, COMPUTE LEAKANCE
            FLENG = ACLNGWC(IFN,6)
            CWCn =  FSKIN * FPER * FLENG/ FANISO
          ELSEIF(IFCON.EQ.2)THEN
C
C5B-----------CONNECTION IS ACROSS A LEAKANCE TERM LIKE CONDUIT FLOW PROCESS PACKAGE, LEAKANCE IS INPUT
            CWCn = FSKIN
          ELSEIF(IFCON.EQ.0.OR.IFCON.EQ.1)THEN
C
C5C-----------CONNECTION USES THIEM EQUATION LIKE MULTI-NODE WELL PACKAGE
            FLENG = ACLNGWC(IFN,6)
            CALL CLNR(IFTYP,FRAD)
            CWND = LOG(RO / FRAD) + FSKIN
C5C1------------COMPUTE THE CONDUCTANCE TERM            
            CWCn = 2.0*PI*HK(NL) * SQRT(1.0/FANISO)*FLENG / CWND
          ENDIF
          PGF(IIS) = CWCn
C--------------------------------------------------------------------------------------
C6-----------ALSO FILL AREA, IVC, CL1, CL2 - NEEDED FOR TRANSPORT
          FAHL(IIS) = FPER * FLENG
          IVC(IIS) = 4
          CL1(IIS) = FRAD
          CL2(IIS) = RO
        ENDDO
      ENDDO
C-----------------------------------------------------------------------------------
C7------------FILL X-SECTIONAL AREA NEEDED FOR TRANSPORT AND STORAGE TERM
      DO IFNC=1,NCLNNDS
        IFTYP = ACLNNDS(IFNC,2)
        N = ACLNNDS(IFNC,1)
        CALL CLNA(IFTYP,AREAF)
        AREA(N) = AREAF
      ENDDO
C
      IF(IDEALLOC_HY.EQ.2)  DEALLOCATE(HK)
C7------RETURN
      RETURN
      END
C--------------------------------------------------------------------------------------
      SUBROUTINE ADDIAJA_CLN (SMAT)
C     ******************************************************************
C     ADD IA AND JA OF CLN NODES TO THE SUBSURFACE IA AND JA ARRAYS
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
C
      USE GLOBAL,     ONLY:NODES,NEQS,NJA,IA,JA
      USE CLN1MODULE, ONLY:NCLNNDS,IA_CLN,JA_CLN,ACLNGWC,ACLNNDS,NCLNGWC
      USE SPARSEMODULE
      TYPE(SPARSEMATRIX), INTENT(INOUT) :: SMAT
C
C     ------------------------------------------------------------------
C1--------ADD EXISTING IA_CLN AND JA_CLN PATTERN TO SMAT USING GLOBAL NODE NUMBERS
      DO NCL=1,NCLNNDS
        N = NCL + NODES
        DO JJ=IA_CLN(NCL),IA_CLN(NCL+1)-1
           MCL=JA_CLN(JJ)
           M = MCL + NODES
           CALL SMAT%ADDCONNECTION(N,M,1)
        ENDDO
      ENDDO
C2--------ADD GROUNDWATER CONNECTION TO CLN NODES TO THE SPARSEMODULE DATA STRUCTURE
      DO IFN = 1,NCLNGWC
        I1 = ACLNGWC(IFN,1)
        ND1 = ACLNNDS(I1,1)
        ND2 = ACLNGWC(IFN,2)
        CALL SMAT%ADDCONNECTION(ND1,ND2,0)  !INODUP=0 AS NO DUPLICATES ARE EXPECTED
        CALL SMAT%ADDCONNECTION(ND2,ND1,0)  !INODUP=0 AS NO DUPLICATES ARE EXPECTED
      ENDDO
C10------RETURN
      RETURN
      END
C------------------------------------------------------------------------
      SUBROUTINE CLN1FM(KPER)
C     ******************************************************************
C     COMPUTE CONDUCTANCE TERM IN AMAT AND ADD STORAGE TO AMAT AND RHS
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:ISSFLG,IWADICLN
C     ------------------------------------------------------------------
C
C1-----CALCULATE CONDUIT-CONDUIT AND CONDUIT-MATRIX CONDUCTANCES IN AMAT
      CALL SCLN1H4
C
C2------SET STEADY-STATE FLAG
      ISS=ISSFLG(KPER)
C
C3------IF THE SIMULATION IS TRANSIENT ADD STORAGE TO DIAGONAL AND RHS
      IF(ISS.NE.0) GO TO 201
      CALL SCLN1S4
 201  CONTINUE      
C4------PROVIDE VERTICAL FLOW CORRECTION
      IF(IWADICLN.NE.0) CALL SCLN1WADI
C5------RETURN
      RETURN
      END
C------------------------------------------------------------------------
      SUBROUTINE SCLN1WADI
C     ******************************************************************
C     COMPUTE VERTICAL FLOW CORRECTION FOR CLN-CLN AND CLN-GW FLOW
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NODES,NLAY,IBOUND,HNEW,BUFF,BOT,TOP,ISYM,IOUT,
     1    NODLAY,AMAT,RHS,IA,JA,JAS,PGF
      USE CLN1MODULE, ONLY:  NCLN,NNDCLN,NCLNNDS,ACLNNDS,CLNCON,
     1   ACLNGWC,NCLNGWC,IFLINCLN,HWADICC,HWADICG,ICCWADICLN,ICGWADICLN,
     2   IA_CLN,JA_CLN
      USE GWFBCFMODULE,ONLY:IWDFLG,WETFCT,IHDWET,IWETIT,LAYCON,HDRY,
     1                      HK,WETDRY,LAYAVG
      USE SMSMODULE, ONLY: EPSILON
C
      DOUBLE PRECISION HD,BBOT,TTOP,THCK,ZERO,PERIF,PERIW,SILLBOT,
     1           TOTTHICK,X,Y
C     ------------------------------------------------------------------
C1------FILL HWADICC TERM FOR EACH CLN DOMAIN NODE WITH FLOW CORRECTION
      ALLOCATE( HWADICC(NCLNNDS))
      HWADICC = 0.0
      DO I=1,NCLNNDS
        N = I + NODES
        IF(ICCWADICLN(I).NE.0)THEN
          X = HNEW(N) - ACLNNDS(I,5)
          CALL WADIFN(X,Y)
          HWADICC(I) = Y + ACLNNDS(I,5)
        ELSE
          HWADICC(I) = HNEW(N)
        ENDIF    
      ENDDO    
C----------------------------------------------------------------------------
C2------LOOP OVER ALL CLN-CLN CONNECTIONS FOR LEAKAGE CORRECTION
      DO NC1 = 1,NCLNNDS
C2A----------loop over all connections of node NC1 
        DO II_CLN = IA_CLN(NC1)+1,IA_CLN(NC1+1)-1
          NC2 = JA_CLN(II_CLN)
          IF(NC2.GT.NC1) CYCLE
          ND1 = ACLNNDS(NC1,1)   !  NC1 + NODES
          ND2 = ACLNNDS(NC2,1)   !  NC2 + NODES
          IF(IBOUND(ND1).EQ.0.OR.IBOUND(ND2).EQ.0) CYCLE
C2B---------FIND LOWER ROW NUMBER FOR UPPER DIAGONAL OF PGF
          IF(ND2.GT.ND1)THEN
            NL = ND1
            NH = ND2
          ELSE
            NL = ND2
            NH = ND1
          ENDIF
C2C---------COMPUTE AND FILL CORRECTION TERM FOR CLN-CLN CONNECTION
          DO II = IA(NL)+1,IA(NL+1)-1
            JJ = JA(II)
            IF(JJ.NE.NH) CYCLE
            IIS = JAS(II)
C2D---------FIND UPSTREAM AND DOWNSTREAM NODES
            IUP = NL
            IF(HNEW(NH).GT.HNEW(NL)) IUP = NH
            IDN = NL
            IF(IUP.EQ.NL) IDN = NH
C2E---------SKIP CORRECTION IF DOWNSTREAM NODE DOES NOT NEED CORRECTION
            IDNL = IDN - NODES
            IF(ICCWADICLN(IDNL).EQ.0) CYCLE
C2F-----------FIND MATRIX LOCATION OF DOWNSTREAM NODE          
            ILOC = II  !MATRIX LOCATION FOR NL BEING DOWNSTREAM NODE
            IF(NH.EQ.IDN) ILOC = ISYM(II)            
C2G---------FILL CORRECTION FOR CONNECTION 
            RHS(IDN) = RHS(IDN) + AMAT(ILOC)*(HWADICC(IDNL) - HNEW(IDN))
            RHS(IUP) = RHS(IUP) - AMAT(ILOC)*(HWADICC(IDNL) - HNEW(IDN))
          ENDDO
        ENDDO
      ENDDO
C-----------------------------------------------------------------------------
C3------FILL HWADICG TERM FOR CLN-GWF CONNECTIONS WITH D/S FLOW CORRECTION
C-----------------------------------------------------------------------------
      ALLOCATE( HWADICG(NCLNGWC))
      HWADICG = 0.0
C3A------LOOP OVER ALL CLN-GWF CONNECTIONS FOR LEAKAGE CORRECTION      
      DO IFN=1,NCLNGWC
        IH = ACLNGWC(IFN,1)          
        NH = ACLNNDS(IH,1)
        NL = ACLNGWC(IFN,2)
        IF(IBOUND(NH).EQ.0.OR.IBOUND(NL).EQ.0) CYCLE          
C3B---------FIND UPSTREAM AND DOWNSTREAM NODES
        IUP = NL
        IF(HNEW(NH).GT.HNEW(NL)) IUP = NH
        IDN = NL
        IF(IUP.EQ.NL) IDN = NH
C3C------FILL THE CORRECTED HEAD OF THE DOWNSTREAM CELL OF CLN-GW CONNECTION IN HWADICG
        IF(ICGWADICLN(IFN).NE.0)THEN
          SILLBOT = ACLNNDS(IH,5)
          IF(BOT(NL).GT.SILLBOT) SILLBOT = BOT(NL)
          X = HNEW(IDN) - SILLBOT
          CALL WADIFN(X,Y)
          HWADICG(IFN) = Y + SILLBOT
        ELSE
          HWADICG(IFN) = HNEW(IDN)
        ENDIF
C3E---------COMPUTE AND FILL FLOW CORRECTION FOR CLN-GWF CONNECTION
        DO II = IA(NL)+1,IA(NL+1)-1
          JJ = JA(II)
          IF(JJ.NE.NH) CYCLE
C3F-----------FIND MATRIX LOCATION OF DOWNSTREAM NODE          
          ILOC = II  !MATRIX LOCATION FOR NL BEING DOWNSTREAM NODE
          IF(NH.EQ.IDN) ILOC = ISYM(II)
C3G-------FILL CORRECTION FOR CONNECTION 
          RHS(IDN) = RHS(IDN) + AMAT(ILOC)*(HWADICG(IFN) - HNEW(IDN)) 
          RHS(IUP) = RHS(IUP) - AMAT(ILOC)*(HWADICG(IFN) - HNEW(IDN))
        ENDDO
      ENDDO
C
C4-----RETURN.
      RETURN
      END
C------------------------------------------------------------------------
      SUBROUTINE SCLN1H4
C     ******************************************************************
C     COMPUTE CONDUCTANCE FOR CLN FROM PGF AND SATURATED THICKNESS, IN AMAT
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NODES,NLAY,IBOUND,HNEW,BUFF,BOT,TOP,ISYM,IOUT,
     1    NODLAY,AMAT,RHS,IA,JA,JAS,PGF,ICONCV,Sn,AKRC,AKR,iunsat
      USE CLN1MODULE, ONLY:  NCLN,NNDCLN,NCLNNDS,ACLNNDS,CLNCON,
     1    ACLNGWC,NCLNGWC,ACLNCOND,IFLINCLN,IA_CLN,JA_CLN
      USE GWFBCFMODULE,ONLY:IWDFLG,WETFCT,IHDWET,IWETIT,LAYCON,HDRY,
     1                      HK,WETDRY,LAYAVG
      USE SMSMODULE, ONLY: EPSILON
C
      DOUBLE PRECISION HD,BBOT,TTOP,THCK,ZERO,PERIF,PERIW,
     1           TOTTHICK
C     ------------------------------------------------------------------
C1A------SET CONSTANTS
      ZERO=0.
C----------------------------------------------------------------------------
C1B------LOOP THROUGH EACH CLN CELL AND COMPUTE FRACTION SATURATED
C
      DO 200 ICLN=1,NCLNNDS
        N = ACLNNDS(ICLN,1)
        IFLIN = IFLINCLN(ICLN)
        IF(IBOUND(N).EQ.0.OR.IFLIN.EQ.1) CYCLE
C
C1C------CALCULATE SATURATED THICKNESS.
        HD=HNEW(N)
        BBOT = ACLNNDS(ICLN,5)
        CALL CLN_THIK(ICLN,HD,BBOT,THCK)
C
C1D-----STORE IN Sn ARRAY AND MOVE TO NEXT NODE.
        Sn(N)=THCK
        AKR(N) = THCK
  200 CONTINUE
C----------------------------------------------------------------------------
C2------FILL AKRC WITH UPSTREAM KR OF THE CONNECTION FOR ALL CLN-CLN CONNECTIONS
C----------------------------------------------------------------------------      
C2A------LOOP OVER ALL CLN NODES 
      DO NC1 = 1,NCLNNDS
C------------loop over all connections of node NC1 
        DO II_CLN = IA_CLN(NC1)+1,IA_CLN(NC1+1)-1
          NC2 = JA_CLN(II_CLN)
          IF(NC2.GT.NC1) CYCLE
          ND1 = ACLNNDS(NC1,1)   !  NC1 + NODES
          ND2 = ACLNNDS(NC2,1)   !  NC2 + NODES
          IF(IBOUND(ND1).EQ.0.OR.IBOUND(ND2).EQ.0) CYCLE
C2B---------FIND LOWER ROW NUMBER FOR UPPER DIAGONAL OF PGF
          IF(ND2.GT.ND1)THEN
            NL = ND1
            NH = ND2
          ELSE
            NL = ND2
            NH = ND1
          ENDIF
C2C---------COMPUTE AND FILL AKRC TERM FOR CLN-CLN CONNECTION
          DO II = IA(NL)+1,IA(NL+1)-1
            JJ = JA(II)
            IF(JJ.NE.NH) CYCLE
            IIS = JAS(II)
C2D---------FIND UPSTREAM NODE AND HIGHER BOT NODE
            IUPS = NL
            IF(HNEW(JJ).GT.HNEW(NL)) IUPS = JJ
            IHBOT = NL
            BNL = ACLNNDS(NL-NODES,5)
            BJJ = ACLNNDS(JJ-NODES,5)
            IF(BJJ.GT.BNL) IHBOT = JJ
C2E---------FILL AKRC FOR CONNECTION
            INDK = 0
            IF(IUPS.EQ.IHBOT) INDK = 1
            IF(ABS(BJJ-BNL).LT.0.01) INDK = 1
            IF(INDK.EQ.1)THEN
              AKRC(IIS) = AKR(IUPS)
            ELSE
              IFLIN = IFLINCLN(IUPS-NODES)
              IF(IFLIN.EQ.1) CYCLE
              HD=HNEW(IUPS)
              ICLN= IHBOT-NODES
              BBOT = ACLNNDS(ICLN,5)
              CALL CLN_THIK(ICLN,HD,BBOT,THCK)
              AKRC(IIS) = THCK
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C-----------------------------------------------------------------------------
C3------FILL AKRC WITH UPSTREAM AKR OF THE CONNECTION FOR ALL CLN-GWF CONNECTIONS
C-----------------------------------------------------------------------------
      DO IFN = 1,NCLNGWC
        IH = ACLNGWC(IFN,1)
        NH = ACLNNDS(IH,1)
        NL = ACLNGWC(IFN,2)
        IF(IBOUND(NH).EQ.0.OR.IBOUND(NL).EQ.0) CYCLE
C3A---------COMPUTE AND FILL AKRC TERM FOR CLN-GWF CONNECTION
        DO II = IA(NL)+1,IA(NL+1)-1
          JJ = JA(II)
          IF(JJ.NE.NH) CYCLE
          IIS = JAS(II)
C3B---------FIND UPSTREAM NODE AND HIGHER BOT NODE
          IUPS = NL
          IF(HNEW(JJ).GT.HNEW(NL)) IUPS = JJ
          IHBOT = NL
          BNH = ACLNNDS(IH,5)
          BNL = BOT(NL)
          IF(BNH.GT.BNL) IHBOT = NH
          IF(ACLNNDS(IH,3).EQ.1)THEN
C3C---------FILL AKRC FOR HORIZONTAL CLN CELL (USE UPSTREAM WETTED PERIMETER)
              BBOT = ACLNNDS(IH,5)
              IF(HNEW(IUPS).GT.(BBOT-EPSILON))THEN !OTHERWISE AKRC IS ZERO FOR THE CONNECTION
                HD=HNEW(IUPS)
                IFTYP = ACLNNDS(IH,2)
                CALL CLNP (IFTYP,PERIF)
                CALL CLNPW (IH,HD,PERIW)
                THCK = PERIW/PERIF
                AKRC(IIS) = THCK
              ENDIF
          ELSE
C3D---------FILL ARKC FOR VERTICAL CLN CELL (USE UPSTREAM SATURATIONS)
            INDK = 0
            IF(IUPS.EQ.IHBOT) INDK = 1
            IF(ABS(BNH-BNL).LT.0.01) INDK = 1
            IF(INDK.EQ.1)THEN
              AKRC(IIS) = AKR(IUPS)
            ELSE
              HD=HNEW(IUPS)
              IF(IUPS.EQ.NH) THEN !CLN CELL IS UPSTREAM, GWF CELL HAS HIGHER BOT
                BBOT = BOT(IHBOT)
                CALL CLN_THIK(IH,HD,BBOT,THCK)
              ELSE !GWF CELL IS UPSTREAM, CLN CELL HAS HIGHER BOT
                BBOT=ACLNNDS(NH-NODES,5)
                TTOP=TOP(NL)
                TOTTHICK = TTOP - BBOT
                CALL SAT_THIK(N,HD,TOTTHICK,BBOT,THCK)
              ENDIF
              AKRC(IIS) = THCK
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C----------------------------------------------------------------------------
C3-----COMPUTE CONDUIT-CONDUIT CONDUCTANCE FROM AKRC  AND PGF
C3A----LOOP OVER ALL CONDUIT SEGMENTS
      DO NC1 = 1,NCLNNDS
C3B----------loop over all connections of node NC1 
        DO II_CLN = IA_CLN(NC1)+1,IA_CLN(NC1+1)-1
          NC2 = JA_CLN(II_CLN)
          IF(NC2.GT.NC1) CYCLE
          ND1 = ACLNNDS(NC1,1)   !  NC1 + NODES
          ND2 = ACLNNDS(NC2,1)   !  NC2 + NODES
          IF(IBOUND(ND1).EQ.0.OR.IBOUND(ND2).EQ.0) CYCLE
C3C---------FIND LOWER ROW NUMBER FOR UPPER DIAGONAL OF PGF
          IF(ND2.GT.ND1)THEN
            NL = ND1
            NH = ND2
          ELSE
            NL = ND2
            NH = ND1
          ENDIF
C3D---------COMPUTE AND FILL AMAT TERM FOR CONDUIT-CONDUIT CONNECTION
          DO II = IA(NL)+1,IA(NL+1)-1
            JJ = JA(II)
            IF(JJ.NE.NH) CYCLE
            IIS = JAS(II)
            IUPS = JJ
            IF(HNEW(JJ).LT.HNEW(NL)) IUPS = NL
C3D1--------FILL OFFDIAGONAL TERMS IN ROWS NL AND NH
            AMAT(II) = PGF(IIS)*AKRC(IIS)
            AMAT(ISYM(II)) = PGF(IIS)*AKRC(IIS)
C3D2--------ADD TO DIAGONAL TERMS IN ROWS NL AND NH
            AMAT(IA(NL)) = AMAT(IA(NL)) - PGF(IIS)*AKRC(IIS)
            AMAT(IA(NH)) = AMAT(IA(NH)) - PGF(IIS)*AKRC(IIS)
          ENDDO
C
        ENDDO
      ENDDO
C----------------------------------------------------------------------------
C4----COMPUTE CONDUIT-MATRIX CONDUCTANCE FROM AKRC AND PGF
C4A---Loop over all conduit nodes
      DO IFN = 1,NCLNGWC
        IH = ACLNGWC(IFN,1)
        NH = ACLNNDS(IH,1)
        NL = ACLNGWC(IFN,2)
        IF(IBOUND(NH).EQ.0.OR.IBOUND(NL).EQ.0) CYCLE
        DO II = IA(NL)+1,IA(NL+1)-1
          JJ = JA(II)
          IF(JJ.NE.NH) CYCLE
          IIS = JAS(II)
C
          IUPS = JJ
          IF(HNEW(JJ).LT.HNEW(NL)) IUPS = NL
C4B---------FILL OFFDIAGONAL TERMS IN ROWS NL AND NH
          AMAT(II) = PGF(IIS)*AKRC(IIS)
          AMAT(ISYM(II)) = PGF(IIS)*AKRC(IIS)
C4C---------ADD TO DIAGONAL TERMS IN ROWS NL AND NH
          AMAT(IA(NL)) = AMAT(IA(NL)) - PGF(IIS)*AKRC(IIS)
          AMAT(IA(NH)) = AMAT(IA(NH)) - PGF(IIS)*AKRC(IIS)
        ENDDO
      ENDDO
C
C5-----RETURN.
      RETURN
      END
C
C -----------------------------------------------------------------------
      SUBROUTINE SCLN1S4
C     ******************************************************************
C     FILL STORAGE TERMS IN AMAT AND RHS FOR CLN DOMAIN.
C     ******************************************************************
      USE GLOBAL, ONLY:JA,IA,NODES,NEQS,AMAT,RHS,IBOUND,Sn,So,HNEW,AREA
      USE CLN1MODULE, ONLY:NCLN,NCLNNDS,ACLNNDS,NNDCLN,CLNCON,IFLINCLN
      USE GWFBASMODULE,ONLY:DELT
      DOUBLE PRECISION SATO,SATN,EPS,HD,THCK,DS,RHO2,TLED,BBOT
C----------------------------------------------------------------------
C1------LOOP OVER ALL CLN NODES
      TLED = 1.0/DELT
      DO ICLN=1,NCLNNDS
        N = ACLNNDS(ICLN,1)
        IFLIN = IFLINCLN(ICLN)
        IF(IBOUND(N).EQ.0.OR.IFLIN.EQ.1) CYCLE
C2--------COMPUTE TERM USING NEWTON LINEARIZATION
        SATO = So(N)
        SATN = Sn(N)
        EPS = 1.0E-3
        HD=HNEW(N)+ EPS
        BBOT = ACLNNDS(ICLN,5)
        CALL CLN_THIK(ICLN,HD,BBOT,THCK)
        DS = (THCK - SATN)/EPS
        IF(DS.LT.1.0E-7) DS = 1.0E-7
        RHO2  = AREA(N) * ACLNNDS(ICLN,4) * TLED
        AMAT(IA(N)) = AMAT(IA(N)) - RHO2 * DS
        RHS(N) = RHS(N) - RHO2*DS*HNEW(N) + RHO2 * (SATN-SATO)
      ENDDO
C3-----RETURN
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE CLN_THIK(ICLN,HD,BBOT,THCK)
C     ******************************************************************
C     COMPUTE FRACTION OF TOTAL VOLUME THAT IS SATURATED
C     FOR CONDUIT NODE AND STORE IN THCK -
C     FRACTION SATURATED DEPENDS ON CONDUIT ORIENTATION
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:BOT,TOP,ISSFLG,NODES
      USE CLN1MODULE, ONLY: ACLNNDS,ACLNCOND
      DOUBLE PRECISION THCK,HD,BBOT,TTOP,X,S,V,COF1,COF2,FACTOR1,FACTOR2
     *  ,eps,acof,y,TOTTHICK,FRAD,PI,DEPTH,AREAF,AREAW
C     ------------------------------------------------------------------
C1------GET DIRECTION OF LINE SEGMENT
      IFDIR = ACLNNDS(ICLN,3)
      I = ACLNNDS(ICLN,1)
C--------------------------------------------------------
      IF(IFDIR.EQ.0)THEN
C2-------VERTICAL LINE SEGMENT
        TOTTHICK = ACLNNDS(ICLN,4)
        CALL SAT_THIK(I,HD,TOTTHICK,BBOT,THCK)
      ELSEIF(IFDIR.EQ.1)THEN
C3-------HORIZONTAL LINE SEGMENT CONDUIT
        IC = ACLNNDS(ICLN,2)
        CALL CLNA(IC,AREAF)
        CALL CLNAW(ICLN,HD,AREAW)
        THCK = AREAW / AREAF
      ELSEIF(IFDIR.EQ.2)THEN
C4-------ANGLED CONDUIT
        FANGLE = ACLNNDS(ICLN,6)
        TOTTHICK = ACLNNDS(ICLN,4) * SIN(FANGLE)
        BBOT = ACLNNDS(ICLN,5)
        I = ACLNNDS(ICLN,1)
        CALL SAT_THIK(I,HD,TOTTHICK,BBOT,THCK)
      ENDIF
      IF(THCK.LT.1.0E-7) THCK = 1.0E-7
C
C5------RETURN.
      RETURN
      END
C--------------------------------------------------------------------------------
      SUBROUTINE CLNV(ICLN,THCK)
C     ******************************************************************
C     COMPUTE VERTICAL GRID DIMENSION FOR CLN NODE AND STORE IN THCK
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:BOT,TOP,ISSFLG,NODES
      USE CLN1MODULE, ONLY: ACLNNDS,ACLNCOND
      DOUBLE PRECISION THCK,HD,BBOT,TTOP,X,S,V,COF1,COF2,FACTOR1,FACTOR2
     *  ,eps,acof,y,TOTTHICK,Aw,FRAD,PI,DEPTH
C     ------------------------------------------------------------------
C1------GET DIRECTION OF LINE SEGMENT
      IFDIR = ACLNNDS(ICLN,3)
      I = ACLNNDS(ICLN,1)
C--------------------------------------------------------
      IF(IFDIR.EQ.0)THEN
C2-------VERTICAL LINE SEGMENT
        THCK = ACLNNDS(ICLN,4)
      ELSEIF(IFDIR.EQ.1)THEN
C3-------HORIZONTAL LINE SEGMENT CONDUIT
        PI = 3.1415926
        IC = ACLNNDS(ICLN,2)
        CALL CLNR(IC,FRAD)
        THCK = 2.0 * PI * FRAD
      ELSEIF(IFDIR.EQ.2)THEN
C4-------ANGLED CONDUIT
        FANGLE = ACLNNDS(ICLN,6)
        THCK = ACLNNDS(ICLN,4) * SIN(FANGLE)
      ENDIF
C
C5------RETURN.
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE CLN1BDS(KSTP,KPER)
C     ******************************************************************
C     COMPUTE STORAGE BUDGET FLOW TERM FOR CLN.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:NCOL,NROW,NLAY,ISSFLG,IBOUND,HNEW,HOLD,
     1             BUFF,TOP,IOUT,NODES,NODLAY,IUNSTR,Sn,So,AREA
      USE CLN1MODULE, ONLY: ACLNNDS,NCLNNDS,ICLNCB,IFLINCLN
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,VBVL,VBNM,DELT,PERTIM,TOTIM
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION STOIN,STOUT,SSTRG,SIN,SOUT,TLED,HSING,STRG,
     *  RHO,RHO1,RHO2,SNEW,SOLD,ONE,SATN,SATO,HD,THCK,BBOT
C
      DATA TEXT /'     CLN STORAGE'/
C     ------------------------------------------------------------------
      ISS=ISSFLG(KPER)
C
C1------INITIALIZE BUDGET ACCUMULATORS AND 1/DELT.
      ZERO=0.
      STOIN=ZERO
      STOUT=ZERO
C
C2------IF STEADY STATE, STORAGE TERM IS ZERO
      IF(ISS.NE.0) GOTO 400
C
      ONE=1.0
      TLED=ONE/DELT
C
C3------IF CELL-BY-CELL FLOWS WILL BE SAVED, SET FLAG IBD.
      IBD=0
      IF(ICLNCB.GT.0) IBD=ICBCFL
C
C4------CLEAR BUFFER.
      DO 210 N=1,NCLNNDS
      BUFF(N)=ZERO
210   CONTINUE
C
C5------LOOP THROUGH EVERY CLN CELL IN THE GRID.
      DO ICLN=1,NCLNNDS
        N = ACLNNDS(ICLN,1)
        IFLIN = IFLINCLN(ICLN)
        IF(IBOUND(N).EQ.0.OR.IFLIN.EQ.1) CYCLE
        HD=HNEW(N)
        SATO = So(N)
        BBOT = ACLNNDS(ICLN,5)
        CALL CLN_THIK(ICLN,HD,BBOT,THCK)
        SATN = THCK
C6--------COMPUTE STORAGE
        STRG  = AREA(N) * ACLNNDS(ICLN,4) * TLED * (SATO-SATN)
C
C7-------STORE STORAGE TERM IN BUFFER AND ADD TO ACCUMULATORS.
        BUFF(ICLN)=STRG
        SSTRG=STRG
        IF(STRG.LT.ZERO) THEN
          STOUT=STOUT-SSTRG
        ELSE
          STOIN=STOIN+SSTRG
        END IF
      ENDDO
C
  300 CONTINUE
C
C8-----IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER.
        IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     1                       ICLNCB,BUFF,NCLNNDS,1,1,IOUT)
        IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT,ICLNCB,
     1            BUFF,NCLNNDS,1,1,IOUT,DELT,PERTIM,TOTIM,IBOUND)
C
C9------ADD TOTAL RATES AND VOLUMES TO VBVL & PUT TITLE IN VBNM.
  400 CONTINUE
      SIN=STOIN
      SOUT=STOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+SIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+SOUT*DELT
      VBVL(3,MSUM)=SIN
      VBVL(4,MSUM)=SOUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
C
C10----RETURN.
      RETURN
      END

      SUBROUTINE GWF2CLNU1BDCHWR(KSTP,KPER)
C     ******************************************************************
C     SAVE FLOW FROM CONSTANT-HEAD CELLS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,ITRNSP,NOVFC,
     1 TOP,IOUT,NODES,NEQS,NODLAY,IA,JA,JAS,IUNSTR,IVC,ISYM,INCLN,FLOWJA
      USE CLN1MODULE, ONLY: NCLNNDS,ICLNCB
      USE GWFBASMODULE,ONLY:MSUM,VBVL,VBNM,DELT,PERTIM,TOTIM,ICBCFL,
     1                      ICHFLG
      USE GWTBCTMODULE, ONLY: CBCH
      USE SMSMODULE, ONLY: AMATFL
C
      CHARACTER*16 TEXT(1)
      DOUBLE PRECISION HD,CHIN,CHOUT,XX1,TMP,RATE,CHCH1,HDIFF,
     *  X1,CIN,COUT
C
      DATA TEXT(1) /'  CLN CONST HEAD'/
C     ------------------------------------------------------------------
C
C1------CLN DOMAIN
      IBD=0
      IF(ICLNCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(ICLNCB.GT.0) IBD=ICBCFL
      IF(ITRNSP.GT.0.AND.IBD.EQ.0) IBD = 999
      CHIN = 0.0
      CHOUT = 0.0
C     
C1A----CLEAR BUFFER
      DO N=NODES+1,NEQS
          BUFF(N)=0.
      ENDDO
C
      IF(IBD.EQ.2) THEN
C2A-----IF SAVING CELL-BY-CELL FLOW IN A LIST, COUNT CONSTANT-HEAD
C2A-----CELLS AND WRITE HEADER RECORDS.         
        NCH=0
        DO 8 N=NODES+1,NEQS
          IF(IBOUND(N).LT.0) NCH=NCH+1
8       CONTINUE
C2B-------WRITE HEADER FOR THE CLN DOMAINLIST       
 
        CALL UBDSV2U(KSTP,KPER,TEXT(1),ICLNCB,NCLNNDS,
     1       NCH,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C3------LOOP THROUGH EACH CELL AND WRITE FLOW FROM EACH
C3------CONSTANT-HEAD CELL.
      IBDLBL = 0
      DO 201 N=NODES+1,NEQS
C
C4------IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
        IF (IBOUND(N).GE.0)GO TO 201
C
C5--------ACCUMULATE INDIVIDUAL CHD FLOWS FROM FLOWJA ARRAY
          RATE=0.0
          DO II = IA(N)+1,IA(N+1)-1
            X1=FLOWJA(II)
            XX1=X1
            IF(X1.LT.ZERO) THEN
              CHOUT=CHOUT-XX1
            ELSE
              CHIN=CHIN+XX1
            ENDIF
            RATE=RATE+X1
          ENDDO
C
C6--------PRINT THE FLOW FOR THE CELL IF REQUESTED.
        IF(IBD.LT.0) THEN
          IF(IBDLBL.EQ.0) WRITE(IOUT,899) TEXT(1),KPER,KSTP
  899     FORMAT(1X,/1X,A,'   PERIOD',I3,'   STEP',I3)
          WRITE(IOUT,910) N,RATE
  910     FORMAT(1X,'NODE',I8,'   RATE',1PG15.6)
          IBDLBL=1
        END IF
C
C7------IF SAVING CELL-BY-CELL FLOW IN LIST, WRITE FLOW FOR CELL.
        IF(IBD.EQ.2)THEN
          SRATE = RATE
          CALL UBDSVAU(ICLNCB,NODES,N,SRATE,IBOUND)
C--------------------------------------------------------------
        ENDIF
C
C8--------STORE SUM IN BUFFER.
        BUFF(N)=RATE
        IF(ITRNSP.GT.0) CBCH(N) = RATE
C
  201 CONTINUE     
C
C9-------SAVE C-B-C FLOWS FOR CLN NODES
        IF(IBD.EQ.1)THEN
          CALL UBUDSVU(KSTP,KPER,TEXT(1),ICLNCB,BUFF(NODES+1),NCLNNDS,
     1                 IOUT,PERTIM,TOTIM)
        ENDIF
C
C10-----SAVE TOTAL CONSTANT HEAD FLOWS AND VOLUMES IN VBVL TABLE
C11-----FOR INCLUSION IN BUDGET. PUT LABELS IN VBNM TABLE.
      CIN=CHIN
      COUT=CHOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+CIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+COUT*DELT
      VBVL(3,MSUM)=CIN
      VBVL(4,MSUM)=COUT
      VBNM(MSUM)=TEXT(1)
      MSUM=MSUM+1
C
C12-----RETURN.
      RETURN
      END

      SUBROUTINE CLN1BDADJ(KSTP,KPER)
C     ******************************************************************
C     COMPUTE FLOW BETWEEN CLN-CLN AND CLN-MATRIX
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,AMAT,NODLAY,
     1           BOT,IOUT,NODES,NJA,IA,JA,JAS,IUNSTR,ISYM,ITRNSP,FLOWJA,
     1           IWADICLN
      USE CLN1MODULE, ONLY: ICLNCB,NCLN,NNDCLN,CLNCON,NCLNNDS,ACLNNDS,
     1           ACLNGWC,ICCWADICLN,ICGWADICLN,NCLNGWC
      USE GWFBASMODULE,ONLY:ICBCFL,DELT,PERTIM,TOTIM,ICHFLG
      USE GWFBCFMODULE,ONLY:IBCFCB,LAYCON
      USE GWTBCTMODULE, ONLY: CBCF
      USE SMSMODULE, ONLY: AMATFL
C
      DOUBLE PRECISION HD,TMP,HDIFF,HDN,X,Y,SILLBOT
      REAL, DIMENSION(:),ALLOCATABLE :: FLOWCLNCLN(:),FLOWCLNGW(:)
C     ------------------------------------------------------------------
C
C1------IF CELL-BY-CELL FLOWS WILL BE SAVED IN A FILE, SET FLAG IBD.
C1------RETURN IF FLOWS ARE NOT BEING SAVED OR RETURNED.
      ZERO=0.
      IBD=0
      IF(ICLNCB.GT.0) IBD=ICBCFL
      IF(ICLNCB.LT.0) IBD = -1
      IF(ITRNSP.GT.0.AND.IBD.EQ.0) IBD = 999
!      IF(IBD.EQ.0) RETURN
C
C2-----COMPUTE FOR ALL CLN NODES
      DO ICLN = 1,NCLNNDS
        N = ACLNNDS(ICLN,1)
        IF(IBOUND(N).EQ.0) CYCLE
C3---------COMPUTE AND FILL FLOW TERM FROM CLN NODE TO ALL ITS CONNECTIONS
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IF(IBOUND(JJ).EQ.0) CYCLE
          IF(ICHFLG.EQ.0) THEN
            IF((IBOUND(N).LE.0) .AND. (IBOUND(JJ).LE.0)) CYCLE
          END IF
C
C4---------CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
          HD=HNEW(JJ)
          HDIFF=HNEW(N)-HD
C5---------TAKE CARE OF WADI TERMS
          IF(IWADICLN.NE.0)THEN
            IDN = JJ
            IUP = N
            IF(HNEW(N).LT.HD)THEN 
                IDN = N
                IUP = JJ
            ENDIF
            IDNCLN = IDN-NODES
            IF(JJ.GT.NODES)THEN  
C5A--------FOR CLN-CLN CONNECTION
              IF(ICCWADICLN(IDNCLN).NE.0)THEN
                X = HNEW(IDN) - ACLNNDS(IDNCLN,5)
                CALL WADIFN(X,Y)
                HDN = Y + ACLNNDS(IDNCLN,5)
                HDIFF = HNEW(IUP) - HDN
                IF(IUP.EQ.JJ) HDIFF = - HDIFF
              ENDIF
            ELSE  
C5A--------FOR CLN-GW CONNECTION
C5A1---------FIND CLN-GW CONNECTION NUMBER
              DO IFN = 1,NCLNGWC
                IGW = ACLNGWC(IFN,2)
                IF(IGW.EQ.JJ) GO TO 10
              ENDDO
10            CONTINUE
C5A2---------COMPUTE HDIFF FOR CONNECTION              
              IF(ICGWADICLN(IFN).NE.0)THEN
                NL = ACLNGWC(IFN,2)
                IH = ACLNGWC(IFN,1)
                SILLBOT = ACLNNDS(IH,5)
                IF(BOT(NL).GT.SILLBOT) SILLBOT = BOT(NL)
                X = HNEW(IDN) - SILLBOT
                CALL WADIFN(X,Y)
                HDN = Y + SILLBOT
                HDIFF = HNEW(IUP) - HDN
                IF(IUP.EQ.JJ) HDIFF = - HDIFF
              ENDIF
            ENDIF
          ENDIF
C6---------COMPUTE FLUX IN TMP ARRAY
          FLOWJA(II)= HDIFF*AMATFL(II)
        ENDDO
C
      ENDDO
C
C7-----RETURN.
      RETURN
      END
C---------------------------------------------------------------------------------------------
      SUBROUTINE CLN1BDWR(KSTP,KPER)
C     ******************************************************************
C     WRITE FLOW BETWEEN CLN-CLN AND CLN-MATRIX.  THESE CLN-CLN FLOWS
C     ARE WRITTEN SO THAT FLOW IS POSITIVE OUT OF A CLN CELL.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,AMAT,NODLAY,
     1    TOP,IOUT,NODES,NJA,IA,JA,JAS,IUNSTR,ISYM,ITRNSP,FLOWJA
      USE CLN1MODULE, ONLY: ICLNCB,NCLN,NNDCLN,CLNCON,NCLNNDS,ACLNNDS,
     1    NCLNGWC,ACLNGWC,IA_CLN,JA_CLN,NJA_CLN
      USE GWFBASMODULE,ONLY:ICBCFL,DELT,PERTIM,TOTIM,ICHFLG
      USE GWFBCFMODULE,ONLY:IBCFCB,LAYCON
      USE GWTBCTMODULE, ONLY: CBCF
C
      CHARACTER*16 TEXT(3)
      DOUBLE PRECISION HD,TMP,HDIFF
      REAL, DIMENSION(:),ALLOCATABLE :: FLOWCLNCLN(:),FLOWCLNGW(:)
C
      DATA TEXT(1) /'   FLOW CLN FACE'/
      DATA TEXT(2) /'      GWF TO CLN'/
      DATA TEXT(3) /'       CLN FLOWS'/
C     ------------------------------------------------------------------
C
C1------RETURN IF FLOWS ARE NOT BEING SAVED OR RETURNED.
      IBD=0
      IF(ICLNCB.GT.0) IBD=ICBCFL
      IF(ICLNCB.LT.0) IBD = -1
      IF(ITRNSP.GT.0.AND.IBD.EQ.0) IBD = 999
      IF(IBD.EQ.0) RETURN
      ZERO = 0.
C2------ALLOCATE TEMPORARY ARRAY FOR FLOW ACCUMULATIONS
      LCLN = NJA_CLN
      ALLOCATE(FLOWCLNCLN(LCLN))
      ALLOCATE(FLOWCLNGW(NCLNGWC))
C
C3------INITIALIZE FLOW ACCUMULATION ARRAYS
      DO IJ=1,LCLN
        FLOWCLNCLN(IJ)=ZERO
      ENDDO
      DO IJ=1,NCLNGWC
        FLOWCLNGW(IJ)=ZERO
      ENDDO
C----------------------------------------------------------------------------
C4-----MOVE CLN-CLN FLOW INTO TEMPORARY ARRAY
C4A------LOOP OVER ALL CLN NODES 
      DO NC1 = 1,NCLNNDS
C4B----------loop over all connections of node NC1 
        DO II_CLN = IA_CLN(NC1)+1,IA_CLN(NC1+1)-1
          NC2 = JA_CLN(II_CLN)
          !IF(NC2.GT.NC1) CYCLE
          ND1 = ACLNNDS(NC1,1)   !  NC1 + NODES
          ND2 = ACLNNDS(NC2,1)   !  NC2 + NODES
          IF(IBOUND(ND1).EQ.0.OR.IBOUND(ND2).EQ.0) CYCLE
C4B---------FILL FLOW TERM FOR CLN-CLN CONNECTION
          DO II = IA(ND1)+1,IA(ND1+1)-1
            JJ = JA(II)
            IF(JJ.NE.ND2) CYCLE
            IF(ICHFLG.EQ.0) THEN
              IF((IBOUND(ND1).LE.0) .AND. (IBOUND(JJ).LE.0)) CYCLE
            END IF
            IIS = JAS(II)
            FLOWCLNCLN(II_CLN)= -FLOWJA(II)
            IF(ITRNSP.GT.0) CBCF(IIS) = FLOWJA(II)
          ENDDO
        ENDDO
      ENDDO
C4D------RECORD CLN-CLN FLOW
      IF(IBD.EQ.1)
     1   CALL UBUDSVU(KSTP,KPER,TEXT(1),ICLNCB,FLOWCLNCLN,LCLN,IOUT,
     1         PERTIM,TOTIM)
      IF(IBD.EQ.2) CALL UBDSV1U(KSTP,KPER,TEXT(1),ICLNCB,FLOWCLNCLN,
     1         LCLN,IOUT,DELT,PERTIM,TOTIM,IBOUND,NODES)
      IF(IBD.EQ.-1)THEN
C4E-----WRITE FLOWS TO OUTPUT FILE
      WRITE(IOUT,1) TEXT(1),KSTP,KPER
1     FORMAT(/1X,'WRITING "',A16,'" BELOW',1X,
     1     'AT TIME STEP',I7,', STRESS PERIOD',I7/
     2  1X,'CLN CELL AND LIST OF CONNECTED CLN CELLS AND FLOWS')
C
      DO NC1 = 1,NCLNNDS
        WRITE(IOUT,2) NC1,(JA_CLN(II),FLOWCLNCLN(II),
     1  II= IA_CLN(NC1)+1,IA_CLN(NC1+1)-1)
2       FORMAT(I10,200(I10,G15.6))
      ENDDO
3     FORMAT(12E15.6)
      ENDIF
C
C----------------------------------------------------------------------------
C5-----MOVE CLN-MATRIX FLOW FOR EACH CLN NODE INTO TEMPORARY ARRAY
      DO NN = 1,NCLNGWC
        IH = ACLNGWC(NN,1)
        ND1 = ACLNNDS(IH,1)
        NL = ACLNGWC(NN,2)
        IF(IBOUND(ND1).EQ.0.OR.IBOUND(NL).EQ.0) CYCLE
C5B-------FIND CLN-MATRIX CONNECTION
        DO II = IA(ND1)+1,IA(ND1+1)-1
          JJ = JA(II)
          IF(JJ.NE.NL) CYCLE
          IIS = JAS(II)
          IF(ICHFLG.EQ.0) THEN
            IF((IBOUND(ND1).LE.0) .AND. (IBOUND(JJ).LE.0)) CYCLE
          END IF
C
C5C-------FILL FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
          FLOWCLNGW(NN) = -FLOWJA(II)
          IF(ITRNSP.GT.0) CBCF(IIS) = FLOWCLNGW(NN)
        ENDDO
      ENDDO
C5D-----RECORD CLN-MATRIX FLOW
      IF(IBD.EQ.1)
     1   CALL UBUDSVU(KSTP,KPER,TEXT(2),ICLNCB,FLOWCLNGW,NCLNGWC,IOUT,
     1         PERTIM,TOTIM)
      IF(IBD.EQ.2) CALL UBDSV1U(KSTP,KPER,TEXT(2),ICLNCB,FLOWCLNGW,
     1     NCLNGWC,IOUT,DELT,PERTIM,TOTIM,IBOUND,NODES)
      IF(IBD.EQ.-1)THEN
C5E-----WRITE FLOWS TO OUTPUT FILE
      IF(IOUT.GT.0) WRITE(IOUT,4) TEXT(2),KSTP,KPER
4     FORMAT(/1X,'WRITING "',A16,'" BELOW',1X,
     1     'AT TIME STEP',I7,', STRESS PERIOD',I7)
      WRITE(IOUT,3)(FLOWCLNGW(N),N=1,NCLNGWC)
      ENDIF
C6------DEALLOCATE ALL TEMPORARY ARRAYS
      DEALLOCATE(FLOWCLNCLN)
      DEALLOCATE(FLOWCLNGW)
C7-------RETURN
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE SCLN1D(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     CALCULATE, PRINT, AND SAVE DRAWDOWNS IN CLN CELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY: HNEW,STRT,IBOUND,IOUT,NODES
      USE CLN1MODULE, ONLY: NCLNNDS,ICLNDD
      USE GWFBASMODULE, ONLY: PERTIM,TOTIM,IDDNFM,IDDNUN,LBDDSV,
     2                        CDDNFM,IOFLG
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION SSTRT
      REAL,          SAVE,    DIMENSION(:),    ALLOCATABLE ::BUFF
C
      DATA TEXT /'    CLN DRAWDOWN'/
C     ------------------------------------------------------------------
      ALLOCATE(BUFF(NCLNNDS))
C
C1------FOR EACH CLN NODE CALCULATE DRAWDOWN IF PRINT OR SAVE IS REQUESTED.
      DO 59 N=1,NCLNNDS
C
C2------CALCULATE DRAWDOWN FOR THE NODE
      NG = N+NODES
      BUFF(N)=HNEW(NG)
      SSTRT=STRT(NG)
      IF(IBOUND(NG).NE.0) BUFF(N)=SSTRT-HNEW(NG)
   59 CONTINUE
C
C3------CALL ULAPRS OR ULAPRW TO PRINT DRAWDOWN.
      IF(ISA.NE.0) THEN
        IF(IOFLG(1,2).NE.0) THEN
          IF(IDDNFM.LT.0) CALL ULAPRS(BUFF(1),TEXT,KSTP,KPER,
     1                  NCLNNDS,1,1,-IDDNFM,IOUT)
          IF(IDDNFM.GE.0) CALL ULAPRW(BUFF(1),TEXT,KSTP,KPER,
     1                  NCLNNDS,1,1,IDDNFM,IOUT)
          IPFLG=1
        ENDIF
C
      END IF
C
C4------DETERMINE IF DRAWDOWN SHOULD BE SAVED.
C4------IF SO THEN CALL A ULASAV OR ULASV2 TO RECORD DRAWDOWN.
      IFIRST=1
      IF(ICLNDD.LE.0) GO TO 80
      IF(IOFLG(1,4).EQ.0) GO TO 80
        NSTRT = NODES+1
        IF(IFIRST.EQ.1) WRITE(IOUT,74) ICLNDD,KSTP,KPER
   74   FORMAT(1X,/1X,'CLN DRAWDOWN WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        IF(CDDNFM.EQ.' ') THEN
           CALL ULASAV(BUFF(1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCLNNDS,
     1                1,1,ICLNDD)
        ELSE
           CALL ULASV2(BUFF(1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCLNNDS,
     1                1,1,ICLNDD,CDDNFM,LBDDSV,IBOUND(NSTRT))
        END IF
C
80    CONTINUE
      DEALLOCATE(BUFF)

C
C5------RETURN.
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE SCLN1H(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     PRINT AND SAVE HEADS IN CLN CELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:HNEW,IBOUND,IOUT,NODES
      USE CLN1MODULE, ONLY:  NCLNNDS,ICLNHD
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IHEDFM,IHEDUN,LBDDSV,
     2                      CDDNFM,IOFLG,CHEDFM
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION SSTRT
      REAL,          SAVE,    DIMENSION(:),    ALLOCATABLE ::BUFF
C
      DATA TEXT /'       CLN HEADS'/
C     ------------------------------------------------------------------
      ALLOCATE(BUFF(NCLNNDS))
C
C1------FOR EACH CLN NODE PUT HEADS IN BUFF IF PRINT OR SAVE IS REQUESTED.
      DO 59 N=1,NCLNNDS
C
C2------Save heads in buffer array BUFF
        NG = N+NODES
        BUFF(N)=HNEW(NG)
   59 CONTINUE
C
C3------CALL ULAPRS OR ULAPRW TO PRINT heads.
      IF(ISA.NE.0) THEN
        IF(IOFLG(1,1).NE.0) THEN
          IF(IHEDFM.LT.0) CALL ULAPRS(BUFF(1),TEXT,KSTP,KPER,
     1                  NCLNNDS,1,1,-IHEDFM,IOUT)
          IF(IHEDFM.GE.0) CALL ULAPRW(BUFF(1),TEXT,KSTP,KPER,
     1                  NCLNNDS,1,1,IHEDFM,IOUT)
          IPFLG=1
        ENDIF
C
      END IF
C
C4------DETERMINE IF HEAD SHOULD BE SAVED.
C4------IF SO THEN CALL A ULASAV OR ULASV2 TO RECORD HEAD.
      IFIRST=1
      IF(ICLNHD.LE.0) GO TO 80
        NSTRT = NODES+1
        IF(IOFLG(1,3).EQ.0) GO TO 80
        IF(IFIRST.EQ.1) WRITE(IOUT,74) ICLNHD,KSTP,KPER
   74   FORMAT(1X,/1X,'CLN HEAD WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        IF(CHEDFM.EQ.' ') THEN
           CALL ULASAV(BUFF(1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCLNNDS,
     1                1,1,ICLNHD)
        ELSE
           CALL ULASV2(BUFF(1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCLNNDS,
     1                1,1,ICLNHD,CHEDFM,LBDDSV,IBOUND(NSTRT))
        END IF
C
80    CONTINUE
      DEALLOCATE(BUFF)

C
C5------RETURN.
      RETURN
      END
C----------------------------------------------------------------------------
      SUBROUTINE SCLN1IB(KSTP,KPER)
C     ******************************************************************
C     RECORD IBOUND OF CLN NODES
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: IBOUND,IOUT
      USE CLN1MODULE, ONLY:  NCLNNDS,ICLNIB
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IBOUUN,LBBOSV,CBOUFM,IOFLG
C
      INTEGER,  SAVE,    DIMENSION(:),    ALLOCATABLE ::ITEMP
      CHARACTER*16 TEXT
      DATA TEXT /'          IBOUND'/
C     ------------------------------------------------------------------
      IF(ICLNIB.LE.0) RETURN
      ALLOCATE (ITEMP(NCLNNDS))
C
C1------FOR EACH CLN NODE PUT IBOUND IN ITMP IF SAVE IS REQUESTED.
      DO 59 N=1,NCLNNDS
        NG = N+NODES
        ITEMP(N)=IBOUND(NG)
   59 CONTINUE
C
C2------SAVE IBOUND WHEN REQUESTED.
      IF(IOFLG(1,7).EQ.0) GO TO 79
      WRITE(IOUT,74) ICLNIB,KSTP,KPER
   74 FORMAT(1X,/1X,'CLN IBOUND WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
C
      CALL ULASV3(ITEMP(1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCLNNDS,
     1                1,1,ICLNIB,CBOUFM,LBBOSV)
   79   CONTINUE
C
C3------RETURN.
      DEALLOCATE(ITEMP)
      RETURN
      END
C
C -----------------------------------------------------------------------
      SUBROUTINE CLN1DA
C  DEALLOCATE CLN DATA
      USE CLN1MODULE
C
        DEALLOCATE(NCLN,ICLNCB,ICLNHD,ICLNDD,ICLNIB,NCLNNDS)
        DEALLOCATE(ACLNNDS)
        DEALLOCATE(IFLINCLN,ICCWADICLN,ICGWADICLN,ACLNGWC,ACLNCOND)
C
      RETURN
      END
