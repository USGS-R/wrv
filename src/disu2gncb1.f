      SUBROUTINE GNCb2DISU1AR(IN,TEXT,MXGNCb,GNCb,IRGNCb,
     1  ISYMGNCb,MXADJb,IADJMATb,IPRGNCb)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR GHOST NODE WITH n ADDITIONAL CONTRIBUTING NODES
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:LAYHDT,IOUT,NODES,IUNSTR,NODLAY,IUNIT,
     1                NEQS,NLAY
C
      DIMENSION GNCb(4+2*MXADJb,MXGNCb),IRGNCb(MXADJb,MXGNCb) 
C     GNCb(1, 1:MXGNCb) - NODE NUMBER ON WHICH GNC BOUNDARY APPLIES
C     GNCb(1+1:1+MXADJB, 1:MXGNCb) - CONTRIBUTING NODE NUMBER (FOR MXADNb CONTRIBUTING NODES 
C     GNCb(1+(1+MXADNb):1+2MXADNb, 1:MXGNCb) - CONTRIBUTING FACTOR FOR THE CONTRIBUTING NODES
C     GNCb(2+2*MXADJb, 1:MXGNCb) - CONTRIBUTING FACTOR FROM THE BOUNDARY ITSELF 
C     GNCb(3+2*MXADJb, 1:MXGNCb) - Cnm STORED TEMPORARILY HERE  
C     GNCb(4+2*MXADJb, 1:MXGNCb) - H(boundary) is stored temporarily here       
      CHARACTER*200 LINE
      CHARACTER*3 TEXT
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE.
      INGNCB = IN
      WRITE(IOUT,1) TEXT
    1 FORMAT(1X,/1X,'GNCb -- GHOST NODE CORRECTION PROCESS ',
     &   ' VERSION 1, 6/18/2011. FOR ',A3,' BOUNDARY PACKAGE'/85('-'))
      WRITE(IOUT,9) TEXT,MXGNCb
    9 FORMAT(1X,'MAXIMUM NUMBER OF',A3,' CELLS WITH GNC CORRECTION',1X,
     1  'MXGNCb = ',I7)
C
C6------READ ADJACENT CONTRIBUTING NODES AND CONTRIBUTIONS FOR EACH GNC BOUNDARY
C6------NOTE THAT THE GNC BOUNDARIES ARE IN FRONT OF THE LIST OF ALL BOUNDARIES FOR THIS BC TYPE.
      IF(IPRGNCb.EQ.1) WRITE(IOUT,4) TEXT
4     FORMAT(1X,'LIST OF: BOUNDARY CELL, ADJACENT CONTRIBUTING CELLS',
     1  1X, 'AND CONTRIBUTING FACTORS FOR ',A3,'BOUNDARY PACKAGE')
      DO IGNCb =1,MXGNCb
C       READ BOUNDARY NODE NUMBER, BOUNDARY'S CONTRIBUTION FACTOR, FOLLOWED BY 
C       EACH CONTRIBUTING NODE NUMBER WITH ITS CONTRIBUTING FACTOR 
C       NOTE: CONTRIBUTION OF NODE "N" IS CALCULATED AS 1 - SUM (ALL OTHER FACTORS).      
        READ(INGNCb,*) GNCb(1,IGNCb),GNCb(2+2*MXADJb,IGNCB),
     1      (GNCb(1+IADJb,IGNCb),
     1               GNCb(1+IADJb+MXADJb,IGNCb),IADJb = 1, MXADJb)
        IF(IPRGNCb.EQ.1)
     1    WRITE(IOUT,5) GNCb(1,IGNCb),GNCb(2+2*MXADJb,IGNCB),
     1      (GNCb(1+IADJb,IGNCb),
     1                  GNCb(1+IADJb+MXADJb,IGNCb),IADJb = 1, MXADJb)
5       FORMAT(1X,F8.0,1X,F8.4,200(1X,F8.0,5X,F8.4))
      ENDDO
C
C12---RETURN
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE ADDIAJA_GNCb (NGNCb,ISYMGNCb,MXADJb,MXGNCb,GNCb,IRGNCb,
     1  IADJMATb)
C     ******************************************************************
C     ADD IA AND JA OF GNCb NODES TO THE GLOBAL IA AND JA ARRAYS AND
C     PREPARE GNCb ARRAYS
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
C
      USE GLOBAL,     ONLY:NODES,NEQS,NJA,IA,JA
      INTEGER, SAVE,    DIMENSION(:),ALLOCATABLE  ::IAT
      INTEGER, SAVE,    DIMENSION(:),ALLOCATABLE  ::JAT
      INTEGER, SAVE,    DIMENSION(:,:),ALLOCATABLE  ::IGFOUNDb
      DIMENSION GNCb(4+2*MXADJb,MXGNCb),IRGNCb(MXADJb,MXGNCb)
C
C     ------------------------------------------------------------------
C
C1--------SKIP CHANGING MATRIX STRUCTURE IF SYMMETRIC MATRIX WITH RHS UPDATE
      IF(ISYMGNCb.EQ.1) GO TO 10
C
C2--------ALLOCATE AND FILL TEMPORARY IA AND JA ARRAYS FOR DOMAIN
      ALLOCATE(IAT(NEQS+1))
      ALLOCATE(JAT(NJA))
      ALLOCATE(IGFOUNDb(NGNCb,MXADJb))
      DO N=1,NEQS+1
        IAT(N) = IA(N)
      ENDDO
      DO IJA=1,NJA
        JAT(IJA) = JA(IJA)
      ENDDO
      DEALLOCATE(JA)
C
C3------CONVERT IA TO INDICATE CONNECTIONS PER ROW OF MATRIX
      DO N=1,NEQS
        IA(N) = IA(N+1) - IA(N)
      ENDDO
C4------ADD GNCb CONNECTION TO IA
      CALL SFILLIA_GNCb(IAT,JAT,NEQS,NJA,IGFOUNDb,NGNCb,
     *   GNCb,MXADJb,MXGNCb)
C
C5------RE-COMPUTE CUMULATIVE OF CONNECTIONS PER ROW IN IA
      DO II=2,NEQS+1
        IA(II) = IA(II) + IA(II-1)
      ENDDO
C-------IA(N+1) IS CUMULATIVE_IA(N) + 1
      DO II=NEQS+1,2,-1
        IA(II) = IA(II-1) + 1
      ENDDO
      IA(1) = 1
C6------GET NEW NJA AND ALLOCATE NEW JA ACCORDINGLY
      NJA = IA(NEQS+1) - 1
      ALLOCATE(JA(NJA))
      JA = 0
C7------FILL BLOCK TERMS INTO JA ARRAY AS PER NEW IA
      DO N=1,NEQS
        IJA = IA(N)
        DO IT = IAT(N),IAT(N+1)-1
          JA(IJA) = JAT(IT)
          IJA = IJA + 1
        ENDDO
      ENDDO
C
C8------FILL JA TERMS FOR GNCb DOMAIN
      CALL FILLJA_GNCb (IGFOUNDb,NGNCb,GNCb,MXADJb,MXGNCb)
C9------DEALLOCATE TEMPORATY ARRAYS
      DEALLOCATE(IAT,JAT,IGFOUNDb)
C-----------------------------------------------------------------------------
C10------PREPARE: FIND AND STORE LOCATION OF NODE J IN ROWS N AND M.
      IF(IADJMATb.EQ.1)
     1  CALL SGNCb2DISU1MC(NGNCb,GNCb,IRGNCb,ISYMGNCb,MXADJb,MXGNCb)
10    CONTINUE
C11------RETURN
      RETURN
      END
C ---------------------------------------------------------------------
      SUBROUTINE SFILLIA_GNCb(IAT,JAT,NEQS,NJA,IGFOUNDb,NGNCb,
     *   GNCb,MXADJb,MXGNCb)
C     ******************************************************************
C     INCLUDE GNCb CONNECTIVITIES IN ROW INDICATOR ARRAY IA WHEN NEEDED
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IA
      INTEGER IAT(NEQS+1)
      INTEGER JAT(NJA),IGFOUNDb(NGNCb,MXADJb)
      DIMENSION GNCb(4+2*MXADJb,MXGNCb)
C     ------------------------------------------------------------------
C
C1------LOOP OVER ALL GNCb NODES
      DO IFN = 1,NGNCb
        ND1 = GNCb(1,IFN)
C1------FIND M (ND2)AND J1 (ND3) FOR EACH GNCb NODE IN LIST
        DO IADJb = 1,MXADJb
          ND3 = GNCb(1+IADJb,IFN)
C-----------------------------------------------------------------------------
C6----------DO ONLY FOR ND1 TO ND3 CONNECTIONS - ND2 IS THE BOUNDARY
C-----------------------------------------------------------------------------
          IFOUND = 0
          DO II=IAT(ND1),IAT(ND1+1)-1
C12-----------SEE IF ND1 AND ND3 ARE ALREADY CONNECTED IN PREVIOUS JA LIST
            JJ = JAT(II)
            IF(JJ.EQ.ND3)THEN
              IFOUND = 1
              GO TO 30
            ENDIF
          ENDDO
30        CONTINUE
C13---------SEE IF ND1 AND ND3 ARE CONNECTED IN PREVIOUS GHOST NODE LIST
          DO JFN = 1,IFN-1
            JD1 = GNCb(1,JFN)
            DO JADJb = 1,MXADJb
              JD3 = GNCb(1+JADJb,JFN)
              IF((JD1.EQ.ND1.AND.JD3.EQ.ND3). OR.
     1           (JD3.EQ.ND1.AND.JD1.EQ.ND3))THEN
                IFOUND = 1
                GO TO 40
              ENDIF
            ENDDO
          ENDDO
40        CONTINUE
C
C14--------SEE IF THE ND3 NODES ARE THE SAME - IF SO, CONNECTION WAS FOUND EARLIER
          IF(IADJb.GT.1)THEN
            DO JADJb = 1,IADJb-1
              JD3 = GNCb(1+JADJb,IFN)
              IF(JD3.EQ.ND3)THEN
                IFOUND=1
              ENDIF
            ENDDO
          ENDIF
C-----------------------------------------------------------------------------
C14---------SAVE IFOUND IN ARRAY IGFOUND FOR USE IN CONSTRUCTING JA
          IGFOUNDb(IFN,IADJb) = IFOUND
C15---------IF ND1 TO ND3 CONNECTION IS NOT FOUND, ADD THAT CONNECTION TO LIST
          IF(IFOUND.EQ.0)THEN
            IA(ND1) = IA(ND1) + 1
            IA(ND3) = IA(ND3) + 1
          ENDIF
C-----------------------------------------------------------------------------
        ENDDO
C-----------------------------------------------------------------------------
      ENDDO
C
C21------RETURN
      RETURN
      END
C
C -----------------------------------------------------------------------
      SUBROUTINE FILLJA_GNCb (IGFOUNDb,NGNCb,GNCb,MXADJb,MXGNCb)
C     ******************************************************************
C     FILL JA ARRAY FOR GHOST NODE CORRECTION CELLS
C     ******************************************************************
      USE GLOBAL, ONLY:JA,IA,NODES,NEQS
      INTEGER IGFOUNDb(NGNCb,MXADJb)
      DIMENSION GNCb(4+2*MXADJb,MXGNCb)
C-----------------------------------------------------------------------
C
C1-------CHECK FOR ALL GNCb NODES
C
      DO IFN = 1,NGNCb
         ND1 = GNCb(1,IFN)
C
C2------FOR CONNECTION OF N2 TO CONTRIBUTING NODES J
        DO IADJb = 1,MXADJb
C
C--------------------------------------------------------------------
C4----------DO ONLY FOR CONNECTION OF N1 - N2 IS THE BOUNDARY
C--------------------------------------------------------------------
C5----------FOR CONNECTION OF N2 TO FIRST CONTRIBUTING NODE J1
          IFOUND = IGFOUNDb(IFN,IADJb)
          IF(IFOUND.EQ.0)THEN
C5A----------IF GHOST NODE CONNECTION WAS NOT FOUND, MAKE CONNECTION
            CALL FINDJA(ND1,ND3)
            CALL FINDJA(ND3,ND1)
          ENDIF
C
        ENDDO
      ENDDO
C7------RETURN
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE SGNCb2DISU1MC(NGNCb,GNCb,IRGNCb,ISYMGNCb,MXADJb,MXGNCb)
C     ******************************************************************
C     PREPARE GNCb VARIABLES:
C     FIND AND STORE LOCATION OF NODE J IN ROW N.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,IA,JA,JAS,ISYM,NODLAY,NLAY
      DIMENSION GNCb(4+2*MXADJb,MXGNCb),IRGNCb(MXADJb,MXGNCb)
C     ------------------------------------------------------------------
C
C1----DO FOR EACH GNCb CELL.
      DO 10 IG = 1,NGNCb
C
C2------FIND NODE NUMBERS FOR GHOST AND ASSOCIATED CELLS.
        N1 = GNCb(1,IG)
        DO IADJb = 1,MXADJb
          N3 = GNCb(1+IADJb,IG)
C
C---------------------------------------------------------------------------
          IF(ISYMGNCb.EQ.0)THEN
C4----------FIND LOCATION OF GHOST CELL J IN ROW N AND SAVE
            DO II = IA(N1)+1,IA(N1+1)-1
              JJ = JA(II)
              IF(JJ.EQ.N3)THEN
                IRGNCb(IADJb,IG) = II
                GO TO 200
              ENDIF
            ENDDO
200         CONTINUE
          ENDIF
        ENDDO
C--------------------------------------------------------------------------
   10 CONTINUE
C--------------------------------------------------------------------------
C
C8------RETURN
      RETURN
      END
C
C--------------------------------------------------------------------------------------
      SUBROUTINE GNCb2DISU1FM(NGNCb,GNCb,IRGNCb,ISYMGNCb,MXADJb,MXGNCb,
     1  BOTb,ICONSTRAINTb)
C     ******************************************************************
C     MODIFY EQUATION FOR GHOST NODE CONTRIBUTIONS
C     ROWS OF MATRIX FOR IMPLICIT; RHS VECTOR FOR EXPLICIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,HNEW,TOP,BOT,IBOUND,IOUT,
     1                      AMAT,PGF,FAHL,IA,JA,JAS,ISYM,RHS,HNEW
      DIMENSION GNCb(4+2*MXADJb,MXGNCb),IRGNCb(MXADJb,MXGNCb),
     1  BOTb(MXGNCb)
      DOUBLE PRECISION ALPHA,Cnm,ATERM,RTERM,HD,HNBAR,HB,HN,BN,SIGALJ
C     ------------------------------------------------------------------
C
C---------------------------------------------------------------------------
C1------FOR EACH GHOST NODE, MODIFY COLUMNS N AND J (GHOST NODE CONTRIBUTORS)
C1------IN ROWS N AND M FOR FLOW BETWEEN NODES N AND M.
      DO 20 IG=1,NGNCb
C
C2------GET CELL LOCATIONS AND PROPERTIES.
        N1 = GNCb(1,IG)
        IF(IBOUND(N1).EQ.0) CYCLE
C
C2--------EVALUAE CONSTRAINT ON BOUNDARY AND SKIP IF HEAD IS BELOW CONSTRAINT          
        ICASE = 1
        IF(ICONSTRAINTb.EQ.1)THEN
          HN = HNEW(N1)
          BN = BOTb(IG)
          SIGALJ = GNCb(2+2*MXADJb,IG)
          HD = GNCb(2+2*MXADJb,IG)* GNCb(4+2*MXADJb,IG)
          DO IADJb = 1,MXADJb
            N3 = GNCb(1+IADJb,IG)
            IF(IBOUND(N3).EQ.0) CYCLE
            ALPHA = GNCb(1+MXADJb+IADJb,IG)
            SIGALJ = SIGALJ + ALPHA
            HD = HD + ALPHA*HNEW(N3)
          ENDDO
          HNBAR = (1.0-SIGALJ)*HNEW(N1) + HD
          IF(HNBAR.LT.BN.AND.HN.LT.BN) ICASE=0 ! NO CORRECTION NEEDED
          IF(HNBAR.GT.BN.AND.HN.GT.BN) ICASE=1 ! FULL CORRECTION NEEDED
          IF(HNBAR.LT.BN.AND.HN.GT.BN) ICASE=2 ! CORRECT HN APPLIED BC TO USE BN
          IF(HNBAR.GT.BN.AND.HN.LT.BN) ICASE=3 ! CORRECT BN APPLIED BC TO USE HN
        ENDIF
        IF(ICASE.EQ.0) THEN 
          CYCLE
        ELSE
C
C3-----------FIND LOCATION OF N2 IN ROW N1 AND ADJUST AMAT FOR ROWS N1 AND N2
          Cnm = GNCb(3+2*MXADJb,IG)   
          DO IADJb = 1,MXADJb
            Jn = GNCb(1+IADJb,IG)
            IF(IBOUND(JN).EQ.0) CYCLE
            ALPHA = GNCb(MXADJb+1+IADJb,IG)
            ATERM = ALPHA * Cnm
C-------------------------------------------------------------------------------            
C3A-------------UNSYMMETRIC IMPLEMENTATION
            IF(ISYMGNCb.EQ.0)THEN
C
              IF(ICASE.EQ.1)THEN
C3A1----------------FOR ROW N1
C3A1---------------LESSEN N1 LOCATION BY (ALPHA-1)*Cnm IN DIAGONAL OF ROW N1
                LOC_JN = IRGNCb(IADJb,IG)              
                AMAT(IA(N1)) = AMAT(IA(N1)) + ATERM
C3A2---------------PUT -ATERM IN LOCATION J OF ROW N
                AMAT(LOC_JN) = AMAT(LOC_JN) - ATERM
              ELSEIF(ICASE.EQ.2)THEN 
                AMAT(IA(N1)) = AMAT(IA(N1)) + ATERM
                RTERM = ATERM* BN
                RHS(N1) = RHS(N1) + RTERM
              ELSEIF(ICASE.EQ.3)THEN
                LOC_JN = IRGNCb(IADJb,IG)
                AMAT(LOC_JN) = AMAT(LOC_JN) - ATERM
                RTERM = ATERM* BN
                RHS(N1) = RHS(N1) - RTERM                           
              ENDIF
            ELSEIF(ISYMGNCb.EQ.1)THEN
C-------------------------------------------------------------------------------             
C4A---------------SYMMETRIC IMPLEMENTATION
              IF(ICASE.EQ.1)THEN
                RTERM = ATERM*(HNEW(N1) - HNEW(Jn))
              ELSEIF(ICASE.EQ.2)THEN
                RTERM = ATERM*(HNEW(N1) - BN)
              ELSEIF(ICASE.EQ.3)THEN
                RTERM = ATERM*(BN - HNEW(Jn)) 
              ENDIF
              RHS(N1) = RHS(N1) - RTERM
            ELSEIF(ISYMGNCb.EQ.2)THEN
C-------------------------------------------------------------------------------             
C4B---------------SYMMETRIC IMPLEMENTATION WITH ONLY DIAGONAL KEPT ON LHS
              IF(ICASE.EQ.1)THEN
                AMAT(IA(N1)) = AMAT(IA(N1)) + ATERM
                RTERM = ATERM* HNEW(Jn)
                RHS(N1) = RHS(N1) + RTERM
              ELSEIF(ICASE.EQ.2)THEN
                AMAT(IA(N1)) = AMAT(IA(N1)) + ATERM
                RTERM = ATERM * BN
                RHS(N1) = RHS(N1) + RTERM
              ELSEIF(ICASE.EQ.3)THEN
                RTERM = ATERM*(BN - HNEW(Jn))
                RHS(N1) = RHS(N1) - RTERM
              ENDIF
            ENDIF
          ENDDO
C-------------------------------------------------------------------------------           
C4C-------------CONTRIBUTION FROM BOUNDARY
          ATERM = GNCb(2+2*MXADJb,IG)*Cnm
          HB = GNCb(4+2*MXADJb,IG)
          IF(ICASE.EQ.1)THEN  
            RTERM = ATERM * (HNEW(N1) - HB)
          ELSEIF(ICASE.EQ.2)THEN
            RTERM = ATERM*(HNEW(N1) - BN)
          ELSEIF(ICASE.EQ.3)THEN
            RTERM = ATERM*(BN - HN) 
          ENDIF
          RHS(N1) = RHS(N1) - RTERM
C          
        ENDIF  
   20 CONTINUE

C
C5------RETURN
      RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE SGNCb2BCFU1BDADJ(NGNCb,GNCb,ISYMGNCb,MXADJb,MXGNCb,
     1  ADJFLUXb,BOTb,ICONSTRAINTb)
C     ******************************************************************
C     ADJUST CBC FLUX FOR GHOST NODE TERMS
C     ******************************************************************
      USE GLOBAL, ONLY:JA,IA,NODES,NEQS,IVC,JAS,ISYM,IBOUND,ICONCV,IOUT,
     1                 HNEW,AMAT,FLOWJA
      USE GWFBCFMODULE, ONLY: LAYCON
      DIMENSION GNCb(4+2*MXADJb,MXGNCb),ADJFLUXb(NGNCb),BOTb(MXGNCb)
      DOUBLE PRECISION QNJ1,QMJ1,QNJ2,QMJ2,Cnm,ATERM,ALPHA,CORRECTnm,
     *  SIGALJ,HD,HNBAR,HN,BN
C-----------------------------------------------------------------------
C
C1------DO FOR EACH GNCb NODE
      DO IG=1,NGNCb
C
C2--------GET NODE NUMBERS AND GHOST NODE PROPERTIES AND LOCATIONS
        N1 = GNCb(1,IG)
        IF(IBOUND(N1).EQ.0) CYCLE
        SIGALJ = GNCb(2+2*MXADJb,IG)
        HD = GNCb(2+2*MXADJb,IG)* GNCb(4+2*MXADJb,IG)
        DO IADJb = 1,MXADJb
          N3 = GNCb(1+IADJb,IG)
          IF(IBOUND(N3).EQ.0) CYCLE
          ALPHA = GNCb(1+MXADJb+IADJb,IG)
          SIGALJ = SIGALJ + ALPHA
          HD = HD + ALPHA*HNEW(N3)
        ENDDO
        HNBAR = (1.0-SIGALJ)*HNEW(N1) + HD
        ICASE=1
        IF(ICONSTRAINTb.EQ.1)THEN 
          HN = HNEW(N1)
          BN = BOTb(IG)
          IF(HNBAR.LT.BN.AND.HN.LT.BN) ICASE=0 ! NO CORRECTION NEEDED
          IF(HNBAR.GT.BN.AND.HN.GT.BN) ICASE=1 ! FULL CORRECTION NEEDED
          IF(HNBAR.LT.BN.AND.HN.GT.BN) ICASE=2 ! CORRECT HN APPLIED BC TO USE BN
          IF(HNBAR.GT.BN.AND.HN.LT.BN) ICASE=3 ! CORRECT BN APPLIED BC TO USE HN
        ENDIF
        IF(ICASE.EQ.0)THEN
          CYCLE
        ELSEIF(ICASE.EQ.1)THEN
          ATERM = SIGALJ * HNEW(N1) - HD
        ELSEIF(ICASE.EQ.2)THEN
          ATERM = SIGALJ * HNEW(N1) - BN
        ELSEIF(ICASE.EQ.3)THEN 
          ATERM = BN - HD
        ENDIF
C
C6-----------ADJUST FLUXES FROM FLUX ARRAY
        Cnm = GNCb(3+2*MXADJb,IG)
        CORRECTnm = ATERM * Cnm
        ADJFLUXb(IG) = CORRECTnm
      ENDDO
C
C7------RETURN
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE SGNCb2BCFU1BDADJ1(NGNCb,GNCb,IRGNCb,ISYMGNCb,MXADJb,
     &  MXGNCb,BOTb,ICONSTRAINTb)
C     ******************************************************************
C     RESET GHOST NODE CONTRIBUTIONS IN MATRIX FOR IMPLICIT GNCb
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,HNEW,TOP,BOT,IBOUND,IOUT,
     1                      AMAT,PGF,FAHL,IA,JA,JAS,ISYM,RHS,HNEW
      DIMENSION GNCb(4+2*MXADJb,MXGNCb),IRGNCb(MXADJb,MXGNCb),
     1 BOTb(MXGNCb)
      DOUBLE PRECISION ALPHA,Cnm,ATERM,ATERM1,ATERM2,RTERM,HN,BN,
     1  HNBAR,SIGALJ
C     ------------------------------------------------------------------
C---------------------------------------------------------------------------
C1------RETURN IF EXPLICIT GNC
      IF(ISYMGNCb.EQ.1) RETURN
C2------FOR EACH GHOST NODE, RESET COLUMNS N AND J (GHOST NODE CONTRIBUTORS)
C2------IN ROWS N AND M FOR FLOW BETWEEN NODES N AND M.
      DO 20 IG=1,NGNCb
C
C3------GET CELL LOCATIONS AND PROPERTIES.
        N1 = GNCb(1,IG)
        IF(IBOUND(N1).EQ.0) CYCLE
C 
C2--------EVALUAE CONSTRAINT ON BOUNDARY AND SKIP IF HEAD IS BELOW CONSTRAINT          
        ICASE = 1
        IF(ICONSTRAINTb.EQ.1)THEN
          HD = GNCb(2+2*MXADJb,IG)* GNCb(4+2*MXADJb,IG)
          SIGALJ = GNCb(2+2*MXADJb,IG)
          DO IADJb = 1,MXADJb
            N3 = GNCb(1+IADJb,IG)
            IF(IBOUND(N3).EQ.0) CYCLE
            ALPHA = GNCb(1+MXADJb+IADJb,IG)
            SIGALJ = SIGALJ + ALPHA
            HD = HD + ALPHA*HNEW(N3)
          ENDDO
          HNBAR = (1.0-SIGALJ)*HNEW(N1) + HD
          HN = HNEW(N1)
          BN = BOTb(IG)
          IF(HNBAR.LT.BN.AND.HN.LT.BN) ICASE=0 ! NO CORRECTION NEEDED
          IF(HNBAR.GT.BN.AND.HN.GT.BN) ICASE=1 ! FULL CORRECTION NEEDED
          IF(HNBAR.LT.BN.AND.HN.GT.BN) ICASE=2 ! CORRECT HN APPLIED BC TO USE BN
          IF(HNBAR.GT.BN.AND.HN.LT.BN) ICASE=3 ! CORRECT BN APPLIED BC TO USE HN          
        ENDIF
        IF(ICASE.EQ.0) CYCLE
C
C-----------ADJUST MATRIX TO PRE-GNC STATE FOR MASS BALANCE COMPUTATIONS        
        Cnm = GNCb(3+2*MXADJb,IG)          
        DO IADJb = 1,MXADJb
          N3 = GNCb(1+IADJb,IG)
          IF(IBOUND(N3).EQ.0) CYCLE
          ALPHA = GNCb(1+MXADJb+IADJb,IG)
C
C4-----------FIND LOCATION OF N2 IN ROW N1 AND ADJUST AMAT FOR ROWS N1 AND N2
          ATERM =  ALPHA * Cnm
          IF(ICASE.EQ.1)THEN 
C4A1------------FOR ROW N1
C4A1-----------LESSEN N1 LOCATION BY (ALPHA-1)*Cnm IN DIAGONAL OF ROW N1
            AMAT(IA(N1)) = AMAT(IA(N1)) - ATERM
C4A2-----------PUT -ATERM IN LOCATION J OF ROW N IF FULLY IMPLICIT
            IF(ISYMGNCb.EQ.0)THEN 
              LOC_JN = IRGNCb(IADJb,IG)
              AMAT(LOC_JN) = AMAT(LOC_JN) + ATERM
            ENDIF
          ELSEIF(ICASE.EQ.2)THEN 
            AMAT(IA(N1)) = AMAT(IA(N1)) - ATERM
          ELSEIF(ICASE.EQ.3.AND.ISYMGNCb.EQ.0)THEN
            LOC_JN = IRGNCb(IADJb,IG)
            AMAT(LOC_JN) = AMAT(LOC_JN) + ATERM
          ENDIF
        ENDDO
   20 CONTINUE

C
C5------RETURN
      RETURN
      END
C---------------------------------------------------------------------------
