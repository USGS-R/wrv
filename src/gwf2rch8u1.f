      MODULE GWFRCHMODULE
        INTEGER, SAVE, POINTER                 ::NRCHOP,IRCHCB,MXNDRCH
        INTEGER,SAVE, POINTER ::NPRCH,IRCHPF,INIRCH,NIRCH
        REAL,    SAVE,   DIMENSION(:),  ALLOCATABLE      ::RECH
        INTEGER, SAVE,   DIMENSION(:),  ALLOCATABLE    ::IRCH
        REAL,    SAVE,   DIMENSION(:),  ALLOCATABLE      ::RCHF
       END MODULE GWFRCHMODULE
C
      SUBROUTINE GWF2RCH8U1AR(IN,INBCT)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR RECHARGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,IFREFM,NODLAY,IUNSTR
      USE GWFRCHMODULE,ONLY:NRCHOP,IRCHCB,NPRCH,IRCHPF,RECH,IRCH,
     1  MXNDRCH,INIRCH,NIRCH,RCHF
C
      CHARACTER*200 LINE
      CHARACTER*4 PTYP
C     ------------------------------------------------------------------
C
C1-------ALLOCATE SCALAR VARIABLES.
      ALLOCATE(NRCHOP,IRCHCB,MXNDRCH)
      ALLOCATE(NPRCH,IRCHPF,INIRCH,NIRCH)
C
C2------IDENTIFY PACKAGE.
      IRCHPF=0
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'RCH -- RECHARGE PACKAGE, VERSION 7, 5/2/2005',
     1' INPUT READ FROM UNIT ',I4)
C
C3------READ NRCHOP AND IRCHCB.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARARRAL(IN,IOUT,LINE,NPRCH)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') NRCHOP,IRCHCB
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NRCHOP,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRCHCB,R,IOUT,IN)
      END IF
C
C3B------READ MXNDRCH FOR NRCHOP=2, OTHERWISE SET TO NODLAY(1).
      IF(IUNSTR.EQ.1.AND.NRCHOP.EQ.2)THEN
        READ(IN,*) MXNDRCH
      ELSE
        MXNDRCH = NODLAY(1)
      ENDIF
C
C4------CHECK TO SEE THAT OPTION IS LEGAL.
      IF(NRCHOP.LT.1.OR.NRCHOP.GT.3) THEN
        WRITE(IOUT,8) NRCHOP
    8   FORMAT(1X,'ILLEGAL RECHARGE OPTION CODE (NRCHOP = ',I5,
     &       ') -- SIMULATION ABORTING')
        CALL USTOP(' ')
      END IF
C
C5------OPTION IS LEGAL -- PRINT OPTION CODE.
      IF(NRCHOP.EQ.1) WRITE(IOUT,201)
  201 FORMAT(1X,'OPTION 1 -- RECHARGE TO TOP LAYER')
      IF(NRCHOP.EQ.2) WRITE(IOUT,202)
  202 FORMAT(1X,'OPTION 2 -- RECHARGE TO ONE SPECIFIED NODE IN EACH',
     1     ' VERTICAL COLUMN')
      IF(NRCHOP.EQ.3) WRITE(IOUT,203)
  203 FORMAT(1X,'OPTION 3 -- RECHARGE TO HIGHEST ACTIVE NODE IN',
     1     ' EACH VERTICAL COLUMN')
C
C6------IF CELL-BY-CELL FLOWS ARE TO BE SAVED, THEN PRINT UNIT NUMBER.
      IF(IRCHCB.GT.0) WRITE(IOUT,204) IRCHCB
  204 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
C
C7------ALLOCATE SPACE FOR THE RECHARGE (RECH) AND INDICATOR (IRCH)
C7------ARRAYS.
      ALLOCATE (RECH(MXNDRCH))
      ALLOCATE (IRCH(MXNDRCH))
C8------IF TRANSPORT IS ACTIVE THEN ALLOCATE ARRAY TO STORE FLUXES
      IF(INBCT.GT.0)THEN
        ALLOCATE (RCHF(MXNDRCH))
      ENDIF
C
C8------READ NAMED PARAMETERS
      WRITE(IOUT,5) NPRCH
    5 FORMAT(1X,//1X,I5,' Recharge parameters')
      IF(NPRCH.GT.0) THEN
         DO 20 K=1,NPRCH
         CALL UPARARRRP(IN,IOUT,N,0,PTYP,1,1,0)
         IF(PTYP.NE.'RCH') THEN
            WRITE(IOUT,7)
    7       FORMAT(1X,'Parameter type must be RCH')
            CALL USTOP(' ')
         END IF
   20    CONTINUE
      END IF
C
C9------RETURN
      RETURN
      END
      SUBROUTINE GWF2RCH8U1RP(IN)
C     ******************************************************************
C     READ RECHARGE DATA FOR STRESS PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,DELR,DELC,
     1  NODLAY,AREA,IUNSTR
      USE GWFRCHMODULE,ONLY:NRCHOP,NPRCH,IRCHPF,RECH,IRCH,INIRCH,NIRCH
      REAL, DIMENSION(:,:),ALLOCATABLE  ::TEMP
      INTEGER, DIMENSION(:,:),ALLOCATABLE  ::ITEMP
C
      CHARACTER*24 ANAME(2)
      CHARACTER(LEN=200) line
C
      DATA ANAME(1) /'    RECHARGE LAYER INDEX'/
      DATA ANAME(2) /'                RECHARGE'/
C     ------------------------------------------------------------------
      ALLOCATE (TEMP(NCOL,NROW))
      ALLOCATE (ITEMP(NCOL,NROW))
C2------IDENTIFY PACKAGE.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'RCH -- RECHARGE PACKAGE, VERSION 7, 5/2/2005',
     1' INPUT READ FROM UNIT ',I4)
C
C2------READ FLAGS SHOWING WHETHER DATA IS TO BE REUSED.
      lloc = 1
      CALL URDCOM(In, Iout, line)
      IF(IFREFM.EQ.0)THEN
        IF(NRCHOP.EQ.2) THEN
          READ(LINE,'(2I10)') INRECH,INIRCH
        ELSE
          READ(LINE,'(3I10)') INRECH
          INIRCH = NODLAY(1)
        ENDIF
      ELSE
        IF(NRCHOP.EQ.2) THEN
          CALL URWORD(line, lloc, istart, istop, 2, inrech, r, Iout, In)
          CALL URWORD(line, lloc, istart, istop, 2, inirch, r, Iout, In)
        ELSE
          CALL URWORD(line, lloc, istart, istop, 2, inrech, r, Iout, In)
          INIRCH = NODLAY(1)
        ENDIF
      END IF
C
      IF(INIRCH.GE.0) NIRCH = INIRCH
C
C3------TEST INRECH TO SEE HOW TO DEFINE RECH.
      IF(INRECH.LT.0) THEN
C
C3A-----INRECH<0, SO REUSE RECHARGE ARRAY FROM LAST STRESS PERIOD.
        WRITE(IOUT,3)
    3   FORMAT(1X,/1X,'REUSING RECH FROM LAST STRESS PERIOD')
      ELSE
        IF(IUNSTR.EQ.0)THEN
C
C3B-----INRECH=>0, SO READ RECHARGE RATE.
          IF(NPRCH.EQ.0) THEN
C
C3B1--------THERE ARE NO PARAMETERS, SO READ RECH USING U2DREL.
            CALL U2DREL(TEMP,ANAME(2),NROW,NCOL,0,IN,IOUT)
          ELSE
C3B2--------DEFINE RECH USING PARAMETERS.  INRECH IS THE NUMBER OF
C3B2--------PARAMETERS TO USE THIS STRESS PERIOD.
            CALL PRESET('RCH')
            WRITE(IOUT,33)
   33       FORMAT(1X,///1X,
     1      'RECH array defined by the following parameters:')
            IF(INRECH.EQ.0) THEN
              WRITE(IOUT,34)
   34         FORMAT(' ERROR: When parameters are defined for the RCH',
     &      ' Package, at least one parameter',/,' must be specified',
     &      ' each stress period -- STOP EXECUTION (GWF2RCH8U1RPLL)')
              CALL USTOP(' ')
            END IF
            CALL UPARARRSUB2(TEMP,NCOL,NROW,0,INRECH,IN,IOUT,'RCH',
     1            ANAME(2),'RCH',IRCHPF)
          END IF
          N=0
          DO I=1,NROW
          DO J=1,NCOL
            N=N+1
            RECH(N)=TEMP(J,I)
          ENDDO
          ENDDO
        ELSE ! READ RECH FOR UNSTRUCTURED GRID
C3B-------INRECH=>0, SO READ RECHARGE RATE.
          IF(NPRCH.EQ.0) THEN
C
C3B1--------THERE ARE NO PARAMETERS, SO READ RECH USING U2DREL.
            CALL U2DREL(RECH,ANAME(2),1,NIRCH,0,IN,IOUT)
          ELSE
C
C3B2--------DEFINE RECH USING PARAMETERS.  INRECH IS THE NUMBER OF
C3B2--------PARAMETERS TO USE THIS STRESS PERIOD.
            CALL PRESET('RCH')
            WRITE(IOUT,33)
            IF(INRECH.EQ.0) THEN
              WRITE(IOUT,34)
              CALL USTOP(' ')
            END IF
            CALL UPARARRSUB2(RECH,NIRCH,1,0,INRECH,IN,IOUT,'RCH',
     1            ANAME(2),'RCH',IRCHPF)
          END IF
        ENDIF
      ENDIF
C
C5------IF NRCHOP=2 THEN A LAYER INDICATOR ARRAY IS NEEDED.  TEST INIRCH
C5------TO SEE HOW TO DEFINE IRCH.
        IF(NRCHOP.EQ.2) THEN
          IF(INIRCH.LT.0) THEN
C
C5A---------INIRCH<0, SO REUSE LAYER INDICATOR ARRAY FROM LAST STRESS PERIOD.
            WRITE(IOUT,2)
    2       FORMAT(1X,/1X,'REUSING IRCH FROM LAST STRESS PERIOD')
          ELSE
C
C5B---------INIRCH=>0, SO CALL U2DINT TO READ LAYER INDICATOR ARRAY(IRCH)
            IF(IUNSTR.EQ.0)THEN
              CALL U2DINT(ITEMP,ANAME(1),NROW,NCOL,0,IN,IOUT)
              N=0
              DO 57 IR=1,NROW
              DO 57 IC=1,NCOL
                N=N+1
                IF(ITEMP(IC,IR).LT.1 .OR. ITEMP(IC,IR).GT.NLAY) THEN
                  WRITE(IOUT,56) IC,IR,ITEMP(IC,IR)
   56             FORMAT(/1X,'INVALID LAYER NUMBER IN IRCH FOR COLUMN',
     1            I4,'  ROW',I4,'  :',I4)
                 CALL USTOP(' ')
                END IF
                IRCH(N) = (ITEMP(IC,IR)-1)*NROW*NCOL + (IR-1)*NCOL + IC
   57         CONTINUE
              NIRCH=NROW*NCOL
            ELSE
              CALL U2DINT(IRCH,ANAME(1),1,NIRCH,0,IN,IOUT)
            END IF
          END IF
        ELSE ! NRCHOP IS NOT 2 SO SET TOP LAYER OF NODES IN IRCH
          DO I=1,NIRCH
            IRCH(I) = I
          ENDDO
        END IF
C
C-------IF RECHARGE IS READ THEN MULTIPLY BY AREA TO GIVE FLUX
        IF(INRECH.GE.0) THEN
C
C4--------MULTIPLY RECHARGE RATE BY CELL AREA TO GET VOLUMETRIC RATE.
          DO 50 NN=1,NIRCH
            N = IRCH(NN)
            RECH(NN)=RECH(NN)*AREA(N)
   50     CONTINUE
        END IF
C---------------------------------------------------------------
      DEALLOCATE(TEMP)
      DEALLOCATE(ITEMP)
C6------RETURN
      RETURN
      END
      SUBROUTINE GWF2RCH8U1FM(KPER)
C     ******************************************************************
C     SUBTRACT RECHARGE FROM RHS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IBOUND,RHS
      USE GWFRCHMODULE,ONLY:NRCHOP,RECH,IRCH,NIRCH
      DOUBLE PRECISION RECHFLUX
C     ------------------------------------------------------------------
C
C1------FILL RECH ON ACTIVE NODE.
      DO 10 NN=1,NIRCH
        N = IRCH(NN)
        RECHFLUX = RECH(NN)
C
C2------FIND TOP-MOST ACTIVE NODE IF NOT N
        IF(NRCHOP.EQ.3.AND.IBOUND(N).EQ.0)THEN
          CALL FIRST_ACTIVE_BELOW(N)
        ENDIF
C---------------------------------------------------------
C3------IF CELL IS VARIABLE HEAD
        IF(IBOUND(N).GT.0) then
            RHS(N)=RHS(N)-RECHFLUX
        ENDIF
   10 CONTINUE
C
C6------RETURN
      RETURN
      END
C----------------------------------------------
      subroutine FIRST_ACTIVE_BELOW(N)
C     ******************************************************************
C     FIND FIRST ACTIVE NODE BELOW NODE N
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IBOUND,IA,JA,JAS,NODLAY,IVC,IVSD,NODES
C----------------------------------------------      
      IF(IVSD.EQ.-1)THEN 
C-------VERTICALLY ADJACENT NODE BELOW IS N + NNDLAY 
        NNDLAY = NODLAY(1)    
3       CONTINUE  
        JJ = N + NNDLAY
        IF(JJ.GT.NODES) RETURN
        IF(IBOUND(JJ).NE.0)THEN ! on const head node is ok (no effect)
          N = JJ
          GO TO 2
        ELSE
          N = JJ
          GO TO 3
        ENDIF
2       CONTINUE        
      ELSE
C-------FIND VERTICALLY ADJACENT NODE BELOW 
4       CONTINUE
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IIS = JAS(II)
          IF(IVC(IIS).EQ.1.AND.JJ.GT.N)THEN !VERTICAL DIRECTION DOWN
            IF(JJ.GT.NODES) RETURN
            IF(IBOUND(JJ).NE.0)THEN ! on const head node is ok (no effect)
              N = JJ
              GO TO 5
            ELSE
              N = JJ
              GO TO 4
            ENDIF
          ENDIF
        ENDDO
5       CONTINUE
      ENDIF 
C
C------RETURN
      RETURN
      END
      SUBROUTINE GWF2RCH8U1BD(KSTP,KPER,INBCT)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR RECHARGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,BUFF,IA,JA,JAS,NODES,
     1             NODLAY,IUNSTR,IVC,hnew
      USE GWFBASMODULE,ONLY:MSUM,VBVL,VBNM,ICBCFL,DELT,PERTIM,TOTIM
      USE GWFRCHMODULE,ONLY:NRCHOP,IRCHCB,RECH,IRCH,NIRCH,RCHF
C
      DOUBLE PRECISION RATIN,RATOUT,QQ
      DOUBLE PRECISION RECHFLUX,acoef,eps,pe,hd,rch
      CHARACTER*16 TEXT
      INTEGER,ALLOCATABLE,DIMENSION(:,:) :: ITEMP
      INTEGER,ALLOCATABLE,DIMENSION(:) :: IBUFF
      DATA TEXT /'        RECHARGE'/
C     ------------------------------------------------------------------
C
C2------CLEAR THE RATE ACCUMULATORS.
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
C
C3------CLEAR THE BUFFER & SET FLAG FOR SAVING CELL-BY-CELL FLOW TERMS.
      DO 2 N=1,NODES
      BUFF(N)=ZERO
    2 CONTINUE
      IF(INBCT.GT.0)THEN
        DO N=1,NIRCH
          RCHF(N) = ZERO
        ENDDO
      ENDIF
      IBD=0
      IF(IRCHCB.GT.0) IBD=ICBCFL
      ALLOCATE(IBUFF(NIRCH)) 
C
C5------PROCESS EACH RECHARGE CELL LOCATION.
        DO 10 NN=1,NIRCH
        N = IRCH(NN)
        RECHFLUX = RECH(NN)
C---------------------------------------------------------
C-------FIND TOP-MOST ACTIVE NODE IF NOT N
        IF(NRCHOP.EQ.3.AND.IBOUND(N).EQ.0)THEN
          CALL FIRST_ACTIVE_BELOW(N)
        ENDIF
        IBUFF(NN) = N
C---------------------------------------------------------
C5A-----IF CELL IS VARIABLE HEAD, THEN DO BUDGET FOR IT.
        IF(IBOUND(N).GT.0) THEN
          QQ = RECHFLUX
          Q=QQ
C
C5B-----ADD RECH TO BUFF.
          BUFF(N)=QQ
          IF(INBCT.GT.0) RCHF(NN) = Q
C
C5C-----IF RECH POSITIVE ADD IT TO RATIN, ELSE ADD IT TO RATOUT.
          IF(Q.GE.ZERO) THEN
            RATIN=RATIN+QQ
          ELSE
            RATOUT=RATOUT-QQ
          END IF
        END IF
   10   CONTINUE
C
C8------IF CELL-BY-CELL FLOW TERMS SHOULD BE SAVED, CALL APPROPRIATE
C8------UTILITY MODULE TO WRITE THEM.
      IF(IUNSTR.EQ.0)THEN
        IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IRCHCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
        IF(IBD.EQ.2) THEN
          ALLOCATE(ITEMP(NCOL,NROW))
          N=0
          DO I=1,NROW
            DO J=1,NCOL
              N=N+1
              ITEMP(J,I)= (IBUFF(N)-1) / (NCOL*NROW) + 1
            ENDDO
          ENDDO
          CALL UBDSV3(KSTP,KPER,TEXT,IRCHCB,BUFF,ITEMP,NRCHOP,
     1                NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
          DEALLOCATE(ITEMP)
        ENDIF
      ELSE
        IF(IBD.EQ.1) CALL UBUDSVU(KSTP,KPER,TEXT,IRCHCB,BUFF,NODES,
     1                          IOUT,PERTIM,TOTIM)
        IF(IBD.EQ.2) CALL UBDSV3U(KSTP,KPER,TEXT,IRCHCB,BUFF,IBUFF,
     1           NIRCH,NRCHOP,NODES,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      ENDIF
C
C9------MOVE TOTAL RECHARGE RATE INTO VBVL FOR PRINTING BY BAS1OT.
      ROUT=RATOUT
      RIN=RATIN
      VBVL(4,MSUM)=ROUT
      VBVL(3,MSUM)=RIN
C
C10-----ADD RECHARGE FOR TIME STEP TO RECHARGE ACCUMULATOR IN VBVL.
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
C
C11-----MOVE BUDGET TERM LABELS TO VBNM FOR PRINT BY MODULE BAS_OT.
      VBNM(MSUM)=TEXT
C
C12-----INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
      DEALLOCATE(IBUFF)
C
C13-----RETURN
      RETURN
      END
C----------------------------------------------------------------
      SUBROUTINE GWF2RCH8U1DA(INBCT)
C  Deallocate RCH DATA
      USE GWFRCHMODULE
C
        DEALLOCATE(NRCHOP)
        DEALLOCATE(IRCHCB)
        DEALLOCATE(MXNDRCH)
        DEALLOCATE(NPRCH)
        DEALLOCATE(IRCHPF)
        DEALLOCATE(INIRCH)
        DEALLOCATE(NIRCH)
        DEALLOCATE(RECH)
        DEALLOCATE(IRCH)
        IF(INBCT.GT.0)THEN
        DEALLOCATE(RCHF)
        ENDIF
C
      RETURN
      END
