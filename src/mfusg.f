C     ******************************************************************
C     MAIN CODE FOR U.S. GEOLOGICAL SURVEY UNSTRUCTURED MODULAR MODEL --
C                              MODFLOW-USG
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C1------USE package modules.
      USE GLOBAL
      USE GWFBASMODULE
      USE GWFEVTMODULE, ONLY:NEVTOP
      USE GWFRCHMODULE, ONLY:NRCHOP
      USE GWFLAKMODULE, ONLY:NLAKESAR,THETA,STGOLD,STGNEW,VOL
c      USE GWFUZFMODULE, ONLY: IUZFBND, FINF, VKS
      USE SMSMODULE, ONLY: MXITER,IBFLAG
c      USE DE4MODULE
c      USE GMGMODULE
C
C jfisher 2016-10-20: remove dependency on 'openspec.inc', contents included here.
      CHARACTER*20 ACCESS,FORM,ACTION(2)
      DATA ACCESS/'STREAM'/
      DATA FORM/'UNFORMATTED'/
      DATA (ACTION(I),I=1,2)/'READ','READWRITE'/
C     INCLUDE 'openspec.inc'
C
C-------ASSIGN VERSION NUMBER AND DATE
      CHARACTER*40 VERSION
      CHARACTER*10 MFVNAM
      PARAMETER (VERSION='1.3.00 12/01/2015')
      PARAMETER (MFVNAM='-USG') !USG = Un-Structured Grids
C
      CHARACTER*80 HEADNG(2)
      CHARACTER*200 FNAME
      INTEGER IBDT(8)
C
      CHARACTER*4 CUNIT(NIUNIT)
      DATA CUNIT/'BCF6', 'WEL ', 'DRN ', 'RIV ', 'EVT ', '    ', 'GHB ',  !  7
     &           'RCH ', '    ', '    ', '    ', 'OC  ', 'SMS ', 'PCB ',  ! 14
     &           'BCT ', 'FHB ', 'RES ', 'STR ', 'IBS ', 'CHD ', 'HFB6',  ! 21
     &           'LAK ', 'LPF ', 'DIS ', 'DISU', 'PVAL', '    ', 'HOB ',  ! 28
     &           'CLN ', '    ', 'ZONE', 'MULT', 'DROB', 'RVOB', 'GBOB',  ! 35
     &           'GNC ', '    ', 'CHOB', 'ETS ', 'DRT ', '    ', 'GMG ',  ! 42
     &           'hyd ', 'SFR ', '    ', 'GAGE', 'LVDA', '    ', 'lmt6',  ! 49
     &           'MNW1', '    ', '    ', 'KDEP', 'SUB ', 'UZF ', 'gwm ',  ! 56
     &           'SWT ', 'PATH', '    ', '    ', '    ', '    ', '    ',  ! 63
     &           37*'    '/
C     ------------------------------------------------------------------
C
C2------WRITE BANNER TO SCREEN AND DEFINE CONSTANTS.
      WRITE (*,1) MFVNAM,VERSION
    1 FORMAT (/,34X,'MODFLOW',A,/,
     &4X,'U.S. GEOLOGICAL SURVEY MODULAR FINITE-DIFFERENCE',
     &' GROUNDWATER FLOW MODEL',/,29X,'Version ',A/)
      INUNIT = 99
C
C3------GET THE NAME OF THE NAME FILE
      CALL GETNAMFIL(FNAME)
      MAXUNIT= INUNIT
C
C4------OPEN NAME FILE.
      OPEN (UNIT=INUNIT,FILE=FNAME,STATUS='OLD',ACTION=ACTION(1))
      NC=INDEX(FNAME,' ')
      WRITE(*,490)' Using NAME file: ',FNAME(1:NC)
  490 FORMAT(A,A)
C
C5------Get current date and time, assign to IBDT, and write to screen
      CALL DATE_AND_TIME(VALUES=IBDT)
      WRITE(*,2) (IBDT(I),I=1,3),(IBDT(I),I=5,7)
    2 FORMAT(1X,'Run start date and time (yyyy/mm/dd hh:mm:ss): ',
     &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2,/)
C
C6------ALLOCATE AND READ (AR) PROCEDURE
      IGRID=1
      NSOL=1
C6A-----READ AND PREPARE BASIC ARRAYS AND DISCRETIZE DOMAIN FOR ALL PROCESSES
      CALL GLO2BAS8AR(INUNIT,CUNIT,VERSION,24,31,32,MAXUNIT,12,
     1                HEADNG,26,MFVNAM,29,27,30,36)
C6B-----READ AND PREPARE BCF AND LPF PROPERTIES AND FILL PGF ARRAY
      IF(IUNIT(1).GT.0.OR.IUNIT(23).GT.0)
     *   CALL GWF2BCFU1AR(IUNIT(1),IUNIT(22),IUNIT(23))
C6C-------READ AND PREPARE CLN PROPERTIES, KADI, IBOUND AND STARTING HEAD
C6C-------AND FILL PGF ARRAY WITH CONSTANT TERMS FOR CLN DOMAIN
      IF(IUNIT(29).GT.0) CALL CLN2BAS1AR
C--------------------------------------------------------------
C-------TRANSPORT INPUT
!      IF(IUNIT(15).GT.0) CALL GWT2BCT1AR(IUNIT(15))
!      IF(IUNIT(14).GT.0) CALL GWT2PCB1AR(IUNIT(14))
C--------------------------------------------------------------
C6E-------BOUNDARY CONDITIONS INPUT
      IF(IUNIT(2).GT.0) CALL GWF2WEL7U1AR(IUNIT(2))
      IF(IUNIT(3).GT.0) CALL GWF2DRN7U1AR(IUNIT(3))
      IF(IUNIT(4).GT.0) CALL GWF2RIV7U1AR(IUNIT(4))
      IF(IUNIT(5).GT.0) CALL GWF2EVT8U1AR(IUNIT(5),IUNIT(15))
      IF(IUNIT(7).GT.0) CALL GWF2GHB7U1AR(IUNIT(7))
      IF(IUNIT(8).GT.0) CALL GWF2RCH8U1AR(IUNIT(8),(IUNIT(15)))
      IF(IUNIT(16).GT.0) CALL GWF2FHB7U1AR(IUNIT(16))
CSP      IF(IUNIT(17).GT.0) CALL GWF2RES7U1AR(IUNIT(17),IGRID)
      IF(IUNIT(18).GT.0) CALL GWF2STR7U1AR(IUNIT(18))
CSP      IF(IUNIT(19).GT.0) CALL GWF2IBS7U1AR(IUNIT(19),IUNIT(54),IGRID)
      IF(IUNIT(20).GT.0) CALL GWF2CHD7U1AR(IUNIT(20))
      IF(IUNIT(21).GT.0) CALL GWF2HFB7U1AR(IUNIT(21))
      IF(IUNIT(44).GT.0) CALL GWF2SFR7U1AR(IUNIT(44),IUNIT(1),IUNIT(23),
     1                           IUNIT(37),IUNIT(15),NSOL,IOUTS)
CSP      IF(IUNIT(55).GT.0) CALL GWF2UZF1AR(IUNIT(55),IUNIT(1),
CSP     1                                   IUNIT(23),IUNIT(37),IGRID)
      IF(IUNIT(22).GT.0 .OR. IUNIT(44).GT.0)THEN
        CALL GWF2LAK7U1AR(IUNIT(22),IUNIT(44),IUNIT(15),IUNIT(55),NSOL)
CSP        IF(IDEALLOC_LPF.EQ.2) CALL GWF2LPFU1DA
        IDEALLOC_LPF = 1
      ENDIF
      IF(IUNIT(46).GT.0) CALL GWF2GAG7U1AR(IUNIT(46),IUNIT(44),
     1                                     IUNIT(22))
CSP      IF(IUNIT(39).GT.0) CALL GWF2ETS7U1AR(IUNIT(39),IGRID)
CSP      IF(IUNIT(40).GT.0) CALL GWF2DRT7U1AR(IUNIT(40),IGRID)
      IF(IUNIT(54).GT.0) CALL GWF2SUB7U1AR(IUNIT(54))
C---------------------------------------------------------------------------
C6F----- SOLVER INPUT
      IF(IUNIT(13).GT.0) THEN
        CALL SMS7U1AR(IUNIT(13))
      ELSE
        CALL USTOP(
     1    'Error.  Sparse Matrix Solver (SMS) Package is required.')
      ENDIF
C6G-------IAG IS NOT NEEDED FURTHER SO DEALLOCATE
      IF(INCLN.NE.0.OR.INGNC.NE.0.OR.INGNC2.NE.0.OR.INGNCn.NE.0) THEN
        DEALLOCATE(IAG)
      ENDIF
csp------may need iag for writing only GW fluxes and in original ia formats
C
CSP      IF(IUNIT(50).GT.0) CALL GWF2MNW7U1AR(IUNIT(50),IUNIT(9),
CSP     1                     IUNIT(10),0,IUNIT(13),
CSP     2                     0,IUNIT(42),FNAME,IGRID)
!csp      IF(IUNIT(57).GT.0) CALL GWF2SWT7U1AR(IUNIT(57),IGRID)
C
C  Observation allocate and read
CSP      CALL OBS2BAS7U1AR(IUNIT(28),IGRID)
CSP      IF(IUNIT(33).GT.0) CALL OBS2DRN7U1AR(IUNIT(33),IUNIT(3),IGRID)
CSP      IF(IUNIT(34).GT.0) CALL OBS2RIV7U1AR(IUNIT(34),IUNIT(4),IGRID)
CSP      IF(IUNIT(35).GT.0) CALL OBS2GHB7U1AR(IUNIT(35),IUNIT(7),IGRID)
CSP      IF(IUNIT(38).GT.0) CALL OBS2CHD7U1AR(IUNIT(38),IGRID)
C---------------------------------------------------------------------------
C7------SIMULATE EACH STRESS PERIOD.
      DO 100 KPER = 1, NPER
        KKPER = KPER
        CALL GWF2BAS8ST(KKPER)
CSP        IF(IUNIT(19).GT.0) CALL GWF2IBS7ST(KKPER,IGRID)
        IF(IUNIT(54).GT.0) CALL GWF2SUB7ST(KKPER)
!csp        IF(IUNIT(57).GT.0) CALL GWF2SWT7ST(KKPER,IGRID)
C
C7B-----READ AND PREPARE INFORMATION FOR STRESS PERIOD.
C----------READ USING PACKAGE READ AND PREPARE MODULES.
        IF(IUNIT(29).GT.0) CALL CLN1RP(IUNIT(29))
        IF(IUNIT(2).GT.0) CALL GWF2WEL7U1RP(IUNIT(2))
        IF(IUNIT(3).GT.0) CALL GWF2DRN7U1RP(IUNIT(3))
        IF(IUNIT(4).GT.0) CALL GWF2RIV7U1RP(IUNIT(4))
        IF(IUNIT(5).GT.0) CALL GWF2EVT8U1RP(IUNIT(5))
        IF(IUNIT(7).GT.0) CALL GWF2GHB7U1RP(IUNIT(7))
        IF(IUNIT(8).GT.0) CALL GWF2RCH8U1RP(IUNIT(8))
CSP        IF(IUNIT(17).GT.0) CALL GWF2RES7U1RP(IUNIT(17),IGRID)
        IF(IUNIT(18).GT.0) CALL GWF2STR7U1RP(IUNIT(18))
        IF(IUNIT(20).GT.0) CALL GWF2CHD7U1RP(IUNIT(20))
        IF(IUNIT(44).GT.0) CALL GWF2SFR7U1RP(IUNIT(44),IUNIT(15),
     1                                     IUNIT(22),KKPER,NSOL,
     2                                     IOUTS,IUNIT(1),IUNIT(23))
CSP        IF(IUNIT(55).GT.0) CALL GWF2UZF1RP(IUNIT(55),KKPER,IGRID)
        IF(IUNIT(22).GT.0) CALL GWF2LAK7U1RP(IUNIT(22),IUNIT(1),
     1               IUNIT(15),IUNIT(23),IUNIT(37),IUNIT(44),IUNIT(55),
     2               KKPER,NSOL,IOUTS)
        IF(IUNIT(46).GT.0.AND.KKPER.EQ.1) CALL GWF2GAG7U1RP(IUNIT(15),
     1             IUNIT(22),IUNIT(55),NSOL)
CSP        IF(IUNIT(39).GT.0) CALL GWF2ETS7U1RP(IUNIT(39),IGRID)
CSP        IF(IUNIT(40).GT.0) CALL GWF2DRT7U1RP(IUNIT(40),IGRID)
CSP        IF(IUNIT(50).GT.0) CALL GWF2MNW7U1RP(IUNIT(50),IUNIT(1),
CSP     1                            IUNIT(23),IUNIT(37),KKPER,IGRID)
!      IF(IUNIT(14).GT.0) CALL GWT2PCB1RP(IUNIT(14))
C
C7C-----SIMULATE EACH TIME STEP.
        DO 90 KSTP = 1, NSTP(KPER)
          KKSTP = KSTP
C
C7C1----CALCULATE TIME STEP LENGTH. SET HOLD=HNEW.
          CALL GWF2BAS7AD(KKPER,KKSTP)
C--------------------------------------------------------------------------------
C-------SKIP FLOW SOLUTION FOR S-S FLOW AND TRANSIENT TRANSPORT OR ONLY TRANSPORT
          IDOFLOW = 1
          IF(ISSFLG(KPER).EQ.1.AND.KSTP.GT.1.AND.ITRNSP.EQ.1)IDOFLOW=0
          IF(ITRNSP.EQ.2) IDOFLOW = 0
          IF(IDOFLOW.EQ.0)THEN
C-------DETERMINE WHICH TRANSPORT OUTPUT IS NEEDED.
          CALL GWF2BAS7OC(KKSTP,KKPER,ICNVG,IUNIT(12))
            GO TO 222
          ENDIF
C--------------------------------------------------------------------------------
C7C1A---PREPARE ARRAYS FOR THIS TIME STEP FOR VARIOUS TRANSIENT PACKAGES
          IF(IUNIT(20).GT.0) CALL GWF2CHD7AD(KKPER)
          IF(IUNIT(1).GT.0) CALL GWF2BCFU1AD(KKPER)
CSP          IF(IUNIT(17).GT.0) CALL GWF2RES7AD(KKSTP,KKPER,IGRID)
          IF(IUNIT(16).GT.0) CALL GWF2FHB7AD
          IF(IUNIT(22).GT.0) CALL GWF2LAK7AD(KKPER,KKSTP,IUNIT(15))
CSP          IF(IUNIT(50).GT.0) CALL GWF2MNW7AD(IUNIT(1),IUNIT(23),
CSP     1                                       IUNIT(37),IGRID)
C
C---------INDICATE IN PRINTOUT THAT SOLUTION IS FOR HEADS
          CALL UMESPR('SOLVING FOR HEAD',' ',IOUT)
          WRITE(*,25)KPER,KSTP
   25     FORMAT(' Solving:  Stress period: ',i5,4x,
     &       'Time step: ',i5,4x,'Groundwater Flow Eqn.')
C
C7C2----ITERATIVELY FORMULATE AND SOLVE THE FLOW EQUATIONS.
          DO 30 KITER = 1, MXITER
            KKITER = KITER
31          CONTINUE
C
C7C2A---FORMULATE THE FINITE DIFFERENCE EQUATIONS.
            CALL GWF2BAS7U1FM
            IF(IUNIT(1).GT.0) CALL GWF2BCFU1FM(KKITER,KKSTP,KKPER)
C-------------FORMULATE FOR CLN DOMAIN EQUATIONS AND THEIR INTERACTION WITH BCF DOMAIN
            IF(IUNIT(29).GT.0) CALL CLN1FM(KKPER)
C
C---------------------------------------------------------------------------------
C7C2B-------ADJUST AMAT FOR SECOND ORDER CORRECTION OF K, AND VARIABLE CONTRIBUTING FACTORS (IFLALPHAn=1)
C7C2B-------ON UNCONFINED GHOST NODE TERM
CSP            IF(INGNC.NE.0)THEN
CSP              CALL SGNC2BCFU1S
CSP            ENDIF
CSP            IF(INGNC2.NE.0)THEN
CSP              CALL SGNCT2BCFU1S
CSP            ENDIF
            IF(INGNCn.NE.0)THEN
              CALL SGNCn2BCFU1S
            ENDIF
C----------------------------------------------------------------------------------
C7C2C--------------FORMULATE BOUNDARIES
            IF(IUNIT(21).GT.0) CALL GWF2HFB7U1FM
            IF(IUNIT(2).GT.0) CALL GWF2WEL7U1FM
            IF(IUNIT(3).GT.0) CALL GWF2DRN7U1FM
            IF(IUNIT(4).GT.0) CALL GWF2RIV7U1FM
            IF(IUNIT(5).GT.0) THEN
              IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(0)
              CALL GWF2EVT8U1FM
              IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(1)
            END IF
            IF(IUNIT(7).GT.0) CALL GWF2GHB7U1FM
            IF(IUNIT(8).GT.0) THEN
               IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(0)
               CALL GWF2RCH8U1FM(kper)
               IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(1)
            END IF
            IF(IUNIT(16).GT.0) CALL GWF2FHB7U1FM
CSP            IF(IUNIT(17).GT.0) CALL GWF2RES7U1FM(IGRID)
            IF(IUNIT(18).GT.0) CALL GWF2STR7U1FM
CSP            IF(IUNIT(19).GT.0) CALL GWF2IBS7U1FM(KKPER,IGRID)
CSP            IF(IUNIT(39).GT.0) CALL GWF2ETS7U1FM(IGRID)
CSP            IF(IUNIT(40).GT.0) CALL GWF2DRT7U1FM(IGRID)
CSP            IF(IUNIT(55).GT.0) CALL GWF2UZF1FM(KKPER,KKSTP,
CSP     1                                 IUNIT(44),IUNIT(22),IGRID)
            IF(IUNIT(44).GT.0) CALL GWF2SFR7U1FM(KKITER,KKPER,KKSTP,
     1                              IUNIT(22),NLAKESAR,IUNIT(8))
            IF(IUNIT(22).GT.0) CALL GWF2LAK7U1FM(KKITER,KKPER,KKSTP,
     1                                     IUNIT(44),IUNIT(55))
CSP            IF(IUNIT(50).GT.0) CALL GWF2MNW7U1FM(KKITER,IUNIT(1),
CSP     1                               IUNIT(23),IUNIT(37),IGRID)
            IF(IUNIT(54).GT.0) CALL GWF2SUB7U1FM(KKPER,KKITER,IUNIT(9))
!csp            IF(IUNIT(57).GT.0) CALL GWF2SWT7U1FM(KKPER,IGRID)
C----------------------------------------------------------------------------
C7C2D---MAKE ONE CUT AT AN APPROXIMATE SOLUTION.
            IF (IUNIT(13).GT.0) THEN
              CALL GLO2SMS1AP(IOUT,KITER,ICNVG,KSTP,KPER)
              IF(IBFLAG.EQ.1) GO TO 31
            END IF
C----------------------------------------------------------------------------
C7C2E---IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.
            IF (ICNVG.EQ.1) GOTO 33
  30      CONTINUE
          KITER = MXITER
C
   33     CONTINUE
C
C7C3----DETERMINE WHICH OUTPUT IS NEEDED.
          CALL GWF2BAS7OC(KKSTP,KKPER,ICNVG,IUNIT(12))
C
C-----------------------------------------------------------------------------
C7C4-------RESET AMAT TO PRE-GHOST CONDITIONS FOR ALL GNC NODES FOR MASS BALANCE
C-----------------------------------------------------------------------------
      IF(INGNCn.NE.0)THEN
        CALL SGNCn2BCFU1BDADJ1
      ENDIF
C-----------------------------------------------------------------
C
C7C4A----CALCULATE BUDGET TERMS.
          MSUM = 1
C7C4A1----COMPUTE STORAGE TERMS
          IF (IUNIT(1).GT.0) CALL GWF2BCFU1BDS(KKSTP,KKPER)
          IF (IUNIT(29).GT.0) CALL CLN1BDS(KKSTP,KKPER)
C74A2-----COMPUTE FLOW FROM CONSTANT HEAD AND CBC FLOWS
          ALLOCATE(FLOWJA(NJA))
          IF (IUNIT(1).GT.0) THEN
            CALL GWF2BCFU1BDADJ(KKSTP,KKPER)
          ENDIF
          IF (IUNIT(29).GT.0) THEN
            CALL CLN1BDADJ(KKSTP,KKPER)
          ENDIF
C---------------------------------------------------------------------
C7C4B-----ADJUST THE GNC CORRECTIONS
          IF(INGNCn.NE.0)THEN
            CALL SGNCn2BCFU1BDADJ
          ENDIF
C-----------------------------------------------------------------
C7C5----SAVE CELL-BY-CELL FLOW TERMS
          IF (IUNIT(1).GT.0) THEN
            CALL GWF2BCFU1BDCHWR(KKSTP,KKPER)
            CALL GWF2BCFU1BDADJWR(KKSTP,KKPER)
          ENDIF
          IF (IUNIT(29).GT.0) THEN
            CALL GWF2CLNU1BDCHWR(KKSTP,KKPER)
            CALL CLN1BDWR(KKSTP,KKPER)
          ENDIF
          DEALLOCATE(FLOWJA)
C--------------------------------------------------------------------------------
C7C6----CALCULATE AND SAVE BUDGET TERMS FOR BOUNDARIES
          IF(IUNIT(2).GT.0) CALL GWF2WEL7U1BD(KKSTP,KKPER)
          IF(IUNIT(3).GT.0) CALL GWF2DRN7U1BD(KKSTP,KKPER)
          IF(IUNIT(4).GT.0) CALL GWF2RIV7U1BD(KKSTP,KKPER)
C
          IF(IUNIT(5).GT.0) THEN
             IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(0)
             CALL GWF2EVT8U1BD(KKSTP,KKPER,IUNIT(15))
             IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(1)
          END IF
C
          IF(IUNIT(7).GT.0) CALL GWF2GHB7U1BD(KKSTP,KKPER)
C
          IF(IUNIT(8).GT.0) THEN
             IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(0)
             CALL GWF2RCH8U1BD(KKSTP,KKPER,(IUNIT(15)))
             IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(1)
          END IF
C
          IF(IUNIT(16).GT.0) CALL GWF2FHB7U1BD(KKSTP,KKPER)
CSP          IF(IUNIT(17).GT.0) CALL GWF2RES7U1BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(18).GT.0) CALL GWF2STR7U1BD(KKSTP,KKPER)
CSP          IF(IUNIT(19).GT.0) CALL GWF2IBS7U1BD(KKSTP,KKPER,IGRID)
CSP          IF(IUNIT(39).GT.0) CALL GWF2ETS7U1BD(KKSTP,KKPER,IGRID)
CSP          IF(IUNIT(40).GT.0) CALL GWF2DRT7U1BD(KKSTP,KKPER,IGRID)
CSP          IF(IUNIT(55).GT.0) CALL GWF2UZF1BD(KKSTP,KKPER,IUNIT(22),
CSP     1                             IGRID)
          IF(IUNIT(44).GT.0) CALL GWF2SFR7U1BD(KKSTP,KKPER,IUNIT(15),
     1                        IUNIT(22),IUNIT(46),IUNIT(55),NSOL,
     2                        IUNIT(8))
          IF(IUNIT(22).GT.0) CALL GWF2LAK7U1BD(KKPER,KKSTP,IUNIT(46),
     1                                       IUNIT(44),IUNIT(55))
CSP          IF(IUNIT(50).GT.0) CALL GWF2MNW7U1BD(NSTP(KPER),KKSTP,KKPER,
CSP     1                      IGRID)
          IF(IUNIT(54).GT.0) CALL GWF2SUB7U1BD(KKSTP,KKPER)
!csp          IF(IUNIT(57).GT.0) CALL GWF2SWT7U1BD(KKSTP,KKPER,IGRID)
C
C  Observation simulated equivalents
CSP          CALL OBS2BAS7SE(IUNIT(28),IGRID)
CSP          IF(IUNIT(33).GT.0) CALL OBS2DRN7SE(IGRID)
CSP          IF(IUNIT(34).GT.0) CALL OBS2RIV7SE(IGRID)
CSP          IF(IUNIT(35).GT.0) CALL OBS2GHB7SE(IGRID)
CSP          IF(IUNIT(38).GT.0) CALL OBS2CHD7SE(KKPER,IGRID)
C
C7C6A---PRINT AND/OR SAVE DATA.
          CALL GWF2BAS7OT(KKSTP,KKPER,ICNVG,1)
CSP          IF(IUNIT(19).GT.0) CALL GWF2IBS7OT(KKSTP,KKPER,IUNIT(19),
CSP     1                                       IGRID)
          IF(IUNIT(54).GT.0) CALL GWF2SUB7OT(KKSTP,KKPER,IUNIT(54))
!csp          IF(IUNIT(57).GT.0) CALL GWF2SWT7OT(KKSTP,KKPER,IGRID)
C
C7C6B---JUMP TO END OF PROGRAM IF CONVERGENCE WAS NOT ACHIEVED.
          IF(ICNVG.EQ.0) GO TO 110
222       CONTINUE
!          IF(ITRNSP.EQ.1.OR.ITRNSP.EQ.2)
!     *      CALL GWT2BCT1SOLVE(KITER,KSTP,KPER)
C
C-----END OF TIME STEP (KSTP) AND STRESS PERIOD (KPER) LOOPS
   90   CONTINUE
  100 CONTINUE
C
C
CSP      IF(IUNIT(50).NE.0) CALL GWF2MNW7OT(IGRID)
C
C8------END OF SIMULATION
C-------SAVE RESTART RECORDS FOR SUB PACKAGE
110   CONTINUE
C
C  Observation output
CSP      IF(IUNIT(28).GT.0) CALL OBS2BAS7OT(IUNIT(28),IGRID)
CSP      IF(IUNIT(33).GT.0) CALL OBS2DRN7OT(IGRID)
CSP      IF(IUNIT(34).GT.0) CALL OBS2RIV7OT(IGRID)
CSP      IF(IUNIT(35).GT.0) CALL OBS2GHB7OT(IGRID)
CSP      IF(IUNIT(38).GT.0) CALL OBS2CHD7OT(IGRID)
      CALL GLO1BAS6ET(IOUT,IBDT,1)
C
C9------CLOSE FILES AND DEALLOCATE MEMORY.  GWF2BAS7U1DA MUST BE CALLED
C9------LAST BECAUSE IT DEALLOCATES IUNIT.
      IF(IUNIT(1).GT.0) CALL GWF2BCFU1DA(IUNIT(23))
CSP      IF(IUNIT(23).GT.0.AND.IDEALLOC_LPF.EQ.0) CALL GWF2LPFU1DA
      IF(IUNIT(2).GT.0) CALL GWF2WEL7U1DA
      IF(IUNIT(3).GT.0) CALL GWF2DRN7U1DA
      IF(IUNIT(4).GT.0) CALL GWF2RIV7U1DA
      IF(IUNIT(5).GT.0) CALL GWF2EVT8U1DA(IUNIT(15))
      IF(IUNIT(7).GT.0) CALL GWF2GHB7U1DA
      IF(IUNIT(8).GT.0) CALL GWF2RCH8U1DA(IUNIT(15))
      IF(IUNIT(16).GT.0) CALL GWF2FHB7U1DA
CSP      IF(IUNIT(17).GT.0) CALL GWF2RES7U1DA(IGRID)
      IF(IUNIT(18).GT.0) CALL GWF2STR7U1DA
CSP      IF(IUNIT(19).GT.0) CALL GWF2IBS7U1DA(IGRID)
      IF(IUNIT(20).GT.0) CALL GWF2CHD7U1DA
      IF(IUNIT(21).GT.0) CALL GWF2HFB7U1DA
      IF(IUNIT(22).GT.0 .OR. IUNIT(44).GT.0)CALL GWF2LAK7U1DA(IUNIT(22))

CSP      IF(IUNIT(39).GT.0) CALL GWF2ETS7U1DA(IGRID)
CSP      IF(IUNIT(40).GT.0) CALL GWF2DRT7U1DA(IGRID)
CSP      IF(IUNIT(42).GT.0) CALL GMG7U1DA(IGRID)
      IF(IUNIT(44).GT.0) CALL GWF2SFR7U1DA
      IF(IUNIT(46).GT.0) CALL GWF2GAG7U1DA
CSP      IF(IUNIT(50).GT.0) CALL GWF2MNW7U1DA(IGRID)
      IF(IUNIT(54).GT.0) CALL GWF2SUB7U1DA
CSP      IF(IUNIT(55).GT.0) CALL GWF2UZF1DA(IGRID)
!csp      IF(IUNIT(57).GT.0) CALL GWF2SWT7U1DA(IGRID)
      CALL GWF2SMS7U1DA
CSP      CALL OBS2BAS7U1DA(IUNIT(28),IGRID)
CSP      IF(IUNIT(33).GT.0) CALL OBS2DRN7U1DA(IGRID)
CSP      IF(IUNIT(34).GT.0) CALL OBS2RIV7U1DA(IGRID)
CSP      IF(IUNIT(35).GT.0) CALL OBS2GHB7U1DA(IGRID)
CSP      IF(IUNIT(38).GT.0) CALL OBS2CHD7U1DA(IGRID)
       IF(INGNCn.GT.0) CALL GNCn2DISU1DA
      CALL GWF2BAS7U1DA
C
C10-----END OF PROGRAM.
      IF(ICNVG.EQ.0) THEN
        WRITE(*,*) ' Failure to converge'
      ELSE
        WRITE(*,*) ' Normal termination of simulation'
      END IF
      CALL USTOP(' ')
C
      END
      SUBROUTINE GETNAMFIL(FNAME)
C     ******************************************************************
C     GET THE NAME OF THE NAME FILE
C     ******************************************************************
C        SPECIFICATIONS:
C
C     ------------------------------------------------------------------
      CHARACTER*(*) FNAME
      CHARACTER*200 COMLIN
      LOGICAL EXISTS
C     ------------------------------------------------------------------
C
C Get name file from command line or user interaction.
        FNAME=' '
        COMLIN=' '
C *** Subroutines GETARG and GETCL are extensions to Fortran 90/95 that
C *** allow a program to retrieve command-line arguments.  To enable
C *** Modflow-2000 to read the name of a Name file from the command
C *** line, either GETARG or GETCL must be called, but not both.  As
C *** distributed, the call to GETARG is uncommented.  For compilers
C *** that support GETCL but not GETARG, comment out the call to GETARG
C *** and uncomment the call to GETCL.  The calls to both GETARG and
C *** GETCL may be commented out for compilers that do not support
C *** either extension.
        CALL GETARG(1,COMLIN)
C        CALL GETCL(COMLIN)
        ICOL = 1
        IF(COMLIN.NE.' ') THEN
          FNAME=COMLIN
        ELSE
   15     WRITE (*,*) ' Enter the name of the NAME FILE: '
          READ (*,'(A)') FNAME
          CALL URWORD(FNAME,ICOL,ISTART,ISTOP,0,N,R,0,0)
          FNAME=FNAME(ISTART:ISTOP)
          IF (FNAME.EQ.' ') GOTO 15
        ENDIF
        INQUIRE (FILE=FNAME,EXIST=EXISTS)
        IF(.NOT.EXISTS) THEN
          NC=INDEX(FNAME,' ')
          FNAME(NC:NC+3)='.nam'
          INQUIRE (FILE=FNAME,EXIST=EXISTS)
          IF(.NOT.EXISTS) THEN
            WRITE (*,480) FNAME(1:NC-1),FNAME(1:NC+3)
  480       FORMAT(1X,'Can''t find name file ',A,' or ',A)
            CALL USTOP(' ')
          ENDIF
        ENDIF
C
      RETURN
      END
      SUBROUTINE GLO1BAS6ET(IOUT,IBDT,IPRTIM)
C     ******************************************************************
C     Get end time and calculate elapsed time
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IBDT(8), IEDT(8), IDPM(12)
      DATA IDPM/31,28,31,30,31,30,31,31,30,31,30,31/ ! Days per month
      DATA NSPD/86400/  ! Seconds per day
C     ------------------------------------------------------------------
C
C     Get current date and time, assign to IEDT, and write.
      CALL DATE_AND_TIME(VALUES=IEDT)
      WRITE(*,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
 1000 FORMAT(1X,'Run end date and time (yyyy/mm/dd hh:mm:ss): ',
     &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)
      IF(IPRTIM.GT.0) THEN
        WRITE(IOUT,'(1X)')
        WRITE(IOUT,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
      END IF
C
C     Calculate elapsed time in days and seconds
      NDAYS=0
      LEAP=0
      IF (MOD(IEDT(1),4).EQ.0) LEAP = 1
      IBD = IBDT(3)            ! BEGIN DAY
      IED = IEDT(3)            ! END DAY
C     FIND DAYS
      IF (IBDT(2).NE.IEDT(2)) THEN
C       MONTHS DIFFER
        MB = IBDT(2)             ! BEGIN MONTH
        ME = IEDT(2)             ! END MONTH
        NM = ME-MB+1             ! NUMBER OF MONTHS TO LOOK AT
        IF (MB.GT.ME) NM = NM+12
        MC=MB-1
        DO 10 M=1,NM
          MC=MC+1                ! MC IS CURRENT MONTH
          IF (MC.EQ.13) MC = 1
          IF (MC.EQ.MB) THEN
            NDAYS = NDAYS+IDPM(MC)-IBD
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ELSEIF (MC.EQ.ME) THEN
            NDAYS = NDAYS+IED
          ELSE
            NDAYS = NDAYS+IDPM(MC)
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ENDIF
   10   CONTINUE
      ELSEIF (IBD.LT.IED) THEN
C       START AND END IN SAME MONTH, ONLY ACCOUNT FOR DAYS
        NDAYS = IED-IBD
      ENDIF
      ELSEC=NDAYS*NSPD
C
C     ADD OR SUBTRACT SECONDS
      ELSEC = ELSEC+(IEDT(5)-IBDT(5))*3600.0
      ELSEC = ELSEC+(IEDT(6)-IBDT(6))*60.0
      ELSEC = ELSEC+(IEDT(7)-IBDT(7))
      ELSEC = ELSEC+(IEDT(8)-IBDT(8))*0.001
C
C     CONVERT SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
      NDAYS = ELSEC/NSPD
      RSECS = MOD(ELSEC,86400.0)
      NHOURS = RSECS/3600.0
      RSECS = MOD(RSECS,3600.0)
      NMINS = RSECS/60.0
      RSECS = MOD(RSECS,60.0)
      NSECS = RSECS
      RSECS = MOD(RSECS,1.0)
      MSECS = NINT(RSECS*1000.0)
      NRSECS = NSECS
      IF (RSECS.GE.0.5) NRSECS=NRSECS+1
C
C     Write elapsed time to screen
        IF (NDAYS.GT.0) THEN
          WRITE(*,1010) NDAYS,NHOURS,NMINS,NRSECS
 1010     FORMAT(1X,'Elapsed run time: ',I3,' Days, ',I2,' Hours, ',I2,
     &      ' Minutes, ',I2,' Seconds',/)
        ELSEIF (NHOURS.GT.0) THEN
          WRITE(*,1020) NHOURS,NMINS,NRSECS
 1020     FORMAT(1X,'Elapsed run time: ',I2,' Hours, ',I2,
     &      ' Minutes, ',I2,' Seconds',/)
        ELSEIF (NMINS.GT.0) THEN
          WRITE(*,1030) NMINS,NSECS,MSECS
 1030     FORMAT(1X,'Elapsed run time: ',I2,' Minutes, ',
     &      I2,'.',I3.3,' Seconds',/)
        ELSE
          WRITE(*,1040) NSECS,MSECS
 1040     FORMAT(1X,'Elapsed run time: ',I2,'.',I3.3,' Seconds',/)
        ENDIF
C
C     Write times to file if requested
      IF(IPRTIM.GT.0) THEN
        IF (NDAYS.GT.0) THEN
          WRITE(IOUT,1010) NDAYS,NHOURS,NMINS,NRSECS
        ELSEIF (NHOURS.GT.0) THEN
          WRITE(IOUT,1020) NHOURS,NMINS,NRSECS
        ELSEIF (NMINS.GT.0) THEN
          WRITE(IOUT,1030) NMINS,NSECS,MSECS
        ELSE
          WRITE(IOUT,1040) NSECS,MSECS
        ENDIF
      ENDIF
C
      RETURN
      END
