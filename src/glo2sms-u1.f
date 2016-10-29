      SUBROUTINE SMS7U1AR(IN)

      USE GLOBAL, ONLY: NODES,IOUT,STRT,IBOUND,AMAT,RHS,HNEW,NJA,NEQS,
     1            NLAY,ILAYCON4,ISYMFLG,INCLN,INGNC,INGNC2,INGNCn
      USE GWFBCFMODULE, ONLY: LAYCON
      USE SMSMODULE
csp      USE GNCMODULE, ONLY:ISYMGNC
csp      USE GNC2MODULE, ONLY:ISYMGNC2
      USE GNCnMODULE, ONLY:ISYMGNCn
      USE XMDMODULE, ONLY: IACL
      USE PCGUMODULE,ONLY:PCGU7U1AR
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      INTRINSIC INT
      EXTERNAL URDCOM, URWORD, UPARLSTAL
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER IN
!     ------------------------------------------------------------------
!     LOCAL VARIABLES
!     ------------------------------------------------------------------
      INTEGER lloc, istart, istop, i, n, K, IFDPARAM, MXVL, NPP
      INTEGER IPCGUM
      CHARACTER(LEN=200) line
      REAL r, HCLOSEdum, HICLOSEdum,  thetadum, amomentdum,yo
      REAL akappadum, gammadum, BREDUCDUM,BTOLDUM,RESLIMDUM
!     LOCAL VARIABLES FOR GCG SOLVER

!     ------------------------------------------------------------------
!
C1------IDENTIFY PACKAGE AND INITIALIZE.
      WRITE(IOUT,1) IN
    1 FORMAT(1X,/1X,'SMS -- SPARSE MATRIX SOLVER PACKAGE, VERSION 7',
     1', 5/2/2005',/,9X,'INPUT READ FROM UNIT',I3)
      ALLOCATE (HCLOSE, HICLOSE,BIGCHOLD,BIGCH)
      ALLOCATE (ITER1,THETA,MXITER,LINMETH,NONMETH,IPRSMS)
      ALLOCATE (Akappa,Gamma,Amomentum,Breduc,Btol,RES_LIM,
     *  Numtrack,IBFLAG)
C
      i = 1
      Numtrack = 0
      THETA = 1.0
      Akappa = 0.0
      Gamma = 0.0
      Amomentum = 0.0
      Btol = 0
      Breduc = 0.
      RES_LIM = 0.
      IBFLAG = 0
! Check if default solver values will be used
      lloc = 1
      IFDPARAM = 0
      CALL URDCOM(In, IOUT, line)
      NPP = 0
      MXVL = 0
      CALL UPARLSTAL(IN,IOUT,LINE,NPP,MXVL)
      lloc = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SIMPLE') THEN
        IFDPARAM = 1
         WRITE(IOUT,21)
   21    FORMAT(1X,'SIMPLE OPTION:',/,
     1     1X,'DEFAULT SOLVER INPUT VALUES FOR FAST SOLUTIONS')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'MODERATE') THEN
         IFDPARAM=2
         WRITE(IOUT,23)
   23    FORMAT(1X,'MODERATE OPTION:',/,1X,'DEFAULT SOLVER',
     1         ' INPUT VALUES REFLECT MODERETELY NONLINEAR MODEL')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'COMPLEX') THEN
         IFDPARAM=3
         WRITE(IOUT,25)
   25    FORMAT(1X,'COMPLEX OPTION:',/,1X,'DEFAULT SOLVER',
     1 ' INPUT VALUES REFLECT STRONGLY NONLINEAR MODEL')
      ELSE
        BACKSPACE IN
        WRITE(IOUT,'(A)') ' ALL SOLVER INPUT DATA WILL BE READ ',
     +                     'FROM THE SOLVER INPUT FILE. '
      END IF
C2------Read nonlinear iteration parameters and linear solver selection index
      lloc = 1
      CALL URDCOM(In, Iout, line)
      CALL URWORD(line, lloc, istart, istop, 3, i, HCLOSEdum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, HICLOSEdum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, MXITER, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, ITER1, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, IPRSMS, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Nonmeth, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Linmeth, r, Iout, In)
      IF(NONMETH.NE.0)THEN
        IF ( IFDPARAM.EQ.0 ) THEN
        lloc = 1
        CALL URDCOM(In, Iout, line)
        CALL URWORD(line, lloc, istart, istop, 3, i, thetadum, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 3, i,akappadum, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 3, i, gammadum, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 3,i,amomentdum, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 2, Numtrack, r, Iout, In)
        Theta = Thetadum
        Akappa = akappadum
        Gamma = gammadum
        Amomentum = amomentdum
        IF( NUMTRACK.GT.0 ) THEN
        CALL URWORD(line, lloc, istart, istop, 3, i,  Btoldum, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 3, i,Breducdum, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 3, i,RESLIMDUM, Iout, In)
        Btol = Btoldum
        Breduc = Breducdum
        RES_LIM = RESLIMDUM
        ENDIF
        ELSE
        CALL SET_RELAX(IFDPARAM)
        END IF
      END IF
C
      HCLOSE = HCLOSEDUM
      HICLOSE = HICLOSEDUM
      IF ( Theta.LT.CLOSEZERO ) Theta = 1.0e-3
C
      ILAYCON4=0
      DO K=1,NLAY
        IF(LAYCON(K).EQ.4)THEN
          ILAYCON4=1
        ENDIF
      ENDDO
c
c      IF(ILAYCON4.NE.1.AND.INCLN.EQ.0)THEN
c        IF(NONMETH.GT.0)NONMETH = -NONMETH
c      ENDIF
C3------Echo input of nonlinear iteratin parameters and linear solver index
      WRITE(IOUT,9002) HCLOSE,HICLOSE,MXITER,ITER1,iprsms,
     * NONMETH,LINMETH
C
 9002 FORMAT(1X,'OUTER ITERATION CONVERGENCE CRITERION (HCLOSE) = ',
     &  E15.6,
     &      /1X,'INNER ITERATION CONVERGENCE CRITERION (HICLOSE) = ',
     &  E15.6,
     &      /1X,'MAXIMUM NUMBER OF OUTER ITERATIONS (MXITER)     = ',I9,
     &      /1X,'MAXIMUM NUMBER OF INNER ITERATIONS (ITER1)      = ',I9,
     &      /1X,'SOLVER PRINTOUT INDEX             (IPRSMS)      = ',I9,
     &      /1X,'NONLINEAR ITERATION METHOD    (NONLINMETH)      = ',I9,
     &      /1X,'LINEAR SOLUTION METHOD           (LINMETH)      = ',I9)
C
      IF(NONMETH.NE.0)THEN
        WRITE(IOUT,9003)THETA,AKAPPA,GAMMA,AMOMENTUM,NUMTRACK
        IF(NUMTRACK.NE.0) WRITE(IOUT,9004) BTOL,BREDUC,RES_LIM
      ENDIF
9003  FORMAT(1X,'D-B-D WEIGHT REDUCTION FACTOR      (THETA)      = ',
     &  E15.6,
     &      /1X,'D-B-D WEIGHT INCREASE INCREMENT    (KAPPA)      = ',
     &  E15.6,
     &      /1X,'D-B-D PREVIOUS HISTORY FACTOR      (GAMMA)      = ',
     &  E15.6,
     &      /1X,'MOMENTUM TERM                  (AMOMENTUM)      = ',
     &  E15.6,
     &      /1X,'MAXIMUM NUMBER OF BACKTRACKS    (NUMTRACK)      = ',I9)
9004  FORMAT(1X,'BACKTRACKING TOLERANCE FACTOR       (BTOL)      = ',
     &  E15.6,
     &      /1X,'BACKTRACKING REDUCTION FACTOR     (BREDUC)      = ',
     &  E15.6,
     &      /1X,'BACKTRACKING REIDUAL LIMIT       (RES_LIM)      = ',
     &  E15.6)
      IF(MXITER.LE.0) THEN
        WRITE(*,5)
        CALL USTOP(' ')
      ELSEIF(ITER1.LE.0) THEN
        WRITE(*,7)
        CALL USTOP(' ')
      ENDIF
    5 FORMAT(/1X,'ERROR: OUTER ITERATION NUMBER MUST BE > 0.')
    7 FORMAT(/1X,'ERROR: INNER ITERATION NUMBER MUST BE > 0.')
c
      ISYMFLG = 1
      IF ( Nonmeth.GT.0 )Then
        Write(iout,*) '***Newton Linearization will be used***'
        Write(iout,*)
        ISYMFLG = 0
      ELSEIF ( Nonmeth.EQ.0 )Then
        Write(iout,*) '***Picard Linearization will be used***'
        Write(iout,*)
      ELSEIF ( Nonmeth.LT.0 )Then
        Write(iout,*) '***Picard Linearization will be used with relaxat
     *ion***'
        Write(iout,*)
      ELSE
        Write(iout,*) '***Incorrect value for variable Nonmeth was ',
     +                'specified. Check input.***'
        Write(iout,*)
        Call USTOP('  ')
      END IF
C4------Call secondary subroutine to initialize and read linear solver parameters
      IF ( Linmeth==1 )Then
C4a-------for XMD solver
        Write(iout,*) '***XMD linear solver will be used***'
        CALL XMD7U1AR(IN,IFDPARAM)
        Write(iout,*)
        ISYMFLG = 0
        IF(IACL.EQ.0) ISYMFLG = 1
      ELSEIF ( Linmeth==2 )Then
C4b-------for pcgu solver
        Write(iout,*) '***PCGU linear solver will be used***'
        CALL PCGU7U1AR(IN, NJA, NEQS, MXITER, HICLOSE, ITER1, IPRSMS,
     +                 IFDPARAM, IPCGUM)
        Write(iout,*)
!        ISYMFLG = 1
        ISYMFLG = 0
        IF ( IPCGUM.EQ.1 ) ISYMFLG = 1
      ELSE
C4c-----Incorrect linear solver flag
        Write(iout,*) '***Incorrect value for Linear solution method ',
     +                'specified. Check input.***'
        Write(iout,*)
        Call USTOP('  ')
      END IF
csp      IF(INGNC.NE.0.)THEN
csp        IF(ISYMGNC.EQ.0.AND.ISYMFLG.EQ.1)THEN
csp          WRITE(IOUT,*) '***ISYMGNC and ISYMFLG mismatch, unsymmetric
csp     1 option selected with symmetric solver. Stopping.***'
csp          STOP
csp        ENDIF
csp      ENDIF
csp      IF(INGNC2.NE.0.)THEN
csp        IF(ISYMGNC2.EQ.0.AND.ISYMFLG.EQ.1)THEN
csp          WRITE(IOUT,*) '***ISYMGNC and ISYMFLG mismatch, unsymmetric
csp     1 option selected with symmetric solver. Stopping.***'
csp          STOP
csp        ENDIF
csp      ENDIF
      IF(INGNCn.NE.0.)THEN
        IF(ISYMGNCn.EQ.0.AND.ISYMFLG.EQ.1)THEN
          WRITE(IOUT,*) '***ISYMGNCn and ISYMFLG mismatch, unsymmetric
     1 option selected with symmetric solver. Stopping.***'
          STOP
        ENDIF
      ENDIF
C
C---------------------------------------------------------------------------------
C5-----Allocate space for nonlinear arrays and initialize
      ALLOCATE(HTEMP(NEQS))
      ALLOCATE (Hncg(MXITER),Lrch(3,MXITER))
      ALLOCATE (HncgL(MXITER),LrchL(MXITER))
      ALLOCATE (AMATFL(NJA))
      IF(IABS(NONMETH).EQ.1)THEN
        ALLOCATE (Wsave(NEQS),hchold(NEQS),DEold(NEQS))
        WSAVE = 0.
        HCHOLD = 0.
        DEold = 0.
      ENDIF
      Hncg = 0.0D0
      LRCH = 0
      HncgL = 0.0D0
      LRCHL = 0
C6------Return
      RETURN
      END
C-----------------------------------------------------------------------------
C
      SUBROUTINE GLO2SMS1AP(IOUT,KITER,ICNVG,KSTP,KPER)
C******************************************************************
C PERFORM RESIDUAL REDUCTION AND NEWTON LINEARIZATION AND
C PREPARE FOR SPARSE SOLVER, AND CHECK CONVERGENCE OF NONLINEARITIES
C******************************************************************
      USE SMSMODULE
      USE XMDMODULE
      USE GLOBAL, ONLY: NCOL,NROW,NODES,IBOUND,AMAT,HNEW,RHS,IUNSTR,
     *            AKR,AKRC,IA,JA,NJA,NJAS,NSTP,NEQS,ILAYCON4,
     *            INCLN,INGNC,INGNC2,INGNCn,DWADI,HWADI,NOVFC,IWADICLN
      USE CLN1MODULE, ONLY: NCLNNDS,HWADICC,HWADICG,DWADICC,DWADICG
      USE GWFBCFMODULE, ONLY: HWADIGW,DWADIGW
      USE GWFBASMODULE, ONLY: HNOFLO,IFRCNVG
      save itp
      double precision abigch,hdif,ahdif,ADIAG,big
C---------------------------------------------------------------------
C
C1------ADJUST MATRIX FOR GHOST NODE CONTRIBUTION IF GNC MODULE IS ON
csp      IF(INGNC.GT.0) CALL GNC2DISU1FM
csp      IF(INGNC2.GT.0) CALL GNCT2DISU1FM
      IF(INGNCn.GT.0) CALL GNCn2DISU1FM
C--------------------------------------------------------------------
C2------PERFORM RESIDUAL REDUCTION CYCLES IF REQUIRED
      IF(NUMTRACK.GT.0)THEN
        IF(KITER.EQ.1.AND.IBFLAG.EQ.0)THEN
C2A-------WRITE HEADER FOR SOLVER OUTPUT SUMMARY WITH BACKTRACKING
          WRITE(IOUT,11)
11        FORMAT(/' Outer-Iteration  Inner-Iteration  Backtracking  ',
     1    'Number of        Incoming       Outgoing  Maximum Head',
     1    'Change      Maximum Head Change'/
     1    '     Number           Count           Flag       Backtracks',
     1    7X,'Residual       Residual           Value              ',
     1    'Location')
        ENDIF
C
C2B-------CALL SUBROUTINE TO DETERMINE IF BACKTRACKING IS NEEDED
        CALL SGLO2SMS1RR(KITER,KSTP,KPER)
C2C-------RETURN TO COMPUTE FLOW EQUATION IF BACKTRACKING IS REQUIRED
        IF(IBFLAG.EQ.1) THEN
          IF(ILAYCON4.EQ.1.OR.INCLN.GT.0)THEN
            DEALLOCATE(AKRC,AKR)
          ENDIF
          IF(NOVFC.NE.1) DEALLOCATE (HWADIGW)
          IF(IWADICLN.NE.0) DEALLOCATE (HWADICC, HWADICG)
          RETURN
        ENDIF
      ELSE
C2D-------WRITE HEADER FOR SOLVER OUTPUT SUMMARY WITHOUT BACKTRACKING
        IF(KITER.EQ.1)THEN
          WRITE(IOUT,12)
12        FORMAT(/' Outer-Iteration  Inner-Iteration    Maximum Head ',
     1    'Change  Maximum Head Change'/
     1    '     Number           Count               Value',
     1    14X,'Location')
        ENDIF
      ENDIF
C-----------------------------------------------------------------------------
C2-------CALCULATE AND FILL DERIVATIVE TERMS IN JACOBIAN
C2-------FOR NEWTON METHOD FOR LAYERS WITH LAYCON = 4
C2-------THE DERIVATIVE TERMS INCLUDE DKR FOR UNCONFINED AND DWADI FOR VERTICAL FLOW CORRECTION
      IF(NONMETH.GT.0) THEN
        ALLOCATE (DKDH(NEQS),DKDHC(NJAS))
        ALLOCATE (HWADI(NJAS),DWADI(NJAS))
        DKDH = 0.0
        DKDHC = 0.0
        DWADI = 1.0
C2A-------CALL EXTERNAL SUBROUTINE TO COMPUTE DERIVATIVE TERMS FOR POROUS MEDIUM
        CALL SSMS2BCFU1DK
C2B-------CALL EXTERNAL SUBROUTINE TO COMPUTE DERIVATIVE TERMS FOR CLN
        CALL SSMS2CLN1DK
C----------------------------------------------------------------------------
C2C-------TRANSFER WADI TERMS INTO GLOBAL MATRICES
        CALL SSMS2WADITRAN
C----------------------------------------------------------------------------
C2D-------TRANSFER FLOW TERMS INTO AMATFL FOR USE IN MASS BALANCE CALCULATION
        DO J=1,NJA
          AMATFL(J) = AMAT(J)
        ENDDO
C
C2E-------FILL DERIVATIVE TERMS INTO JACOBIAN MATRIX AND ON RHS
        CALL SGLO2SMS1N4
C2F-------MODIFY DERIVATIVE TERMS AND RHS FOR GHOST NODE CONTRIBUTION
csp        IF(INGNC.GT.0) CALL GNC2DISU1N4
csp        IF(INGNC2.GT.0) CALL GNCT2DISU1N4
        IF(INGNCn.GT.0) CALL GNCn2DISU1N4
C2G-------DEALLOCATE ARRAYS
        DEALLOCATE(DKDHC, DKDH)
        DEALLOCATE(HWADI, DWADI)
        IF(NOVFC.NE.1) DEALLOCATE (DWADIGW)
        IF(IWADICLN.NE.0) DEALLOCATE (DWADICC, DWADICG)
      ELSE
C2H-------SAVE AMAT FOR USE IN MASS BALANCE CALCULATION
        DO J=1,NJA
          AMATFL(J) = AMAT(J)
        ENDDO
      ENDIF
C
      IF(ILAYCON4.EQ.1.OR.INCLN.GT.0)THEN
        DEALLOCATE(AKRC,AKR)
      ENDIF
      IF(NOVFC.NE.1) DEALLOCATE (HWADIGW)
      IF(IWADICLN.NE.0) DEALLOCATE (HWADICC, HWADICG)
C
C3-----TAKE CARE OF LOOSE ENDS FOR ALL NODES BEFORE CALL TO SOLVER
      BIG = 1.0E20
      DO N=1,NEQS
C3a-------STORE HNEW IN TEMPORARY LOCATION
        HTEMP(N) = HNEW(N)
C3b-------SET DIRICHLET BOUNDARY AND NO-FLOW CONDITION
        IF(IBOUND(N).LE.0)THEN
          AMAT(IA(N)) = 1.0
          RHS(N) = HNEW(N)
          DO JJ = IA(N)+1,IA(N+1)-1
            AMAT(JJ) = 0.
          ENDDO
clang -- Replaced ibound<=0 approach in v 1.3.00
ccb          AMAT(IA(N)) = 1.0*BIG
ccb          RHS(N) = HNEW(N)*BIG
ccb          DO JJ = IA(N)+1,IA(N+1)-1
ccb            AMAT(JJ) = AMAT(JJ) / BIG
ccb          ENDDO
        ELSE
C3c---------TAKE CARE OF ZERO ROW DIAGONAL
          ADIAG = ABS(AMAT(IA(N)))
          IF(ADIAG.LT.1.0E-15)THEN
            AMAT(IA(N)) = 1.0E06
            RHS(N) = RHS(N) + HNEW(N)*1.0E06
          ENDIF
        ENDIF
      ENDDO
C-----------------------------------------------------------------------
C4------call linear solver
      ILUFLAG = 1 ! ILU PRECONDITIONING IS NEEDED
      CALL SOLVERS(IOUT,KITER,ICNVG,KSTP,KPER,AMAT,HNEW,RHS,IBOUND,
     *  HICLOSE,HNOFLO,ITP,NEQS,NJA,ILUFLAG,IN_ITER)
C
C-----------------------------------------------------------------------
C-------SET BACK ROWS FOR DIRICHLET BOUNDARY FOR MASS BALANCE
CCB      DO N=1,NEQS
CCB        IF(IBOUND(N).LE.0)THEN
CCB          DO JJ = IA(N)+1,IA(N+1)-1
CCB            AMAT(JJ) = AMAT(JJ) * BIG
CCB          ENDDO
CCB        ENDIF
CCB      ENDDO
C------------------------------------------------------------
C5------CHECK OUTER ITERATION CONVERGENCE
      NB=1
      ICNVG=0
      BIGCH=0.0
      ABIGCH=0.0
      DO N=1,NODES
        IF(IBOUND(N).EQ.0) CYCLE
        HDIF=HNEW(N)-HTEMP(N)
        AHDIF=ABS(HDIF)
        IF(AHDIF.GE.ABIGCH)THEN
          BIGCH= HDIF
          ABIGCH= AHDIF
          NB = N
        ENDIF
      ENDDO
C
      IF(ABIGCH.LE.HCLOSE) ICNVG=1
C
C5a------STORE MAXIMUM CHANGE VALUE AND LOCATION
      HNCG(KITER) = BIGCH
C
      IF(IUNSTR.EQ.0)THEN !GET LAYER, ROW AND COLUMN FOR STRUCTURED GRID
        KLAYER = (NB-1) / (NCOL*NROW) + 1
        IJ = NB - (KLAYER-1)*NCOL*NROW
        IROW = (IJ-1)/NCOL + 1
        JCOLMN = IJ - (IROW-1)*NCOL
        LRCH(1,KITER) = KLAYER
        LRCH(2,KITER) = IROW
        LRCH(3,KITER) = JCOLMN
        IF(NUMTRACK.GT.0)THEN
          write(iout,20)kiter,IN_ITER,bigch,klayer,Irow,jcolmn
20        format(i9,I17,69x,g15.6,6x,3i6,2x,'lay row col')
        ELSE
           write(iout,21)kiter,IN_ITER,bigch,klayer,Irow,jcolmn
21         format(I9,I17,10X,G16.5,6X,3I6,2x,'lay row col')
        ENDIF

      ELSE
        LRCH(1,KITER) = NB
        IF(NUMTRACK.GT.0)THEN
          write(iout,22)kiter,IN_ITER,bigch,nb
22        format(i9,I17,69x,g15.6,9x,i9,11x,'GWF-node number')
        ELSE
          write(iout,23)kiter,IN_ITER,bigch,nb
23        format(I9,I17,10X,G16.5,9X,I9,11x,'GWF-node number')
        ENDIF
      ENDIF
C6------CHECK OUTER ITERATION CONVERGENCE FOR CLN-CELLS
      IF(INCLN.EQ.0) GO TO 204
      NB=1
      ICNVGL=0
      BIGCHL=0.0
      ABIGCHL=0.0
      DO N=NODES+1,NODES+NCLNNDS
        IF(IBOUND(N).EQ.0) CYCLE
        HDIF=HNEW(N)-HTEMP(N)
        AHDIF=ABS(HDIF)
        IF(AHDIF.GE.ABIGCHL)THEN
          BIGCHL= HDIF
          ABIGCHL= AHDIF
          NB = N
        ENDIF
      ENDDO
C
      IF(ABIGCHL.LE.HCLOSE) ICNVGL=1
C
C6a-----STORE MAXIMUM CHANGE VALUE AND LOCATION FOR CLN-CELLS
      HNCGL(KITER) = BIGCHL
      LRCHL(KITER) = NB - NODES
      IF(NUMTRACK.GT.0)THEN
        write(iout,24)kiter,IN_ITER,bigchl,nb-nodes
24      format(i9,I17,69x,g15.6,9x,i9,11x,'CLN-node number')
      ELSE
         write(iout,25)kiter,IN_ITER,bigchl,nb-nodes
25       format(I9,I17,10X,G16.5,9X,I9,11x,'CLN-node number')
      ENDIF
C-----NOT CONVERGED, IF EITHER IS NOT CONVERGED
      IF(ICNVG.EQ.0.OR.ICNVGL.EQ.0) ICNVG = 0
204   CONTINUE
C
C7------USE CONVERGE OPTION TO FORCE CONVERGENCE
      IF(IFRCNVG.EQ.1.AND.KITER.EQ.MXITER)THEN
        ICNVG=1
      ENDIF
C
C-----------------------------------------------------------
C8-------PERFORM UNDERRELAXATION WITH DELTA-BAR-DELTA
      IF(NONMETH.NE.0.AND.ICNVG.EQ.0) CALL GLO2SMS1UR(kiter)
C
C9------WRITE ITERATION SUMMARY FOR CONVERGED SOLUTION
      IF(ICNVG.EQ.0 .AND. KITER.NE.MXITER) GOTO 600
      IF(KSTP.EQ.1) WRITE(IOUT,1000)
 1000 FORMAT(/1X)
      WRITE(IOUT,1010) KITER,KSTP,KPER
 1010 FORMAT(1X,I5,' CALLS TO SPARSE MATRIX SOLVER PACKAGE ',
     & ' IN FLOW TIME STEP',I4,' STRESS PERIOD',I4)
C
c      IF(ICNVG.EQ.0 .OR. KSTP.EQ.NSTP(KPER))
C9A------FOR BCF NODES
       CALL SSMS2BCFU1P(HNCG,LRCH,KITER,MXITER,IOUT,IUNSTR)
C9B------FOR CLN NODES
       CALL SSMS2CLN7P(KITER,MXITER,IOUT,IUNSTR,HNCGL,LRCHL,INCLN)
C
  600 CONTINUE
C10-----RETURN
      RETURN
      END
C-----------------------------------------------------------------------------
       SUBROUTINE SOLVERS(IOUT,KITER,ICNVG,KSTP,KPER,AMAT,SOLN,RHS,IBND,
     *  STOL,VNOFLO,ITP,NEQS,NJA,ILUFLAG,IN_ITER)
C******************************************************************
C PREPARE FOR SPARSE SOLVER, AND CHECK CONVERGENCE OF NONLINEARITIES
C******************************************************************
      USE PCGUMODULE,ONLY:PCGU7U1AP
      USE SMSMODULE
      USE XMDMODULE
      USE GLOBAL, ONLY: NCOL,NROW,IUNSTR,IA,JA,NSTP,NLAY
      REAL VNOFLO
      double precision ecnvrg,rcnvrg,acnvrg,resmax, xratio, dxnorm
      DOUBLE PRECISION R_NORM_GMRES,STOL
      DOUBLE PRECISION AMAT(NJA),SOLN(NEQS),RHS(NEQS)
      INTEGER IBND(NEQS)
C---------------------------------------------------------------------
C1-------PRINT MATRIX AND RHS FOR CHECKING
      itestmat = 0
      if(itestmat.eq.1)then
        open(99,file='mat_USGs.TXT')
        WRITE(99,*)'NODE, RHS, AMAT FOLLOW'
        DO N=1,NEQS
          I1 = IA(N)
          I2 = IA(N+1)-1
          WRITE(99,66)N,RHS(N),(AMAT(I),I=I1,I2)
        ENDDO
66      FORMAT(I9,1X,G15.6,2X,10G15.6)
        CLOSE (99)
        stop
      endif
C
C-----------------------------------------------------------
C2-------CALL LINEAR SOLVER
C-----------------------------------------------------------
        IF(LINMETH.EQ.1)THEN
C
C2A---------CALL XMD SOLVER
          IF(ILUFLAG.EQ.0) GO TO 300 ! SKIP FACTORIZATION IF ONLY RHS IS UPDATED
C2A1-------- ILU FACTORIZATION
          IF(IDROPTOL.EQ.0 .OR. ILUREUSE) THEN
C2A2--------numerical factorization only for level based scheme
            call xmdnfctr(amat, rhs, ia, ja, nja, neqs, ierr)
          ELSE
C2A3--------level/drop tolerance preconditioning
            call xmdprecd(amat, rhs, epsrn, ia, ja, nja, neqs, level,
     1            ierr)
            ILUREUSE = .TRUE.
          END IF
300       CONTINUE
C
C2A4---------solve matrix
          iter = iter1
          call xmdsolv(amat, rhs, soln, stol, rrctol, ia, ja, nja, neqs,
     [             north,iter, iacl, ierr)
          IN_ITER = ITER
C--------------------------------------------------------------
        ELSEIF(LINMETH.EQ.2)THEN
C2B---------CALL PCGC SOLVER
C
          CALL PCGU7U1AP(AMAT, RHS, SOLN, IA, JA,
     &               ICNVG,KSTP,KPER,MXITER,KITER,IN_ITER,IOUT)
        ENDIF
C
C ----------------------------------------------------------------------
C3------PRINT SOLUTION FOR CHECKING
      if(itestmat.eq.1)then
        WRITE(IOUT,*)'MATRIX SOLUTION FOLLOWS'
        WRITE(IOUT,67)(n,SOLN(N),N=1,NEQS)
67    FORMAT(10(I8,G15.6))
        stop
      endif
C
C4-------Return
      RETURN
      END
C-------------------------------------------------------------------------
      SUBROUTINE SSMS2BCFU1P(CNCG,LRCH,ITP,MXITER,IOUT,IUNSTR)
C******************************************************************
C PRINT MAXIMUM HEAD CHANGES FOR ALL ITERATIONS FOR POROUS MEDIUM NODES
C******************************************************************
C
      IMPLICIT  NONE
      INTEGER   LRCH,ITP,MXITER,ITER1,IOUT,I,J,IUNSTR,INCLN,LRCHL
      DOUBLE PRECISION      CNCG
      DIMENSION CNCG(MXITER),LRCH(3,MXITER)
C---------------------------------------------------------------------------
C1-----FOR POROUS MATRIX NODES
      WRITE(IOUT,2)ITP
2     FORMAT(/1X,'TOTAL OF ',I7,'OUTER ITERATIONS')
      IF(IUNSTR.EQ.1)THEN
        WRITE(IOUT,15)
   15   FORMAT(1X,' MAXIMUM CHANGE FOR EACH ITERATION:'
     &      /1X, 5('  MAX. CHANGE        NODE')/1X,132('-'))
        WRITE(IOUT,20) (CNCG(J),LRCH(1,J),J=1,ITP)
   20   FORMAT((1X,5(G13.5,', ',I10)))
      ELSE
        WRITE(IOUT,5)
    5   FORMAT(1X,' MAXIMUM CHANGE FOR EACH ITERATION:'
     &      /1X, 5(' MAX. CHANGE   LAYER,  ROW,  COL')/1X,138('-'))
        WRITE(IOUT,10) (CNCG(J),(LRCH(I,J),I=1,3),J=1,ITP)
   10   FORMAT((1X,5(G12.4,' (',I5,',',I5,',',I5,')')))
      ENDIF
C---------------------------------------------------------------------------
C2------RETURN
      RETURN
      END
C-------------------------------------------------------------------------
      SUBROUTINE SSMS2CLN7P(ITP,MXITER,IOUT,IUNSTR,
     1  CNCGL,LRCHL,INCLN)
C******************************************************************
C PRINT MAXIMUM HEAD CHANGES FOR ALL ITERATIONS FOR CLN CELLS
C******************************************************************
C
      IMPLICIT  NONE
      INTEGER   LRCH,ITP,MXITER,ITER1,IOUT,I,J,IUNSTR,INCLN,LRCHL
      DOUBLE PRECISION      CNCGL
      DIMENSION CNCGL(MXITER),LRCHL(MXITER)
C---------------------------------------------------------------------------
C1-----FOR CLN CELLS
      IF(INCLN.EQ.0) GO TO 101
        WRITE(IOUT,35)
   35   FORMAT(/1X,' MAXIMUM CHANGE FOR CLN NODE AT EACH ITERATION:'
     &      /1X, 5('  MAX. CHANGE    CLN-CELL')/1X,132('-'))
        WRITE(IOUT,40) (CNCGL(J),LRCHL(J),J=1,ITP)
   40   FORMAT((1X,5(G13.5,', ',I10)))
101   CONTINUE
C---------------------------------------------------------------------------
C2------RETURN
      RETURN
      END
C-----------------------------------------------------------------------------
      SUBROUTINE GLO2SMS1UR(kiter)
C     ******************************************************************
C     UNDER RELAX AS PER DELTA-BAR-DELTA OR COOLEY FORMULA
C     ******************************************************************
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     -----------------------------------------------------------------
      USE GLOBAL, ONLY:Ibound, Hnew, bot, Iout, Nodes,ICONCV,
     1  NLAY,NODLAY,NEQS,INCLN,iunsat
      USE CLN1MODULE, ONLY: ACLNNDS,NCLNNDS
      USE GWFBCFMODULE, ONLY: LAYCON
      USE SMSMODULE
      IMPLICIT NONE
!     -----------------------------------------------------------------
!     ARGUMENTS
!     -----------------------------------------------------------------
      INTEGER kiter
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      DOUBLE PRECISION  ww, hsave,DELH,RELAX,RELAXOLD,ES,
     *  AES,amom,closebot,FELEV
      SAVE RELAXOLD
      INTEGER K,N,NNDLAY,NSTRT,I,kk
!     -----------------------------------------------------------------
      closebot = 0.9
      IF(ABS(NONMETH).EQ.1) THEN
C1-------OPTION FOR USING DELTA-BAR-DELTA SCHEME TO UNDER-RELAX SOLUTION FOR ALL EQUATIONS
        DO N=1,NEQS
C
C2---------COMPUTE NEWTON STEP-SIZE (DELTA H) AND INITIALIZE D-B-D PARAMETERS
          DELH = HNEW(N) - HTEMP(N)
          IF ( kiter.EQ.1 )THEN
            Wsave(N) = 1.0D0
            Hchold(N) = 1.0E-20
            DEold(N) = 0.0D0
          END IF
C
C3---------COMPUTE NEW RELAXATION TERM AS PER DELTA-BAR-DELTA
          ww = Wsave(N)
          IF ( DEold(N)*DELH.LT.0.0D0 ) THEN
C3A-----------FOR FLIP-FLOP CONDITION, DECREASE FACTOR
c             ww = -0.5* DEold(N) / DELH
c             if(ww.gt.1.0) ww = 1.0
c             if(ww.lt.1.0e-08) ww = 1.0e-08
             ww = Theta*Wsave(N)
          ELSE
C3B---------WHEN CHANGE IS OF SAME SIGN, INCREASE FACTOR
c             ww = 1.0
            ww = Wsave(N) + akappa
          END IF
          IF ( ww.GT.1.0d0 ) ww = 1.0d0
          Wsave(N) = ww
C4----------COMPUTE EXPONTENTIAL AVERAGE OF PAST CHANGES IN Hchold
          If(kiter.eq.1)then !this method does it right after Newton - need to do it after underrelaxation and backtracking.
            Hchold(N) = DELH
          Else
            Hchold(N) = (1-gamma) * DELH + gamma * Hchold(N)
          Endif
C5--------STORE SLOPE (CHANGE) TERM FOR NEXT ITERATION
         DEold(N) = DELH
C
C6----------COMPUTE ACCEPTED STEP-SIZE AND NEW HEAD
          amom = 0.0
          if(kiter.gt.4) amom = amomentum
          DELH = DELH * ww + amom * Hchold(N)
          Hnew(N) = HTEMP(N) + DELH
C7----------ACCOUNT FOR ICONCV=0 CONDITION FOR LAYCON=4 CASE
          IF ( ICONCV.EQ.0.AND.IUNSAT.EQ.0) THEN
            IF(N.LE.NODES)THEN  !-------FOR POROUS MEDIUM NODES
            DO K=1,NLAY
              NNDLAY = NODLAY(K)
              NSTRT = NODLAY(K-1)+1
              IF(N.GE.NSTRT.AND.N.LE.NNDLAY)THEN
               KK = K
               GO TO 11
              ENDIF
            ENDDO
11          CONTINUE
              IF ( LAYCON(KK).EQ.4 ) THEN
                IF ( Hnew(N).LT.Bot(N) ) THEN
                  hsave = Hnew(N)
                  Hnew(N) = HTEMP(N)*(1.0-closebot) + Bot(N)*closebot
                  DELH = Hnew(N) - hsave
                END IF
              ENDIF
cc---need below to be outside of the ICONCV if-check (or not at all).
cc            ELSE !-----------------------FOR CLN CELLS
cc              FELEV = ACLNNDS(I,7)
cc              IF ( Hnew(N).LT.FELEV ) THEN
cc                hsave = Hnew(N)
cc                Hnew(N) = HTEMP(N)*(1.0-closebot) + FELEV*closebot
cc                DELH = Hnew(N) - hsave
CC              END IF
            ENDIF
          ENDIF
C---------COMPUTE EXPONTENTIAL AVERAGE OF PAST CHANGES IN Hchold after correction for ICONCV=0
C          If(kiter.eq.1)then
C            Hchold(N) = DELH
C          Elseif(numtrack.eq.0)then
C            Hchold(N) = (1-gamma) * DELH + gamma * Hchold(N)
C          Endif
C
          ENDDO
C---------------------------------------------------------------------------
      ELSEIF(ABS(NONMETH).EQ.2) THEN
C8-------DO COOLEY UNDERRELAXATION
        IF(KITER.EQ.1)THEN
          RELAX = 1.0
          RELAXOLD = 1.0
          BIGCHOLD = BIGCH
        ELSE
C9---------COMPUTE RELAXATION FACTOR
          ES = BIGCH / (BIGCHOLD*RELAXOLD)
          AES = ABS(ES)
          IF(ES.LT.-1.0E0)THEN
            RELAX = 0.5/AES
          ELSE
            RELAX = (3.0+ES) / (3.0+AES)
          ENDIF
        ENDIF
        RELAXOLD = RELAX
C10---------MODIFY COOLEY TO USE EXPONENTIAL AVERAGE OF PAST CHANGES AS PER LINE BELOW
        BIGCHOLD = (1-gamma)*BIGCH  + gamma*BIGCHOLD  !this method does it right after Newton - need to do it after underrelaxation and backtracking.
C        if(numtrack.eq.0) BIGCHOLD = (1-gamma)*BIGCH  + gamma*BIGCHOLD
        IF(RELAX.LT.1.0)THEN
C11---------COMPUTE NEW HEAD AFTER UNDER-RELAXATION
          DO N = 1, NEQS
            DELH = HNEW(N) - HTEMP(N)
            HNEW(N) = HTEMP(N) + RELAX * DELH
          ENDDO
        ENDIF
C12-------ACCOUNT FOR ICONCV=0 CONDITION APPROPRIATELY
         IF ( ICONCV.EQ.0.AND.IUNSAT.EQ.0) THEN
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            IF ( LAYCON(K).EQ.4 ) THEN
              DO N=NSTRT,NNDLAY
                IF ( Hnew(N).LT.Bot(N) ) THEN
                  Hnew(N) = HTEMP(N)*(1.0-closebot) + Bot(N)*closebot
                END IF
              ENDDO
            ENDIF
          ENDDO
cc---need below to be outside of the ICONCV if-check (or not at all).
ccC---------FOR CLN CELLS
cc          IF(INCLN.EQ.0) GO TO 101
cc          DO N=1,NCLNNDS
cc              FELEV = ACLNNDS(I,7)
cc              IF ( Hnew(N).LT.FELEV ) THEN
cc                Hnew(N) = HTEMP(N)*(1.0-closebot) + FELEV*closebot
cc              END IF
cc          ENDDO
101       CONTINUE
C
        ENDIF
C---------------------------------------------------------------------------
      ENDIF
C13-----RETURN
      RETURN
      END SUBROUTINE GLO2SMS1UR
C
C-----------------------------------------------------------------------
      SUBROUTINE SSMS2BCFU1DK
C     ******************************************************************
C     COMPUTE DERIVATIVE OF FLOW TERM FOR POROUS MATRIX NODES
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,BOT,TOP,ISYM,
     1                IOUT,NODLAY,AMAT,RHS,IA,JA,JAS,ICONCV,AKRC,AKR,
     2                NODES,NEQS,NOVFC
      USE GWFBCFMODULE,ONLY:LAYCON,DWADIGW
      USE SMSMODULE, ONLY: DKDH,NONMETH,EPSILON,DKDHC
C
      DOUBLE PRECISION HD,BBOT,TTOP,THCK,ZERO,CONSTERM,FLOWTERM,SW,
     *  TERM,TOTTHICK,EKR,X,Y
C     ------------------------------------------------------------------
      ZERO=0.
C-----------------------------------------------------------------------------
C1------FOR EVERY GWF NODE IN EVERY LAYER COMPUTE THE DERIVATIVE AND STORE IN DKDH
C1------DOING THIS FOR NODES AND THEN FILLING IN CONNECTIVITY MATRIX SAVES COMPUTATIONS
C-----------------------------------------------------------------------------
      DO 100 K=1,NLAY
        IF(LAYCON(K).EQ.4) THEN
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
C
          DO 150 N=NSTRT,NNDLAY
            IF(IBOUND(N).GT.0) THEN  !DKDH IS ZERO ON CONSTANT HEAD NODE
C1A--------------CALCULATE SATURATED THICKNESS DERIVATIVE USING FORWARD DERIVATIVE FOR THE NODE.
              DKDH(N) = AKR(N)
              BBOT=BOT(N)
              TTOP=TOP(N)
              TOTTHICK = TTOP - BBOT
              HD=HNEW(N)+ EPSILON
              CALL SAT_THIK(N,HD,TOTTHICK,BBOT,SW)
              CALL KR_CAL(N,SW,EKR)
              DKDH(N) = (EKR - DKDH(N))/(EPSILON)
CSP..D1               DKDH(N) = EKR
CSP..D1               HD = HNEW(N) - EPSILON
CSP..D1               CALL SAT_THIK(N,HD,TOTTHICK,BBOT,SW)
CSP..D1               CALL KR_CAL(N,SW,EKR)
CSP..D1               DKDH(N) = (DKDH(N) - EKR)/(2.0*EPSILON)
            ENDIF
  150     CONTINUE
         ENDIF
  100 CONTINUE
C-----------------------------------------------------------------------------
C2------FILL DKDHC WITH UPSTREAM DKDH OF THE CONNECTION FOR ALL CONNECTIONS
C-----------------------------------------------------------------------------
      DO 200 K=1,NLAY
        IF(LAYCON(K).NE.4) CYCLE
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        DO N=NSTRT,NNDLAY
          IF(IBOUND(N).NE.0)THEN
C2A---------GO OVER CONNECTIONS OF NODE N AND FILL FOR UPPER SYMMETRIC PART
            DO II = IA(N)+1,IA(N+1)-1
              JJ = JA(II)
              IF(JJ.GE.N.AND.IBOUND(JJ).NE.0.AND.JJ.LE.NODES)THEN
                IIS = JAS(II)
C2B---------FIND UPSTREAM NODE AND HIGHER BOT NODE
                IUPS = N
                IF(HNEW(JJ).GT.HNEW(N)) IUPS = JJ
                IHBOT = N
                IF(BOT(JJ).GT.BOT(N)) IHBOT = JJ
C2C---------FILL DKDHC FOR CONNECTION
                INDK = 0
                IF(IUPS.EQ.IHBOT) INDK = 1
                IF(ABS(BOT(JJ)-BOT(N)).LT.0.01) INDK = 1
                IF(INDK.EQ.1)THEN
                  DKDHC(IIS) = DKDH(IUPS)
                ELSEIF(IBOUND(IUPS).GT.0) THEN  !DKDH IS ZERO ON CONSTANT HEAD NODE
                  DKDHC(IIS) = AKRC(IIS)
                  BBOT=BOT(IHBOT)
                  TTOP=TOP(IHBOT)
                  TOTTHICK = TTOP - BBOT
                  HD=HNEW(IUPS)+ EPSILON
                  CALL SAT_THIK(N,HD,TOTTHICK,BBOT,SW)
                  CALL KR_CAL(N,SW,EKR)
                  DKDHC(IIS) = (EKR - DKDHC(IIS))/(EPSILON)
CSP..D1                  HD=HNEW(IUPS)- EPSILON
CSP..D1                  CALL SAT_THIK(N,HD,TOTTHICK,BBOT,SW)
CSP..D1                  CALL KR_CAL(N,SW,EKR)
CSP..D1                  DKDHC(IIS) = (DKDHC(IIS)-EKR)/(2*EPSILON)
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
  200 CONTINUE
C-----------------------------------------------------------------------------
C3------COMPUTE DWADIGW FOR VERTICAL FLOW CORRECTION ACROSS LAYERS OF GWF DOMAIN
C-----------------------------------------------------------------------------
      IF(NOVFC.EQ.1) GO TO 300
      ALLOCATE (DWADIGW(NODES))
      DWADIGW = 1.0
C3A-----FILL DWADI TERM FOR GW DOMAIN
      DO N=1,NODES
        IF(IBOUND(N).NE.0)THEN
          X = HNEW(N) - TOP(N)
          CALL DWADIFN(X,Y)
          DWADIGW(N) = Y
        ENDIF
      ENDDO
300   CONTINUE
C
C4------RETURN
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE SSMS2CLN1DK
C     ******************************************************************
C     COMPUTE DERIVATIVE OF FLOW TERM FOR CLN NODES
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,BOT,TOP,ISYM,
     1                IOUT,NODLAY,AMAT,RHS,IA,JA,JAS,AKRC,AKR,ICONCV,
     2                NODES,NEQS,INCLN,IWADICLN
      USE CLN1MODULE, ONLY: ACLNNDS,NCLNNDS,CLNCON,NCLN,NNDCLN,
     1      ACLNGWC,NCLNGWC,IFLINCLN,DWADICC,DWADICG,
     1      ICCWADICLN,ICGWADICLN,IA_CLN,JA_CLN,IDXGLO_CLN  
            USE SMSMODULE, ONLY: DKDH,DKDHC,NONMETH,EPSILON
C
      DOUBLE PRECISION HD,BBOT,TTOP,THCK,ZERO,CONSTERM,FLOWTERM,
     *  TERM,TOTTHICK,BHBOT,PERIF,PERIW,X,Y,SILLBOT
C     ------------------------------------------------------------------
      IF(INCLN.EQ.0) RETURN
      ZERO=0.
C-----------------------------------------------------------------------------
C1------FOR EVERY CLN NODE COMPUTE THE SATURATION DERIVATIVE AND STORE IN DKDH
C1------THIS STEP HELPS SAVE ON MULTIPLE COMPUTATIONS FOR DIFFERENT CONNECTIONS
C-----------------------------------------------------------------------------
      DO 100 IFN=1,NCLNNDS
        N = ACLNNDS(IFN,1)
        IFLIN = IFLINCLN(IFN)
        IF(IFLIN.EQ.1) CYCLE
        IF(IBOUND(N).LE.0) CYCLE     !DKDH IS ZERO ON CONSTANT HEAD NODE
C1A-------CALCULATE SATURATED THICKNESS DERIVATIVE USING FORWARD DERIVATIVE FOR THE NODE.
        DKDH(N) = AKR(N)
        HD=HNEW(N)+ EPSILON
        BBOT = ACLNNDS(IFN,5)
        CALL CLN_THIK(IFN,HD,BBOT,THCK)
        DKDH(N) = (THCK - DKDH(N))/(EPSILON)
  100 CONTINUE
C-----------------------------------------------------------------------------
C2------FILL DKDHC WITH UPSTREAM DKDH OF THE CONNECTION FOR ALL CLN-CLN CONNECTIONS
C-----------------------------------------------------------------------------
C2A------LOOP OVER ALL CLN NODES 
      DO NC1 = 1,NCLNNDS
C----------loop over all connections of node NC1 
        DO II_CLN = IA_CLN(NC1)+1,IA_CLN(NC1+1)-1
          NC2 = JA_CLN(II_CLN)
          IF(NC2.GT.NC1) CYCLE
          ND1 = ACLNNDS(NC1,1)   !  NC1 + NODES
          ND2 = ACLNNDS(NC2,1)   !  NC2 + NODES
          IF(IBOUND(ND1).EQ.0.OR.IBOUND(ND2).EQ.0) CYCLE
          II = IDXGLO_CLN(II_CLN)
            IIS = JAS(II)
C2D---------FIND UPSTREAM NODE AND HIGHER BOT NODE
            IUPS = ND1
            IF(HNEW(ND2).GT.HNEW(ND1)) IUPS = ND2
            IHBOT = ND1
            BC1 = ACLNNDS(NC1,5)
            BC2 = ACLNNDS(NC2,5)
            BHBOT =BC1
            IF(BC2.GT.BC1) THEN
                IHBOT = ND2
                BHBOT = BC2
            ENDIF
C2E---------FILL DKDHC FOR CONNECTION
            INDK = 0
            IF(IUPS.EQ.IHBOT) INDK = 1
            IF(ABS(BC2-BC1).LT.0.01) INDK = 1
            IF(INDK.EQ.1)THEN
              DKDHC(IIS) = DKDH(IUPS)
            ELSEIF(IBOUND(IUPS).GT.0) THEN  !DKDH IS ZERO ON CONSTANT HEAD NODE
              IFLIN = IFLINCLN(IUPS-NODES)
              IF(IFLIN.EQ.1) CYCLE
              DKDHC(IIS) = AKRC(IIS)
              HD=HNEW(IUPS)+ EPSILON
              ICLN= IHBOT-NODES
              CALL CLN_THIK(ICLN,HD,BHBOT,THCK)
              DKDHC(IIS) = (THCK - DKDHC(IIS))/(EPSILON)
            ENDIF
C
        ENDDO
      ENDDO
C-----------------------------------------------------------------------------
C3------FILL DKDHC WITH UPSTREAM DKDH OF THE CONNECTION FOR ALL CLN-GWF CONNECTIONS
C-----------------------------------------------------------------------------
      DO IFN = 1,NCLNGWC
        IH = ACLNGWC(IFN,1)
        NH = ACLNNDS(IH,1)
        NL = ACLNGWC(IFN,2)
        IF(IBOUND(NH).EQ.0.OR.IBOUND(NL).EQ.0) CYCLE
C3A---------COMPUTE AND FILL DKDHC TERM FOR CLN-GWF CONNECTION
        DO II = IA(NL)+1,IA(NL+1)-1
          JJ = JA(II)
          IF(JJ.NE.NH) CYCLE
          IIS = JAS(II)
C3B---------FIND UPSTREAM NODE AND HIGHER BOT NODE
          IUPS = NL
          IF(HNEW(JJ).GT.HNEW(NL)) IUPS = JJ
          IHBOT = NL
          BNH = ACLNNDS(NH-NODES,5)
          BNL = BOT(NL)
          IF(BNH.GT.BNL) IHBOT = NH
          IF(ACLNNDS(IH,3).EQ.1)THEN
C3C---------FILL DKDHC FOR HORIZONTAL CLN CELL (USE UPSTREAM WETTED PERIMETER)
              BBOT = ACLNNDS(NH-NODES,5)
              IF(HNEW(IUPS).GT.(BBOT-EPSILON))THEN !OTHERWISE DKDHC IS ZERO FOR THE CONNECTION
                DKDHC(IIS) = AKRC(IIS)
                HD=HNEW(IUPS)+ EPSILON
                ICLN = NH-NODES
                IC = ACLNNDS(ICLN,2)
                CALL CLNP (IC,PERIF)
                CALL CLNPW (ICLN,HD,PERIW)
                THCK = PERIW/PERIF
                DKDHC(IIS) = (THCK - DKDHC(IIS))/(EPSILON)
              ENDIF
          ELSE
C3D---------FILL DKDHC FOR VERTICAL CLN CELL (USE UPSTREAM SATURATIONS)
            INDK = 0
            IF(IUPS.EQ.IHBOT) INDK = 1
            IF(ABS(BNH-BNL).LT.0.01) INDK = 1
            IF(INDK.EQ.1)THEN
              DKDHC(IIS) = DKDH(IUPS)
            ELSEIF(IBOUND(IUPS).GT.0) THEN  !DKDH IS ZERO ON CONSTANT HEAD NODE
              DKDHC(IIS) = AKRC(IIS)
              HD=HNEW(IUPS)+ EPSILON
              IF(IUPS.EQ.NH) THEN !CLN CELL IS UPSTREAM, GWF CELL HAS HIGHER BOT
                IFLIN = IFLINCLN(IUPS-NODES)
                IF(IFLIN.EQ.1) CYCLE
                BBOT = BOT(IHBOT)
                CALL CLN_THIK(IH,HD,BBOT,THCK)
              ELSE !GWF CELL IS UPSTREAM, CLN CELL HAS HIGHER BOT
                BBOT=ACLNNDS(NH-NODES,5)
                TTOP=TOP(NL)
                TOTTHICK = TTOP - BBOT
                CALL SAT_THIK(N,HD,TOTTHICK,BBOT,THCK)
              ENDIF
              DKDHC(IIS) = (THCK - DKDHC(IIS))/(EPSILON)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C4------FILL DWADICC TERM FOR CLN-CLN FLOW TERM WITH FLOW CORRECTION
C----------------------------------------------------------------------
      IF(IWADICLN.EQ.0) RETURN
C4A------ALLOCATE AND INITIALIZE DWADICC TO 1 (DEFAULT SLOPE=1 FOR DARCY EQUATION)
      ALLOCATE( DWADICC(NCLNNDS))
      DWADICC = 1.0
C4B------FILL DWADICC WITH WADI DH TERM
      DO I=1,NCLNNDS
        IF(ICCWADICLN(I).NE.0)THEN
          N = I + NODES
          IF(IBOUND(N).NE.0) THEN
            X = HNEW(N) - ACLNNDS(I,5)
            CALL DWADIFN(X,Y)
            DWADICC(I) = Y
          ENDIF
        ENDIF
      ENDDO
C-----------------------------------------------------------------------------
C5------FILL DWADICG TERM FOR CLN-GWF CONNECTIONS WITH D/S FLOW CORRECTION
C-----------------------------------------------------------------------------
C5A------ALLOCATE AND INITIALIZE DWADICG TO 1 (DEFAULT SLOPE=1 FOR DARCY EQUATION)
      ALLOCATE( DWADICG(NCLNGWC))
      DWADICG = 1.0
C5B------LOOP OVER ALL CLN-GWF CONNECTIONS FOR LEAKAGE CORRECTION
      DO IFN=1,NCLNGWC
        IF(ICGWADICLN(IFN).NE.0)THEN
          IH = ACLNGWC(IFN,1)
          NH = ACLNNDS(IH,1)
          NL = ACLNGWC(IFN,2)
          IF(IBOUND(NH).EQ.0.OR.IBOUND(NL).EQ.0) CYCLE
C3B---------FIND DOWNSTREAM NODE
        IDN = NL
        IF(HNEW(NH).LT.HNEW(NL)) IDN = NH
C5B------FILL THE CORRECTED DHWADI OF THE DOWNSTREAM CONNECTION TO THE CLN DOMAIN IN DWADICG
          SILLBOT = ACLNNDS(IH,5)
          IF(BOT(NL).GT.SILLBOT) SILLBOT = BOT(NL)
          X = HNEW(IDN) - SILLBOT
          CALL DWADIFN(X,Y)
          DWADICG(IFN) = Y
        ENDIF
      ENDDO
C----------------------------------------------------------------------------
C6------RETURN
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE SSMS2WADITRAN
C     ******************************************************************
C     TRANSFER NEWTON-RAPHSON WADI TERMS TO GLOBAL MATRICES FOR GWF AND CLN DOMAINS
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:IBOUND,NLAY,NODLAY,HNEW,ISYM,HWADI,DWADI,
     1                IOUT,NODLAY,AMAT,RHS,IA,JA,JAS,PGF,IVC,ICONCV,
     2                NODES,NEQS,INCLN,NOVFC,IWADICLN
      USE CLN1MODULE, ONLY: ACLNNDS,NCLNNDS,CLNCON,NCLN,NNDCLN,
     1                ACLNGWC,HWADICC,DWADICC,HWADICG,DWADICG,NCLNGWC,
     2                IA_CLN,JA_CLN,IDXGLO_CLN
      USE GWFBCFMODULE,ONLY:LAYCON,HWADIGW,DWADIGW
      USE SMSMODULE, ONLY: DKDHC,NONMETH,EPSILON
C
      DOUBLE PRECISION HD,BBOT,TTOP,THCK,ZERO,CONSTERM,FLOWTERM,
     *  TERM,TOTTHICK
C     ------------------------------------------------------------------
C1-----FILL HWADI AND DWADI FOR GROUNDWATER DOMAIN
C-----------------------------------------------------------------------
C1A------LOOP OVER ALL LAYERS
      DO 300 K=1,NLAY
      NNDLAY = NODLAY(K)
      NSTRT = NODLAY(K-1)+1
C1B-----LOOP OVER ALL NODES OF THE LAYER
      DO 220 N=NSTRT,NNDLAY
C1C-----IF CELL IS EXTERNAL (IBOUND<=0) THEN SKIP IT.
      IF(IBOUND(N).EQ.0) GO TO 220
C
C1D-----GO OVER CONNECTIONS OF CELL N TO FILL ARRAYS
      DO II = IA(N)+1,IA(N+1)-1
        JJ = JA(II)
        IF(IBOUND(JJ).EQ.0) CYCLE
        IIS = JAS(II)
C1E-------FIND IF CORRECTION IS NEEDED
        ICORRECT = 0
C1F-------FIND N, THE DOWNWARD VERTICAL NODE WHICH NEEDS CORRECTION
        IF(JJ.GT.N.AND.IVC(IIS).EQ.1) ICORRECT = 1
C1G-------SHUT OFF CORRECTION FOR NOVFC CASES OR IF CONFINED
        IF(LAYCON(K).EQ.4) ICORRECT = 0
        IF(NOVFC.EQ.1) ICORRECT = 0
        IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.2) ICORRECT = 0
C
        IDN = N
        IF(HNEW(JJ).LT.HNEW(N)) IDN = JJ
        IF(ICORRECT.EQ.1)THEN
C1H---------FILL ARRAYS WITH CORRECTED DOWNSTREAM HEADS
          HWADI(IIS) = HWADIGW(IDN)
          DWADI(IIS) = DWADIGW(IDN)
        ELSE
C1I---------FILL ARRAYS WITH OROGINAL DOWNSTREAM HEADS
          HWADI(IIS) = HNEW(IDN)
          DWADI(IIS) = 1.0
        ENDIF
      ENDDO
  220 CONTINUE
  250 CONTINUE
  300 CONTINUE
C------------------------------------------------------------------
      IF(INCLN.EQ.0) RETURN
C-----------------------------------------------------------------------
C2-------FILL HWADI AND DWADI FOR CLN-CLN D/S CONNECTIONS
C----------------------------------------------------------------------
      DO NC1 = 1,NCLNNDS
C----------loop over all connections of node NC1 
        DO II_CLN = IA_CLN(NC1)+1,IA_CLN(NC1+1)-1
          NC2 = JA_CLN(II_CLN)
          IF(NC2.GT.NC1) CYCLE
          ND1 = ACLNNDS(NC1,1)   !  NC1 + NODES
          ND2 = ACLNNDS(NC2,1)   !  NC2 + NODES
          IF(IBOUND(ND1).EQ.0.OR.IBOUND(ND2).EQ.0) CYCLE
          II = IDXGLO_CLN(II_CLN)
            IIS = JAS(II)
C2D---------FIND UPSTREAM AND DOWNSTREAM NODES
            IUPS = ND1
            IF(HNEW(ND2).GT.HNEW(ND1)) IUPS = ND2
            IDN = ND1
            IF(IUPS.EQ.ND1) IDN = ND2
C
            IF(IWADICLN.EQ.1)THEN
C2E-------------FILL ARRAYS WITH CORRECTED DOWNSTREAM HEADS OR HNEW STORED IN HWADICC
              IDN = IDN - NODES
              HWADI(IIS) = HWADICC(IDN)
              DWADI(IIS) = DWADICC(IDN)
            ELSE
C2F-------------FILL ARRAYS WITH ORIGINAL DOWNSTREAM HEADS
              HWADI(IIS) = HNEW(IDN)
              DWADI(IIS) = 1.0
             ENDIF
        ENDDO
      ENDDO
C-----------------------------------------------------------------------------
C3------FILL HWADI AND DWADI  FOR CLN-GWF CONNECTIONS (D/S ALREADY IN HWADICG AND DWADICG)
C-----------------------------------------------------------------------------
C3A------LOOP OVER ALL CLN-GWF CONNECTIONS
      DO IFN=1,NCLNGWC
        IH = ACLNGWC(IFN,1)
        NH = ACLNNDS(IH,1)
        NL = ACLNGWC(IFN,2)
        IF(IBOUND(NH).EQ.0.OR.IBOUND(NL).EQ.0) CYCLE
C
        DO II = IA(NL)+1,IA(NL+1)-1
          JJ = JA(II)
          IF(JJ.NE.NH) CYCLE
          IIS = JAS(II)
C3B---------FIND UPSTREAM AND DOWNSTREAM NODES
          IUPS = NL
          IF(HNEW(NH).GT.HNEW(NL)) IUPS = NH
          IDN = NL
          IF(IUPS.EQ.NL) IDN = NH
C
          IF(IWADICLN.EQ.1)THEN
C3C-----------FILL ARRAYS WITH CORRECTED DOWNSTREAM HEADS OR HNEW STORED IN HWADICG
            HWADI(IIS) = HWADICG(IFN)
            DWADI(IIS) = DWADICG(IFN)
          ELSE
C3D-----------FILL ARRAYS WITH ORIGINAL DOWNSTREAM HEADS
            HWADI(IIS) = HNEW(IDN)
            DWADI(IIS) = 1.0
          ENDIF
        ENDDO
      ENDDO
C----------------------------------------------------------------------------
C6------RETURN
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE SGLO2SMS1N4
C     ******************************************************************
C     COMPUTE JACOBIAN FOR ALL NODES WITH LAYCON OF 4 IF NONMETH IS > 0
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,BOT,TOP,ISYM,
     1                IOUT,NODLAY,AMAT,RHS,IA,JA,JAS,PGF,IVC,ICONCV,
     2                NODES,NEQS,INCLN,HWADI,DWADI
      USE CLN1MODULE, ONLY: ACLNNDS,NCLNNDS,CLNCON,NCLN,NNDCLN
      USE GWFBCFMODULE,ONLY:LAYCON
      USE SMSMODULE, ONLY: DKDHC,NONMETH,EPSILON
C
      DOUBLE PRECISION HD,BBOT,TTOP,THCK,ZERO,CONSTERM,FLOWTERM,
     *  TERM,TOTTHICK,FILLEDTERM,HDS
C     ------------------------------------------------------------------
      ZERO=0.
C----------------------------------------------------------------------------
C1------- FOR EACH LAYER, FILL NEWTON TERMS IN AMAT AND RHS
C----------------------------------------------------------------------------
cc      DO 101 K=1,NLAY
cc        IF(LAYCON(K).EQ.4) THEN
C
C----------------------------------------------------------------------------
C1--------COMPUTE DERIVATIVE TERM AND RHS FOR ACTIVE NODES (IBOUND>0)
cc          NNDLAY = NODLAY(K)
cc          NSTRT = NODLAY(K-1)+1
cc          DO N=NSTRT,NNDLAY
C1-------STORE NEWTON TERMS FOR ALL EQUATIONS IN JACOBIAN AND RHS
          do n=1,neqs
            IF(IBOUND(N).NE.0)THEN
C2-----------GO OVER CONNECTIONS OF NODE N AND FILL
              DO II = IA(N)+1,IA(N+1)-1
                JJ = JA(II)
CC                IF(JJ.GT.N.AND.JJ.LE.NODES.AND.IBOUND(JJ).NE.0)THEN
                IF(JJ.GT.N.AND.IBOUND(JJ).NE.0)THEN
                  IIS = JAS(II)
C3------------------VERTICAL DIRECTION WITH CONSTANT CV DOES NOT HAVE A NONLINEARITY SO SKIP
                  IF(IVC(IIS).EQ.1.AND.ICONCV.EQ.1)THEN
C ------------------DO NOTHING
                  ELSE
C4-------------------HORIZONTAL DIRECTION OR NONLINEAR VERTICAL TERM NEEDS UPDATE OF MATRIX AND RHS
C4A------------------FIND UPSTREAM AND DOWNSTREAM LOCATIONS
                    IUPS = JJ
                    IF(HNEW(JJ).LT.HNEW(N)) IUPS = N
                    IDN = N
                    IF(IUPS.EQ.N) IDN = JJ
C4B-----------------COMPUTE ADDITIONAL TERM FOR JACOBIAN
                    CONSTERM = - PGF(IIS) * (HNEW(IUPS) - HWADI(IIS))
                    FILLEDTERM = AMAT(II)
                    IPIV1 = IA(N)
                    IPIV2 = IA(JJ)
                    IF(IUPS.EQ.N)THEN
C------------------------------------------------------------------------------------------------
C4C-------------------FILL JACOBIAN FOR N BEING UPSTREAM NODE
                      HDS = HNEW(JJ)
                      ILOC = ISYM(II)
                      TERM = CONSTERM*DKDHC(IIS)
                      RHS(N) = RHS(N) + TERM* HNEW(N)
     1                       + AMAT(ILOC) * (DWADI(IIS)*HDS - HDS)
                      RHS(JJ) = RHS(JJ) - TERM* HNEW(N)
     1                       - AMAT(ILOC) * (DWADI(IIS)*HDS - HDS)
C4C1------------------FILL IN ROW OF N
                      AMAT(IPIV1) = AMAT(IPIV1) + TERM
                      IF(IBOUND(N).GT.0)THEN  ! DO NOT FILL NEWTON TERM IN OFF-DIAGONAL OF CONSTANT HEAD
                        AMAT(II) = AMAT(II) * DWADI(IIS)
                      ENDIF
C4C1------------------FILL IN ROW OF JJ
                      AMAT(IPIV2) = AMAT(IPIV2) -
     1                   (DWADI(IIS) - 1.0)*FILLEDTERM
                      IF(IBOUND(JJ).GT.0)THEN ! DO NOT FILL NEWTON TERM IN ROW OF CONSTANT HEAD
                        AMAT(ISYM(II)) =AMAT(ISYM(II)) - TERM
                      ENDIF
                    ELSE
C------------------------------------------------------------------------------------------------
C4D-------------------FILL JACOBIAN FOR JJ BEING UPSTREAM NODE
                      HDS = HNEW(N)
                      ILOC = II
                      TERM = -CONSTERM*DKDHC(IIS)
                      RHS(N) = RHS(N) + TERM*HNEW(JJ)
     1                       + AMAT(ILOC) * (DWADI(IIS)*HDS - HDS)
                      RHS(JJ) = RHS(JJ) - TERM*HNEW(JJ)
     1                       - AMAT(ILOC) * (DWADI(IIS)*HDS - HDS)
C4D1------------------FILL IN ROW OF N
                      AMAT(IPIV1) = AMAT(IPIV1) -
     1                   (DWADI(IIS) - 1.0)*FILLEDTERM
                      IF(IBOUND(N).GT.0)THEN  ! DO NOT FILL NEWTON TERM IN OFF-DIAGONAL OF CONSTANT HEAD
                        AMAT(II) = AMAT(II) + TERM
                      ENDIF
C4D2------------------FILL IN ROW OF JJ
                      AMAT(IPIV2) = AMAT(IPIV2) - TERM
                      IF(IBOUND(JJ).GT.0)THEN ! DO NOT FILL NEWTON TERM IN OFF-DIAGONAL OF CONSTANT HEAD
                        AMAT(ISYM(II)) = AMAT(ISYM(II)) * DWADI(IIS)
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDDO
cc        ENDIF
cc101   ENDDO
C
C-------------------------------------------------------------------------
C------- COMPUTE CLN-CLN NEWTON TERMS AND FILL AMAT AND RHS
CC      IF(INCLN.EQ.0) GO TO 102
CCC3A------LOOP OVER ALL CLN NODES 
CC      DO NC1 = 1,NCLNNDS
CCC----------loop over all connections of node NC1 
CC        DO II_CLN = IA_CLN(NC1)+1,IA_CLN(NC1+1)-1
CC            IF(NC2.GT.NC1) CYCLE
CC          NC2 = JA_CLN(II_CLN)
CC          ND1 = ACLNNDS(NC1,1)   !  NC1 + NODES
CC          ND2 = ACLNNDS(NC2,1)   !  NC2 + NODES
CC          IF(IBOUND(ND1).EQ.0.OR.IBOUND(ND2).EQ.0) CYCLE
CCC---------FIND LOWER ROW NUMBER FOR UPPER DIAGONAL OF PGF
CC          IF(ND2.GT.ND1)THEN
CC            NL = ND1
CC            NH = ND2
CC          ELSE
CC            NL = ND2
CC            NH = ND1
CC          ENDIF
CCC---------COMPUTE AND FILL AMAT TERM FOR CLN-CLN CONNECTION
CC          DO II = IA(NL)+1,IA(NL+1)-1
CC            JJ = JA(II)
CC            IF(JJ.NE.NH) CYCLE
CC            IUPS = JJ
CC            IF(HNEW(JJ).LT.HNEW(NL)) IUPS = NL
CC            CONSTERM = PGF(IIS) * (HNEW(JJ) - HNEW(NL))
CC            IPIV1 = IA(NL)
CC            IPIV2 = IA(JJ)
CC            IF(IUPS.EQ.NL)THEN
CC              TERM = CONSTERM*DKDH(NL)
CC              AMAT(IPIV1) = AMAT(IPIV1) + TERM
CC              AMAT(ISYM(II)) = AMAT(ISYM(II)) - TERM
CC              RHS(NL) = RHS(NL) + TERM* HNEW(NL)
CC              RHS(JJ) = RHS(JJ) - TERM* HNEW(NL)
CC            ELSE
CC              TERM = CONSTERM*DKDH(JJ)
CC              AMAT(II) = AMAT(II) + TERM
CC              AMAT(IPIV2) = AMAT(IPIV2) - TERM
CC              RHS(NL) = RHS(NL) + TERM*HNEW(JJ)
CC              RHS(JJ) = RHS(JJ) - TERM*HNEW(JJ)
CC            ENDIF
CC          ENDDO
CCC
CC        ENDDO
CC      ENDDO
CC102   CONTINUE
CCC-------------------------------------------------------------------------
CCC------- COMPUTE CLN-CLN NEWTON TERMS AND FILL AMAT AND RHS
CC      IF(INCLN.EQ.0) GO TO 103
CCC9----COMPUTE CLN-MATRIX CONDUCTANCE FROM UPSTREAM SATURATED FRACTION AND PGF
CC      DO IFN = 1,NCLNNDS
CC        NH = ACLNNDS(IFN,1)
CC        NL = ACLNNDS(IFN,2)
CC        IF(IBOUND(NH).EQ.0.OR.IBOUND(NL).EQ.0) CYCLE
CC        DO II = IA(NL)+1,IA(NL+1)-1
CC          JJ = JA(II)
CC          IF(JJ.NE.NH) CYCLE
CCC
CC          IUPS = JJ
CC          IF(HNEW(JJ).LT.HNEW(NL)) IUPS = NL
CC          CONSTERM = PGF(IIS) * (HNEW(JJ) - HNEW(NL))
CC          IPIV1 = IA(NL)
CC          IPIV2 = IA(JJ)
CC          IF(IUPS.EQ.NL)THEN
CC            TERM = CONSTERM*DKDH(NL)
CC            AMAT(IPIV1) = AMAT(IPIV1) + TERM
CC            AMAT(ISYM(II)) = AMAT(ISYM(II)) - TERM
CC            RHS(NL) = RHS(NL) + TERM* HNEW(NL)
CC            RHS(JJ) = RHS(JJ) - TERM* HNEW(NL)
CC          ELSE
CC            TERM = CONSTERM*DKDH(JJ)
CC            AMAT(II) = AMAT(II) + TERM
CC            AMAT(IPIV2) = AMAT(IPIV2) - TERM
CC            RHS(NL) = RHS(NL) + TERM*HNEW(JJ)
CC            RHS(JJ) = RHS(JJ) - TERM*HNEW(JJ)
CC          ENDIF
CC        ENDDO
CC      ENDDO
CC103   CONTINUE
C-------------------------------------------------------------------------
C
C5------RETURN.
      RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE SGLO2SMS1RR(KITER,KSTP,KPER)
C     ******************************************************************
C     COMPUTE RESIDUAL AND EVALUATE IF REDUCTION IS REQUIRED
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:HNEW,IOUT,NODES,IBOUND,NEQS
      USE SMSMODULE, ONLY: IBFLAG,BTOL,BREDUC,NUMTRACK,HCLOSE,HTEMP,
     *                     GAMMA,HCHOLD,BIGCHOLD,BIGCH,RES_LIM,NONMETH
C
      DOUBLE PRECISION RES_PREV,RES_NEW,CHMAX,DELH,ABSDELH,RESIN,
     *  HDIF,AHDIF,ABIGCH
      INTEGER IBCOUNT
      SAVE RES_PREV,RES_NEW,IBCOUNT,RESIN
C     ------------------------------------------------------------------
C
C1------AT FIRST ITERATION
      IF(KITER.EQ.1)THEN
c1A------RESET RESIDUAL REDUCTION COUNT AND FLAG
        IBCOUNT = 0
        IBFLAG = 0
C
C1B------INITIALIZE PREVIOUS RESIDUAL AND RETURN
        CALL RES_FUNC(RES_PREV)
        WRITE(IOUT,66) KITER,IBFLAG,IBCOUNT,RES_PREV,RES_PREV
        RETURN
      ENDIF
66    FORMAT(I9,17X,I13,I14,7X,G15.6,2X,G15.6)
C--------------------------------------------------------------
C2------COMPUTE CURRENT RESIDUAL
      CALL RES_FUNC(RES_NEW)
      IF(IBCOUNT.EQ.0) RESIN = RES_NEW
C
C3-----SET COUNT AND FLAG IF DESIRED RESIDUAL REDUCTION DID NOT OCCUR
      IF(RES_NEW.GT.RES_PREV * BTOL)THEN
C
C3A-------BUT NO BACKTRACKING IF MAXIMUM TRACKS ARE EXCEEDED SO RETURN
        IF(IBCOUNT.GE.NUMTRACK)THEN
          IBFLAG = 2
          WRITE(IOUT,66) KITER,IBFLAG,IBCOUNT,RESIN,RES_PREV
          IBFLAG = 0
          IBCOUNT = 0
          RES_PREV = RES_NEW
          RETURN
        ENDIF
C
C3B-------BUT NO BACKTRACKING IF RESIDUAL IS SMALLER THAN LIMIT SO RETURN
        IF(RES_NEW.LT.RES_LIM)THEN
          IBFLAG = 3
          WRITE(IOUT,66) KITER,IBFLAG,IBCOUNT,RESIN,RES_NEW
          IBFLAG = 0
          IBCOUNT = 0
          RES_PREV = RES_NEW
          RETURN
        ENDIF
C
C3C-------ALSO NO BACKTRACKING IF MAXIMUM CHANGE IS LESS THAN CLOSURE SO RETURN
        CHMAX = 0.0
        DO N=1,NEQS
          DELH = BREDUC*(HNEW(N) - HTEMP(N))
          ABSDELH = ABS(DELH)
          IF(ABSDELH.GT.CHMAX) CHMAX = ABSDELH
        ENDDO
        IF(CHMAX.LT.HCLOSE)THEN
          IBFLAG = 4
          WRITE(IOUT,66) KITER,IBFLAG,IBCOUNT,RESIN,RES_NEW
          IBFLAG = 0
          IBCOUNT = 0
          RES_PREV = RES_NEW
          RETURN
        ENDIF
C
C4-------PERFORM BACKTRACKING IF FREE OF CONSTRAINTS AND SET COUNTER AND FLAG
        DO N=1,NEQS
          DELH = BREDUC*(HNEW(N) - HTEMP(N))
          HNEW(N) = HTEMP(N) + DELH
        ENDDO
        IBCOUNT = IBCOUNT + 1
        IBFLAG = 1
C
C5------RESET COUNT AND FLAG IF DESIRED RESIDUAL REDUCTION DID OCCUR
      ELSE
        WRITE(IOUT,66) KITER,IBFLAG,IBCOUNT,RESIN,RES_NEW
        IBFLAG = 0
        IBCOUNT = 0
        RES_PREV = RES_NEW
      ENDIF
C
C6-------UPDATE HISTORY TERM HERE IF RESIDUAL REDUCTION IS PERFORMED (NUMTRACK > 0)
C6-------AND ALL THE BACKTRACKING IS DONE
      IF(IBFLAG.EQ.1) RETURN
C
cc      IF(ABS(NONMETH).EQ.1)THEN
cc        DO N = 1,NEQS
cc          DELH = HNEW(N) - HTEMP(N)
cc          Hchold(N) = (1-gamma) * DELH + gamma * Hchold(N)
cc        ENDDO
C      ELSEIF(ABS(NONMETH).EQ.2)THEN
C        BIGCH=0.0
C        ABIGCH=0.0
C        DO N = 1,NEQS
C          IF(IBOUND(N).EQ.0) CYCLE
C          HDIF=HNEW(N)-HTEMP(N)
C          AHDIF=ABS(HDIF)
C          IF(AHDIF.GE.ABIGCH)THEN
C            BIGCH= HDIF
C            ABIGCH= AHDIF
C          ENDIF
C        ENDDO
C        BIGCHOLD = (1-gamma)*BIGCH  + gamma*BIGCHOLD
cc      ENDIF
C
C7------RETURN.
      RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE RES_FUNC(RES)
C     ******************************************************************
C     COMPUTE RESIDUAL - USING MEAN SQUARE RESIDUAL
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:IBOUND,HNEW,ISYM,IOUT,NEQS,AMAT,RHS,IA,JA
      USE SMSMODULE, ONLY: IBFLAG,BTOL,BREDUC,NUMTRACK,HCLOSE,HTEMP
      DOUBLE PRECISION ROWSUM,RESIDUAL,RES
C     ------------------------------------------------------------------
C
C1------COMPUTE Q FOR ALL NODES
      RESIDUAL = 0.0
      DO N=1,NEQS
        IF(IBOUND(N).GT.0)THEN
          ROWSUM = 0.0
          DO J = IA(N),IA(N+1)-1
            JJ = JA(J)
            ROWSUM = ROWSUM + AMAT(J) * HNEW(JJ)
          ENDDO
C2----------COMPUTE MEAN SQUARE RESIDUAL FROM Q OF EACH NODE
          RESIDUAL = RESIDUAL +  (ROWSUM - RHS(N))**2
        ENDIF
      ENDDO
      RES = RESIDUAL
C3------RETURN
      RETURN
      END SUBROUTINE RES_FUNC
C
C-----------------------------------------------------------------------
      SUBROUTINE GWF2SMS7U1DA
      USE SMSMODULE
      USE PCGUMODULE,ONLY:PCGU7U1DA
C
      DEALLOCATE(HTEMP)
      DEALLOCATE (Hncg,Lrch)
      DEALLOCATE(AMATFL)
      DEALLOCATE (Akappa,Gamma,Amomentum,Breduc,Btol,Numtrack,THETA)
      DEALLOCATE (HncgL,LrchL)
      IF(IABS(NONMETH).EQ.1)THEN
        DEALLOCATE (Wsave,hchold,DEold)
      ENDIF
      DEALLOCATE (HCLOSE, HICLOSE,BIGCHOLD,BIGCH)
      IF(LINMETH.EQ.1) THEN
        CALL XMD7DA
        CALL XMDLIBDA
      ELSEIF(LINMETH.EQ.2) THEN
        CALL PCGU7U1DA
      ENDIF
      DEALLOCATE (ITER1,MXITER,LINMETH,NONMETH,IPRSMS)
      DEALLOCATE(RES_LIM)
      DEALLOCATE(IBFLAG)
C
      RETURN
      END
C
      SUBROUTINE SET_RELAX(IFDPARAM)
      USE SMSMODULE, ONLY: Akappa,Gamma,Amomentum,Breduc,Btol,Numtrack,
     +                     THETA
      INTEGER IFDPARAM
C Simple option
      SELECT CASE ( IFDPARAM )
      CASE ( 1 )
        Theta = 1.0
        Akappa = 0.0
        Gamma = 0.0
        Amomentum = 0.0
        Numtrack = 0
        Btol = 0.0
        Breduc = 0.0
        Res_lim = 0.0
C Moderate
       CASE ( 2 )
        Theta = 0.9
        Akappa = 0.0001
        Gamma = 0.0
        Amomentum = 0.0
        Numtrack = 0
        Btol = 0.0
        Breduc = 0.0
        Res_lim = 0.0
C Complex
       CASE ( 3 )
        Theta = 0.8
        Akappa = 0.0001
        Gamma = 0.0
        Amomentum = 0.0
        Numtrack = 20
        Btol = 1.05
        Breduc = 0.1
        Res_lim = 0.002
      END SELECT
      RETURN
      END
