      SUBROUTINE DECOMP(NDIM,N,A,COND,IPVT,WORK)
C
      INTEGER NDIM,N
      REAL A(NDIM,N),COND,WORK(N)
      INTEGER IPVT(N)
C
C     �PO�PAMMA B���C��ET PA��O�EH�E BE�ECTBEHHO� MATP���
C     �OCPE�CTBOM �A�CCOBA �CK���EH�� � O�EH�BAET
C     O��C�OB�EHHOCT�  MATP���.
C
C     OHA �C�O����ETC� ��� B���C�EH�� PE�EH��
C     ��HE�H�X C�CTEM.
C
C     BXO�HA� �H�OPMA���.
C
C     NDIM -�A�B�EHHA� CTPO�HA� PA�MEPHOCT� MACC�BA,
C           CO�EP�A�E�O A.
C
C     N    -�OP��OK MATP���.
C
C     A    -MATP��A,KOTOP�� H��HO PA��O��T�.
C
C     B�XO�HA� �H�OPMA���.
C
C     A     CO�EP��T BEPXH�� TPE��O��H�� MATP��� U
C           � ���T�BA���� �EPECTAHOBK� BEPC��
C           H��HE� TPE��O��HO� MATP��� I-L,TAK�E,
C           �TO (MATP��A  �EPECTAHOBOK) *A=L*U
C
C     COND -O�EHKA O��C�OB�EHHOCT� A.
C           ��� ��HE�HO� C�CTEM� A*X=B ��MEHEH�� B A � B
C           MO��T B��BAT�  ��MEHEH�� B X,�O����E B COND PA�.
C           EC�� COND+1.0.EQ.COND, TO  A B �PE�E�AX MA��HHO�
C           TO�HOCT� �B��ETC� B�PO��EHHO� MATP��E�. COND
C           �O�A�AETC� PABH�M 1.0E+32,EC�� O�HAP��EHA TO�HA�
C           B�PO��EHHOCT�.
C
C     IPVT -BEKTOP BE����X ��EMEHTOB.
C           IPVT(K)=�H�EKC K-� BE���E� CTPOK�
C           IPVT(N)=(-1)**(��C�O �EPECTAHOBOK)
C
C     PA�O�EE �O�E. BEKTOP WORK �O��EH ��T� O��CAH �
C             BK���EH B B��OB. E�O BXO�HOE CO�EP�AH�E O���HO
C             HE �AET BA�HO� �H�OPMA���.
C
C     O�PE�E��TE�� MATP��� A MO�ET ��T� �O���EH HA B�XO�E
C     �O �OPM��E:
C          DET(A)=IPVT(N)*A(1,1)*A(2,2)*...*A(N,N).
C
      REAL EK,T,ANORM,YNORM,ZNORM
      INTEGER NM1,I,J,K,KP1,KB,KM1,M
C
      IPVT(N)=1
      IF(N.EQ.1)GO TO 80
      NM1=N-1
C
C     B���C��T� 1-HOPM� MATP��� A
C
      ANORM=0.0
      DO 10 J=1,N
        T=0.0
        DO 5 I=1,N
          T=T+ABS(A(I,J))
    5   CONTINUE
        IF(T.GT.ANORM) ANORM=T
   10 CONTINUE
C
C     �A�CCOBO �CK���EH�E C �ACT��H�M B��OPOM
C     BE���E�O ��EMEHTA
C
      DO 35 K=1,NM1
        KP1=K+1
C
C       HA�T� BE����� ��EMEHT
C
        M=K
        DO 15 I=KP1,N
          IF(ABS(A(I,K)).GT.ABS(A(M,K))) M=I
   15   CONTINUE
        IPVT(K)=M
        IF(M.NE.K)IPVT(N)=-IPVT(N)
        T=A(M,K)
        A(M,K)=A(K,K)
        A(K,K)=T
C
C       �PO��CT�T� �TOT �A�,EC�� BE����� ��EMEHT PABEH H���
C
        IF(T.EQ.0.0)GO TO 35
C
C       B���C��T� MHO��TE��
C
        DO 20 I=KP1,N
          A(I,K)=-A(I,K)/T
   20   CONTINUE
C
C       �EPECTAB��T� � �CK���AT� �O CTO���AM
C
        DO 30 J=KP1,N
          T=A(M,J)
          A(M,J)=A(K,J)
          A(K,J)=T
          IF(T.EQ.0.0)GO TO 30
          DO 25 I=KP1,N
            A(I,J)=A(I,J)+A(I,K)*T
   25     CONTINUE
   30   CONTINUE
   35 CONTINUE
C
C     COND=(1-HOPMA MATP��� A)*(O�EHKA ��� 1-HOPM� MATP���,
C     O�PATHO� K A)
C     O�EHKA �O���AETC� �OCPE�CTBOM O�HO�O �A�A METO�A
C     O�PATH�X �TEPA��� ��� HA�MEH��E�O C�H����PHO�O
C     BEKTOPA. �TO TPE��ET PE�EH�� �B�X C�CTEM �PABHEH��,
C     (TPAHC�OH�POBAHHA� ��� A) *Y=E � A*Z=Y, ��E E-BEKTOP
C     �� +1 � -1, B��PAHH�� TAK, �TO�� MAKC�M���POBAT�
C     BE����H� Y.
C     ESTIMATE=(1-HOPMA Z)/(1-HOPMA Y)
C
C     PE��T� C�CTEM� (TPAHC�OH�POBAHHA� ��� A)*Y=E
C
      DO 50 K=1,N
        T=0.0
        IF(K.EQ.1)GO TO 45
        KM1=K-1
        DO 40 I=1,KM1
          T=T+A(I,K)*WORK(I)
   40   CONTINUE
   45   EK=1.0
        IF(T.LT.0.0)EK=-1.0
        IF(A(K,K).EQ.0.0)GO TO 90
        WORK(K)=-(EK+T)/A(K,K)
   50 CONTINUE
      DO 60 KB=1,NM1
        K=N-KB
        T=WORK(K)
        KP1=K+1
        DO 55 I=KP1,N
          T=T+A(I,K)*WORK(I)
   55   CONTINUE
        WORK(K)=T
        M=IPVT(K)
        IF(M.EQ.K)GO TO 60
        T=WORK(M)
        WORK(M)=WORK(K)
        WORK(K)=T
   60 CONTINUE
C
      YNORM=0.0
      DO 65 I=1,N
        YNORM=YNORM+ABS(WORK(I))
   65 CONTINUE
C
C     PE��T� C�CTEM� A*Z=Y
C
      CALL SOLVE(NDIM,N,A,WORK,IPVT)
C
      ZNORM=0.0
      DO 70 I=1,N
        ZNORM=ZNORM+ABS(WORK(I))
   70 CONTINUE
C
C     O�EH�T� O��C�OB�EHHOCT�
C
      COND=ANORM*ZNORM/YNORM
      IF(COND.LT.1.0)COND=1.0
      RETURN
C
C     C���A� MATP��� 1*1
C
   80 COND=1.0
      IF(A(1,1).NE.0.0)RETURN
C
C     TO�HA� B�PO��EHHOCT�
C
   90 CONTINUE
      COND=1.0E+32
      RETURN
      END
