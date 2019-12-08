      SUBROUTINE DECOMP(NDIM,N,A,COND,IPVT,WORK)
C
      INTEGER NDIM,N
      REAL A(NDIM,N),COND,WORK(N)
      INTEGER IPVT(N)
C
C     ÏPOÃPAMMA BÛ×ÈCËßET PAÇËOÆEHÈE BEÙECTBEHHOÉ MATPÈÖÛ
C     ÏOCPEÄCTBOM ÃAÓCCOBA ÈCKËÞ×EHÈß È OÖEHÈBAET
C     OÁÓCËOBËEHHOCTÜ  MATPÈÖÛ.
C
C     OHA ÈCÏOËÜÇÓETCß ÄËß BÛ×ÈCËEHÈß PEØEHÈÉ
C     ËÈHEÉHÛX CÈCTEM.
C
C     BXOÄHAß ÈHÔOPMAÖÈß.
C
C     NDIM -ÇAßBËEHHAß CTPO×HAß PAÇMEPHOCTÜ MACCÈBA,
C           COÄEPÆAÙEÃO A.
C
C     N    -ÏOPßÄOK MATPÈÖÛ.
C
C     A    -MATPÈÖA,KOTOPÓÞ HÓÆHO PAÇËOÆÈTÜ.
C
C     BÛXOÄHAß ÈHÔOPMAÖÈß.
C
C     A     COÄEPÆÈT BEPXHÞÞ TPEÓÃOËÜHÓÞ MATPÈÖÓ U
C           È Ó×ÈTÛBAÞÙÓÞ ÏEPECTAHOBKÈ BEPCÈÞ
C           HÈÆHEÉ TPEÓÃOËÜHOÉ MATPÈÖÛ I-L,TAKÈE,
C           ×TO (MATPÈÖA  ÏEPECTAHOBOK) *A=L*U
C
C     COND -OÖEHKA OÁÓCËOBËEHHOCTÈ A.
C           ÄËß ËÈHEÉHOÉ CÈCTEMÛ A*X=B ÈÇMEHEHÈß B A È B
C           MOÃÓT BÛÇBATÜ  ÈÇMEHEHÈß B X,ÁOËÜØÈE B COND PAÇ.
C           ECËÈ COND+1.0.EQ.COND, TO  A B ÏPEÄEËAX MAØÈHHOÉ
C           TO×HOCTÈ ßBËßETCß BÛPOÆÄEHHOÉ MATPÈÖEÉ. COND
C           ÏOËAÃAETCß PABHÛM 1.0E+32,ECËÈ OÁHAPÓÆEHA TO×HAß
C           BÛPOÆÄEHHOCTÜ.
C
C     IPVT -BEKTOP BEÄÓÙÈX ÝËEMEHTOB.
C           IPVT(K)=ÈHÄEKC K-É BEÄÓÙEÉ CTPOKÈ
C           IPVT(N)=(-1)**(×ÈCËO ÏEPECTAHOBOK)
C
C     PAÁO×EE ÏOËE. BEKTOP WORK ÄOËÆEH ÁÛTÜ OÏÈCAH È
C             BKËÞ×EH B BÛÇOB. EÃO BXOÄHOE COÄEPÆAHÈE OÁÛ×HO
C             HE ÄAET BAÆHOÉ ÈHÔOPMAÖÈÈ.
C
C     OÏPEÄEËÈTEËÜ MATPÈÖÛ A MOÆET ÁÛTÜ ÏOËÓ×EH HA BÛXOÄE
C     ÏO ÔOPMÓËE:
C          DET(A)=IPVT(N)*A(1,1)*A(2,2)*...*A(N,N).
C
      REAL EK,T,ANORM,YNORM,ZNORM
      INTEGER NM1,I,J,K,KP1,KB,KM1,M
C
      IPVT(N)=1
      IF(N.EQ.1)GO TO 80
      NM1=N-1
C
C     BÛ×ÈCËÈTÜ 1-HOPMÓ MATPÈÖÛ A
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
C     ÃAÓCCOBO ÈCKËÞ×EHÈE C ×ACTÈ×HÛM BÛÁOPOM
C     BEÄÓÙEÃO ÝËEMEHTA
C
      DO 35 K=1,NM1
        KP1=K+1
C
C       HAÉTÈ BEÄÓÙÈÉ ÝËEMEHT
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
C       ÏPOÏÓCTÈTÜ ÝTOT ØAÃ,ECËÈ BEÄÓÙÈÉ ÝËEMEHT PABEH HÓËÞ
C
        IF(T.EQ.0.0)GO TO 35
C
C       BÛ×ÈCËÈTÜ MHOÆÈTEËÈ
C
        DO 20 I=KP1,N
          A(I,K)=-A(I,K)/T
   20   CONTINUE
C
C       ÏEPECTABËßTÜ È ÈCKËÞ×ATÜ ÏO CTOËÁÖAM
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
C     COND=(1-HOPMA MATPÈÖÛ A)*(OÖEHKA ÄËß 1-HOPMÛ MATPÈÖÛ,
C     OÁPATHOÉ K A)
C     OÖEHKA ÏOËÓ×AETCß ÏOCPEÄCTBOM OÄHOÃO ØAÃA METOÄA
C     OÁPATHÛX ÈTEPAÖÈÉ ÄËß HAÈMEHÜØEÃO CÈHÃÓËßPHOÃO
C     BEKTOPA. ÝTO TPEÁÓET PEØEHÈß ÄBÓX CÈCTEM ÓPABHEHÈÉ,
C     (TPAHCÏOHÈPOBAHHAß ÄËß A) *Y=E È A*Z=Y, ÃÄE E-BEKTOP
C     ÈÇ +1 È -1, BÛÁPAHHÛÉ TAK, ×TOÁÛ MAKCÈMÈÇÈPOBATÜ
C     BEËÈ×ÈHÓ Y.
C     ESTIMATE=(1-HOPMA Z)/(1-HOPMA Y)
C
C     PEØÈTÜ CÈCTEMÓ (TPAHCÏOHÈPOBAHHAß ÄËß A)*Y=E
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
C     PEØÈTÜ CÈCTEMÓ A*Z=Y
C
      CALL SOLVE(NDIM,N,A,WORK,IPVT)
C
      ZNORM=0.0
      DO 70 I=1,N
        ZNORM=ZNORM+ABS(WORK(I))
   70 CONTINUE
C
C     OÖEHÈTÜ OÁÓCËOBËEHHOCTÜ
C
      COND=ANORM*ZNORM/YNORM
      IF(COND.LT.1.0)COND=1.0
      RETURN
C
C     CËÓ×AÉ MATPÈÖÛ 1*1
C
   80 COND=1.0
      IF(A(1,1).NE.0.0)RETURN
C
C     TO×HAß BÛPOÆÄEHHOCTÜ
C
   90 CONTINUE
      COND=1.0E+32
      RETURN
      END
