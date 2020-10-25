      REAL FUNCTION FMIN(AX,BX,F,TOL)
      REAL AX,BX,F,TOL
C
C     BÛ×ÈCËßET ÏPÈÁËÈÆEHÈE X K TO×KE, ÃÄE F ÄOCTÈÃAET
C     MÈHÈMÓMA HA ÈHTEPBAËE (AX,BX)
C
C     BXOÄHAß ÈHÔOPMAÖÈß.
C
C     AX   ËEBÛÉ KOHEÖ ÈCXOÄHOÃO ÈHTEPBAËA
C     BX   ÏPABÛÉ KOHEÖ ÈCXOÄHOÃO ÈHTEPBAËA
C     F    ÏOÄÏPOÃPAMMA-ÔÓHKÖÈß, KOTOPAß BÛ×ÈCËßET F(X)
C          ÄËß ËÞÁOÃO X B ÈHTEPBAËE (AX,BX)
C    TOL   ÆEËAEMAß ÄËÈHA ÈHTEPBAËA HEOÏPEÄEËEHHOCTÈ
C          KOHE×HOÃO PEÇÓËÜTATA (.GE.0.0)
C
C    BÛXOÄHAß ÈHÔOPMAÖÈß.
C
C    FMIN  AÁCÖÈCCA, AÏÏPOKCÈMÈPÓÞÙAß TO×KÓ,
C          ÃÄE F ÄOCTÈÃAET MÈHÈMÓMA
C
C       METOÄ ÈCÏOËÜÇÓET KOMÁÈHAÖÈÞ ÏOÈCKA ÇOËOTOÃO CE×EHÈß
C    È ÏOCËEÄOBATEËÜHOÉ ÏAPAÁOËÈ×ECKOÉ ÈHTEPÏOËßÖÈÈ. CXOÄÈ-
C    MOCTÜ HÈKOÃÄA HE ÁÛBAET ÇHA×ÈTEËÜHO XÓÆE, ×EM ÄËß
C    ÏOÈCKA ÔÈÁOHA××È. ECËÈ F ÈMEET HEÏPEPÛBHÓÞ BTOPÓÞ
C    ÏPOÈÇBOÄHÓÞ, ÏOËOÆÈTEËÜHÓÞ B TO×KE MÈHÈMÓMA (HE
C    COBÏAÄAÞÙEÉ HÈ C AX,HÈ C BX), TO CXOÄÈMOCTÜ CBEPX-
C    ËÈHEÉHAß È OÁÛ×HO ÈMEET ÏOPßÄOK ÏPÈMEPHO 1.324...
C       ÔÓHKÖÈß F HÈKOÃÄA HE BÛ×ÈCËßETCß B ÄBÓX TO×KAX,
C    OTCTOßÙÈX ÄPÓÃ OT ÄPÓÃA MEHEE ×EM HA EPS*ABS(X)+(TOL/3),
C    ÃÄE EPS ÏPÈÁËÈÇÈTEËÜHO PABHO KBAÄPATHOMÓ KOPHÞ ÈÇ
C    OTHOCÈTEËÜHOÉ MAØÈHHOÉ TO×HOCTÈ. ECËÈ F-ÓHÈMOÄAËÜHAß
C    ÔÓHKÖÈß È BÛ×ÈCËEHHÛE ÇHA×EHÈß F COXPAHßÞT ÓHÈMOÄAËÜ-
C    HOCTÜ ÏPÈ COÁËÞÄEHÈÈ ÓKAÇAHHOÃO ÓCËOBÈß PAÇÄEËEHHOCTÈ,
C    TO FMIN AÏÏPOKCÈMÈPÓET AÁCÖÈCCÓ ÃËOÁAËÜHOÃO MÈHÈMÓMA F
C    HA ÈHTEPBAËE (AX,BX) C OØÈÁKOÉ, MEHÜØEÉ 3*EPS*ABS(X)+TOL.
C    ECËÈ F HE ßBËßETCß ÓHÈMOÄAËÜHOÉ, TO FMIN MOÆET C TOÉ ÆE
C    TO×HOCTÜÞ AÏÏPOKCÈMÈPOBATÜ ËOKAËÜHÛÉ MÈHÈMÓM, BOÇMOÆHO,
C    HE COBÏAÄAÞÙÈÉ C ÃËOÁAËÜHÛM.
C       ÝTA ÏOÄÏPOÃPAMMA-ÔÓHKÖÈß ßBËßETCß CËEÃKA MOÄÈÔÈÖÈPO-
C    BAHHOÉ BEPCÈEÉ AËÃOË 60-ÏPOÖEÄÓPÛ LOCALMIN, ÏPÈBEÄEHHOÉ
C    B KHÈÃE RICARD BRENT, ALGORITHMS FOR MINIMIZATION
C    WITHOUT DERIVATIVES, PRENTICE-HALL, INC.(1973).
C
      REAL A,B,C,D,E,EPS,XM,P,Q,R,TOL1,TOL2,U,V,W
      REAL FU,FV,FW,FX,X,ABS,SIGN
C
C     C ECTÜ BOÇBEÄEHHAß B KBAÄPAT BEËÈ×ÈHA,
C     OÁPATHAß K ÇOËOTOMÓ CE×EHÈÞ
C
      C=0.5*(3.0-SQRT(5.0))
C
C     EPS ÏPÈÁËÈÇÈTEËÜHO PABHO KBAÄPATHOMÓ KOPHÞ ÈÇ
C     OTHOCÈTEËÜHOÉ MAØÈHHOÉ TO×HOCTÈ
C
      EPS=1.0
   10 EPS=EPS/2.0
      TOL1=1.0+EPS
      IF(TOL1.GT.1.0) GO TO 10
      EPS=SQRT(EPS)
C
C     ÏPÈCBOEHÈE HA×AËÜHÛX ÇHA×EHÈÉ
C
      A=AX
      B=BX
      V=A+C*(B-A)
      W=V
      X=V
      E=0.0
      FX=F(X)
      FV=FX
      FW=FX
C
C     ÇÄECÜ HA×ÈHAETCß OCHOBHOÉ ÖÈKË
C
   20 XM=0.5*(A+B)
      TOL1=EPS*ABS(X)+TOL/3.0
      TOL2=2.0*TOL1
C
C     ÏPOBEPÈTÜ KPÈTEPÈÉ OKOH×AHÈß
C
      IF(ABS(X-XM).LE.(TOL2-0.5*(B-A))) GO TO 90
C
C     HEOÁXOÄÈMO ËÈ ÇOËOTOE CE×EHÈE
C
      IF(ABS(E).LE.TOL1) GO TO 40
C
C     ÏOCTPOÈTÜ ÏAPAÁOËÓ
C
      R=(X-W)*(FX-FV)
      Q=(X-V)*(FX-FW)
      P=(X-V)*Q-(X-W)*R
      Q=2.0*(Q-R)
      IF(Q.GT.0.0) P=-P
      Q=ABS(Q)
      R=E
      E=D
C
C     ÏPÈEMËEMA ËÈ ÏAPAÁOËA
C
   30 IF(ABS(P).GE.ABS(0.5*Q*R)) GO TO 40
      IF(P.LE.Q*(A-X)) GO TO 40
      IF(P.GE.Q*(B-X)) GO TO 40
C
C     ØAÃ ÏAPAÁOËÈ×ECKOÉ ÈHTEPÏOËßÖÈÈ
C
      D=P/Q
      U=X+D
C
C     F HE CËEÄÓET BÛ×ÈCËßTÜ CËÈØKOM ÁËÈÇKO K AX ÈËÈ BX
C
      IF((U-A).LT.TOL2) D=SIGN(TOL1,XM-X)
      IF((B-U).LT.TOL2) D=SIGN(TOL1,XM-X)
      GO TO 50
C
C     ØAÃ ÇOËOTOÃO CE×EHÈß
C
   40 IF(X.GE.XM) E=A-X
      IF(X.LT.XM)E=B-X
      D=C*E
C
C     F HE CËEÄÓET BÛ×ÈCËßTÜ CËÈØKOM ÁËÈÇKO K X
C
   50 IF(ABS(D).GE.TOL1) U=X+D
      IF(ABS(D).LT.TOL1) U=X+SIGN(TOL1,D)
      FU=F(U)
C
C     ÏPÈCBOÈTÜ HOBÛE ÇHA×EHÈß ÏAPAMETPAM A,B,V,W È X
C
      IF(FU.GT.FX) GO TO 60
      IF(U.GE.X) A=X
      IF(U.LT.X) B=X
      V=W
      FV=FW
      W=X
      FW=FX
      X=U
      FX=FU
      GO TO 20
C
   60 IF(U.LT.X) A=U
      IF(U.GE.X) B=U
      IF(FU.LE.FW) GO TO 70
      IF(W.EQ.X) GO TO 70
      IF(FU.LE.FV) GO TO 80
      IF(V.EQ.X) GO TO 80
      IF(V.EQ.W) GO TO 80
      GO TO 20
C
   70 V=W
      FV=FW
      W=U
      FW=FU
      GO TO 20
C
   80 V=U
      FV=FU
      GO TO 20
C
C     KOHEÖ OCHOBHOÃO ÖÈKËA
C
   90 FMIN=X
      RETURN
      END
