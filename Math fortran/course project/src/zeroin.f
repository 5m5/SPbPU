      REAL FUNCTION ZEROIN(AX,BX,F,TOL)
      REAL AX,BX,F,TOL
C
C     HÓËÜ ÔÓHKÖÈÈ F(X) BÛ×ÈCËßETCß B ÈHTEPBAËE AX,BX
C
C     BXOÄHAß ÈHÔOPMAÖÈß..
C
C     AX     ËEBÛÉ KOHEÖ ÈCXOÄHOÃO ÈHTEPBAËA
C     BX     ÏPABÛÉ KOHEÖ ÈCXOÄHOÃO ÈHTEPBAËA
C     F      ÏOÄÏPOÃPAMMA-ÔÓHKÖÈß, KOTOPAß BÛ×ÈCËßET F(X)
C            ÄËß ËÞÁOÃO X B ÈHTEPBAËE AX BX
C     TOL    ÆEËAEMAß ÄËÈHA ÈHTEPBAËA HEOÏPEÄEËEHHOCTÈ
C            KOHE×HOÃO PEÇÓËÜTATA
C
C     BÛXOÄHAß ÈHÔOPMAÖÈß...
C
C     ZEROIN AÁCÖÈCCA, AÏÏPOKCÈMÈPÓÞÙAß HÓËÜ ÔÓHKÖÈÈ F B
C            ÈHTEPBAËE AX, BX
C
C        ÁEÇ ÏPOBEPKÈ ÏPEÄÏOËAÃAETCß, ×TO F(AX) È F(BX) ÈMEÞT
C     ÏPOTÈBOÏOËOÆHÛE ÇHAKÈ.
C        ZEROIN BÛ×ÈCËßET HÓËÜ X B ÇAÄAHHOM ÈHTEPBAËE AX, BX
C     B ÏPEÄEËAX ÄOÏÓCKA HA OØÈÁKÓ  4*MACHEPS*ABS(X) + TOL,
C     ÃÄE MACHEPS-OTHOCÈTEËÜHAß MAØÈHHAß TO×HOCTÜ.
C        ÝTA ÏOÄÏPOÃPAMMA-ÔÓHKÖÈß ÏPEÄCTABËßET COÁOÉ CËEÃKA
C     MOÄÈÔÈÖÈPOBAHHÓÞ TPAHCËßÖÈÞ AËÃOË 60-ÏPOÖEÄÓPÛ ZERO,
C     ÏPÈBEÄEHHOÉ B KHÈÃE RICHARD BRENT, ALGORITHMS FOR
C     MINIMIZATION WITHOUT DERIVATIVES,PRENTICE HALL,INC.(1973).
C
      REAL A,B,C,D,E,EPS,FA,FB,FC,TOL1,XM,P,Q,R,S
C
C     BÛ×ÈCËÈTÜ EPS,OTHOCÈTEËÜHÓÞ MAØÈHHÓÞ TO×HOCTÜ
C
      EPS=1.0
   10 EPS=EPS/2.0
      TOL1=1.0+EPS
      IF(TOL1.GT.1.0) GO TO 10
C
C     ÏPÈCBOEHÈE HA×AËÜHÛX ÇHA×EHÈÉ
C
      A=AX
      B=BX
      FA=F(A)
      FB=F(B)
C
C     HA×ATÜ ØAÃ
C
   20 C=A
      FC=FA
      D=B-A
      E=D
   30 IF(ABS(FC).GE.ABS(FB)) GO TO 40
      A=B
      B=C
      C=A
      FA=FB
      FB=FC
      FC=FA
C
C     ÏPOBEPKA CXOÄÈMOCTÈ
C
   40 TOL1=2.0*EPS*ABS(B)+0.5*TOL
      XM=0.5*(C-B)
      IF(ABS(XM).LE.TOL1) GO TO 90
C
C     HEOÁXOÄÈMA ËÈ ÁÈCEKÖÈß
C
      IF(FB.EQ.0.0) GO TO 90
      IF(ABS(E).LT.TOL1) GO TO 70
      IF(ABS(FA).LE.ABS(FB)) GO TO 70
C
C     BOÇMOÆHA ËÈ KBAÄPATÈ×HAß ÈHTEPÏOËßÖÈß
C
      IF(A.NE.C)GO TO 50
C
C     ËÈHEÉHAß ÈHTEPÏOËßÖÈß
C
      S=FB/FA
      P=2.0*XM*S
      Q=1.0-S
      GO TO 60
C
C     OÁPATHAß KBAÄPATÈ×HAß ÈHTEPÏOËßÖÈß
C
   50 Q=FA/FC
      R=FB/FC
      S=FB/FA
       P=S*(2.0*XM*Q*(Q-R)-(B-A)*(R-1.0))
      Q=(Q-1.0)*(R-1.0)*(S-1.0)
C
C     BÛÁPATÜ ÇHAKÈ
C
   60 IF(P.GT.0.0) Q=-Q
      P=ABS(P)
C
C     ÏPÈEMËEMA ËÈ ÈHTEPÏOËßÖÈß
C
      IF((2.0*P).GE.(3.0*XM*Q-ABS(TOL1*Q))) GO TO 70
      IF(P.GE.ABS(0.5*E*Q)) GO TO 70
      E=D
      D=P/Q
      GO TO 80
C
C     ÁÈCEKÖÈß
C
   70 D=XM
      E=D
C
C     ÇABEPØÈTÜ ØAÃ
C
   80 A=B
      FA=FB
      IF(ABS(D).GT.TOL1) B=B+D
      IF(ABS(D).LE.TOL1) B=B+SIGN(TOL1,XM)
      FB=F(B)
      IF((FB*(FC/ABS(FC))).GT.0.0) GO TO 20
      GO TO 30
C
C     KOH×EHO
C
   90 ZEROIN=B
      RETURN
      END
