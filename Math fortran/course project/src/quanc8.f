      SUBROUTINE QUANC8(FUN,A,B,ABSERR,RELERR,RESULT,
     *                  ERREST,NOFUN,FLAG)
C
      REAL FUN,A,B,ABSERR,RELERR,RESULT,ERREST,FLAG
      INTEGER NOFUN
C
C     OÖEHÈTÜ ÈHTEÃPAË ÄËß FUN(X) OT A ÄO B C ÇAÄAHHOÉ
C     ÏOËÜÇOBATEËEM TO×HOCTÜÞ
C     ABTOMATÈ×ECKAß AÄAÏTÈBHAß ÏPOÃPAMMA, OCHOBAHHAß HA
C     ÔOPMÓËE HÜÞTOHA-KOTEÖA 8-OÃO ÏOPßÄKA
C
C     BXOÄHAß ÈHÔOPMAÖÈß
C
C    FUN    -ÈMß BHEØHEÉ ÔÓHKÖÈÈ,PEAËÈÇÓÞÙEÉ
C            BÛ×ÈCËEHÈE ÏOÄÈHTEÃPAËÜHOÉ ÔÓHKÖÈÈ
C
C    A      -HÈÆHÈÉ  ÏPEÄEË ÈHTEÃPÈPOBAHÈß
C
C    B      -BEPXHÈÉ ÏPEÄEË ÈHTEÃPÈPOBAHÈß (B MOÆET
C            ÁÛTÜ MEHÜØE, ×EM  A)
C
C    RELERR -OTHOCÈTEËÜHAß ÆEËAEMAß ÏOÃPEØHOCTÜ
C
C    ABSERR -AÁCOËÞTHAß ÆEËAEMAß  ÏOÃPEØHOCTÜ
C
C    BÛXOÄHAß ÈHÔOPMAÖÈß
C
C    RESULT -ÏPÈÁËÈÆEHHOE ÇHA×EHÈE ÈHTEÃPAËA
C
C    ERREST -OÖEHKA BEËÈ×ÈHÛ ÄEÉCTBÈTEËÜHOÉ OØÈÁKÈ
C
C    NOFUN  -×ÈCËO ÇHA×EHÈÉ ÔÓHKÖÈÈ,ÈCÏOËÜÇOBAHHÛX
C            ÏPÈ BÛ×ÈCËEHÈÈ ÈHTEÃPAËA.
C
C    FLAG   -ÈHÄÈKATOP HAÄEÆHOCTÈ. ECËÈ FLAG PABEH HÓËÞ,
C            TO RESULT ÓÄOBËETBOPßET ÇAÄAHHOÉ ÃPAHÈÖE
C            ÏOÃPEØHOCTÈ. ECËÈ FLAG=XXX.YYY, TO  XXX-×ÈCËO
C            ÈHTEPBAËOB ÄËß KOTOPÛX HE ÁÛËO CXOÄÈMOCTÈ,
C            A 0.YYY-ÄOËß OCHOBHOÃO ÈHTEPBAËA, OCTABØAßCß
C            ÄËß OÁPAÁOTKÈ B TOT MOMEHT,KOÃÄA ÏOÄÏPOÃPAMMA
C            ÏPÈÁËÈÇÈËACÜ K ÏPEÄEËÜHOMÓ ×ÈCËÓ ÄËß NOFUN.
C
      REAL W0,W1,W2,W3,W4,AREA,X0,F0,STONE,STEP,COR11,TEMP
      REAL QPREV,QNOW,QDIFF,QLEFT,ESTERR,TOLERR
      REAL QRIGHT(31),F(16),X(16),FSAVE(8,30),XSAVE(8,30)
C
C     *** ÝTAÏ1 *** ÏPÈCBOEHÈE HA×AËÜHÛX ÇHA×EHÈÉ
C     ÏEPEMEHHÛM, HE ÇABÈCßÙÈM OT ÈHTEPBAËA.
C     ÃEHEPÈPOBAHÈE KOHCTAHT.
C
      INTEGER LEVMIN,LEVMAX,LEVOUT,NOMAX,NOFIN,LEV,NIM,I,J
      LEVMIN=1
      LEVMAX=30
      LEVOUT=6
      NOMAX=5000
      NOFIN=NOMAX-8*(LEVMAX-LEVOUT+2**(LEVOUT+1))
C
C     ECËÈ NOFUN ÄOCTÈÃAET ÇHA×EHÈß NOFIN, TO TPEBOÃA
C
      W0=3956.0/14175.0
      W1=23552.0/14175.0
      W2=-3712.0/14175.0
      W3=41984.0/14175.0
      W4=-18160.0/14175.0
C
C     ÏPÈCBOÈTÜ HÓËEBÛE ÇHA×EHÈß ÏEPEMEHHÛM CÓMMAM
C
      FLAG=0.0
      RESULT=0.0
      COR11=0.0
      ERREST=0.0
      AREA=0.0
      NOFUN=0
      IF(A.EQ.B)RETURN
C
C     *** ÝTAÏ 2 *** ÏPÈCBOEHÈE HA×AËÜHÛX ÇHA×EHÈÉ
C     ÏEPEMEHHÛM, ÇABÈCßÙÈM OT ÈHTEPBAËA, B
C     COOTBETCTBÈÈ C ÏEPBÛM ÈHTEPBAËOM
C
      LEV=0
      NIM=1
      X0=A
      X(16)=B
      QPREV=0.0
      F0=FUN(X0)
      STONE=(B-A)/16.0
      X(8)=(X0+X(16))/2.0
      X(4)=(X0+X(8))/2.0
      X(12)=(X(8)+X(16))/2.0
      X(2)=(X0+X(4))/2.0
      X(6)=(X(4)+X(8))/2.0
      X(10)=(X(8)+X(12))/2.0
      X(14)=(X(12)+X(16))/2.0
      DO 25 J=2,16,2
      F(J)=FUN(X(J))
   25 CONTINUE
      NOFUN=9
C
C     *** ÝTAÏ 3 *** OCHOBHÛE BÛ×ÈCËEHÈß
C     TPEÁÓÞTCß QPREV, X0, X2, X4,...,X16, F0, F2, F4,...
C     ,F16. BÛ×ÈCËßÞTCß X1, X3,...,X15, F1, F3,...,F15,
C     QLEFT,QRIGHT,QNOW,QDIF,AREA
C
   30 X(1)=(X0+X(2))/2.0
      F(1)=FUN(X(1))
      DO 35 J=3,15,2
      X(J)=(X(J-1)+X(J+1))/2.0
      F(J)=FUN(X(J))
   35 CONTINUE
      NOFUN=NOFUN+8
      STEP=(X(16)-X0)/16.0
      QLEFT=(W0*(F0+F(8))+W1*(F(1)+F(7))+W2*
     *      (F(2)+F(6))+W3*(F(3)+F(5))+W4*F(4))*STEP
      QRIGHT(LEV+1)=(W0*(F(8)+F(16))+W1*(F(9)+F(15))+
     *       W2*(F(10)+F(14))+W3*(F(11)+F(13))+W4*F(12))*STEP
      QNOW=QLEFT+QRIGHT(LEV+1)
      QDIFF=QNOW-QPREV
      AREA=AREA+QDIFF
C
C     *** ÝTAÏ 4 *** ÏPOBEPKA CXOÄÈMOCTÈ ÄËß ÈHTEPBAËA
C
      ESTERR=ABS(QDIFF)/1023.0
      TOLERR=AMAX1(ABSERR,RELERR*ABS(AREA))*(STEP/STONE)
      IF(LEV.LT.LEVMIN)GO TO 50
      IF(LEV.GE.LEVMAX)GO TO 62
      IF(NOFUN.GT.NOFIN)GO TO 60
      IF(ESTERR.LE.TOLERR)GO TO 70
C
C     *** ÝTAÏ 5 *** CXOÄÈMOCTÈ HET.
C     ÓCTAHOBÈTÜ CËEÄÓÞÙÈÉ ÈHTEPBAË
C
   50 NIM=2*NIM
      LEV=LEV+1
C
C     ÇAÏOMHÈTÜ ÝËEMEHTÛ, OTHOCßÙÈECß K ÏPABOÉ ÏOËOBÈHE
C     ÈHTEPBAËA, ÄËß ÁÓÄÓÙEÃO ÈCÏOËÜÇOBAHÈß.
C
      DO 52 I=1,8
      FSAVE(I,LEV)=F(I+8)
      XSAVE(I,LEV)=X(I+8)
   52 CONTINUE
C
C     COÁPATÜ ÝËEMEHTÛ, OTHOCßÙÈECß K ËEBOÉ ÏOËOBÈHE
C     ÈHTEPBAËA ÄËß HEMEÄËEHHOÃO ÈCÏOËÜÇOBAHÈß
C
      QPREV=QLEFT
      DO 55 I=1,8
      J=-I
      F(2*J+18)=F(J+9)
      X(2*J+18)=X(J+9)
   55 CONTINUE
      GO TO 30
C
C     *** ÝTAÏ 6 *** "ÏOÆAPHÛÉ" PAÇÄEË
C     ×ÈCËO ÇHA×EHÈÉ ÔÓHKÖÈÈ ÁËÈÇKO K TOMÓ, ×TOÁÛ
C     ÏPEBÛCÈTÜ ÓCTAHOBËEHHÛÉ ÏPEÄEË.
C
   60 NOFIN=2*NOFIN
      LEVMAX=LEVOUT
      FLAG=FLAG+(B-X0)/(B-A)
      GO TO 70
C
C     TEKÓÙEE ÏPEÄEËÜHOE ÇHA×EHÈE ÃËÓÁÈHÛ ÄEËEHÈß
C     ÏOÏOËAM PABHO LEVMAX
C
   62 FLAG=FLAG+1.0
C
C     *** ÝTAÏ 7 *** CXOÄÈMOCTÜ ÄËß ÈHTEPBAËA ÈMEET MECTO
C     ÏPÈÁABÈTÜ O×EPEÄHÛE CËAÃAEMÛE K ÏEPEMEHHÛM CÓMMAM.
C
   70 RESULT=RESULT+QNOW
      ERREST=ERREST+ESTERR
      COR11=COR11+QDIFF/1023.0
C
C     ÓCTAHOBÈTÜ CËEÄÓÞÙÈÉ ÈHTEPBAË.
C
   72 IF(NIM.EQ.2*(NIM/2))GO TO 75
      NIM=NIM/2
      LEV=LEV-1
      GO TO 72
   75 NIM=NIM+1
      IF(LEV.LE.0)GO TO 80
C
C     COÁPATÜ ÝËEMEHTÛ, HEOÁXOÄÈMÛE ÄËß CËEÄÓÞÙEÃO ÈHTEBAËA
C
      QPREV=QRIGHT(LEV)
      X0=X(16)
      F0=F(16)
      DO 78 I=1,8
      F(2*I)=FSAVE(I,LEV)
      X(2*I)=XSAVE(I,LEV)
   78 CONTINUE
      GO TO 30
C
C     *** ÝTAÏ 8 *** ÇAKËÞ×ÈTEËÜHÛE OÏEPAÖÈÈ È BÛXOÄ
C
   80 RESULT=RESULT+COR11
C
C     OÁECÏE×ÈTÜ, ×TOÁÛ ÇHA×EHÈE ÏEPEMEHHOÉ ERREST
C     ÁÛËO HE MEHÜØE ÓPOBHß OKPÓÃËEHÈÉ
C
      IF(ERREST.EQ.0.0)RETURN
   82 TEMP=ABS(RESULT)+ERREST
      IF(TEMP.NE.ABS(RESULT))RETURN
      ERREST=2.0*ERREST
      GO TO 82
      END
