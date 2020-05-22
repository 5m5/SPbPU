      SUBROUTINE RKFS(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,
     *                YP,H,F1,F2,F3,F4,F5,SAVRE,SAVAE,
     *                NFE,KOP,INIT,JFLAG,KFLAG)
C
C     METOÄ PÓHÃE-KÓTTA-ÔEËÜÁEPÃA ×ETBEPTOÃO-ÏßTOÃO ÏOPßÄKA
C     RKFS ÈHTEÃPÈPÓET CÈCTEMÓ OÁÛKHOBEHHÛX ÄÈÔÔE-
C     PEHÖÈAËÜHÛX ÓPABHEHÈÉ ÏEPBOÃO ÏOPßÄKA(CM. KOM-
C     MEHTAPÈÉ K RKF45). MACCÈBÛ YP,F1,F2,F3,F4 È F5
C     (PAÇMEPHOCTÈ ÏO KPAÉHEÉ MEPE NEQN) È ÏEPEMEH-
C     HÛE H,SAVRE,SAVAE,NFE,KOP,INIT,JFLAG È KFLAG
C     ÈCÏOËÜÇÓÞTCß BHÓTPÈ ÏPOÃPAMMÛ È BÛHECEHÛ B
C     CÏÈCOK BÛÇOBA,×TOÁÛ COXPAHÈTÜ ÈX OÏPEÄEËEH-
C     HOCTÜ ÏPÈ ÏOBTOPHOM OÁPAÙEHÈÈ.ÏOÝTOMÓ ÈX ÇHA-
C     ×EHÈß HE ÄOËÆHÛ  ÈÇMEHßTÜCß  ÏOËÜÇOBATEËEM.
C     BOÇMOÆHÛÉ ÈHTEPEC ÏPEÄCTABËßÞT ÏAPAMETPÛ
C     YP  -ÏPOÈÇBOÄHAß BEKTOPA PEØEHÈß B TO×KE T
C     H   -ÏPEÄÏOËAÃAEMÛÉ PAÇMEP ØAÃA ÄËß O×EPEÄHOÃO ÝTAÏA
C     NFE -C×ET×ÈK ×ÈCËA BÛ×ÈCËEHÈÉ ÔÓHKÖÈÈ
C
      LOGICAL HFAILD,OUTPUT
C
      INTEGER NEQN,IFLAG,NFE,KOP,INIT,JFLAG,KFLAG
      REAL Y(NEQN),T,TOUT,RELERR,ABSERR,H,YP(NEQN),
     *     F1(NEQN),F2(NEQN),F3(NEQN),F4(NEQN),F5(NEQN),
     *     SAVRE,SAVAE
C
      EXTERNAL F
C
      REAL A,AE,DT,EE,EEOET,ESTTOL,ET,HMIN,REMIN,
     *     RER,S,SCALE,TOL,TOLN,U26,EPSP1,EPS,YPK
C
      INTEGER K,MAXNFE,MFLAG
C
      REAL AMAX1,AMIN1
      SAVE
C
C     REMIN-ÝTO MÈHÈMAËÜHOE ÄOÏÓCTÈMOE ÇHA×EHÈE ÄËß
C     RELERR.ÏOÏÛTKÈ ÏOËÓ×ÈTÜ ÏO ÝTOÉ ÏOÄÏPOÃPAMME
C     ÁOËEE BÛCOKÓÞ TO×HOCTÜ OÁÛ×HO CTOßT O×EHÜ
C     ÄOPOÃO È ÇA×ACTÓÞ ÁEÇÓCÏEØHÛ.
C
      DATA REMIN/1.E-12/
C
C     CTOÈMOCTÜ C×ETA KOHTPOËÈPÓETCß TPEÁOBAHÈEM,
C     ×TOÁÛ KOËÈ×ECTBO BÛ×ÈCËEHÈÉ ÔÓHKÖÈÈ ÁÛËO OÃ-
C     PAHÈ×EHO BEËÈ×ÈHOÉ,ÏPÈÁËÈÇÈTEËÜHO PABHOÉ ÇHA-
C     ×EHÈÞ ÏAPAMETPA MAXNFE.ÏPÈHßTOE ÇÄECÜ ÇHA×E-
C     HÈE ÏPÈMEPHO COOTBETCTBÓET 500 ØAÃAM.
C
      DATA MAXNFE/3000/
C
C     ÏPOBEPÈTÜ BXOÄHÛE ÏAPAMETPÛ
C
      IF(NEQN.LT.1)GO TO 10
      IF((RELERR.LT.0.0).OR.(ABSERR.LT.0.0))GO TO 10
      MFLAG=IABS(IFLAG)
      IF((MFLAG.EQ.0).OR.(MFLAG.GT.8))GO TO 10
      IF(MFLAG.NE.1)GO TO 20
C
C     ÏEPBÛÉ BÛÇOB,BÛ×ÈCËÈTÜ MAØÈHHOE ÝÏCÈËOH
C
      EPS=1.0
    5 EPS=EPS/2.0
      EPSP1=EPS+1.
      IF(EPSP1.GT.1.)GO TO 5
      U26=26.*EPS
      GO TO 50
C
C     OØÈÁKA BXOÄHOÉ ÈHÔOPMAÖÈÈ
C
   10 IFLAG=8
      RETURN
C
C     ÏPOBEPÈTÜ BOÇMOÆHOCTÈ ÏPOÄOËÆEHÈß
C
   20 IF((T.EQ.TOUT).AND.(KFLAG.NE.3))GO TO 10
      IF(MFLAG.NE.2)GO TO 25
C
C     IFLAG=+2 ÈËÈ -2
C
      IF((KFLAG.EQ.3).OR.(INIT.EQ.0))GO TO 45
      IF(KFLAG.EQ.4)GO TO 40
      IF((KFLAG.EQ.5).AND.(ABSERR.EQ.0.0))GO TO 30
      IF((KFLAG.EQ.6).AND.(RELERR.LE.SAVRE).AND.
     *(ABSERR.LE.SAVAE))GO TO 30
      GO TO 50
C
C     IFLAG=3,4,5,6,7 ÈËÈ 8
C
   25 IF(IFLAG.EQ.3)GO TO 45
      IF(IFLAG.EQ.4)GO TO 40
      IF((IFLAG.EQ.5).AND.(ABSERR.GT.0.0))GO TO 45
C
C     ÈHTEÃPÈPOBAHÈE HEËÜÇß ÏPOÄOËÆATÜ,ÏOCKOËÜKÓ ÏOËÜ-
C     ÇOBATEËÜ HE BÛÏOËHÈË ÈHCTPÓKÖÈÉ,COOTBETCTBÓÞÙÈX
C     ÇHA×EHÈßM IFLAG=5,6,7 ÈËÈ 8
C
   30 PRINT 35
   35 FORMAT(/20X,48HÈHTEÃPÈPOBAHÈE ÏPEPBAHO, ÏOCKOËÜKÓ ÏOËÜÇOBATEËÜ ,
     *11HHE BÛÏOËHÈË/20X,34HÈHCTPÓKÖÈÉ RKF45, COOTBETCTBÓÞÙÈX ,
     *27HÇHA×EHÈßM IFLAG=5,6,7 ÈËÈ 8)
      STOP
C
C     ÏEPEOÏPEÄEËÈTÜ C×ET×ÈK ×ÈCËA BÛ×ÈCËEHÈÉ ÔÓHKÖÈÈ
C
   40 NFE=0
      IF(MFLAG.EQ.2)GO TO 50
C
C     ÏEPEOÏPEÄEËÈTÜ ÇHA×EHÈE FLAG,ÓCTAHOBËEHHOE
C     ÏPÈ ÏPEÄÛÄÓÙEM OÁPAÙEHÈÈ
C
   45 IFLAG=JFLAG
      IF(KFLAG.EQ.3)MFLAG=IABS(IFLAG)
C
C     COXPAHÈTÜ BXOÄHOE ÇHA×EHÈE IFLAG È ÓCTAHOBÈTÜ
C     ÇHA×EHÈE FLAG, COOTBETCTBÓÞÙEE ÏPOÄOËÆEHÈÞ,
C     ÄËß ÁÓÄÓÙEÉ ÏPOBEPKÈ
C
   50 JFLAG=IFLAG
      KFLAG=0
C
C     COXPAHÈTÜ ÇHA×EHÈß RELERR È ABSERR ÄËß BXOÄHOÉ
C     ÏPOBEPKÈ ÏPÈ ÏOCËEÄÓÞÙÈX OÁPAÙEHÈßX
C
      SAVRE=RELERR
      SAVAE=ABSERR
C
C     ÓCTAHOBÈTÜ ÇHA×EHÈE ÃPAHÈÖÛ ÄËß OTHOCÈTEËÜ-
C     HOÉ ÏOÃPEØHOCTÈ,PABHOE KAK MÈHÈMÓM 2*EPS+
C     REMIN,×TOÁÛ ÈÇÁEÆATÜ TPÓÄHOCTEÉ,CBßÇAHHÛX
C     C TPEÁOBAHÈEM HEÄOCTÈÆÈMOÉ TO×HOCTÈ
C
      RER=2.*EPS+REMIN
      IF(RELERR.GE.RER)GO TO 55
C
C     ÇAÄAHHAß ÃPAHÈÖA OTHOCÈTEËÜHOÉ ÏOÃPEØHOCTÈ
C     CËÈØKOM MAËA
C
      RELERR=RER
      IFLAG=3
      KFLAG=3
      RETURN
C
   55 DT=TOUT-T
C
      IF(MFLAG.EQ.1)GO TO 60
      IF(INIT.EQ.0)GO TO 65
      GO TO 80
C
C     ÏPÈCBOEHÈE HA×AËÜHÛX ÇHA×EHÈÉ (ÈHÈÖÈÈPOBA-
C             HÈE)-ÓCTAHOBÈTÜ ÇHA×EHÈE ÓKAÇATEËß
C             OKOH×AHÈß ÈHÈÖÈÈPOBAHÈß,INIT
C             ÓCTAHOBÈTÜ ÇHA×EHÈE ÓKAÇATEËß CËÈØ-
C             KOM ÁOËÜØOÃO ÇATPEÁOBAHHOÃO ×ÈCËA BÛ-
C             XOÄHÛX TO×EK,KOP
C             BÛ×ÈCËÈTÜ HA×AËÜHÛE ÏPOÈÇBOÄHÛE
C             ÓCTAHOBÈTÜ ÇHA×EHÈE C×ET×ÈKA ×ÈCËA
C             BÛ×ÈCËEHÈÉ ÔÓHKÖÈÈ,NFE
C             OÖEHÈTÜ HA×EËÜHÓÞ BEËÈ×ÈHÓ ØAÃA
C
   60 INIT=0
      KOP=0
C
      A=T
      CALL F(A,Y,YP)
      NFE=1
      IF(T.NE.TOUT)GO TO 65
      IFLAG=2
      RETURN
C
   65 INIT=1
      H=ABS(DT)
      TOLN=0.
      DO 70 K=1,NEQN
      TOL=RELERR*ABS(Y(K))+ABSERR
      IF(TOL.LE.0)GO TO 70
      TOLN=TOL
      YPK=ABS(YP(K))
      IF(YPK*H**5.GT.TOL)H=(TOL/YPK)**0.2
   70 CONTINUE
      IF(TOLN.LE.0.0)H=0.0
      H=AMAX1(H,U26*AMAX1(ABS(T),ABS(DT)))
      JFLAG=ISIGN(2,IFLAG)
C
C     ÏPÈCBOÈTÜ BEËÈ×ÈHE ØAÃA ÇHAK,COOTBETCTBÓÞÙÈÉ
C     ÈHTEÃPÈPOBAHÈÞ B HAÏPABËEHÈÈ OT T K TOUT
C
   80 H=SIGN(H,DT)
C
C     ÏPOBEPKA, HACKOËÜKO CEPÜEÇHO BËÈßHÈE HA RKF45
C     CËÈØKOM ÁOËÜØOÃO ÇATPEÁOBAHHOÃO ×ÈCËA BÛXOÄ-
C     HÛX TO×EK
C
      IF(ABS(H).GE.2.0*ABS(DT))KOP=KOP+1
      IF(KOP.NE.100)GO TO 85
C
C     ×PEÇMEPHAß ×ACTOTA BÛXOÄOB
C
      KOP=0
      IFLAG=7
      RETURN
C
   85 IF(ABS(DT).GT.U26*ABS(T))GO TO 95
C
C     ECËÈ O×EHÜ ÁËÈÇKO K TO×KE BÛXOÄA,ÏPOÝKCTPAÏO-
C     ËÈPOBATÜ È BEPHÓTÜCß ÏO MECTÓ BÛÇOBA
C
      DO 90 K=1,NEQN
   90 Y(K)=Y(K)+DT*YP(K)
      A=TOUT
      CALL F(A,Y,YP)
      NFE=NFE+1
      GO TO 300
C
C     ÏPÈCBOÈTÜ HA×AËÜHOE ÇHA×EHÈE ÈHÄÈKATOPÓ TO×KÈ
C     BÛXOÄA
C
   95 OUTPUT=.FALSE.
C
C     ×TOÁÛ ÈÇÁEÆATÜ HEOÏPABÄAHHOÃO MAØÈHHOÃO HÓËß
C     ÏPÈ BÛ×ÈCËEHÈÈ ÔÓHKÖÈÈ OT ÃPAHÈÖ ÏOÃPEØHO-
C     CTEÉ,ÏPOMACØTAÁÈPOBATÜ ÝTÈ ÃPAHÈÖÛ
C
      SCALE=2./RELERR
      AE=SCALE*ABSERR
C
C     ÏOØAÃOBOE ÈHTEÃPÈPOBAHÈE
C
  100 HFAILD=.FALSE.
C
C     ÓCTAHOBÈTÜ HAÈMEHÜØÓÞ ÄOÏÓCTÈMÓÞ BEËÈ×ÈHÓ ØAÃA
C
      HMIN=U26*ABS(T)
C
C     ÈCÏPABÈTÜ ÏPÈ HEOÁXOÄÈMOCTÈ BEËÈ×ÈHÓ ØAÃA,
C     ×TOÁÛ ÄOCTÈÃHÓTÜ TO×KÈ BÛXOÄA. PACC×ÈTATÜ HA
C     ÄBA ØAÃA BÏEPEÄ,×TOÁÛ ÈÇÁEÆATÜ CËÈØKOM PEÇKÈX
C     ÈÇMEHEHÈÉ B BEËÈ×ÈHE ØAÃA È TEM CAMÛM ÓMEHÜ-
C     ØÈTÜ BËÈßHÈE BÛXOÄHÛX TO×EK HA ÏPOÃPAMMÓ.
C
      DT=TOUT-T
      IF(ABS(DT).GE.2.*ABS(H))GO TO 200
      IF(ABS(DT).GT.ABS(H))GO TO 150
C
C     CËEÄÓÞÙÈÉ ÓCÏEØHÛÉ ØAÃ ÇABEPØÈT ÈHTEÃPÈPO-
C     BAHÈE ÄO ÓKAÇAHHOÉ TO×KÈ BÛXOÄA
C
      OUTPUT=.TRUE.
      H=DT
      GO TO 200
C
  150 H=0.5*DT
C
C
C
C     BHÓTPEHHÈÉ OÄHOØAÃOBÛÉ ÈHTEÃPATOP
C
C     ÃPAHÈÖÛ ÏOÃPEØHOCTEÉ ÁÛËÈ ÏPOMACØTAÁÈPOBAHÛ,
C     ×TOÁÛ ÈÇÁEÆATÜ HEOÏPABÄAHHOÃO MAØÈHHOÃO HÓËß
C     ÏPÈ BÛ×ÈCËEHÈÈ ÔÓHKÖÈÈ OT HÈX.
C     ×TOÁÛ ÈÇÁEÆATÜ OÁPAÙEHÈß B HÓËÜ ÇHAMEHATEËß
C     B TECTE,OTHOCÈTEËÜHAß OØÈÁKA ÈÇMEPßETCß  ÏO
C     OTHOØEHÈÞ K CPEÄHEMÓ  ÈÇ BEËÈ×ÈH PEØEHÈß
C     B HA×AËE È KOHÖE ØAÃA.
C     B ÔOPMÓËE,OÖEHÈBAÞÙEÉ OØÈÁKÓ,ÏPOÈÇBEÄEHA
C     ÃPÓÏÏÈPOBKA CËAÃAEMÛX,ÓMEHÜØAÞÙAß ÏOTEPÞ
C     BEPHÛX ÇHAKOB.
C     ×TOÁÛ PAÇËÈ×ATÜ MEÆÄÓ COÁOÉ PAÇHÛE APÃÓMEHTÛ,
C     ÄËß H HE ÄOÏÓCKAÞTCß ÇHA×EHÈß,MEHÜØÈE ÓMHO-
C     ÆEHHOÉ HA 26 OØÈÁKÈ OKPÓÃËEHÈß B T.
C     BBEÄEHÛ ÏPAKTÈ×ECKÈE OÃPAHÈ×EHÈß HA CKOPOCTÜ
C     ÈÇMEHEHÈß BEËÈ×ÈHÛ ØAÃA,×TOÁÛ CÃËAÄÈTÜ ÏPO-
C     ÖECC BÛÁOPA ÝTOÉ BEËÈ×ÈHÛ È ÈÇÁEÆATÜ ×PEÇMEP-
C     HOÃO EE PAÇÁPOCA B ÇAÄA×AX C HAPÓØEHÈEM HEÏPE-
C     PÛBHOCTÈ.
C     ÈÇ ÏPEÄOCTOPOÆHOCTÈ ÏPOÃPAMMA ÁEPET 9/10 OT TOÉ
C     BEËÈ×ÈHÛ ØAÃA,KOTOPAß HÓÆHA ÏO EE OÖEHKE.
C     ECËÈHA ÄAHHOM ØAÃE ÁÛËA HEÓÄA×HAß ÏOÏÛTKA
C     TO ÏPÈ ÏËAHÈPOBAHÈÈ CËEÄÓÞÙEÃO ÓBEËÈ×EHÈE
C     ÄËÈHÛ ØAÃA HE ÄOÏÓCKAETCß. ÝTO ÏOBÛØAET ÝÔÔEK-
C     TÈBHOCTÜ ÏPOÃPAMMÛ ÄËß ÇAÄA× C PAÇPÛBAMÈ È
C     B OÁÙEM CËÓ×AE,ÏOCKOËÜKÓ ÈCÏOËÜÇÓETCß ËOKAËÜ-
C     HAß ÝKCTPAÏOËßÖÈß È ÄOÏOËHÈTEËÜHAß ÏPEÄOCTO-
C     POÆHOCTÜ KAÆETCß OÏPABÄAHHOÉ.
C
C
C     ÏPOBEPÈTÜ ×ÈCËO BÛ×ÈCËEHÈÉ ÏPOÈÇBOÄHÛX.ECËÈ
C     OHO HE ÏPEBÛØAET ÓCTAHOBËEHHOÃO ÏPEÄEËA,ÏO-
C     ÏPOÁOBATÜ ÏPOÄOËÆÈTÜ ÈHTEÃPÈPOBAHÈE C T ÄO T+H
C
  200 IF(NFE.LE.MAXNFE)GO TO 220
C
C     CËÈØKOM ÁOËÜØAß PAÁOTA
C
      IFLAG=4
      KFLAG=4
      RETURN
C
C     ÏPOÄOËÆÈTÜ ÏPÈÁËÈÆEHHOE PEØEHÈE HA OÄÈH ØAÃ ÄËÈHÛ H
C
  220 CALL FEHL(F,NEQN,Y,T,H,YP,F1,F2,F3,F4,F5,F1)
      NFE=NFE+5
C
C     BÛ×ÈCËÈTÜ È CPABHÈTÜ ÄOÏÓCTÈMÛE ÃPAHÈÖÛ È
C     OÖEHKÈ ËOKAËÜHOÉ OØÈÁ,A ÇATEM CHßTÜ MACØTA-
C     ÁÈPOBAHÈE ÃPAHÈÖ.ÇAMETÜTE,×TO OTHOCÈTEËÜHAß
C     OØÈÁKA ÈÇMEPßETCß ÏO OTHOØEHÈÞ K CPEÄHEMÓ ÈÇ
C     BEËÈ×ÈH PEØEHÈß B HA×AËE È KOHÖE ØAÃA.
C
      EEOET=0.
      DO 250 K=1,NEQN
      ET=ABS(Y(K))+ABS(F1(K))+AE
      IF(ET.GT.0.)GO TO 240
C
C     HEÏPABÈËÜHAß ÃPAHÈÖA ÏOÃPEØHOCTÈ
C
      IFLAG=5
      KFLAG=5
      RETURN
C
  240 EE=ABS((-2090.*YP(K)+(21970.*F3(K)-15048.*F4(K)))
     *  +(22528.*F2(K)-27360.*F5(K)))
  250 EEOET=AMAX1(EEOET,EE/ET)
C
      ESTTOL=ABS(H)*EEOET*SCALE/752400.
C
      IF(ESTTOL.LE.1.0)GO TO 260
C
C
C     HEÓÄA×HÛÉ ØAÃ
C            ÓMEHÜØÈTÜ BEËÈ×ÈHÓ ØAÃA È CHOBA ÏO-
C            ÏPOÁOBATÜ
C            ÓMEHÜØEHÈE OÃPAHÈ×ÈBAETCß CHÈÇÓ MHO-
C            ÆÈTEËEM 1/10
C
      HFAILD=.TRUE.
      OUTPUT=.FALSE.
      S=0.1
      IF(ESTTOL.LT.59049.)S=0.9/ESTTOL**0.2
      H=S*H
      IF(ABS(H).GT.HMIN)GO TO 200
C
C     ÇAÄAHHAß ÃPAHÈÖA OØÈÁKÈ HEÄOCTÈÆÈMA ÄAÆE ÏPÈ
C     HAÈMEHÜØEÉ ÄOÏÓCTÈMOÉ BEËÈ×ÈHE ØAÃA
C
      IFLAG=6
      KFLAG=6
      RETURN
C
C
C     ÓCÏEØHÛÉ ØAÃ
C            ÏOMECTÈTÜ B MACCÈB Y PEØEHÈE B TO×KE
C            T+H È BÛ×ÈCËÈTÜ ÏPOÈÇBOÄHÛE B ÝTOÉ
C            TO×KE
C
  260 T=T+H
      DO 270 K=1,NEQN
  270 Y(K)=F1(K)
      A=T
      CALL F(A,Y,YP)
      NFE=NFE+1
C
C
C     BÛÁPATÜ BEËÈ×ÈHÓ CËEÄÓÞÙEÃO ØAÃA
C     ÓBEËÈ×EHÈE OÃPAHÈ×EHO MHOÆÈTEËEM 5
C     ECËÈ HA ÄAHHOM ØAÃE ÁÛËA HEÓÄA×HAß
C     ÏOÏÛTKA,TO ÄËß CËEÄÓÞÙEÃO HE ÄOÏÓ-
C     CKAETCß BÛÁOP ÁOËÜØEÉ BEËÈ×ÈHÛ ØAÃA
C
      S=5.
      IF(ESTTOL.GT.1.889568E-4)S=0.9/ESTTOL**0.2
      IF(HFAILD)S=AMIN1(S,1.0)
      H=SIGN(AMAX1(S*ABS(H),HMIN),H)
C
C     KOHEÖ OÄHOØAÃOBOÃO ÈHTEÃPATOPA
C
C
C     HÓÆHO ËÈ ÄEËATÜ O×EPEÄHOÉ ØAÃ
C
      IF(OUTPUT)GO TO 300
      IF(IFLAG.GT.0)GO TO 100
C
C
C     ÈHTEÃPÈPOBAHÈE ÓCÏEØHO ÇABEPØEHO
C
C     PEÆÈM OÄHOØAÃOBOÃO ÈHTEÃPÈPOBAHÈß
C
      IFLAG=-2
      RETURN
C
C     PEÆÈM ÈHTEÃPÈPOBAHÈß HA ÈHTEPBAËE
C
  300 T=TOUT
      IFLAG=2
      RETURN
C
      END
