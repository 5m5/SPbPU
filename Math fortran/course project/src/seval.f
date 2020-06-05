      REAL FUNCTION SEVAL(N,U,X,Y,B,C,D)
      INTEGER N
      REAL U,X(N),Y(N),B(N),C(N),D(N)
C
C  �TA �O��PO�PAMMA B���C��ET �HA�EH�E K����ECKO�O
C  C��A�HA
C
C  SEVAL=Y(I)+B(I)*(U-X(I))+C(I)*(U-X(I))**2+
C             D(I)*(U-X(I))**3
C
C  ��E X(I).LT.U.LT.X(I+1). �C�O����ETC� CXEMA
C  �OPHEPA
C
C  EC�� U.LT.X(1), TO �EPETC� �HA�EH�E I=1.
C  EC�� U.GE.X(N), TO �EPETC� �HA�EH�E I=N.
C
C  BXO�HA� �H�OPMA���.
C
C     N     -��C�O �A�AHH�X TO�EK
C     U     -A�C��CCA, ��� KOTOPO� B���C��ETC� �HA�EH�E
C            C��A�HA
C     X,Y   -MACC�B� �A�AHH�X A�C��CC � OP��HAT
C     B,C,D -MACC�B� KO������EHTOB C��A�HA, B���C�EHH�E
C            �O��PO�PAMMO� SPLINE
C
C  EC�� �O CPABHEH�� C �PE������M B��OBOM U HE
C  HAXO��TC� B TOM �E �HTEPBA�E, TO ��� PA��CKAH��
C  H��HO�O �HTEPBA�A �P�MEH�ETC� �BO��H�� �O�CK.
C
      INTEGER I,J,K
      REAL DX
      DATA I/1/
      IF(I.GE.N) I=1
      IF(U.LT.X(I)) GO TO 10
      IF(U.LE.X(I+1)) GO TO 30
C
C  �BO��H�� �O�CK
C
 10   I=1
      J=N+1
 20   K=(I+J)/2
      IF(U.LT.X(K))J=K
      IF(U.GE.X(K))I=K
      IF(J.GT.I+1)GO TO 20
C
C  B���C��T� C��A�H
C
 30   DX=U-X(I)
      SEVAL=Y(I)+DX*(B(I)+DX*(C(I)+DX*D(I)))
      RETURN
      END
