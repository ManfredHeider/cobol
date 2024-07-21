      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0643.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0643.CPv  $
      *
      *   Rev 2.4   Dec 17 2009 09:30:04   t13ae31
      *AKI00887: Central - Beiträge korrekt anzeigen
      *
      *   Rev 2.3   Jul 27 2005 13:03:28   t13ae31
      *AKI00469: Anpassungen Central-DTA
      *
      *   Rev 2.2   Jan 10 2002 14:28:30   t13ex58
      *FKI00557
      *Änderung Feldlänge LEISTUNG/V SUMME
      *von S9(5) COMP-3 -->  S9(5)V99 COMP-3 
      *
      *   Rev 2.1   Jun 21 1999 11:14:36   T13EX50
      *Anpassen an die neue Satzstruktur
      *
      *   Rev 2.0   Apr 15 1999 11:33:00   T13EX50
      *Produktionsübergabe - CKV
      *
      *   Rev 1.1   Mar 25 1999 10:36:34   T13EX50
      *Erste Version
      *
      *   Rev 1.0   25 Aug 1998 09:26:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *01.02.99!NEUERSTELLUNG                          !HERWEG   ! 01  *
      *AKI00022!                                       !         !     *
      *        !                                       !         !     *
      *31.05.99!EINBAU WHR-KZ                          !HERWEG   ! 1.1 *
      *FKI00042!                                       !         !     *
      *        !                                       !         !     *
      *08.01.02!Änderung Feldlänge  VLEIST/SBTEIL      !MORFELD  ! 1.2 *
      *FKI00557!von S9(5) COMP-3 --> S9(5)V99 COMP-3   !         !     *
      *        !                                       !         !     *
      *28.02.05!                                       !         !     *
      *AKI00469! neue DTA-Schnittstelle                ! Hübner  !     *
      *        !                                       !         !     *
      *14.12.09!                                       !         !     *
      *AKI00887! KZ Beitrag gültig eingefügt           ! Hübner  !     *
      *        !                                       !         !     *
      ******************************************************************
CCC009*    SKIP1
       01  DRKI0643.
           10 CKV-AEND-SATZ00.
              20  CKV-AE-KEY-FELDER.
280205***        25 CKV-AE-KEY-VNR         PIC X(14).
280205***        25 CKV-AE-KEY-UNR         PIC X(04).
280205         25 CKV-AE-KEY-VNR         PIC 9(09).
280205         25 CKV-AE-KEY-PNR         PIC 9(04).
280205         25 CKV-AE-KEY-PNRVN       PIC 9(04).
               25 CKV-AE-KEY-LFDNR       PIC 9(04).
      *----------------------------------------------------------------*
      *    ALLGEMEINE VERTRAGSDATEN
      *----------------------------------------------------------------*
              20  CKV-AE-SA1             PIC X.
              20  CKV-AE-SA              PIC XXX.
              20  CKV-AE-FU              PIC X.
              20  CKV-AE-GS              PIC 999.
280205        20  CKV-AE-GS-KZ           PIC X.
280205***       20  CKV-AE-SPARTE          PIC X.
280205***       20  CKV-AE-VNR             PIC X(14).
280205***       20  CKV-AE-UNR             PIC XXXX.
280205        20  CKV-AE-VNR             PIC 9(09).
280205        20  CKV-AE-PNR             PIC 9(04).
              20  CKV-AE-AKTUAL-KZ       PIC X.
280205***       20  CKV-AE-PNRVN-LFDNR     PIC 9(03).
280205        20  CKV-AE-PNRVN-LFDNR     PIC 9(04).
              20  CKV-AE-WHR             PIC X(03).
              20  CKV-AE-WHR-KZ          PIC X.
      *
      *----------------------------------------------------------------*
      *    SPEZIELLE VERTRAGSDATEN  SATZART = 01
      *----------------------------------------------------------------*
              20  CKV-AE-ROLLE-VERTRAG    PIC X(1).
              20  CKV-AE-ROLLE-VERTRAG-KZ PIC X(1).
              20  CKV-AE-ROLLE-TARIF      PIC X(1).
              20  CKV-AE-ROLLE-TARIF-KZ   PIC X(1).
280205***       20  CKV-AE-SONDBETR        PIC X(8).
280205        20  CKV-AE-SONDBETR        PIC X(11).
              20  CKV-AE-SONDBETR-KZ     PIC X(1).
              20  CKV-AE-PARTINF         PIC X(15).
              20  CKV-AE-PARTINF-KZ      PIC X(1).
280205******* Datum generell JJJJMMTT
              20  CKV-AE-VBEG            PIC 9(8).
              20  CKV-AE-VBEG-KZ         PIC X(1).
              20  CKV-AE-VABL            PIC 9(8).
              20  CKV-AE-VABL-KZ         PIC X(1).
              20  CKV-AE-VAEND           PIC 9(8).
              20  CKV-AE-VAEND-KZ        PIC X(1).
280205***       20  CKV-AE-ANREDE          PIC X.
280205        20  CKV-AE-ANREDE          PIC XX.
              20  CKV-AE-ANREDE-KZ       PIC X(1).
              20  CKV-AE-NAME.
280205***           25 CKV-AE-NNAME        PIC X(20).
280205            25 CKV-AE-NNAME        PIC X(40).
                  25 CKV-AE-NNAME-KZ     PIC X(1).
280205***           25 CKV-AE-VNAME        PIC X(10).
280205            25 CKV-AE-VNAME        PIC X(25).
                  25 CKV-AE-VNAME-KZ     PIC X(1).
              20  CKV-AE-AUSL            PIC XXX.
              20  CKV-AE-AUSL-KZ         PIC X(1).
              20  CKV-AE-PLZ             PIC X(6).
              20  CKV-AE-PLZ-KZ          PIC X(1).
280205***       20  CKV-AE-ORT             PIC X(25).
280205        20  CKV-AE-ORT             PIC X(34).
              20  CKV-AE-ORT-KZ          PIC X(1).
              20  CKV-AE-ORTZUKZ         PIC X.
              20  CKV-AE-ORTZUKZ-KZ      PIC X(1).
280205***       20  CKV-AE-STRASSE         PIC X(25).
280205        20  CKV-AE-STRASSE         PIC X(40).
              20  CKV-AE-STRASSE-KZ      PIC X(1).
280205***       20  CKV-AE-BERUF           PIC XXXX.
280205        20  CKV-AE-BERUF           PIC 9(04).
              20  CKV-AE-BERUF-KZ        PIC X(1).
              20  CKV-AE-LAST            PIC X(1).
              20  CKV-AE-LAST-KZ         PIC X(1).
              20  CKV-AE-ZAHLWEISE       PIC X.
              20  CKV-AE-ZAHLWEISE-KZ    PIC X(1).
280205***       20  CKV-AE-PNRVN           PIC 99.
280205        20  CKV-AE-PNRVN           PIC 9(04).
              20  CKV-AE-PNRVN-KZ        PIC X(1).
280205***       20  CKV-AE-NNAME-ALT       PIC X(20).
280205***       20  CKV-AE-VNAME-ALT       PIC X(10).
280205        20  CKV-AE-NNAME-ALT       PIC X(40).
280205        20  CKV-AE-VNAME-ALT       PIC X(25).
280205***       20  CKV-AE-BEITRAG-GES     PIC S9(5)V99       COMP-3.
280205        20  CKV-AE-BEITRAG-GES     PIC S9(09)V99.
              20  CKV-AE-BEITRAG-GES-KZ  PIC X(1).
280205***       20  CKV-AE-GEBDAT-P        PIC S9(7)          COMP-3.
280205        20  CKV-AE-GEBDAT-P        PIC 9(08).
              20  CKV-AE-GEBDAT-P-KZ     PIC X(1).
              20  CKV-AE-GESCHL-P        PIC X(1).
              20  CKV-AE-GESCHL-P-KZ     PIC X(1).
      *
           10 CKV-AEND-SATZ02 REDEFINES CKV-AEND-SATZ00.
      *----------------------------------------------------------------*
      *    PERSONENDATEN SATZART = 02
      *----------------------------------------------------------------*
280205***       20  FILLER                 PIC X(57).
280205***       20  CKV-AE-VORNAME         PIC X(10).
280205        20  FILLER                 PIC X(52).
280205        20  CKV-AE-VORNAME         PIC X(25).
              20  CKV-AE-VORNAME-KZ      PIC X(1).
280205***       20  CKV-AE-GEBDAT          PIC S9(7)          COMP-3.
280205        20  CKV-AE-GEBDAT          PIC 9(08).
              20  CKV-AE-GEBDAT-KZ       PIC X(1).
              20  CKV-AE-GESCHL          PIC X.
              20  CKV-AE-GESCHL-KZ       PIC X(1).
280205***       20  CKV-AE-BERUF2          PIC X(4).
280205        20  CKV-AE-BERUF3          PIC X(4).
              20  CKV-AE-BERUF3-KZ       PIC X(1).
280205***       20  CKV-AE-PERSBEG         PIC 9(6).
280205        20  CKV-AE-PERSBEG         PIC 9(08).
              20  CKV-AE-PERSBEG-KZ      PIC X(1).
280205***       20  CKV-AE-PERSABL         PIC 9(6).
280205        20  CKV-AE-PERSABL         PIC 9(08).
              20  CKV-AE-PERSABL-KZ      PIC X(1).
              20  CKV-AE-PERSAEND        PIC 9(8).
              20  CKV-AE-PERSAEND-KZ     PIC X(1).
280205***       20  CKV-AE-DSS             PIC XX.
280205        20  CKV-AE-DSS             PIC XXXX.
              20  CKV-AE-DSS-KZ          PIC X(1).
280205***       20  CKV-AE-PNRVN-P         PIC 99.
280205        20  CKV-AE-PNRVN-P         PIC 9(04).
141209        20  CKV-AE-PERSBEG-GKZ        PIC X(1).
141209        20  FILLER                 PIC X(241).
     ******       20  FILLER                 PIC X(242).
      *
           10 CKV-AEND-SATZ03 REDEFINES CKV-AEND-SATZ00.
      *----------------------------------------------------------------*
      *    TARIFDATEN SATZART = 03
      *----------------------------------------------------------------*
280205***       20  FILLER                 PIC X(57).
280205***       20  CKV-AE-ZUVST           PIC 9(6).
280205        20  FILLER                 PIC X(52).
280205        20  CKV-AE-ZUVST           PIC 9(08).
              20  CKV-AE-ZUVST-KZ        PIC X(1).
              20  CKV-AE-ZUERSTELL       PIC 9(8).
              20  CKV-AE-ZUERSTELL-KZ    PIC X(1).
280205***       20  CKV-AE-ABVST           PIC 9(6).
280205        20  CKV-AE-ABVST           PIC 9(08).
              20  CKV-AE-ABVST-KZ        PIC X(1).
280205***       20  CKV-AE-ABERSTELL       PIC 9(8).
280205***       20  CKV-AE-ABERSTELL-KZ    PIC X(1).
              20  CKV-AE-TARNAM          PIC X(8).
              20  CKV-AE-TARNAM-KZ       PIC X(1).
280205***       20  CKV-AE-TARFORM         PIC X.
280205        20  CKV-AE-TARFORM         PIC X(10).
              20  CKV-AE-TARFORM-KZ      PIC X(1).
280205***       20  CKV-AE-VLEIST          PIC S9(5)V99       COMP-3.
280205        20  CKV-AE-VLEIST          PIC S9(9)V99.
              20  CKV-AE-VLEIST-KZ       PIC X(1).
280205***       20  CKV-AE-SBTEIL          PIC S9(5)V99       COMP-3.
280205        20  CKV-AE-SBTEIL          PIC S9(9)V99.
              20  CKV-AE-SBTEIL-KZ       PIC X(1).
280205***       20  CKV-AE-BEITRAG         PIC S9(5)V99       COMP-3.
280205        20  CKV-AE-BEITRAG         PIC S9(9)V99.
              20  CKV-AE-BEITRAG-KZ      PIC X(1).
              20  CKV-AE-PRODUKT-ID      PIC 9(1).
              20  CKV-AE-PRODUKT-ID-KZ   PIC X(1).
280205***       20  CKV-AE-PNRVN-T         PIC 99.
280205        20  CKV-AE-PNRVN-T         PIC 9(4).
141209        20  CKV-AE-BEITRAG-GKZ        PIC X(1).
141209        20  FILLER                 PIC X(230).
     ******       20  FILLER                 PIC X(231).
