      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0119.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0119.CPv  $
      *
      *   Rev 2.2   Jul 27 2005 13:04:20   t13ae31
      *AKI00469: Anpassungen Central-DTA
      *
      *   Rev 2.1   Jun 21 1999 11:18:36   T13EX50
      *Anpassen an die neue Satzstruktur 
      *
      *   Rev 2.0   Feb 27 1999 12:53:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   22 Sep 1998 12:12:40   P13SP13
      *Neuerstellung
      *
      *   Rev 1.1   02 Jun 1998 14:07:24   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:48:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE CKV                          *
      *    -----------------------------------                         *
      *    VARIABLE SATZLAENGE                                         *
      *    MIN = 100 BYTES                                             *
      *    MAX = 220 BYTES                                             *
      *                                                                *
      *    N E U                                                       *
      *    Festes Satzlänge 340 Bytes                                  *
      *                                                                *
      *                                                                *
      *                                                                *
      *    ANGELEGT 11.03.1998     / HUEBNER SYSECA                    *
      *               NEUE UND GEAENDERTE DATENFELDER                  *
170998*             17.09.1998     / mta                               *
170998*               NNAME                                            *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *31.05.99! EINBAU WHR-KZ                         !HERWEG   ! 01  *
      *FKI00042!                                       !         !     *
      *        !                                       !         !     *
      *28.02.05!                                       !         !     *
      *AKI00469! neue Schnittstelle                    !Hübner   !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  CKV-SATZ1.
         02 CKV-NEU-S1.
           10  CKV-SA1                 PIC X.
           10  CKV-SA                  PIC XXX.
           10  CKV-FU                  PIC X.
           10  CKV-GS                  PIC XXX.
           10  CKV-VNR                 PIC S9(9).
           10  CKV-PNR                 PIC S9(4).
           10  CKV-TVT                 PIC S9(9).
           10  CKV-ttarnam1            PIC x(8).
           10  CKV1.
               20  CKV-SONDBETR        PIC X(11).
               20  CKV-PARTINF         PIC X(15).
               20  CKV-VBEG            PIC X(10).
               20  CKV-VABL            PIC X(10).
               20  CKV-VAEND           PIC X(10).
               20  CKV-ANREDE          PIC XX.
               20  CKV-NAME            PIC X(40).
               20  CKV-TITEL           PIC X(25).
               20  CKV-ADELSTITEL      PIC X(20).
               20  CKV-VORNAMEVN       PIC X(25).
               20  CKV-AUSL            PIC XXX.
               20  CKV-PLZ             PIC X(6).
               20  CKV-ORT             PIC X(34).
               20  CKV-ORTZUKZ         PIC X.
               20  CKV-STRASSE         PIC X(40).
               20  CKV-BERUF           PIC S9(4).
               20  FILLER              PIC S9(9).
               20  CKV-LAST            PIC X.
               20  CKV-ZAHLWEISE       PIC X.
               20  CKV-PNRVN           PIC S9(4).
               20  CKV-WHR             PIC X(3).
               20  CKV-KUNDENNR        PIC X(10).
               20  CKV-ABVERM          PIC S9(9).
               20  FILLER              PIC X(9).
          02 CKV-ALT-S1.
           10  CKV-SA1                 PIC X.
           10  CKV-SA                  PIC XXX.
           10  CKV-FU                  PIC X.
           10  CKV-GS                  PIC 999.
           10  CKV-SPARTE              PIC X.
           10  CKV-VNR                 PIC X(14).
           10  CKV-UNR                 PIC XXXX.
           10  CKV1.
               20  CKV-SONDBETR        PIC X(8).
               20  CKV-PARTINF         PIC X(15).
               20  CKV-VBEG            PIC S9(7)       COMP-3.
               20  CKV-VABL            PIC S9(7)       COMP-3.
               20  CKV-VAEND           PIC S9(7)       COMP-3.
               20  CKV-ANREDE          PIC X.
               20  CKV-NAME.
170998*-           25 CKV-N            PIC X.
170998             25 CKV-NNAME        PIC X(20).
                   25 CKV-VNAME        PIC X(10).
               20  CKV-AUSL            PIC XXX.
               20  CKV-PLZ             PIC X(6).
               20  CKV-ORT             PIC X(25).
               20  CKV-ORTZUKZ         PIC X.
               20  CKV-STRASSE         PIC X(25).
               20  CKV-BERUF           PIC XXXX.
               20  CKV-LAST            PIC X.
               20  CKV-ZAHLWEISE       PIC X.
               20  CKV-PNRVN           PIC 99.
310599         20  CKV-WHR             PIC X(3).
               20  CKV-RESERVE1        PIC X(52).
       01  CKV-SATZ2.
          02 CKV-NEU-S2.
           10 FILLER                  PIC X(38).
           10 CKV2.
             20  CKV-VORNAME         PIC X(25).
             20  CKV-GEBDAT          PIC X(10).
             20  CKV-GESCHL          PIC X.
             20  CKV-BERUF2          PIC S9(4).
             20  FILLER              PIC S9(9).
             20  CKV-PERSBEG         PIC X(10).
             20  CKV-PERSABL         PIC X(10).
             20  CKV-PERSAEND        PIC X(10).
             20  CKV-DSS             PIC S9(4).
             20  CKV-RESERVE2        PIC X(249).
          02 CKV-ALT-S2.
           10  FILLER                  PIC X(27).
           10  CKV2.
               20  CKV-VORNAME         PIC X(10).
               20  CKV-GEBDAT          PIC S9(7)          COMP-3.
               20  CKV-GESCHL          PIC X.
               20  CKV-BERUF2          PIC X(4).
               20  CKV-PERSBEG         PIC S9(5)          COMP-3.
               20  CKV-PERSABL         PIC S9(5)          COMP-3.
               20  CKV-PERSAEND        PIC S9(7)          COMP-3.
               20  CKV-DSS             PIC XX.
               20  CKV-RESERVE2        PIC X(158).
       01  CKV-SATZ3.
          02 CKV-NEU-S3.
           10 FILLER                  PIC X(38).
           10 CKV3.
            20  CKV-ZUVST           PIC X(10).
            20  CKV-ZUERSTELL       PIC X(10).
            20  CKV-ABVST           PIC X(10).
            20  CKV-TARNAM          PIC X(8).
            20  CKV-TARFORM         PIC X(10).
            20  CKV-VLEIST          PIC S9(9)V99.
            20  CKV-SBTEIL          PIC S9(9)V99.
            20  CKV-BEITRAG         PIC S9(9)V99.
            20  CKV-RESERVE3        PIC X(251).
           02 CKV-ALT-S3.
           10  FILLER                  PIC X(27).
           10  CKV3.
               20  CKV-ZUVST           PIC S9(5)          COMP-3.
               20  CKV-ZUERSTELL       PIC S9(7)          COMP-3.
               20  CKV-ABVST           PIC S9(5)          COMP-3.
               20  CKV-ABERSTELL       PIC S9(7)          COMP-3.
               20  CKV-TARNAM          PIC X(8).
               20  CKV-TARFORM         PIC X.
               20  CKV-FILLER          PIC XXXX.
310599         20  CKV-VLEIST          PIC S9(5)V99       COMP-3.
310599         20  CKV-SBTEIL          PIC S9(5)V99       COMP-3.
               20  CKV-BEITRAG         PIC S9(5)V99       COMP-3.
               20  CKV-RESERVE3        PIC X(150).

