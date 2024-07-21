      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU061S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU061S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:50:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE CKV                          *
      *    -----------------------------------                         *
      *    VARIABLE SATZLAENGE  (ALTE SATZLAENGE = 104 BYTE)           *
      *    MIN = 100 BYTES           ERSTELLT AM 04.05.82.             *
      *    MAX = 200 BYTES           HUEBNER (GEDO)                    *
      *                                                                *
      *         GEAENDERT 14.04.86 / BIEHL:                            *
      *               NEUE UND GEAENDERTE DATENFELDER                  *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  CKV-SATZ1.
           10  CKV-SA1                 PIC X.
           10  CKV-SA                  PIC XXX.
           10  CKV-FU                  PIC X.
           10  CKV-GS                  PIC XXX.
           10  CKV-VNR                 PIC X(15).
           10  CKV-UNR                 PIC XXXX.
           10  CKV1.
               20  CKV-SONDBETR        PIC X(8).
               20  CKV-PARTINF         PIC X(15).
               20  CKV-VBEG            PIC S9(7)       COMP-3.
               20  CKV-VABL            PIC S9(7)       COMP-3.
               20  CKV-VAEND           PIC S9(7)       COMP-3.
               20  CKV-ANREDE          PIC X.
               20  CKV-NAME            PIC X(30).
               20  CKV-AUSL            PIC XXX.
               20  CKV-PLZ             PIC X(6).
               20  CKV-ORT             PIC X(25).
               20  CKV-ORTZUKZ         PIC X.
               20  CKV-STRASSE         PIC X(25).
               20  CKV-BERUF           PIC XXXX.
               20  CKV-LAST            PIC X.
               20  CKV-RESERVE1        PIC X(42).
       01  CKV-SATZ2.
           10  FILLER                  PIC X(27).
           10  CKV2.
               20  CKV-VORNAME         PIC X(10).
               20  CKV-GEBDAT          PIC S9(7)          COMP-3.
               20  CKV-GESCHL          PIC X.
               20  CKV-BERUF2          PIC X(4).
               20  CKV-PERSBEG         PIC S9(5)          COMP-3.
               20  CKV-PERSABL         PIC S9(5)          COMP-3.
               20  CKV-PERSAEND        PIC S9(7)          COMP-3.
               20  CKV-RESERVE2        PIC X(44).
       01  CKV-SATZ3.
           10  FILLER                  PIC X(27).
           10  CKV3.
               20  CKV-ZUVST           PIC S9(5)          COMP-3.
               20  CKV-ZUERSTELL       PIC S9(7)          COMP-3.
               20  CKV-ABVST           PIC S9(5)          COMP-3.
               20  CKV-ABERSTELL       PIC S9(7)          COMP-3.
               20  CKV-TARNAM          PIC X(8).
               20  CKV-TARFORM         PIC X(5).
               20  CKV-VLEIST          PIC S9(5)          COMP-3.
               20  CKV-SBTEIL          PIC S9(5)          COMP-3.
               20  CKV-BEITRAG         PIC S9(5)V99       COMP-3.
               20  CKV-RESERVE3        PIC X(36).

