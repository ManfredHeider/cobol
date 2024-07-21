      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU062S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU062S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:51:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE AM-LEBEN                     *
      *    ---------------------------------------                     *
      *                                                                *
      *    LAENGE = 460 BYTES        ERSTELLT AM 05.05.82.             *
      *                              HUEBNER (GEDO)                    *
      *                                                                *
      *    GEAENDERT OKTOBER 83      HUEBNER (GEDO)                    *
      *                              SATZERWEITERUNG AUF 500 BYTES     *
      *                                                                *
      *    GEAENDERT 28.02.1985      BIEHL                             *
      *                              AML-TARIF 10-STELLIG (VORHER 7-ST)*
      *                              FILLER UM 3 STELLEN GEKUERZT      *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU062S.
           10  AML-VORSATZ.
               20  AML-VSA             PIC XX.
               20  AML-DATUM           PIC 9(6).
               20  AML-TEXT            PIC X(10).
               20  FILLER              PIC X(482).
           10  AML-SATZ1    REDEFINES      AML-VORSATZ.
               20  AML-SA              PIC 999.
               20  AML-ORG             PIC 999.
               20  AML-MC              PIC X(13).
               20  AML-VNR.
                   30  AML-SPARTE      PIC 9.
                   30  AML-VSNR        PIC 9(7).
                   30  AML-UNR         PIC 99.
               20  FILLER              PIC X(8).
               20  AML-PSTAT1          PIC 9.
               20  AML-ANR1            PIC 9.
               20  AML-NAME1           PIC X(34).
               20  AML-GEBD1           PIC 9(6).
               20  AML-AUSL            PIC XXX.
               20  AML-PLZ1            PIC X(5).
               20  AML-ORT1            PIC X(21).
               20  AML-STR1            PIC X(25).
               20  AML-BERUF1          PIC 999.
               20  AML-VBEG            PIC 9(4).
               20  AML-VABL            PIC 9(4).
               20  AML-IART            PIC 99.
               20  AML-AGENT           PIC 9(4).
               20  AML-RBTG            PIC S9(5)V99       COMP-3.
               20  AML-ZAHLW           PIC 9.
               20  AML-TARIF           PIC X(10).
               20  AML-BTGZUST         PIC 9.
               20  AML-BTG             PIC 9(4).
               20  AML-VSR             PIC S9(9)          COMP-3.
               20  AML-ERSCHW          PIC XX.
               20  AML-VERFUEG         PIC XX.
               20  AML-VZBETR          PIC S9(7)V99       COMP-3.
               20  FILLER              PIC X(8).
               20  AML-PSTAT2          PIC 9.
               20  AML-ANR2            PIC 9.
               20  AML-NAME2           PIC X(34).
               20  AML-GEBD2           PIC 9(6).
               20  AML-AUSL2           PIC XXX.
               20  AML-PLZ2            PIC X(5).
               20  AML-ORT2            PIC X(21).
               20  AML-STR2            PIC X(25).
               20  AML-BERUF2          PIC 999.
               20  FILLER              PIC X(8).
               20  AML-PSTAT3          PIC 9.
               20  AML-ANR3            PIC 9.
               20  AML-NAME3           PIC X(34).
               20  AML-GEBD3           PIC 9(6).
               20  AML-AUSL3           PIC XXX.
               20  AML-PLZ3            PIC X(5).
               20  AML-ORT3            PIC X(21).
               20  AML-STR3            PIC X(25).
               20  AML-BERUF3          PIC 999.
               20  FILLER              PIC X(8).
               20  AML-PSTAT4          PIC X.
               20  AML-ANR4            PIC 9.
               20  AML-ARBGEB          PIC X(20).
               20  AML-FANSCHR         PIC X(33).
               20  AML-MAHNKZ          PIC 9.
               20  AML-MAHNDAT         PIC 9(6).
               20  AML-PROVBEG         PIC 9(4).
               20  AML-PROVABL         PIC 9(4).
               20  AML-AVM             PIC S9(7)       COMP-3.
               20  FILLER              PIC X(20).
           10  AML-SATZ2       REDEFINES     AML-VORSATZ.
               20  FILLER              PIC XXX.
               20  AML-DVSU            PIC S9(18).
               20  AML-DJBEITR         PIC S9(16)V99.
               20  AML-DRBEITR         PIC S9(16)V99.
               20  AML-DVZBETR         PIC S9(16)V99.
               20  FILLER              PIC X(425).
           10  AML-NACHSATZ    REDEFINES     AML-VORSATZ.
               20  FILLER              PIC XX.
               20  AML-ANZ             PIC 9(7).
               20  FILLER              PIC X(491).

