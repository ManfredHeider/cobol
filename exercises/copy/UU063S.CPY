      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU063S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU063S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   May 14 1996 07:33:46   T13EX14
      *AFKD0091 Badenia neue Datenfelder
      *
      *   Rev 1.0   Nov 04 1995 09:51:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    18.05.95 13:18:56
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSSATZE BADENIA                      *
      *------------------------------------------                      *
      *                                                                *
      *    LAENGE = 290 BYTES          ERSTELLT AM 06.05.82.           *
      *                                HUEBNER (GEDO)                  *
      *                                                                *
      *       GEAENDERT AM 20.10.83: BAD-BEITRAG PIC S9(5)V99 COMP-3   *
      *                              (VORHER:    PIC  9(5)V99 COMP-3)  *
      *                                                                *
      *       GEAENDERT AM 10.01.85: BAD-VSUM    PIC S9(7)    COMP-3   *
      *       BIEHL                  (VORHER:    PIC  9(7)    COMP-3)  *
      *                              BAD-DARL    PIC S9(7)    COMP-3   *
      *                              (VORHER:    PIC  9(7)    COMP-3)  *
      *                                                                *
      *       GEAENDERT AM 16.10.86: BAD-PARTINFO PIC X(20)   NEU      *
      *       BIEHL                  BAD-LAUFZ    PIC XX      NEU      *
      *                                                                *
      *       GEAENDERT AM 13.01.89: BAD-BWZAHL  PIC S99V9(5) COMP-3   *
      *       HECKER                 NEU                               *
      *                              FILLER      PIC X(5)              *
      *                              (VORHER:    PIC X(9))             *
      *                                                                *
      *       GEAENDERT AM 25.05.93 / BORK                             *
      *       ÄNDERUNG WEGEN UMSTELLUNG AUF DIE NEUE, 5-STELLIGE PLZ   *
      *       KEINE ÄNDERUNGEN VORGENOMMEN!!!                          *
      *                                                                *
      *       GEAENDERT AM 17.03.95 / W.SCHMIDT                        *
      *       GEPACKTE FELDER SIND MIT VORZEICHEN VERSEHEN WORDEN.     *
      *                                                                *
      *       AFKD0091                                                 *
      *       GEAENDERT AM 14.05.96 / W.SCHMIDT                        *
      *       Felder entfernt und neue Felder eingebaut.               *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU063S.
           10  BAD-VORS.
               20  BAD-SA                 PIC XX.
               20  BAD-DAT                PIC 9(6).
               20  BAD-TEXT               PIC X(10).
               20  FILLER                 PIC X(272).
           10  BAD-SATZ      REDEFINES      BAD-VORS.
140596         20  BAD-SA-HAUPTS          PIC XX.
140596*        20  FILLER                 PIC XX.
140596*        20  BAD-MC                 PIC X(16).
               20  BAD-VNR                PIC S9(15)          COMP-3.
               20  BAD-GES                PIC 99.
               20  BAD-ANR                PIC 9.
               20  FILLER REDEFINES BAD-ANR.
                   25  BAD-ANR-A          PIC X.
               20  BAD-NAME               PIC X(60).
               20  BAD-STR                PIC X(26).
               20  BAD-AUSL               PIC XXX.
               20  BAD-PLZ                PIC X(5).
               20  BAD-ORT                PIC X(21).
               20  BAD-GEBD               PIC S9(7)          COMP-3.
               20  BAD-VBNR               PIC S9(7)          COMP-3.
140596*        20  BAD-VDAT               PIC S9(7)          COMP-3.
               20  BAD-ZAHLW              PIC X.
               20  BAD-MAHNKZ             PIC 9.
140596*        20  BAD-MAHNDAT            PIC S9(7)          COMP-3.
               20  BAD-VBEG               PIC S9(7)          COMP-3.
140596*        20  BAD-VABL               PIC S9(7)          COMP-3.
140596*        20  BAD-VZUST              PIC X.
               20  BAD-BERUF              PIC S9(5)          COMP-3.
140596*        20  BAD-DATVAEND           PIC S9(7)          COMP-3.
               20  BAD-PROD               PIC 99.
               20  BAD-VSUM               PIC S9(7)          COMP-3.
               20  BAD-GHABEN             PIC S9(7)V99       COMP-3.
               20  BAD-BEITRAG            PIC S9(5)V99       COMP-3.
               20  BAD-DARL               PIC S9(7)          COMP-3.
140596*        20  BAD-ARBGEBER.
140596*            30  BAD-FIRMA          PIC X(20).
140596*            30  BAD-FANSCHR        PIC X(33).
140596*        20  BAD-VNRALT             PIC 9(15)          COMP-3.
               20  BAD-KONTO              PIC S9             COMP-3.
               20  BAD-PARTINFO           PIC X(20).
               20  BAD-LAUFZ              PIC XX.
               20  FILLER REDEFINES  BAD-LAUFZ.
                   25 BAD-LAUFZ-N         PIC 99.
               20  BAD-BWZAHL             PIC S99V9(5)       COMP-3.

140596         20  BAD-TARIF-T3           PIC X(01).
140596         20  BAD-ZINS-ALLG          PIC S9(3)V99       COMP-3.
140596         20  BAD-ZINS-DARLEHEN      PIC S9(3)V99       COMP-3.
140596         20  BAD-DISAGIO            PIC X(1).
140596         20  BAD-FREI-BETRAG        PIC S9(5)V99       COMP-3.
140596         20  BAD-VFREI-BETRAG       PIC S9(5)V99       COMP-3.
140596         20  BAD-FEHL-BETRAG        PIC S9(7)          COMP-3.
140596         20  BAD-ENDE-ST-LAUFZEIT   PIC S9(9)          COMP-3.
140596         20  BAD-UMS-TABELLE.
140596             30  BAD-UMS-TAB        OCCURS 5.
140596                 40 BAD-UMS-BETRAG  PIC S9(7)V99       COMP-3.
140596                 40 BAD-UMS-SCHL    PIC S9(3)          COMP-3.
140596                 40 BAD-UMS-DATUM   PIC S9(7)          COMP-3.
140596         20  BAD-DATUM-WOHN         PIC S9(9)          COMP-3.
140596         20  BAD-DATUM-ZUTEIL       PIC S9(9)          COMP-3.
140596         20  BAD-BLAND              PIC X(2).
140596         20  BAD-FEHL-BETRAG-DATUM  PIC S9(9)          COMP-3.
               20  FILLER                 PIC X(2).

           10  BAD-NACHS     REDEFINES      BAD-VORS.
140596         20  BAD-SA-NACHS           PIC XX.
140596*        20  FILLER                 PIC XX.
               20  BAD-ANZ                PIC 9(7).
               20  FILLER                 PIC X(281).

