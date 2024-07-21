      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU082S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU082S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:54:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    28.07.93 15:36:00
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE DREBA 44                     *
      *    ---------------------------------------                     *
      *                                                                *
      *    LAENGE = 365 BYTE         ERSTELLT AM 14.05.92.             *
      *             FIX              MUELLER (FFM.)                    *
      *                                                                *
      *    GEÄNDERT 25.05.93 / BORK                                    *
      *    ÄNDERUNG WEGEN DER UMSTELLUNG AUF DIE NEUE, 5-STELLIGE PLZ. *
      *    DIE FELDER POSTEMPFÄNGER WERDEN VON DREBA NICHT GEFÜLLT.    *
      *    AB DER NEUEN VERSION AUSGESTERNT UND FILLER VERGRÖSSERT.    *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU082S.
           10  DREBA-SATZ.
               20  DREBA-SA            PIC X.
               20  DREBA-GES           PIC 99.
               20  DREBA-VNR           PIC S9(15)         COMP-3.
               20  DREBA-MC            PIC X(17).
               20  DREBA-VBNR          PIC 9(7).
               20  DREBA-ANR           PIC 9.
               20  DREBA-ZUNAME        PIC X(30).
               20  DREBA-VNAME         PIC X(30).
               20  DREBA-STR           PIC X(26).
               20  DREBA-AUSL          PIC XXX.
               20  DREBA-PLZ           PIC X(5).
               20  DREBA-ORT           PIC X(21).
               20  DREBA-GEBD          PIC S9(7)          COMP-3.
               20  DREBA-BERUF         PIC X(4).
               20  DREBA-ZAHLW         PIC X.
               20  DREBA-VBEG          PIC S9(7)          COMP-3.
               20  DREBA-VABL          PIC S9(7)          COMP-3.
               20  DREBA-VAEND         PIC S9(7)          COMP-3.
               20  DREBA-PRODSCHL      PIC XX.
               20  DREBA-KONTOST       PIC S9(7)V99       COMP-3.
               20  DREBA-ANTEILE       PIC S9(6)V999      COMP-3.
               20  DREBA-RUECKNAHME    PIC S9(7)V99       COMP-3.
250593***      20  DREBA-POSTEMPF.
250593***          30 DREBA-POSTNAM    PIC X(36).
250593***          30 DREBA-POSTZUS    PIC X(21).
250593***          30 DREBA-POSTSTR    PIC X(25).
250593***          30 DREBA-POSTPLZ    PIC X(4).
250593***          30 DREBA-POSTORT    PIC X(25).
250593***      20  DREBA-REST          PIC X(65).
               20  FILLER              PIC X(176).
           10  DREBA-VORSATZ           REDEFINES DREBA-SATZ.
               20  DREBA-V-SA          PIC X.
               20  DREBA-V-ABSEND      PIC X(27).
               20  DREBA-V-ERSTELLDAT  PIC 9(6).
               20  DREBA-V-DATSTAND    PIC 9(6).
               20  DREBA-V-EMPFAENG    PIC X(27).
               20  DREBA-V-INHALT      PIC X(20).
           10  DREBA-NACHSATZ          REDEFINES DREBA-SATZ.
               20  DREBA-N-SA          PIC X.
               20  DREBA-N-ANZ         PIC 9(7)           COMP-3.
               20  DREBA-N-KONTROLL1   PIC 9(17)          COMP-3.
               20  DREBA-N-KONTROLL2   PIC 9(17)          COMP-3.
               20  DREBA-N-KONTROLL3   PIC 9(17)          COMP-3.
               20  DREBA-N-KONTROLL4   PIC 9(17)          COMP-3.
               20  DREBA-N-KONTROLL5   PIC 9(17)          COMP-3.

