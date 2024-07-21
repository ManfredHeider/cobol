      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0850.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRKI0850.CPv  $
      *
      *   Rev 2.3   Sep 29 1999 16:46:34   T13EX47
      *AKI00052
      *
      *   Rev 2.2   Apr 30 1999 18:25:58   T13EX47
      *das S bei Datum S9 raus. Filler geaendert
      *
      *   Rev 2.1   Apr 28 1999 14:11:42   T13EX47
      *neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 12:58:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:52:46   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER FUER BESTANDSAETZE DREBA 44                     *
      *    SATZLAENGE: 330                                             *

150999 01  DRKI0850.

           10  DREBA-SATZ.

             15    DREBA-ORG.
               20  DREBA-SA            PIC X(001).
      * Verarbeitungs-Kennzeichen:
      *   I=Insert
      *   U=Update
      *   D=Delete
               20  DREBA-VERARB-KZ     PIC X(001).

             15    DREBA-KEY.
               20  DREBA-GES           PIC 9(002).
               20  DREBA-VNR           PIC 9(015).

             15    DREBA-INHALT.
               20  DREBA-MC            PIC X(017).
               20  DREBA-MC-KZ         PIC x.
               20  DREBA-VBNR          PIC 9(007).
               20  DREBA-VBNR-KZ       PIC x.
               20  DREBA-ANR-ALT       PIC 9(001).
               20  DREBA-ANR-NEU       PIC 9(001).
               20  DREBA-ANR-KZ        PIC x.
               20  DREBA-ZUNAME-ALT    PIC X(030).
               20  DREBA-ZUNAME-NEU    PIC X(030).
               20  DREBA-ZUNAME-KZ     PIC x.
               20  DREBA-VNAME-ALT     PIC X(030).
               20  DREBA-VNAME-NEU     PIC X(030).
170            20  DREBA-VNAME-KZ      PIC x.
               20  DREBA-STR           PIC X(026).
               20  DREBA-STR-KZ        PIC x.
               20  DREBA-AUSL          PIC X(003).
               20  DREBA-AUSL-KZ       PIC x.
               20  DREBA-PLZ           PIC X(005).
               20  DREBA-PLZ-KZ        PIC x.
               20  DREBA-ORT           PIC X(021).
               20  DREBA-ORT-KZ        PIC x.
               20  DREBA-GEBD          PIC 9(007).
236            20  DREBA-GEBD-KZ       PIC x.
               20  DREBA-BERUF         PIC X(004).
               20  DREBA-BERUF-KZ      PIC x.
               20  DREBA-ZAHLW         PIC X(001).
               20  DREBA-ZAHLW-KZ      PIC x.
               20  DREBA-VBEG          PIC 9(007).
               20  DREBA-VBEG-KZ       PIC x.
               20  DREBA-VABL          PIC 9(007).
               20  DREBA-VABL-KZ       PIC x.
               20  DREBA-VAEND         PIC 9(007).
               20  DREBA-VAEND-KZ      PIC x.
               20  DREBA-PRODSCHL      PIC X(002).
               20  DREBA-PRODSCHL-KZ   PIC x.
               20  DREBA-KONTOST       PIC S9(7)V9(2).
               20  DREBA-KONTOST-KZ    PIC x.
               20  DREBA-ANTEILE       PIC S9(6)V9(3).
               20  DREBA-ANTEILE-KZ    PIC x.
               20  DREBA-RUECKNAHME    PIC S9(7)V9(2).
301            20  DREBA-RUECKNAHME-KZ PIC x.
330            20  DREBA-FILLER-1      PIC X(029).

           10  DREBA-VORSATZ           REDEFINES DREBA-SATZ.
               20  DREBA-V-SA          PIC X(001).
               20  DREBA-V-ABSEND      PIC X(027).
               20  DREBA-V-ERSTELLDAT  PIC 9(006).
               20  DREBA-V-DATSTAND    PIC 9(006).
               20  DREBA-V-EMPFAENG    PIC X(027).
087            20  DREBA-V-INHALT      PIC X(020).
330            20  DREBA-FILLER-2      PIC X(243).

           10  DREBA-NACHSATZ          REDEFINES DREBA-SATZ.
               20  DREBA-N-SA          PIC X(001).
               20  DREBA-N-ANZ         PIC 9(007).
               20  DREBA-N-KONTROLL1   PIC 9(017).
               20  DREBA-N-KONTROLL2   PIC 9(017).
               20  DREBA-N-KONTROLL3   PIC 9(017).
               20  DREBA-N-KONTROLL4   PIC 9(017).
093            20  DREBA-N-KONTROLL5   PIC 9(017).
330            20  DREBA-FILLER-3      PIC X(237).



