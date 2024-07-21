      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0012.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0012.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:20:38   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:02   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BERUFSGRUPPE-GESAMT.
         10 EA-NI-BERUFSGRUPPE-TAB.
           20 EA-NI-BERUFSGRUPPE-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-BERUFSGRUPPE
                 REDEFINES EA-NI-BERUFSGRUPPE-TAB.
           20 EA-NI-BERUFSGRUPPEID           PIC X.
           20 EA-NI-BERUFSGRUPPETEXT         PIC X.
         10 EA-DCL-BERUFSGRUPPE.
           20 EA-BERUFSGRUPPEID              PIC -----9    .
           20 EA-BERUFSGRUPPETEXT            PIC X(20).
      ******************************************************************
