      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0011.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0011.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:20:30   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:02   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BERUF-GESAMT.
         10 EA-NI-BERUF-TAB.
           20 EA-NI-BERUF-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-BERUF
                 REDEFINES EA-NI-BERUF-TAB.
           20 EA-NI-BERUFID                  PIC X.
           20 EA-NI-BERUFTEXT                PIC X.
           20 EA-NI-AMBERUFSCHLUESSEL        PIC X.
         10 EA-DCL-BERUF.
           20 EA-BERUFID                     PIC --------9 .
           20 EA-BERUFTEXT                   PIC X(80).
           20 EA-AMBERUFSCHLUESSEL           PIC X(6).
      ******************************************************************