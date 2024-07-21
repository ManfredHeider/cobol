      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0305.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0305.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:23:26   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:26   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BUCHUNGSTEXT-GESAMT.
         10 EA-NI-BUCHUNGSTEXT-TAB.
           20 EA-NI-BUCHUNGSTEXT-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-BUCHUNGSTEXT
                 REDEFINES EA-NI-BUCHUNGSTEXT-TAB.
           20 EA-NI-BUCHUNGSTEXTID           PIC X.
           20 EA-NI-BUCHUNGSTEXT             PIC X.
         10 EA-DCL-BUCHUNGSTEXT.
           20 EA-BUCHUNGSTEXTID              PIC -----9    .
           20 EA-BUCHUNGSTEXT                PIC X(40).
      ******************************************************************
