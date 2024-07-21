      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0069.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0069.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:22:20   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:20   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-FIRMENBEZART-GESAMT.
         10 EA-NI-FIRMENBEZART-TAB.
           20 EA-NI-FIRMENBEZART-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-FIRMENBEZART
                 REDEFINES EA-NI-FIRMENBEZART-TAB.
           20 EA-NI-FIRMENBEZARTID           PIC X.
           20 EA-NI-FIRMENBEZARTTEXT         PIC X.
         10 EA-DCL-FIRMENBEZART.
           20 EA-FIRMENBEZARTID              PIC -----9    .
           20 EA-FIRMENBEZARTTEXT            PIC X(20).
      ******************************************************************
