      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0052.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0052.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:21:42   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:14   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-TITEL-GESAMT.
         10 EA-NI-TITEL-TAB.
           20 EA-NI-TITEL-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-TITEL
                 REDEFINES EA-NI-TITEL-TAB.
           20 EA-NI-TITELID                  PIC X.
           20 EA-NI-TITELTEXT                PIC X.
         10 EA-DCL-TITEL.
           20 EA-TITELID                     PIC -----9    .
           20 EA-TITELTEXT                   PIC X(15).
      ******************************************************************
