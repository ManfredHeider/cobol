      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0707.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0707.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:30:02   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:10   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-DOKUMENTART-GESAMT.
         10 EA-NI-DOKUMENTART-TAB.
           20 EA-NI-DOKUMENTART-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-DOKUMENTART
                 REDEFINES EA-NI-DOKUMENTART-TAB.
           20 EA-NI-DOKUMENTARTID            PIC X.
           20 EA-NI-DOKUMENTARTTEXT          PIC X.
         10 EA-DCL-DOKUMENTART.
           20 EA-DOKUMENTARTID               PIC --------9 .
           20 EA-DOKUMENTARTTEXT             PIC X(30).
      ******************************************************************
