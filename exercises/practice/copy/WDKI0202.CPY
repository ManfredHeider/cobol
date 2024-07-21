      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0202.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0202.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:23:06   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:24   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-MAGAZINSTATUS-GESAMT.
         10 EA-NI-MAGAZINSTATUS-TAB.
           20 EA-NI-MAGAZINSTATUS-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-MAGAZINSTATUS
                 REDEFINES EA-NI-MAGAZINSTATUS-TAB.
           20 EA-NI-STATUSID                 PIC X.
           20 EA-NI-STATUSTEXT               PIC X.
         10 EA-DCL-MAGAZINSTATUS.
           20 EA-STATUSID                    PIC -----9    .
           20 EA-STATUSTEXT                  PIC X(90).
      ******************************************************************
