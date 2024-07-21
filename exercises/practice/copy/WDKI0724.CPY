      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0724.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0724.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:31:14   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:16   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-TMPSELSCHRITTE-GESAMT.
         10 EA-NI-TMPSELSCHRITTE-TAB.
           20 EA-NI-TMPSELSCHRITTE-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-TMPSELSCHRITTE
                 REDEFINES EA-NI-TMPSELSCHRITTE-TAB.
           20 EA-NI-SCHRITTNUMMER            PIC X.
           20 EA-NI-SCHRITTKRITERIUM         PIC X.
         10 EA-DCL-TMPSELSCHRITTE.
           20 EA-SCHRITTNUMMER               PIC -----9    .
           20 EA-SCHRITTKRITERIUM            PIC -----9    .
      ******************************************************************
