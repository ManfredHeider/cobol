      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0712.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0712.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:30:50   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:12   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-TERMINGRUND-GESAMT.
         10 EA-NI-TERMINGRUND-TAB.
           20 EA-NI-TERMINGRUND-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-TERMINGRUND
                 REDEFINES EA-NI-TERMINGRUND-TAB.
           20 EA-NI-TERMINGRUNDID            PIC X.
           20 EA-NI-TERMINGRUNDTEXT          PIC X.
         10 EA-DCL-TERMINGRUND.
           20 EA-TERMINGRUNDID               PIC --------9 .
           20 EA-TERMINGRUNDTEXT             PIC X(30).
      ******************************************************************
