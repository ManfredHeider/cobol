      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0320.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0320.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:24:44   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:32   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KFZFAHRZEUGTYP-GESAMT.
         10 EA-NI-KFZFAHRZEUGTYP-TAB.
           20 EA-NI-KFZFAHRZEUGTYP-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-KFZFAHRZEUGTYP
                 REDEFINES EA-NI-KFZFAHRZEUGTYP-TAB.
           20 EA-NI-KFZTYPID                 PIC X.
           20 EA-NI-KFZTYPTEXT               PIC X.
         10 EA-DCL-KFZFAHRZEUGTYP.
           20 EA-KFZTYPID                    PIC -----9    .
           20 EA-KFZTYPTEXT                  PIC X(30).
      ******************************************************************
