      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0014.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0014.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:20:46   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:02   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BUNDESLAND-GESAMT.
         10 EA-NI-BUNDESLAND-TAB.
           20 EA-NI-BUNDESLAND-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-BUNDESLAND
                 REDEFINES EA-NI-BUNDESLAND-TAB.
           20 EA-NI-BUNDESLANDID             PIC X.
           20 EA-NI-BUNDESLANDLANDID         PIC X.
           20 EA-NI-BUNDESLANDTEXT           PIC X.
           20 EA-NI-BUNDESLANDKIST           PIC X.
         10 EA-DCL-BUNDESLAND.
           20 EA-BUNDESLANDID                PIC X(3).
           20 EA-BUNDESLANDLANDID            PIC X(3).
           20 EA-BUNDESLANDTEXT              PIC X(30).
           20 EA-BUNDESLANDKIST              PIC ---------9.99    .
      ******************************************************************
