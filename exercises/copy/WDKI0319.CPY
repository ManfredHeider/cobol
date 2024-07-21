      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0319.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0319.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:24:10   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:30   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KFZDECKSCHLUESSEL-GESAMT.
         10 EA-NI-KFZDECKSCHLUESSEL-TAB.
           20 EA-NI-KFZDECKSCHLUESSEL-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-KFZDECKSCHLUESSEL
                 REDEFINES EA-NI-KFZDECKSCHLUESSEL-TAB.
           20 EA-NI-KDSDECKSCHLHPFLID        PIC X.
           20 EA-NI-KDSDECKSCHLTEXT          PIC X.
         10 EA-DCL-KFZDECKSCHLUESSEL.
           20 EA-KDSDECKSCHLHPFLID           PIC X(2).
           20 EA-KDSDECKSCHLTEXT             PIC X(40).
      ******************************************************************
