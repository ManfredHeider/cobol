      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0321.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0321.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:24:52   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:32   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KFZLEISTUNGSART-GESAMT.
         10 EA-NI-KFZLEISTUNGSART-TAB.
           20 EA-NI-KFZLEISTUNGSART-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-KFZLEISTUNGSART
                 REDEFINES EA-NI-KFZLEISTUNGSART-TAB.
           20 EA-NI-KFZLEISTUNGSARTID        PIC X.
           20 EA-NI-KFZLEISTARTTEXT          PIC X.
         10 EA-DCL-KFZLEISTUNGSART.
           20 EA-KFZLEISTUNGSARTID           PIC -----9    .
           20 EA-KFZLEISTARTTEXT             PIC X(10).
      ******************************************************************
