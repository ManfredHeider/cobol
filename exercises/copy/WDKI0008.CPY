      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0008.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0008.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 15:20:16   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabelle
      *
      *   Rev 2.0   Apr 15 1999 13:32:00   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BANKKONTOART-GESAMT.
         10 EA-NI-BANKKONTOART-TAB.
           20 EA-NI-BANKKONTOART-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-BANKKONTOART
                 REDEFINES EA-NI-BANKKONTOART-TAB.
           20 EA-NI-BANKKTOARTID             PIC X.
           20 EA-NI-BANKKTOARTTEXT           PIC X.
         10 EA-DCL-BANKKONTOART.
           20 EA-BANKKTOARTID                PIC -----9    .
           20 EA-BANKKTOARTTEXT              PIC X(20).
      ******************************************************************