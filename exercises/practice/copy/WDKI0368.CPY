      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0368.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0368.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:27:02   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:50   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ZAHLUNGSART-GESAMT.
         10 EA-NI-ZAHLUNGSART-TAB.
           20 EA-NI-ZAHLUNGSART-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-ZAHLUNGSART
                 REDEFINES EA-NI-ZAHLUNGSART-TAB.
           20 EA-NI-ZAHLUNGSARTID            PIC X.
           20 EA-NI-ZAHLUNGSARTTEXT          PIC X.
         10 EA-DCL-ZAHLUNGSART.
           20 EA-ZAHLUNGSARTID               PIC -----9    .
           20 EA-ZAHLUNGSARTTEXT             PIC X(25).
      ******************************************************************
