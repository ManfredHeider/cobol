      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0366.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0366.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:26:46   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:50   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-WOHNGEBAEUDEART-GESAMT.
         10 EA-NI-WOHNGEBAEUDEART-TAB.
           20 EA-NI-WOHNGEBAEUDEART-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-WOHNGEBAEUDEART
                 REDEFINES EA-NI-WOHNGEBAEUDEART-TAB.
           20 EA-NI-WGEBAEUDEARTID           PIC X.
           20 EA-NI-WGEBAEUDEARTTEXT         PIC X.
           20 EA-NI-WGEBAEUDEARTHAURAT       PIC X.
         10 EA-DCL-WOHNGEBAEUDEART.
           20 EA-WGEBAEUDEARTID              PIC -----9    .
           20 EA-WGEBAEUDEARTTEXT            PIC X(30).
           20 EA-WGEBAEUDEARTHAURAT          PIC X(30).
      ******************************************************************
