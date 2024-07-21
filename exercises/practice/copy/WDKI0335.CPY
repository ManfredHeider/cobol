      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0335.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0335.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:25:34   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:36   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-SPARTE-GESAMT.
         10 EA-NI-SPARTE-TAB.
           20 EA-NI-SPARTE-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-SPARTE
                 REDEFINES EA-NI-SPARTE-TAB.
           20 EA-NI-SPARTEID                 PIC X.
           20 EA-NI-SPARTETEXT               PIC X.
           20 EA-NI-SPARTEKURZTEXT           PIC X.
         10 EA-DCL-SPARTE.
           20 EA-SPARTEID                    PIC -----9    .
           20 EA-SPARTETEXT                  PIC X(30).
           20 EA-SPARTEKURZTEXT              PIC X(04).
      ******************************************************************
