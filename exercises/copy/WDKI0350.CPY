      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0350.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0350.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:26:14   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:42   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERSLEISTUNG-GESAMT.
         10 EA-NI-VERSLEISTUNG-TAB.
           20 EA-NI-VERSLEISTUNG-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-VERSLEISTUNG
                 REDEFINES EA-NI-VERSLEISTUNG-TAB.
           20 EA-NI-VERSLEISTUNGID           PIC X.
           20 EA-NI-VERSLEISTUNGTEXT         PIC X.
         10 EA-DCL-VERSLEISTUNG.
           20 EA-VERSLEISTUNGID              PIC -----9    .
           20 EA-VERSLEISTUNGTEXT            PIC X(30).
      ******************************************************************
