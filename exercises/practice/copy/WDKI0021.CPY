      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0021.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0021.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:21:10   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:04   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-FAMILIENSTAND-GESAMT.
         10 EA-NI-FAMILIENSTAND-TAB.
           20 EA-NI-FAMILIENSTAND-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-FAMILIENSTAND
                 REDEFINES EA-NI-FAMILIENSTAND-TAB.
           20 EA-NI-FAMILIENSTANDID          PIC X.
           20 EA-NI-FAMILIENSTANDTEXT        PIC X.
         10 EA-DCL-FAMILIENSTAND.
           20 EA-FAMILIENSTANDID             PIC -----9    .
           20 EA-FAMILIENSTANDTEXT           PIC X(30).
      ******************************************************************
