      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0019.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0019.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:21:02   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:04   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-EIGENTUMSNUTZART-GESAMT.
         10 EA-NI-EIGENTUMSNUTZART-TAB.
           20 EA-NI-EIGENTUMSNUTZART-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-EIGENTUMSNUTZART
                 REDEFINES EA-NI-EIGENTUMSNUTZART-TAB.
           20 EA-NI-EIGENTUMSNARTID          PIC X.
           20 EA-NI-EIGENTUMSNARTTEXT        PIC X.
         10 EA-DCL-EIGENTUMSNUTZART.
           20 EA-EIGENTUMSNARTID             PIC X(3).
           20 EA-EIGENTUMSNARTTEXT           PIC X(30).
      ******************************************************************
