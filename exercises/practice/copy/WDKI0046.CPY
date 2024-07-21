      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0046.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0046.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:21:30   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:12   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PERSBEZIEHUNGSART-GESAMT.
         10 EA-NI-PERSBEZIEHUNGSART-TAB.
           20 EA-NI-PERSBEZIEHUNGSART-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-PERSBEZIEHUNGSART
                 REDEFINES EA-NI-PERSBEZIEHUNGSART-TAB.
           20 EA-NI-PBEZARTID                PIC X.
           20 EA-NI-PBEZARTKATEGORIE         PIC X.
           20 EA-NI-PBEZARTTEXT              PIC X.
           20 EA-NI-PBEZARTINVBEZARTID       PIC X.
         10 EA-DCL-PERSBEZIEHUNGSART.
           20 EA-PBEZARTID                   PIC -----9    .
           20 EA-PBEZARTKATEGORIE            PIC X(01).
           20 EA-PBEZARTTEXT                 PIC X(15).
           20 EA-PBEZARTINVBEZARTID          PIC -----9    .
      ******************************************************************
