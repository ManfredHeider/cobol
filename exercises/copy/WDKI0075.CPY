      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0075.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0075.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:22:48   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:22   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PERSNAMAENDGRUND-GESAMT.
         10 EA-NI-PERSNAMAENDGRUND-TAB.
           20 EA-NI-PERSNAMAENDGRUND-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-PERSNAMAENDGRUND
                 REDEFINES EA-NI-PERSNAMAENDGRUND-TAB.
           20 EA-NI-PNAGRUNDID               PIC X.
           20 EA-NI-PNAGRUNDTEXT             PIC X.
         10 EA-DCL-PERSNAMAENDGRUND.
           20 EA-PNAGRUNDID                  PIC -----9    .
           20 EA-PNAGRUNDTEXT                PIC X(30).
      ******************************************************************
