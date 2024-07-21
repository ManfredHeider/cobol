      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0057.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0057.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:21:48   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:16   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ZIELWUNSCHTEXT-GESAMT.
         10 EA-NI-ZIELWUNSCHTEXT-TAB.
           20 EA-NI-ZIELWUNSCHTEXT-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-ZIELWUNSCHTEXT
                 REDEFINES EA-NI-ZIELWUNSCHTEXT-TAB.
           20 EA-NI-ZWTEXTID                 PIC X.
           20 EA-NI-ZWGUELTIGKZ              PIC X.
           20 EA-NI-ZWTEXT                   PIC X.
         10 EA-DCL-ZIELWUNSCHTEXT.
           20 EA-ZWTEXTID                    PIC -----9    .
           20 EA-ZWGUELTIGKZ                 PIC X(01).
           20 EA-ZWTEXT.
              49 EA-ZWTEXT-LEN               PIC -----9    .
              49 EA-ZWTEXT-TEXT              PIC X(100).
      ******************************************************************
