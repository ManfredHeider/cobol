      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0037.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0037.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:04:50   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:10   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-NOTIZ-GESAMT.
         10 EA-NI-NOTIZ-TAB.
           20 EA-NI-NOTIZ-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-NOTIZ
                 REDEFINES EA-NI-NOTIZ-TAB.
           20 EA-NI-NOTIZID                  PIC X.
           20 EA-NI-NOTBEARBEITETAM          PIC X.
           20 EA-NI-NOTBEARBEITETVON         PIC X.
           20 EA-NI-NOTIZSUBTYPE             PIC X.
           20 EA-NI-NOTIZTEXT                PIC X.
           20 EA-NI-NOTIZTITEL               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-NOTIZ.
           20 EA-NOTIZID                     PIC X(13).
           20 EA-NOTBEARBEITETAM             PIC X(10).
           20 EA-NOTBEARBEITETVON            PIC X(30).
           20 EA-NOTIZSUBTYPE                PIC X(01).
           20 EA-NOTIZTEXT.
              49 EA-NOTIZTEXT-LEN            PIC -----9    .
              49 EA-NOTIZTEXT-TEXT           PIC X(880).
           20 EA-NOTIZTITEL.
              49 EA-NOTIZTITEL-LEN           PIC -----9    .
              49 EA-NOTIZTITEL-TEXT          PIC X(50).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
