      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0726.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/WDKI0726.CPv  $
      *
      *   Rev 2.2   Jun 03 2002 09:32:42   T13EX27
      *AKI00289 DB-Änderung für neue VP-Analyse
      *
      *   Rev 2.1   22 Oct 2001 17:31:30   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:16   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-SELKRITERIUM-GESAMT.
         10 EA-NI-SELKRITERIUM-TAB.
           20 EA-NI-SELKRITERIUM-ELE PIC X
210502*           OCCURS 8 TIMES.
210502            OCCURS 9 TIMES.
         10 EA-NI-SELKRITERIUM
                 REDEFINES EA-NI-SELKRITERIUM-TAB.
           20 EA-NI-SKRID                    PIC X.
           20 EA-NI-SKRBEZEICHNUNGTEXT       PIC X.
           20 EA-NI-SKRKLASSE                PIC X.
           20 EA-NI-SKRGUELTIGBISDATUM       PIC X.
           20 EA-NI-SKRERLAEUTERUNG          PIC X.
           20 EA-NI-SKRATTRIBUT              PIC X.
           20 EA-NI-SKRALIASBEZEICHN         PIC X.
           20 EA-NI-SKRKONTEXT               PIC X.
210502     20 EA-NI-SKRTYPE                  PIC X.
         10 EA-DCL-SELKRITERIUM.
           20 EA-SKRID                       PIC -----9    .
           20 EA-SKRBEZEICHNUNGTEXT          PIC X(60).
           20 EA-SKRKLASSE.
              49 EA-SKRKLASSE-LEN            PIC -----9    .
              49 EA-SKRKLASSE-TEXT           PIC X(50).
           20 EA-SKRGUELTIGBISDATUM          PIC X(10).
           20 EA-SKRERLAEUTERUNG.
              49 EA-SKRERLAEUTERUNG-LEN      PIC -----9    .
              49 EA-SKRERLAEUTERUNG-TEXT     PIC X(80).
           20 EA-SKRATTRIBUT.
              49 EA-SKRATTRIBUT-LEN          PIC -----9    .
              49 EA-SKRATTRIBUT-TEXT         PIC X(50).
           20 EA-SKRALIASBEZEICHN.
              49 EA-SKRALIASBEZEICHN-LEN     PIC -----9    .
              49 EA-SKRALIASBEZEICHN-TEXT    PIC X(50).
           20 EA-SKRKONTEXT.
              49 EA-SKRKONTEXT-LEN           PIC -----9    .
              49 EA-SKRKONTEXT-TEXT          PIC X(30).
210502     20 EA-SKRTYPE                     PIC X(1).
      ******************************************************************
