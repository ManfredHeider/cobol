      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0031.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/WDKI0031.CPv  $
      *
      *   Rev 2.2   Jun 03 2002 09:32:16   T13EX27
      *AKI00289 DB-Änderung für neue VP-Analyse
      *
      *   Rev 2.1   Apr 16 1999 10:04:40   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KUNDENINTERESSENT-GESAMT.
         10 EA-NI-KUNDENINTERESSENT-TAB.
           20 EA-NI-KUNDENINTERESSENT-ELE PIC X
210502*           OCCURS 9 TIMES.
210502            OCCURS 10 TIMES.
         10 EA-NI-KUNDENINTERESSENT
                 REDEFINES EA-NI-KUNDENINTERESSENT-TAB.
           20 EA-NI-KIPERSONENID             PIC X.
           20 EA-NI-KIPERSONENROLLE          PIC X.
           20 EA-NI-KIKUNDENNUMMER           PIC X.
           20 EA-NI-KIVKUNDEDUZENKZ          PIC X.
           20 EA-NI-KIKUNDENSTATUSID         PIC X.
           20 EA-NI-KIVSABERECHDATUM         PIC X.
           20 EA-NI-KIINFOWUNSCH             PIC X.
           20 EA-NI-KIORTDERARBSTAETT        PIC X.
           20 EA-NI-KIANZAHLBETREUER         PIC X.
210502     20 EA-NI-KIKUNDENAKTEVER          PIC X.
         10 EA-DCL-KUNDENINTERESSENT.
           20 EA-KIPERSONENID                PIC X(13).
           20 EA-KIPERSONENROLLE             PIC X(01).
           20 EA-KIKUNDENNUMMER              PIC --------9 .
           20 EA-KIVKUNDEDUZENKZ             PIC X(01).
           20 EA-KIKUNDENSTATUSID            PIC X(01).
           20 EA-KIVSABERECHDATUM            PIC X(10).
           20 EA-KIINFOWUNSCH                PIC X(01).
           20 EA-KIORTDERARBSTAETT           PIC X(3).
           20 EA-KIANZAHLBETREUER            PIC -----9    .
210502     20 EA-KIKUNDENAKTEVER             PIC X(01).
      ******************************************************************
