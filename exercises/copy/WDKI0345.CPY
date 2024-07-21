      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0345.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WDKI0345.CPv  $
      *
      *   Rev 2.3   Jun 05 2003 17:32:36   t13ex27
      *
      *   Rev 2.2   Jun 03 2002 09:32:36   T13EX27
      *AKI00289 DB-Änderung für neue VP-Analyse
      *
      *   Rev 2.1   Apr 16 1999 10:10:32   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:40   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERSKRANKENTARIF-GESAMT.
         10 EA-NI-VERSKRANKENTARIF-TAB.
           20 EA-NI-VERSKRANKENTARIF-ELE PIC X
210502*           OCCURS 6 TIMES.
210502            OCCURS 8 TIMES.
         10 EA-NI-VERSKRANKENTARIF
                 REDEFINES EA-NI-VERSKRANKENTARIF-TAB.
           20 EA-NI-KRTVERTRAGSID            PIC X.
           20 EA-NI-KRTVERSTARLFDNR          PIC X.
           20 EA-NI-KRTPERSONENNUMMER        PIC X.
           20 EA-NI-KRTBEITRAGVERZ           PIC X.
           20 EA-NI-KRTGESCHAEFTSSTID        PIC X.
           20 EA-NI-KRTGESCHAEFTSST          PIC X.
210502     20 EA-NI-KRTBEITRFRWILLWILL       PIC X.
210502     20 EA-NI-KRTBEITRAGPRIVAT         PIC X.
         10 EA-DCL-VERSKRANKENTARIF.
           20 EA-KRTVERTRAGSID               PIC X(13).
           20 EA-KRTVERSTARLFDNR             PIC -----9    .
           20 EA-KRTPERSONENNUMMER           PIC -----9    .
           20 EA-KRTBEITRAGVERZ              PIC --9.9            .
           20 EA-KRTGESCHAEFTSSTID           PIC -----9    .
           20 EA-KRTGESCHAEFTSST             PIC X(20).
210502*    10 EA-KRTBEITRFRWILLWILL          PIC ---------9.99    .
210502*    10 EA-KRTBEITRAGPRIVAT            PIC ---------9.99    .
050603     20 EA-KRTBEITRFRWILLWILL          PIC ---------9.99    .
050603     20 EA-KRTBEITRAGPRIVAT            PIC ---------9.99    .
      ******************************************************************
