      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0334.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/WDKI0334.CPv  $
      *
      *   Rev 2.2   Jun 03 2002 09:32:32   T13EX27
      *AKI00289 DB-Änderung für neue VP-Analyse
      *
      *   Rev 2.1   Apr 16 1999 10:10:18   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:36   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-RUECKNAHMEPREIS-GESAMT.
         10 EA-NI-RUECKNAHMEPREIS-TAB.
           20 EA-NI-RUECKNAHMEPREIS-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-RUECKNAHMEPREIS
                 REDEFINES EA-NI-RUECKNAHMEPREIS-TAB.
           20 EA-NI-RNPFONDID                PIC X.
           20 EA-NI-RNPAUSGABEBETRAG         PIC X.
           20 EA-NI-RNPDATUMFONDSTAND        PIC X.
           20 EA-NI-RNPRUECKNAHMEBETR        PIC X.
           20 EA-NI-RNPDATUMWERTENTW         PIC X.
           20 EA-NI-RNPWERTENTWICKLUNG       PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-RUECKNAHMEPREIS.
           20 EA-RNPFONDID                   PIC -----9    .
210502*    20 EA-RNPAUSGABEBETRAG            PIC ---9.99          .
210502     20 EA-RNPAUSGABEBETRAG            PIC -----9.99        .
           20 EA-RNPDATUMFONDSTAND           PIC X(10).
210502*    20 EA-RNPRUECKNAHMEBETR           PIC ---9.99          .
210502     20 EA-RNPRUECKNAHMEBETR           PIC -----9.99        .
           20 EA-RNPDATUMWERTENTW            PIC X(10).
210502*    20 EA-RNPWERTENTWICKLUNG          PIC ---9.99          .
210502     20 EA-RNPWERTENTWICKLUNG          PIC -----9.99        .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
