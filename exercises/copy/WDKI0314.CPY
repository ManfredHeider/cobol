      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0314.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WDKI0314.CPv  $
      *
      *   Rev 2.3   Oct 18 2004 09:43:24   t13ex27
      *AKI00436: Datenbankänderungen in KI für Fix 67 EA-Programme
      *
      *   Rev 2.2   Oct 15 2002 08:15:50   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.1   Apr 16 1999 10:07:24   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:30   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-GUTHABENBUCHUNG-GESAMT.
         10 EA-NI-GUTHABENBUCHUNG-TAB.
           20 EA-NI-GUTHABENBUCHUNG-ELE PIC X OCCURS 15 TIMES.
         10 EA-NI-GUTHABENBUCHUNG
                 REDEFINES EA-NI-GUTHABENBUCHUNG-TAB.
           20 EA-NI-GHBVERTRAGSID            PIC X.
           20 EA-NI-GHBGUTHABPRODLFDNR       PIC X.
           20 EA-NI-GHBFONDID                PIC X.
           20 EA-NI-GHBGHALFDNR              PIC X.
           20 EA-NI-GHBBUCHUNGLFDNR          PIC X.
           20 EA-NI-GHBBETRAGLETZTBUCH       PIC X.
           20 EA-NI-GHBBETRLBUWKZ            PIC X.
           20 EA-NI-GHBBUCHUNGSDATUM         PIC X.
           20 EA-NI-GHBGUTHABENANT           PIC X.
           20 EA-NI-GHBKZFINHLETZTBUID       PIC X.
           20 EA-NI-GHBSTEUBETRLETZTBU       PIC X.
           20 EA-NI-GHBSTLETZTBUWKZ          PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-GHBNETTOBETRAG           PIC X.
           20 EA-NI-GHBNETTOBETWKZ           PIC X.
         10 EA-DCL-GUTHABENBUCHUNG.
           20 EA-GHBVERTRAGSID               PIC X(13).
           20 EA-GHBGUTHABPRODLFDNR          PIC -----9.
           20 EA-GHBFONDID                   PIC -----9.
           20 EA-GHBGHALFDNR                 PIC -----9.
           20 EA-GHBBUCHUNGLFDNR             PIC -----9.
           20 EA-GHBBETRAGLETZTBUCH          PIC ---------9.99.
           20 EA-GHBBETRLBUWKZ               PIC X(1).
           20 EA-GHBBUCHUNGSDATUM            PIC X(010).
           20 EA-GHBGUTHABENANT              PIC ------9.9999.
           20 EA-GHBKZFINHLETZTBUID          PIC -----9.
           20 EA-GHBSTEUBETRLETZTBU          PIC ---------9.99.
           20 EA-GHBSTLETZTBUWKZ             PIC X(1).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-GHBNETTOBETRAG              PIC ---------9.99.
           20 EA-GHBNETTOBETWKZ              PIC X(1).
      *****************************************************************
