      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0339.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/WDKI0339.CPv  $
      *
      *   Rev 2.6   Oct 12 2006 12:17:12   t13ex27
      *AKI00624: DB-Änderung für Fix 93 EA-Programme
      *
      *   Rev 2.5   Feb 06 2006 11:54:58   t13ex27
      *FKI00995: DB-Änderung Tabelle UNFALL
      *
      *   Rev 2.4   Oct 17 2005 08:57:40   t13ex27
      *AKI00521: DB-Änderung für Fix 80 EA-Programme und WDKI's
      *
      *   Rev 2.3   Oct 18 2004 09:43:48   t13ex27
      *AKI00436: Datenbankänderungen in KI für Fix 67 EA-Programme
      *
      *   Rev 2.2   May 24 2004 10:53:40   t13ex27
      *DB-Änderung für Fix 61 EA-Programme und WDKI's
      *
      *   Rev 2.1   Apr 16 1999 10:10:24   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:38   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-UNFALL-GESAMT.
         10 EA-NI-UNFALL-TAB.
           20 EA-NI-UNFALL-ELE PIC X OCCURS 60 TIMES.
         10 EA-NI-UNFALL
                 REDEFINES EA-NI-UNFALL-TAB.
           20 EA-NI-UNFVERTRAGSID            PIC X.
           20 EA-NI-UNFVERSTARLFDNR          PIC X.
           20 EA-NI-UNFBERGUNGBETRAG         PIC X.
           20 EA-NI-UNFBERGUNGBETRWKZ        PIC X.
           20 EA-NI-UNFGRUNDSUMME            PIC X.
           20 EA-NI-UNFGRUNDSUMWKZ           PIC X.
           20 EA-NI-UNFHAUSHALTSGELD         PIC X.
           20 EA-NI-UNFHHGELDWKZ             PIC X.
           20 EA-NI-UNFHOEHEPROGRESS         PIC X.
           20 EA-NI-UNFHOEHEPROGWKZ          PIC X.
           20 EA-NI-UNFKENNZEICHENBG         PIC X.
           20 EA-NI-UNFKOSMETIKBETRAG        PIC X.
           20 EA-NI-UNFKOSMBETRWKZ           PIC X.
           20 EA-NI-UNFKURBETRAG             PIC X.
           20 EA-NI-UNFKURBETRAGWKZ          PIC X.
           20 EA-NI-UNFSCHULAUSFGELD         PIC X.
           20 EA-NI-UNFSCHULAUSFGWKZ         PIC X.
           20 EA-NI-UNFUEBERGLSTBETR         PIC X.
           20 EA-NI-UNFUEBERGLSTBWKZ         PIC X.
           20 EA-NI-UNFVEREINBDYNPROZ        PIC X.
           20 EA-NI-UNFVERSSUMTAGGELD        PIC X.
           20 EA-NI-UNFVSTAGGELDWKZ          PIC X.
           20 EA-NI-UNFVERSSUMGENGELD        PIC X.
           20 EA-NI-UNFVSGENGELDWKZ          PIC X.
           20 EA-NI-UNFVERSSUMINVALID        PIC X.
           20 EA-NI-UNFVSINVALIDWKZ          PIC X.
           20 EA-NI-UNFVERSSUMKRANKTG        PIC X.
           20 EA-NI-UNFVSKRANKTGWKZ          PIC X.
           20 EA-NI-UNFVERSSUMTOD            PIC X.
           20 EA-NI-UNFVSTODWKZ              PIC X.
           20 EA-NI-UNFRABFUENFPRINV         PIC X.
           20 EA-NI-UNFFRABFPRINVWKZ         PIC X.
           20 EA-NI-UNFBEITRAG               PIC X.
           20 EA-NI-UNFBEITRAGWKZ            PIC X.
           20 EA-NI-UNFSCHMGELDKB            PIC X.
           20 EA-NI-UNFSCHMGELDKBWKZ         PIC X.
           20 EA-NI-UNFSOFORTLSCHWV          PIC X.
           20 EA-NI-UNFSOFORTLSCHWVWKZ       PIC X.
           20 EA-NI-UNFBEZUGSRECHT           PIC X.
           20 EA-NI-UNFAKTMEHRLLINV          PIC X.
           20 EA-NI-UNFAKTMEHRLINVWKZ        PIC X.
           20 EA-NI-UNFAKTSCHMERZG           PIC X.
           20 EA-NI-UNFAKTSCHMERZGWKZ        PIC X.
           20 EA-NI-UNFAKTPERSONKZ           PIC X.
           20 EA-NI-UNFSUMMENKOMB            PIC X.
           20 EA-NI-UNFZWEIGSCHLUESSEL       PIC X.
           20 EA-NI-UNFZUKBEITRAG            PIC X.
           20 EA-NI-UNFZUKBEITRAGWKZ         PIC X.
           20 EA-NI-UNFKZDIFFWG              PIC X.
           20 EA-NI-UNFDIFFGES               PIC X.
           20 EA-NI-UNFDIFFVNR               PIC X.
           20 EA-NI-UNFDIFFABLAUF            PIC X.
           20 EA-NI-UNFKZDIFFKUEND           PIC X.
           20 EA-NI-UNFVSSCHMOSOA            PIC X.
           20 EA-NI-UNFVSSCHMOSOAWKZ         PIC X.
           20 EA-NI-UNFVSKHTREHA             PIC X.
           20 EA-NI-UNFVSKHTREHAWKZ          PIC X.
           20 EA-NI-UNFLVASS                 PIC X.
           20 EA-NI-UNFLVASSKZ               PIC X.
           20 EA-NI-UNFLVKHTR                PIC X.
         10 EA-DCL-UNFALL.
           20 EA-UNFVERTRAGSID               PIC X(13).
           20 EA-UNFVERSTARLFDNR             PIC -----9.
           20 EA-UNFBERGUNGBETRAG            PIC ---------9.99.
           20 EA-UNFBERGUNGBETRWKZ           PIC X(1).
           20 EA-UNFGRUNDSUMME               PIC ---------9.99.
           20 EA-UNFGRUNDSUMWKZ              PIC X(1).
           20 EA-UNFHAUSHALTSGELD            PIC ---------9.99.
           20 EA-UNFHHGELDWKZ                PIC X(1).
           20 EA-UNFHOEHEPROGRESS            PIC ----9.99.
           20 EA-UNFHOEHEPROGWKZ             PIC X(1).
           20 EA-UNFKENNZEICHENBG            PIC X(1).
           20 EA-UNFKOSMETIKBETRAG           PIC ---------9.99.
           20 EA-UNFKOSMBETRWKZ              PIC X(1).
           20 EA-UNFKURBETRAG                PIC ---------9.99.
           20 EA-UNFKURBETRAGWKZ             PIC X(1).
           20 EA-UNFSCHULAUSFGELD            PIC ---------9.99.
           20 EA-UNFSCHULAUSFGWKZ            PIC X(1).
           20 EA-UNFUEBERGLSTBETR            PIC ---------9.99.
           20 EA-UNFUEBERGLSTBWKZ            PIC X(1).
           20 EA-UNFVEREINBDYNPROZ           PIC ---9.99.
           20 EA-UNFVERSSUMTAGGELD           PIC ---------9.99.
           20 EA-UNFVSTAGGELDWKZ             PIC X(1).
           20 EA-UNFVERSSUMGENGELD           PIC ---------9.99.
           20 EA-UNFVSGENGELDWKZ             PIC X(1).
           20 EA-UNFVERSSUMINVALID           PIC ---------9.99.
           20 EA-UNFVSINVALIDWKZ             PIC X(1).
           20 EA-UNFVERSSUMKRANKTG           PIC ---------9.99.
           20 EA-UNFVSKRANKTGWKZ             PIC X(1).
           20 EA-UNFVERSSUMTOD               PIC ---------9.99.
           20 EA-UNFVSTODWKZ                 PIC X(1).
           20 EA-UNFRABFUENFPRINV            PIC ---------9.99.
           20 EA-UNFFRABFPRINVWKZ            PIC X(1).
           20 EA-UNFBEITRAG                  PIC ---------9.99.
           20 EA-UNFBEITRAGWKZ               PIC X(1).
           20 EA-UNFSCHMGELDKB               PIC ---------9.99.
           20 EA-UNFSCHMGELDKBWKZ            PIC X(1).
           20 EA-UNFSOFORTLSCHWV             PIC ---------9.99.
           20 EA-UNFSOFORTLSCHWVWKZ          PIC X(1).
           20 EA-UNFBEZUGSRECHT              PIC X(40).
           20 EA-UNFAKTMEHRLLINV             PIC ---------9.99.
           20 EA-UNFAKTMEHRLINVWKZ           PIC X(1).
           20 EA-UNFAKTSCHMERZG              PIC ---------9.99.
           20 EA-UNFAKTSCHMERZGWKZ           PIC X(1).
           20 EA-UNFAKTPERSONKZ              PIC X(1).
           20 EA-UNFSUMMENKOMB               PIC X(10).
           20 EA-UNFZWEIGSCHLUESSEL          PIC X(2).
           20 EA-UNFZUKBEITRAG               PIC ---------9.99.
           20 EA-UNFZUKBEITRAGWKZ            PIC X(1).
           20 EA-UNFKZDIFFWG                 PIC X(1).
           20 EA-UNFDIFFGES                  PIC X(90).
           20 EA-UNFDIFFVNR                  PIC X(40).
           20 EA-UNFDIFFABLAUF               PIC X(010).
           20 EA-UNFKZDIFFKUEND              PIC X(1).
           20 EA-UNFVSSCHMOSOA               PIC ---------9.99.
           20 EA-UNFVSSCHMOSOAWKZ            PIC X(1).
           20 EA-UNFVSKHTREHA                PIC ---------9.99.
           20 EA-UNFVSKHTREHAWKZ             PIC X(1).
           20 EA-UNFLVASS                    PIC X(20).
           20 EA-UNFLVASSKZ                  PIC X(1).
           20 EA-UNFLVKHTR                   PIC X(15).
      *****************************************************************
