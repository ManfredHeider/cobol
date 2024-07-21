      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0339.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/WUKI0339.CPv  $
      *
      *   Rev 2.10   Oct 12 2006 13:36:40   t13ex27
      *AKI00555: DB-Änderung für FSAEIGENVERTRÄGE-PMKI0079
      *
      *   Rev 2.9   Apr 26 2006 15:57:50   t13ex27
      *AKI00549: DB-Änderung für Fix 84 - PMKI0079
      *
      *   Rev 2.8   Oct 17 2005 10:07:42   t13ex27
      *AKI00521: DB-Änderung für Fix 80 PMKI0079 und WUKI's
      *
      *   Rev 2.7   Oct 18 2004 09:44:36   t13ex27
      *AKI00436: Datenbankänderungen in KI für Fix 67 EA-Programme
      *
      *   Rev 2.6   May 24 2004 11:45:02   t13ex27
      *DB-Änderung für Fix 61 PMKI0079 und WUKI's
      *
      *   Rev 2.5   25 Jul 2001 10:33:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-UNFALL.
           10 UNFVERTRAGSID               PIC X(13).
           10 UNFVERSTARLFDNR             PIC S9(4) COMP.
           10 UNFBERGUNGBETRAG            PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFBERGUNGBETRAG        PIC X(01).
           10 UNFBERGUNGBETRWKZ           PIC X(1).
           10 UNFGRUNDSUMME               PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFGRUNDSUMME           PIC X(01).
           10 UNFGRUNDSUMWKZ              PIC X(1).
           10 UNFHAUSHALTSGELD            PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFHAUSHALTSGELD        PIC X(01).
           10 UNFHHGELDWKZ                PIC X(1).
           10 UNFHOEHEPROGRESS            PIC S9(4)V9(2) COMP-3.
           10 NIL-UNFHOEHEPROGRESS        PIC X(01).
           10 UNFHOEHEPROGWKZ             PIC X(1).
           10 UNFKENNZEICHENBG            PIC X(1).
           10 UNFKOSMETIKBETRAG           PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFKOSMETIKBETRAG       PIC X(01).
           10 UNFKOSMBETRWKZ              PIC X(1).
           10 UNFKURBETRAG                PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFKURBETRAG            PIC X(01).
           10 UNFKURBETRAGWKZ             PIC X(1).
           10 UNFSCHULAUSFGELD            PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFSCHULAUSFGELD        PIC X(01).
           10 UNFSCHULAUSFGWKZ            PIC X(1).
           10 UNFUEBERGLSTBETR            PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFUEBERGLSTBETR        PIC X(01).
           10 UNFUEBERGLSTBWKZ            PIC X(1).
           10 UNFVEREINBDYNPROZ           PIC S9(3)V9(2) COMP-3.
           10 NIL-UNFVEREINBDYNPROZ       PIC X(01).
           10 UNFVERSSUMTAGGELD           PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFVERSSUMTAGGELD       PIC X(01).
           10 UNFVSTAGGELDWKZ             PIC X(1).
           10 UNFVERSSUMGENGELD           PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFVERSSUMGENGELD       PIC X(01).
           10 UNFVSGENGELDWKZ             PIC X(1).
           10 UNFVERSSUMINVALID           PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFVERSSUMINVALID       PIC X(01).
           10 UNFVSINVALIDWKZ             PIC X(1).
           10 UNFVERSSUMKRANKTG           PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFVERSSUMKRANKTG       PIC X(01).
           10 UNFVSKRANKTGWKZ             PIC X(1).
           10 UNFVERSSUMTOD               PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFVERSSUMTOD           PIC X(01).
           10 UNFVSTODWKZ                 PIC X(1).
           10 UNFRABFUENFPRINV            PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFRABFUENFPRINV        PIC X(01).
           10 UNFFRABFPRINVWKZ            PIC X(1).
           10 UNFBEITRAG                  PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFBEITRAG              PIC X(01).
           10 UNFBEITRAGWKZ               PIC X(1).
           10 UNFSCHMGELDKB               PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFSCHMGELDKB           PIC X(01).
           10 UNFSCHMGELDKBWKZ            PIC X(1).
           10 NIL-UNFSCHMGELDKBWKZ        PIC X(01).
           10 UNFSOFORTLSCHWV             PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFSOFORTLSCHWV         PIC X(01).
           10 UNFSOFORTLSCHWVWKZ          PIC X(1).
           10 NIL-UNFSOFORTLSCHWVWKZ      PIC X(01).
           10 UNFBEZUGSRECHT              PIC X(40).
           10 NIL-UNFBEZUGSRECHT          PIC X(01).
           10 UNFAKTMEHRLLINV             PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFAKTMEHRLLINV         PIC X(01).
           10 UNFAKTMEHRLINVWKZ           PIC X(1).
           10 NIL-UNFAKTMEHRLINVWKZ       PIC X(01).
           10 UNFAKTSCHMERZG              PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFAKTSCHMERZG          PIC X(01).
           10 UNFAKTSCHMERZGWKZ           PIC X(1).
           10 NIL-UNFAKTSCHMERZGWKZ       PIC X(01).
           10 UNFAKTPERSONKZ              PIC X(1).
           10 NIL-UNFAKTPERSONKZ          PIC X(01).
           10 UNFSUMMENKOMB               PIC X(10).
           10 NIL-UNFSUMMENKOMB           PIC X(01).
           10 UNFZWEIGSCHLUESSEL          PIC X(2).
           10 NIL-UNFZWEIGSCHLUESSEL      PIC X(01).
           10 UNFZUKBEITRAG               PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFZUKBEITRAG           PIC X(01).
           10 UNFZUKBEITRAGWKZ            PIC X(1).
           10 NIL-UNFZUKBEITRAGWKZ        PIC X(01).
           10 UNFKZDIFFWG                 PIC X(1).
           10 NIL-UNFKZDIFFWG             PIC X(01).
           10 UNFDIFFGES                  PIC X(90).
           10 NIL-UNFDIFFGES              PIC X(01).
           10 UNFDIFFVNR                  PIC X(40).
           10 NIL-UNFDIFFVNR              PIC X(01).
           10 UNFDIFFABLAUF               PIC X(010).
           10 NIL-UNFDIFFABLAUF           PIC X(01).
           10 UNFKZDIFFKUEND              PIC X(1).
           10 NIL-UNFKZDIFFKUEND          PIC X(01).
           10 UNFVSSCHMOSOA               PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFVSSCHMOSOA           PIC X(01).
           10 UNFVSSCHMOSOAWKZ            PIC X(1).
           10 NIL-UNFVSSCHMOSOAWKZ        PIC X(01).
           10 UNFVSKHTREHA                PIC S9(9)V9(2) COMP-3.
           10 NIL-UNFVSKHTREHA            PIC X(01).
           10 UNFVSKHTREHAWKZ             PIC X(1).
           10 NIL-UNFVSKHTREHAWKZ         PIC X(01).
           10 UNFLVASS                    PIC X(20).
           10 NIL-UNFLVASS                PIC X(01).
           10 UNFLVASSKZ                  PIC X(1).
           10 NIL-UNFLVASSKZ              PIC X(01).
           10 UNFLVKHTR                   PIC X(15).
           10 NIL-UNFLVKHTR               PIC X(01).
      *****************************************************************
