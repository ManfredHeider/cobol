      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0339.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/CTKI0339.CPv  $
      *
      *   Rev 2.5   Sep 18 2006 10:49:18   t13ex27
      *AKI00624: DB-Änderung für Fix 93 DCLGENs
      *
      *   Rev 2.4   Feb 06 2006 11:50:26   t13ex27
      *FKI00995: DB-Änderung Tabelle UNFALL
      *
      *   Rev 2.3   Sep 26 2005 13:53:46   t13ex27
      *AKI00521: DB-Änderung für Fix 80 CTKI's
      *
      *   Rev 2.2   Sep 22 2004 13:07:56   t13ex27
      *AKI00436: Datenbankänderungen in KI für Fix 67 CTKI's
      *
      *   Rev 2.1   May 13 2004 14:13:18   t13ex27
      *DB-Änderung für Fix 61 CTKI's
      *
      *   Rev 2.0   Feb 27 1999 12:50:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:26   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:44   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE UNFALL
      ******************************************************************
CCC005*    EXEC SQL DECLARE UNFALL TABLE
CCC004*    ( UNFVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      UNFVERSTARLFDNR                SMALLINT NOT NULL,
CCC004*      UNFBERGUNGBETRAG               DECIMAL(11, 2),
CCC004*      UNFBERGUNGBETRWKZ              CHAR(1) NOT NULL,
CCC004*      UNFGRUNDSUMME                  DECIMAL(11, 2),
CCC004*      UNFGRUNDSUMWKZ                 CHAR(1) NOT NULL,
CCC004*      UNFHAUSHALTSGELD               DECIMAL(11, 2),
CCC004*      UNFHHGELDWKZ                   CHAR(1) NOT NULL,
CCC004*      UNFHOEHEPROGRESS               DECIMAL(6, 2),
CCC004*      UNFHOEHEPROGWKZ                CHAR(1) NOT NULL,
CCC004*      UNFKENNZEICHENBG               CHAR(1) NOT NULL,
CCC004*      UNFKOSMETIKBETRAG              DECIMAL(11, 2),
CCC004*      UNFKOSMBETRWKZ                 CHAR(1) NOT NULL,
CCC004*      UNFKURBETRAG                   DECIMAL(11, 2),
CCC004*      UNFKURBETRAGWKZ                CHAR(1) NOT NULL,
CCC004*      UNFSCHULAUSFGELD               DECIMAL(11, 2),
CCC004*      UNFSCHULAUSFGWKZ               CHAR(1) NOT NULL,
CCC004*      UNFUEBERGLSTBETR               DECIMAL(11, 2),
CCC004*      UNFUEBERGLSTBWKZ               CHAR(1) NOT NULL,
CCC004*      UNFVEREINBDYNPROZ              DECIMAL(5, 2),
CCC004*      UNFVERSSUMTAGGELD              DECIMAL(11, 2),
CCC004*      UNFVSTAGGELDWKZ                CHAR(1) NOT NULL,
CCC004*      UNFVERSSUMGENGELD              DECIMAL(11, 2),
CCC004*      UNFVSGENGELDWKZ                CHAR(1) NOT NULL,
CCC004*      UNFVERSSUMINVALID              DECIMAL(11, 2),
CCC004*      UNFVSINVALIDWKZ                CHAR(1) NOT NULL,
CCC004*      UNFVERSSUMKRANKTG              DECIMAL(11, 2),
CCC004*      UNFVSKRANKTGWKZ                CHAR(1) NOT NULL,
CCC004*      UNFVERSSUMTOD                  DECIMAL(11, 2),
CCC004*      UNFVSTODWKZ                    CHAR(1) NOT NULL,
CCC004*      UNFRABFUENFPRINV               DECIMAL(11, 2),
CCC004*      UNFFRABFPRINVWKZ               CHAR(1) NOT NULL,
CCC004*      UNFBEITRAG                     DECIMAL(11, 2),
CCC004*      UNFBEITRAGWKZ                  CHAR(1) NOT NULL,
CCC004*      UNFSCHMGELDKB                  DECIMAL(11, 2),
CCC004*      UNFSCHMGELDKBWKZ               CHAR(1),
CCC004*      UNFSOFORTLSCHWV                DECIMAL(11, 2),
CCC004*      UNFSOFORTLSCHWVWKZ             CHAR(1),
CCC004*      UNFBEZUGSRECHT                 CHAR(40),
CCC004*      UNFAKTMEHRLLINV                DECIMAL(11, 2),
CCC004*      UNFAKTMEHRLINVWKZ              CHAR(1),
CCC004*      UNFAKTSCHMERZG                 DECIMAL(11, 2),
CCC004*      UNFAKTSCHMERZGWKZ              CHAR(1),
CCC004*      UNFAKTPERSONKZ                 CHAR(1),
CCC004*      UNFSUMMENKOMB                  CHAR(10),
CCC004*      UNFZWEIGSCHLUESSEL             CHAR(2),
CCC004*      UNFZUKBEITRAG                  DECIMAL(11, 2),
CCC004*      UNFZUKBEITRAGWKZ               CHAR(1),
CCC004*      UNFKZDIFFWG                    CHAR(1),
CCC004*      UNFDIFFGES                     CHAR(90),
CCC004*      UNFDIFFVNR                     CHAR(40),
CCC004*      UNFDIFFABLAUF                  DATE,
CCC004*      UNFKZDIFFKUEND                 CHAR(1),
CCC004*      UNFVSSCHMOSOA                  DECIMAL(11, 2),
CCC004*      UNFVSSCHMOSOAWKZ               CHAR(1),
CCC004*      UNFVSKHTREHA                   DECIMAL(11, 2),
CCC004*      UNFVSKHTREHAWKZ                CHAR(1),
CCC004*      UNFLVASS                       CHAR(20),
CCC004*      UNFLVASSKZ                     CHAR(1),
CCC004*      UNFLVKHTR                      CHAR(15)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  UNFALL
      ******************************************************************
       01  UNFALL-GESAMT.
         05 NI-UNFALL-TAB.
           10 NI-UNFALL-ELE PIC S9(4) COMP OCCURS 60 TIMES.
         05 NI-UNFALL REDEFINES NI-UNFALL-TAB.
           10 NI-UNFVERTRAGSID                       PIC S9(4) COMP.
           10 NI-UNFVERSTARLFDNR                     PIC S9(4) COMP.
           10 NI-UNFBERGUNGBETRAG                    PIC S9(4) COMP.
           10 NI-UNFBERGUNGBETRWKZ                   PIC S9(4) COMP.
           10 NI-UNFGRUNDSUMME                       PIC S9(4) COMP.
           10 NI-UNFGRUNDSUMWKZ                      PIC S9(4) COMP.
           10 NI-UNFHAUSHALTSGELD                    PIC S9(4) COMP.
           10 NI-UNFHHGELDWKZ                        PIC S9(4) COMP.
           10 NI-UNFHOEHEPROGRESS                    PIC S9(4) COMP.
           10 NI-UNFHOEHEPROGWKZ                     PIC S9(4) COMP.
           10 NI-UNFKENNZEICHENBG                    PIC S9(4) COMP.
           10 NI-UNFKOSMETIKBETRAG                   PIC S9(4) COMP.
           10 NI-UNFKOSMBETRWKZ                      PIC S9(4) COMP.
           10 NI-UNFKURBETRAG                        PIC S9(4) COMP.
           10 NI-UNFKURBETRAGWKZ                     PIC S9(4) COMP.
           10 NI-UNFSCHULAUSFGELD                    PIC S9(4) COMP.
           10 NI-UNFSCHULAUSFGWKZ                    PIC S9(4) COMP.
           10 NI-UNFUEBERGLSTBETR                    PIC S9(4) COMP.
           10 NI-UNFUEBERGLSTBWKZ                    PIC S9(4) COMP.
           10 NI-UNFVEREINBDYNPROZ                   PIC S9(4) COMP.
           10 NI-UNFVERSSUMTAGGELD                   PIC S9(4) COMP.
           10 NI-UNFVSTAGGELDWKZ                     PIC S9(4) COMP.
           10 NI-UNFVERSSUMGENGELD                   PIC S9(4) COMP.
           10 NI-UNFVSGENGELDWKZ                     PIC S9(4) COMP.
           10 NI-UNFVERSSUMINVALID                   PIC S9(4) COMP.
           10 NI-UNFVSINVALIDWKZ                     PIC S9(4) COMP.
           10 NI-UNFVERSSUMKRANKTG                   PIC S9(4) COMP.
           10 NI-UNFVSKRANKTGWKZ                     PIC S9(4) COMP.
           10 NI-UNFVERSSUMTOD                       PIC S9(4) COMP.
           10 NI-UNFVSTODWKZ                         PIC S9(4) COMP.
           10 NI-UNFRABFUENFPRINV                    PIC S9(4) COMP.
           10 NI-UNFFRABFPRINVWKZ                    PIC S9(4) COMP.
           10 NI-UNFBEITRAG                          PIC S9(4) COMP.
           10 NI-UNFBEITRAGWKZ                       PIC S9(4) COMP.
           10 NI-UNFSCHMGELDKB                       PIC S9(4) COMP.
           10 NI-UNFSCHMGELDKBWKZ                    PIC S9(4) COMP.
           10 NI-UNFSOFORTLSCHWV                     PIC S9(4) COMP.
           10 NI-UNFSOFORTLSCHWVWKZ                  PIC S9(4) COMP.
           10 NI-UNFBEZUGSRECHT                      PIC S9(4) COMP.
           10 NI-UNFAKTMEHRLLINV                     PIC S9(4) COMP.
           10 NI-UNFAKTMEHRLINVWKZ                   PIC S9(4) COMP.
           10 NI-UNFAKTSCHMERZG                      PIC S9(4) COMP.
           10 NI-UNFAKTSCHMERZGWKZ                   PIC S9(4) COMP.
           10 NI-UNFAKTPERSONKZ                      PIC S9(4) COMP.
           10 NI-UNFSUMMENKOMB                       PIC S9(4) COMP.
           10 NI-UNFZWEIGSCHLUESSEL                  PIC S9(4) COMP.
           10 NI-UNFZUKBEITRAG                       PIC S9(4) COMP.
           10 NI-UNFZUKBEITRAGWKZ                    PIC S9(4) COMP.
           10 NI-UNFKZDIFFWG                         PIC S9(4) COMP.
           10 NI-UNFDIFFGES                          PIC S9(4) COMP.
           10 NI-UNFDIFFVNR                          PIC S9(4) COMP.
           10 NI-UNFDIFFABLAUF                       PIC S9(4) COMP.
           10 NI-UNFKZDIFFKUEND                      PIC S9(4) COMP.
           10 NI-UNFVSSCHMOSOA                       PIC S9(4) COMP.
           10 NI-UNFVSSCHMOSOAWKZ                    PIC S9(4) COMP.
           10 NI-UNFVSKHTREHA                        PIC S9(4) COMP.
           10 NI-UNFVSKHTREHAWKZ                     PIC S9(4) COMP.
           10 NI-UNFLVASS                            PIC S9(4) COMP.
           10 NI-UNFLVASSKZ                          PIC S9(4) COMP.
           10 NI-UNFLVKHTR                           PIC S9(4) COMP.
         05 DCL-UNFALL.
           10 UNFVERTRAGSID        PIC X(13).
           10 UNFVERSTARLFDNR      PIC S9(4) COMP.
           10 UNFBERGUNGBETRAG     PIC S9(9)V9(2) COMP-3.
           10 UNFBERGUNGBETRWKZ    PIC X(1).
           10 UNFGRUNDSUMME        PIC S9(9)V9(2) COMP-3.
           10 UNFGRUNDSUMWKZ       PIC X(1).
           10 UNFHAUSHALTSGELD     PIC S9(9)V9(2) COMP-3.
           10 UNFHHGELDWKZ         PIC X(1).
           10 UNFHOEHEPROGRESS     PIC S9(4)V9(2) COMP-3.
           10 UNFHOEHEPROGWKZ      PIC X(1).
           10 UNFKENNZEICHENBG     PIC X(1).
           10 UNFKOSMETIKBETRAG    PIC S9(9)V9(2) COMP-3.
           10 UNFKOSMBETRWKZ       PIC X(1).
           10 UNFKURBETRAG         PIC S9(9)V9(2) COMP-3.
           10 UNFKURBETRAGWKZ      PIC X(1).
           10 UNFSCHULAUSFGELD     PIC S9(9)V9(2) COMP-3.
           10 UNFSCHULAUSFGWKZ     PIC X(1).
           10 UNFUEBERGLSTBETR     PIC S9(9)V9(2) COMP-3.
           10 UNFUEBERGLSTBWKZ     PIC X(1).
           10 UNFVEREINBDYNPROZ    PIC S9(3)V9(2) COMP-3.
           10 UNFVERSSUMTAGGELD    PIC S9(9)V9(2) COMP-3.
           10 UNFVSTAGGELDWKZ      PIC X(1).
           10 UNFVERSSUMGENGELD    PIC S9(9)V9(2) COMP-3.
           10 UNFVSGENGELDWKZ      PIC X(1).
           10 UNFVERSSUMINVALID    PIC S9(9)V9(2) COMP-3.
           10 UNFVSINVALIDWKZ      PIC X(1).
           10 UNFVERSSUMKRANKTG    PIC S9(9)V9(2) COMP-3.
           10 UNFVSKRANKTGWKZ      PIC X(1).
           10 UNFVERSSUMTOD        PIC S9(9)V9(2) COMP-3.
           10 UNFVSTODWKZ          PIC X(1).
           10 UNFRABFUENFPRINV     PIC S9(9)V9(2) COMP-3.
           10 UNFFRABFPRINVWKZ     PIC X(1).
           10 UNFBEITRAG           PIC S9(9)V9(2) COMP-3.
           10 UNFBEITRAGWKZ        PIC X(1).
           10 UNFSCHMGELDKB        PIC S9(9)V9(2) COMP-3.
           10 UNFSCHMGELDKBWKZ     PIC X(1).
           10 UNFSOFORTLSCHWV      PIC S9(9)V9(2) COMP-3.
           10 UNFSOFORTLSCHWVWKZ   PIC X(1).
           10 UNFBEZUGSRECHT       PIC X(40).
           10 UNFAKTMEHRLLINV      PIC S9(9)V9(2) COMP-3.
           10 UNFAKTMEHRLINVWKZ    PIC X(1).
           10 UNFAKTSCHMERZG       PIC S9(9)V9(2) COMP-3.
           10 UNFAKTSCHMERZGWKZ    PIC X(1).
           10 UNFAKTPERSONKZ       PIC X(1).
           10 UNFSUMMENKOMB        PIC X(10).
           10 UNFZWEIGSCHLUESSEL   PIC X(2).
           10 UNFZUKBEITRAG        PIC S9(9)V9(2) COMP-3.
           10 UNFZUKBEITRAGWKZ     PIC X(1).
           10 UNFKZDIFFWG          PIC X(1).
           10 UNFDIFFGES           PIC X(90).
           10 UNFDIFFVNR           PIC X(40).
           10 UNFDIFFABLAUF        PIC X(10).
           10 UNFKZDIFFKUEND       PIC X(1).
           10 UNFVSSCHMOSOA        PIC S9(9)V9(2) COMP-3.
           10 UNFVSSCHMOSOAWKZ     PIC X(1).
           10 UNFVSKHTREHA         PIC S9(9)V9(2) COMP-3.
           10 UNFVSKHTREHAWKZ      PIC X(1).
           10 UNFLVASS             PIC X(20).
           10 UNFLVASSKZ           PIC X(1).
           10 UNFLVKHTR            PIC X(15).
      ******************************************************************
