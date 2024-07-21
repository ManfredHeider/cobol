      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0353.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/CTKI0353.CPv  $
      *
      *   Rev 2.8   Mar 30 2020 08:39:22   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.7   Dec 09 2019 13:46:24   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050 OZP
      *
      *   Rev 2.6   Jan 11 2016 14:55:42   t13ex78
      *AKI01404: AdvoCard - Kombirabatt und Kundenbonus trennen - Z
      *TKI0353
      *
      *   Rev 2.5   Aug 28 2013 13:18:48   t13ex19
      *AKI01249: AdvoCard - Sofortrabatt
      *
      *   Rev 2.4   Jul 21 2009 13:34:20   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.3   Sep 26 2005 13:54:18   t13ex27
      *AKI00521: DB-Änderung für Fix 80 CTKI's
      *
      *   Rev 2.2   May 13 2004 14:13:36   t13ex27
      *DB-Änderung für Fix 61 CTKI's
      *
      *   Rev 2.1   Jun 10 2002 09:50:32   T13EX27
      *AKI00289 -  DB-Änderun für neue 'VP-Analyse'
      *
      *   Rev 2.0   Feb 27 1999 12:50:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:50   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:59:38   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE VERSSACHTARIF TABLE
CCC004*    ( SATVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      SATVERSTARLFDNR                SMALLINT NOT NULL,
CCC004*      SATZWEIGTEXT                   CHAR(3),
CCC004*      SATZWEIGSCHLUESSEL             CHAR(2),
CCC004*      SATHINWAUFSELBSTB              CHAR(1) NOT NULL,
CCC004*      SATVERSLSTID                   SMALLINT,
CCC004*      SATSELBSTBETEIL                DECIMAL(11, 2),
CCC004*      SATSEBSTBETWKZ                 CHAR(1) NOT NULL,
CCC004*      SATGLASBERECHID                SMALLINT,
CCC004*      SATGLASBERECHWERT              DECIMAL(11, 2),
CCC004*      SATGLASBERWERTWKZ              CHAR(1) NOT NULL,
CCC004*      SATJAHRESBEITRAG               DECIMAL(11, 2),
CCC004*      SATJAHRBEITRWKZ                CHAR(1) NOT NULL,
CCC004*      SATSUBTYPE                     CHAR(1),
CCC004*      SATPRODUKTTEXTFV               CHAR(20),
CCC004*      SATSONSTIGE                    CHAR(30),
CCC004*      SATPRODVARIANTE                CHAR(30),
CCC004*      SATTARIFSB                     DECIMAL(11, 2),
CCC004*      SATTARIFSBWKZ                  CHAR(1),
CCC004*      SATRISKANSCHRPLZ               CHAR(6),
CCC004*      SATRISKANSCHRSTR               CHAR(45),
CCC004*      SATRISKANSCHRORT               CHAR(30),
CCC004*      SATEGPRIVATGH                  DECIMAL(11, 2),
CCC004*      SATEGPRIVATGHWKZ               CHAR(1),
CCC004*      SATEGKUENSTGS                  DECIMAL(11, 2),
CCC004*      SATEGKUENSTGSKZ                CHAR(1),
CCC004*      SATEEGFKOSTENG                 DECIMAL(11, 2),
CCC004*      SATEEGFKOSTENGWKZ              CHAR(1),
CCC004*      SATZUKNETTOJBR                 DECIMAL(11, 2),
CCC004*      SATZUKNETTOJBRWKZ              CHAR(1),
CCC004*      SATKZDIFFWG                    CHAR(1),
CCC004*      SATDIFFGES                     CHAR(90),
CCC004*      SATDIFFVNR                     CHAR(40),
CCC004*      SATDIFFABLAUF                  DATE,
CCC004*      SATKZDIFFKUEND                 CHAR(1),
CCC004*      SATKUNDENBONUS                 CHAR(1),
CCC004*      SATDAUERNACHLASS               CHAR(1),
CCC004*      SATVSPNACHLASS                 CHAR(1),
CCC004*      SATOEFFDIENSTNL                CHAR(1),
CCC004*      SATSOFORTRABATT                CHAR(2),
CCC004*      SATKOMBIRABATT                 SMALLINT,
CCC004*      SATJHRBTRGNETTO                DECIMAL(11, 2),
CCC004*      SATJHRBNETTOWKZ                CHAR(1),
CCC004*      SATRISIKBRTGES                 DECIMAL(11, 2),
CCC004*      SATRISIKBRTGWKZ                CHAR(1),
CCC004*      SATRISIKNETGES                 DECIMAL(11, 2),
CCC004*      SATRISIKNETGWKZ                CHAR(1),
CCC004*      SATMITARBRAB                   SMALLINT,
CCC004*      SATMITARBID                    CHAR(12),
CCC004*      SATVERSANDWEG_KTY              SMALLINT,
CCC004*      SATVERSANDWEG_KNR              SMALLINT,
CCC004*      SATANNAHMEDAT                  DATE,
CCC004*      SATBESTMANAGKZ                 CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VERSSACHTARIF               *
      ******************************************************************
       01  VERSSACHTARIF-GESAMT.
        05 NI-VERSSACHTARIF-TAB.
           10 NI-VERSSACHTARIF-ELE PIC S9(04) COMP OCCURS 53 TIMES.
        05 NI-VERSSACHTARIF REDEFINES NI-VERSSACHTARIF-TAB.
           10 NI-SATVERTRAGSID      PIC S9(4) COMP.
           10 NI-SATVERSTARLFDNR    PIC S9(4) COMP.
           10 NI-SATZWEIGTEXT       PIC S9(4) COMP.
           10 NI-SATZWEIGSCHLUESSEL PIC S9(4) COMP.
           10 NI-SATHINWAUFSELBSTB  PIC S9(4) COMP.
           10 NI-SATVERSLSTID       PIC S9(4) COMP.
           10 NI-SATSELBSTBETEIL    PIC S9(4) COMP.
           10 NI-SATSEBSTBETWKZ     PIC S9(4) COMP.
           10 NI-SATGLASBERECHID    PIC S9(4) COMP.
           10 NI-SATGLASBERECHWERT  PIC S9(4) COMP.
           10 NI-SATGLASBERWERTWKZ  PIC S9(4) COMP.
           10 NI-SATJAHRESBEITRAG   PIC S9(4) COMP.
           10 NI-SATJAHRBEITRWKZ    PIC S9(4) COMP.
           10 NI-SATSUBTYPE         PIC S9(4) COMP.
           10 NI-SATPRODUKTTEXTFV   PIC S9(4) COMP.
           10 NI-SATSONSTIGE        PIC S9(4) COMP.
           10 NI-SATPRODVARIANTE    PIC S9(4) COMP.
           10 NI-SATTARIFSB         PIC S9(4) COMP.
           10 NI-SATTARIFSBWKZ      PIC S9(4) COMP.
           10 NI-SATRISKANSCHRPLZ   PIC S9(4) COMP.
           10 NI-SATRISKANSCHRSTR   PIC S9(4) COMP.
           10 NI-SATRISKANSCHRORT   PIC S9(4) COMP.
           10 NI-SATEGPRIVATGH      PIC S9(4) COMP.
           10 NI-SATEGPRIVATGHWKZ   PIC S9(4) COMP.
           10 NI-SATEGKUENSTGS      PIC S9(4) COMP.
           10 NI-SATEGKUENSTGSKZ    PIC S9(4) COMP.
           10 NI-SATEEGFKOSTENG     PIC S9(4) COMP.
           10 NI-SATEEGFKOSTENGWKZ  PIC S9(4) COMP.
           10 NI-SATZUKNETTOJBR     PIC S9(4) COMP.
           10 NI-SATZUKNETTOJBRWKZ  PIC S9(4) COMP.
           10 NI-SATKZDIFFWG        PIC S9(4) COMP.
           10 NI-SATDIFFGES         PIC S9(4) COMP.
           10 NI-SATDIFFVNR         PIC S9(4) COMP.
           10 NI-SATDIFFABLAUF      PIC S9(4) COMP.
           10 NI-SATKZDIFFKUEND     PIC S9(4) COMP.
           10 NI-SATKUNDENBONUS     PIC S9(4) COMP.
           10 NI-SATDAUERNACHLASS   PIC S9(4) COMP.
           10 NI-SATVSPNACHLASS     PIC S9(4) COMP.
           10 NI-SATOEFFDIENSTNL    PIC S9(4) COMP.
           10 NI-SATSOFORTRABATT    PIC S9(4) COMP.
           10 NI-SATKOMBIRABATT     PIC S9(4) COMP.
           10 NI-SATJHRBTRGNETTO    PIC S9(4) COMP.
           10 NI-SATJHRBNETTOWKZ    PIC S9(4) COMP.
           10 NI-SATRISIKBRTGES     PIC S9(4) COMP.
           10 NI-SATRISIKBRTGWKZ    PIC S9(4) COMP.
           10 NI-SATRISIKNETGES     PIC S9(4) COMP.
           10 NI-SATRISIKNETGWKZ    PIC S9(4) COMP.
           10 NI-SATMITARBRAB       PIC S9(4) COMP.
           10 NI-SATMITARBID        PIC S9(4) COMP.
           10 NI-SATVERSANDWEG-KTY  PIC S9(4) COMP.
           10 NI-SATVERSANDWEG-KNR  PIC S9(4) COMP.
           10 NI-SATANNAHMEDAT      PIC S9(4) COMP.
           10 NI-SATBESTMANAGKZ     PIC S9(4) COMP.
        05 DCL-VERSSACHTARIF.
           10 SATVERTRAGSID        PIC X(13).
           10 SATVERSTARLFDNR      PIC S9(4) USAGE COMP.
           10 SATZWEIGTEXT         PIC X(3).
           10 SATZWEIGSCHLUESSEL   PIC X(2).
           10 SATHINWAUFSELBSTB    PIC X(1).
           10 SATVERSLSTID         PIC S9(4) USAGE COMP.
           10 SATSELBSTBETEIL      PIC S9(9)V9(2) USAGE COMP-3.
           10 SATSEBSTBETWKZ       PIC X(1).
           10 SATGLASBERECHID      PIC S9(4) USAGE COMP.
           10 SATGLASBERECHWERT    PIC S9(9)V9(2) USAGE COMP-3.
           10 SATGLASBERWERTWKZ    PIC X(1).
           10 SATJAHRESBEITRAG     PIC S9(9)V9(2) USAGE COMP-3.
           10 SATJAHRBEITRWKZ      PIC X(1).
           10 SATSUBTYPE           PIC X(1).
           10 SATPRODUKTTEXTFV     PIC X(20).
           10 SATSONSTIGE          PIC X(30).
           10 SATPRODVARIANTE      PIC X(30).
           10 SATTARIFSB           PIC S9(9)V9(2) USAGE COMP-3.
           10 SATTARIFSBWKZ        PIC X(1).
           10 SATRISKANSCHRPLZ     PIC X(6).
           10 SATRISKANSCHRSTR     PIC X(45).
           10 SATRISKANSCHRORT     PIC X(30).
           10 SATEGPRIVATGH        PIC S9(9)V9(2) USAGE COMP-3.
           10 SATEGPRIVATGHWKZ     PIC X(1).
           10 SATEGKUENSTGS        PIC S9(9)V9(2) USAGE COMP-3.
           10 SATEGKUENSTGSKZ      PIC X(1).
           10 SATEEGFKOSTENG       PIC S9(9)V9(2) USAGE COMP-3.
           10 SATEEGFKOSTENGWKZ    PIC X(1).
           10 SATZUKNETTOJBR       PIC S9(9)V9(2) USAGE COMP-3.
           10 SATZUKNETTOJBRWKZ    PIC X(1).
           10 SATKZDIFFWG          PIC X(1).
           10 SATDIFFGES           PIC X(90).
           10 SATDIFFVNR           PIC X(40).
           10 SATDIFFABLAUF        PIC X(10).
           10 SATKZDIFFKUEND       PIC X(1).
           10 SATKUNDENBONUS       PIC X(1).
           10 SATDAUERNACHLASS     PIC X(1).
           10 SATVSPNACHLASS       PIC X(1).
           10 SATOEFFDIENSTNL      PIC X(1).
           10 SATSOFORTRABATT      PIC X(2).
           10 SATKOMBIRABATT       PIC S9(4) USAGE COMP.
           10 SATJHRBTRGNETTO      PIC S9(9)V9(2) USAGE COMP-3.
           10 SATJHRBNETTOWKZ      PIC X(1).
           10 SATRISIKBRTGES       PIC S9(9)V9(2) USAGE COMP-3.
           10 SATRISIKBRTGWKZ      PIC X(1).
           10 SATRISIKNETGES       PIC S9(9)V9(2) USAGE COMP-3.
           10 SATRISIKNETGWKZ      PIC X(1).
           10 SATMITARBRAB         PIC S9(4) USAGE COMP.
           10 SATMITARBID          PIC X(12).
           10 SATVERSANDWEG-KTY    PIC S9(4) USAGE COMP.
           10 SATVERSANDWEG-KNR    PIC S9(4) USAGE COMP.
           10 SATANNAHMEDAT        PIC X(10).
           10 SATBESTMANAGKZ       PIC X(1).
      ************************** ENDE-DCLGEN *************************
