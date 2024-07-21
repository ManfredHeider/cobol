      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0353.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WDKI0353.CPv  $
      *
      *   Rev 2.4   Oct 17 2005 08:58:02   t13ex27
      *AKI00521: DB-Änderung für Fix 80 EA-Programme und WDKI's
      *
      *   Rev 2.3   May 24 2004 10:54:00   t13ex27
      *DB-Änderung für Fix 61 EA-Programme und WDKI's
      *
      *   Rev 2.2   Jun 03 2002 09:32:38   T13EX27
      *AKI00289 DB-Änderung für neue VP-Analyse
      *
      *   Rev 2.1   Apr 16 1999 10:10:48   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:44   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERSSACHTARIF-GESAMT.
         10 EA-NI-VERSSACHTARIF-TAB.
           20 EA-NI-VERSSACHTARIF-ELE PIC X OCCURS 35 TIMES.
         10 EA-NI-VERSSACHTARIF
                 REDEFINES EA-NI-VERSSACHTARIF-TAB.
           20 EA-NI-SATVERTRAGSID            PIC X.
           20 EA-NI-SATVERSTARLFDNR          PIC X.
           20 EA-NI-SATZWEIGTEXT             PIC X.
           20 EA-NI-SATZWEIGSCHLUESSEL       PIC X.
           20 EA-NI-SATHINWAUFSELBSTB        PIC X.
           20 EA-NI-SATVERSLSTID             PIC X.
           20 EA-NI-SATSELBSTBETEIL          PIC X.
           20 EA-NI-SATSEBSTBETWKZ           PIC X.
           20 EA-NI-SATGLASBERECHID          PIC X.
           20 EA-NI-SATGLASBERECHWERT        PIC X.
           20 EA-NI-SATGLASBERWERTWKZ        PIC X.
           20 EA-NI-SATJAHRESBEITRAG         PIC X.
           20 EA-NI-SATJAHRBEITRWKZ          PIC X.
           20 EA-NI-SATSUBTYPE               PIC X.
           20 EA-NI-SATPRODUKTTEXTFV         PIC X.
           20 EA-NI-SATSONSTIGE              PIC X.
           20 EA-NI-SATPRODVARIANTE          PIC X.
           20 EA-NI-SATTARIFSB               PIC X.
           20 EA-NI-SATTARIFSBWKZ            PIC X.
           20 EA-NI-SATRISKANSCHRPLZ         PIC X.
           20 EA-NI-SATRISKANSCHRSTR         PIC X.
           20 EA-NI-SATRISKANSCHRORT         PIC X.
           20 EA-NI-SATEGPRIVATGH            PIC X.
           20 EA-NI-SATEGPRIVATGHWKZ         PIC X.
           20 EA-NI-SATEGKUENSTGS            PIC X.
           20 EA-NI-SATEGKUENSTGSKZ          PIC X.
           20 EA-NI-SATEEGFKOSTENG           PIC X.
           20 EA-NI-SATEEGFKOSTENGWKZ        PIC X.
           20 EA-NI-SATZUKNETTOJBR           PIC X.
           20 EA-NI-SATZUKNETTOJBRWKZ        PIC X.
           20 EA-NI-SATKZDIFFWG              PIC X.
           20 EA-NI-SATDIFFGES               PIC X.
           20 EA-NI-SATDIFFVNR               PIC X.
           20 EA-NI-SATDIFFABLAUF            PIC X.
           20 EA-NI-SATKZDIFFKUEND           PIC X.
         10 EA-DCL-VERSSACHTARIF.
           20 EA-SATVERTRAGSID               PIC X(13).
           20 EA-SATVERSTARLFDNR             PIC -----9.
           20 EA-SATZWEIGTEXT                PIC X(3).
           20 EA-SATZWEIGSCHLUESSEL          PIC X(2).
           20 EA-SATHINWAUFSELBSTB           PIC X(1).
           20 EA-SATVERSLSTID                PIC -----9.
           20 EA-SATSELBSTBETEIL             PIC ---------9.99.
           20 EA-SATSEBSTBETWKZ              PIC X(1).
           20 EA-SATGLASBERECHID             PIC -----9.
           20 EA-SATGLASBERECHWERT           PIC ---------9.99.
           20 EA-SATGLASBERWERTWKZ           PIC X(1).
           20 EA-SATJAHRESBEITRAG            PIC ---------9.99.
           20 EA-SATJAHRBEITRWKZ             PIC X(1).
           20 EA-SATSUBTYPE                  PIC X(1).
           20 EA-SATPRODUKTTEXTFV            PIC X(20).
           20 EA-SATSONSTIGE                 PIC X(30).
           20 EA-SATPRODVARIANTE             PIC X(30).
           20 EA-SATTARIFSB                  PIC ---------9.99.
           20 EA-SATTARIFSBWKZ               PIC X(1).
           20 EA-SATRISKANSCHRPLZ            PIC X(6).
           20 EA-SATRISKANSCHRSTR            PIC X(45).
           20 EA-SATRISKANSCHRORT            PIC X(30).
           20 EA-SATEGPRIVATGH               PIC ---------9.99.
           20 EA-SATEGPRIVATGHWKZ            PIC X(1).
           20 EA-SATEGKUENSTGS               PIC ---------9.99.
           20 EA-SATEGKUENSTGSKZ             PIC X(1).
           20 EA-SATEEGFKOSTENG              PIC ---------9.99.
           20 EA-SATEEGFKOSTENGWKZ           PIC X(1).
           20 EA-SATZUKNETTOJBR              PIC ---------9.99.
           20 EA-SATZUKNETTOJBRWKZ           PIC X(1).
           20 EA-SATKZDIFFWG                 PIC X(1).
           20 EA-SATDIFFGES                  PIC X(90).
           20 EA-SATDIFFVNR                  PIC X(40).
           20 EA-SATDIFFABLAUF               PIC X(010).
           20 EA-SATKZDIFFKUEND              PIC X(1).
      *****************************************************************
