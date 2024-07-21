      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0353.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/WUKI0353.CPv  $
      *
      *   Rev 2.12   Mar 30 2020 08:37:58   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.11   Dec 09 2019 13:47:56   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050 OZP
      *
      *   Rev 2.10   Aug 28 2013 13:18:54   t13ex19
      *AKI01249: AdvoCard - Sofortrabatt
      *
      *   Rev 2.9   Jul 21 2009 13:34:00   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.8   Oct 17 2005 10:08:04   t13ex27
      *AKI00521: DB-Änderung für Fix 80 PMKI0079 und WUKI's
      *
      *   Rev 2.7   May 24 2004 11:45:24   t13ex27
      *DB-Änderung für Fix 61 PMKI0079 und WUKI's
      *
      *   Rev 2.6   Jun 11 2003 13:12:16   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:22   p13sp16
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
250509*AKI00817!neue Attribute SATKUNDENBONUS           !W.Misgeld!    *
      *        !               SATDAUERNACHLASS         !         !    *
      *        !               SATVSPNACHLASS           !         !    *
      *        !               SATOEFFDIENSTNL    !     !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *---------------------------------------------------------------*
        05  WU-WUKI0353.
           10 SATVERTRAGSID                 PIC  X(0013).
           10 SATVERSTARLFDNR               PIC S9(4) COMP.
           10 SATZWEIGTEXT                  PIC  X(0003).
           10 NIL-SATZWEIGTEXT              PIC  X(01).
           10 SATZWEIGSCHLUESSEL            PIC  X(0002).
           10 NIL-SATZWEIGSCHLUESSEL        PIC  X(01).
           10 SATHINWAUFSELBSTB             PIC  X(0001).
           10 SATVERSLSTID                  PIC S9(4) COMP.
           10 NIL-SATVERSLSTID              PIC  X(01).
           10 SATSELBSTBETEIL               PIC S9(9)V9(2) COMP-3.
           10 NIL-SATSELBSTBETEIL           PIC  X(01).
           10 SATSEBSTBETWKZ                PIC  X(0001).
           10 SATGLASBERECHID               PIC S9(4) COMP.
           10 NIL-SATGLASBERECHID           PIC  X(01).
           10 SATGLASBERECHWERT             PIC S9(9)V9(2) COMP-3.
           10 NIL-SATGLASBERECHWERT         PIC  X(01).
           10 SATGLASBERWERTWKZ             PIC  X(0001).
           10 SATJAHRESBEITRAG              PIC S9(9)V9(2) COMP-3.
           10 NIL-SATJAHRESBEITRAG          PIC  X(01).
           10 SATJAHRBEITRWKZ               PIC  X(0001).
           10 SATSUBTYPE                    PIC  X(0001).
           10 NIL-SATSUBTYPE                PIC  X(01).
           10 SATPRODUKTTEXTFV              PIC  X(0020).
           10 NIL-SATPRODUKTTEXTFV          PIC  X(01).
           10 SATSONSTIGE                   PIC  X(0030).
           10 NIL-SATSONSTIGE               PIC  X(01).
           10 SATPRODVARIANTE               PIC  X(0030).
           10 NIL-SATPRODVARIANTE           PIC  X(01).
           10 SATTARIFSB                    PIC S9(9)V9(2) COMP-3.
           10 NIL-SATTARIFSB                PIC  X(01).
           10 SATTARIFSBWKZ                 PIC  X(0001).
           10 NIL-SATTARIFSBWKZ             PIC  X(01).
           10 SATRISKANSCHRPLZ              PIC  X(0006).
           10 NIL-SATRISKANSCHRPLZ          PIC  X(01).
           10 SATRISKANSCHRSTR              PIC  X(0045).
           10 NIL-SATRISKANSCHRSTR          PIC  X(01).
           10 SATRISKANSCHRORT              PIC  X(0030).
           10 NIL-SATRISKANSCHRORT          PIC  X(01).
           10 SATEGPRIVATGH                 PIC S9(9)V9(2) COMP-3.
           10 NIL-SATEGPRIVATGH             PIC  X(01).
           10 SATEGPRIVATGHWKZ              PIC  X(0001).
           10 NIL-SATEGPRIVATGHWKZ          PIC  X(01).
           10 SATEGKUENSTGS                 PIC S9(9)V9(2) COMP-3.
           10 NIL-SATEGKUENSTGS             PIC  X(01).
           10 SATEGKUENSTGSKZ               PIC  X(0001).
           10 NIL-SATEGKUENSTGSKZ           PIC  X(01).
           10 SATEEGFKOSTENG                PIC S9(9)V9(2) COMP-3.
           10 NIL-SATEEGFKOSTENG            PIC  X(01).
           10 SATEEGFKOSTENGWKZ             PIC  X(0001).
           10 NIL-SATEEGFKOSTENGWKZ         PIC  X(01).
           10 SATZUKNETTOJBR                PIC S9(9)V9(2) COMP-3.
           10 NIL-SATZUKNETTOJBR            PIC  X(01).
           10 SATZUKNETTOJBRWKZ             PIC  X(0001).
           10 NIL-SATZUKNETTOJBRWKZ         PIC  X(01).
           10 SATKZDIFFWG                   PIC  X(0001).
           10 NIL-SATKZDIFFWG               PIC  X(01).
           10 SATDIFFGES                    PIC  X(0090).
           10 NIL-SATDIFFGES                PIC  X(01).
           10 SATDIFFVNR                    PIC  X(0040).
           10 NIL-SATDIFFVNR                PIC  X(01).
           10 SATDIFFABLAUF                 PIC  X(010).
           10 NIL-SATDIFFABLAUF             PIC  X(01).
           10 SATKZDIFFKUEND                PIC  X(0001).
           10 NIL-SATKZDIFFKUEND            PIC  X(01).
           10 SATKUNDENBONUS                PIC  X(0001).
           10 NIL-SATKUNDENBONUS            PIC  X(01).
           10 SATDAUERNACHLASS              PIC  X(0001).
           10 NIL-SATDAUERNACHLASS          PIC  X(01).
           10 SATVSPNACHLASS                PIC  X(0001).
           10 NIL-SATVSPNACHLASS            PIC  X(01).
           10 SATOEFFDIENSTNL               PIC  X(0001).
           10 NIL-SATOEFFDIENSTNL           PIC  X(01).
           10 SATSOFORTRABATT               PIC  X(0002).
           10 NIL-SATSOFORTRABATT           PIC  X(01).
           10 SATKOMBIRABATT                PIC S9(4) COMP.
           10 NIL-SATKOMBIRABATT            PIC  X(01).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 SATJHRBTRGNETTO               PIC S9(9)V9(2) COMP-3.
           10 NIL-SATJHRBTRGNETTO           PIC  X(01).
           10 SATJHRBNETTOWKZ               PIC  X(0001).
           10 NIL-SATJHRBNETTOWKZ           PIC  X(01).
           10 SATRISIKBRTGES                PIC S9(9)V9(2) COMP-3.
           10 NIL-SATRISIKBRTGES            PIC  X(01).
           10 SATRISIKBRTGWKZ               PIC  X(0001).
           10 NIL-SATRISIKBRTGWKZ           PIC  X(01).
           10 SATRISIKNETGES                PIC S9(9)V9(2) COMP-3.
           10 NIL-SATRISIKNETGES            PIC  X(01).
           10 SATRISIKNETGWKZ               PIC  X(0001).
           10 NIL-SATRISIKNETGWKZ           PIC  X(01).
           10 SATMITARBRAB                  PIC S9(4) COMP.
           10 NIL-SATMITARBRAB              PIC  X(01).
           10 SATMITARBID                   PIC  X(0012).
           10 NIL-SATMITARBID               PIC  X(01).
           10 SATVERSANDWEG-KTY             PIC S9(4) COMP.
           10 NIL-SATVERSANDWEG-KTY         PIC  X(01).
           10 SATVERSANDWEG-KNR             PIC S9(4) COMP.
           10 NIL-SATVERSANDWEG-KNR         PIC  X(01).
           10 SATANNAHMEDAT                 PIC  X(010).
           10 NIL-SATANNAHMEDAT             PIC  X(01).
           10 SATBESTMANAGKZ                PIC  X(0001).
           10 NIL-SATBESTMANAGKZ            PIC  X(01).
