      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUPA0100.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUPA0100.CPv  $
      *
      *   Rev 2.1   20 Jul 2001 10:28:10   T13EX31
      *APA00143 - DB2-Datenmigration Euro-Umstellung
      *
      *   Rev 2.0   08 Jun 2001 08:34:54   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * TBPA0100                                                       *
      ******************************************************************
         05  TBPA0100.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PMPG-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPG-VERSION         PIC S9(4) USAGE COMP.
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPG-LFD-NR-VNR      PIC S9(9) USAGE COMP.
           10 PMPG-VSTAT           PIC X(4).
           10 PMPG-LAST-UPDATE     PIC X(26).
           10 PMPG-KENNUNG         PIC X(8).
           10 PMPG-INHALT.
              30 PMPG-INHALT-LEN   PIC S9(4) USAGE COMP.
              30 PMPG-INHALT-TEXT  PIC X(3900).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
      * DRPA0295: GES 10                                               *
      ******************************************************************
             30 AMV-DATENSATZ REDEFINES PMPG-INHALT-TEXT.
                35 VNR                              PIC  9(014).
                35 VVNR1                            PIC  9(014).
                35 VVNR2                            PIC  9(014).
                35 VVNR3                            PIC  9(014).
                35 VERTRAG-BEGINN                   PIC  X(010).
                35 VERTRAG-ABLAUF                   PIC  X(010).
                35 FAELLIG-VON                      PIC  X(010).
                35 FAELLIG-BIS                      PIC  X(010).
                35 ERST-DAT                         PIC  X(010).
                35 AVERM                            PIC  9(007).
                35 PGEB-GES                         PIC  9(002).
                35 KUNDE-NAME                       PIC  X(025).
                35 KUNDE-VNAME                      PIC  X(020).
                35 ABR-DAT-PG                       PIC  X(010).
                35 EINL-DAT-PG                      PIC  X(010).
                35 PIN                              PIC  9(007).
                35 ZAHLWEISE                        PIC  9(002).
                35 WAEHRUNG                         PIC  X(003).
260100          35 SYSTEM-KENNZEICHEN               PIC  X(001).
260100          35 WERTSTUECKART                    PIC  X(001).
260100          35 GV-TIMESTAMP                     PIC  X(026).
260100          35 FILLER                           PIC  X(100).
                35 TAB-INDEX                        PIC  9(002).
                35 PRODUKT-TABELLE.
                   40 PRODUKT-ZEILE                 OCCURS 50
                                                INDEXED BY IX-AMV-PROD.
                      45 PGV-PG                     PIC  9(004).
                      45 TARIF                      PIC  X(010).
                      45 PPFL-BEITRAG               PIC S9(007)V99.
                      45 PROV-SATZ                  PIC  9(003)V99.
                      45 PROV-BETRAG                PIC S9(007)V99.
                      45 JAHRES-NT-PRAEMIE          PIC S9(007)V99.
      ******************************************************************
      * DRPA1330: GES. 70                                              *
      ******************************************************************
             30 BADENIA-DATENSATZ REDEFINES PMPG-INHALT-TEXT.
                35 VNR                              PIC  9(011).
                35 VERTRAG-BEGINN                   PIC  9(008).
                35 AVERM                            PIC  9(007).
                35 PGEB-GES                         PIC  9(002).
                35 KUNDE-NNAME                      PIC  X(025).
                35 KUNDE-VNAME                      PIC  X(020).
                35 ABR-DAT-PG                       PIC  9(008).
                35 DAT-LETZTE-KD-ZAHL               PIC  9(008).
                35 PIN                              PIC  9(007).
                35 VERTRAG-WAEHRUNG                 PIC  X(003).
                35 PGV-PG                           PIC  9(004).
                35 TARIF                            PIC  9(003).
                35 VERTRAG-SUMME                    PIC S9(009)V99.
                35 PPFL-SUMME                       PIC S9(009)V99.
230500          35 PROV-SATZ                        PIC  9(003)V999.
                35 PROV-BETRAG                      PIC S9(007)V99.
                35 VERTRAG-SPLIT-KZ                 PIC  X(001).
      ******************************************************************
      * DRPA1393: GES. 41                                              *
      ******************************************************************
             30 III-DATENSATZ REDEFINES PMPG-INHALT-TEXT.
                35 VNR                              PIC  9(015).
                35 VERTRAG-BEGINN                   PIC  X(010).
                35 FAELLIG-VON                      PIC  X(010).
                35 AVERM                            PIC  9(007).
                35 PGEB-GES                         PIC  9(002).
                35 KUNDE-NNAME                      PIC  X(035).
                35 KUNDE-VNAME                      PIC  X(035).
                35 ABR-DAT-PG                       PIC  X(010).
                35 PIN                              PIC  9(007).
                35 ZAHLWEISE                        PIC  9(002).
                35 VERTRAG-WAEHRUNG                 PIC  X(003).
                35 PGV-PG                           PIC  9(004).
                35 WPKN                             PIC  X(012).
                35 SPARPLAN-NR                      PIC  9(002).
                35 PPFL-SUMME                       PIC S9(007)V99.
                35 SPARBETRAG                       PIC S9(007)V99.
                35 PROV-SATZ                        PIC  9(003)V999.
                35 PROV-BETRAG                      PIC S9(007)V99.
      ******************************************************************
      * DRPA1420: GES. 40                                              *
      ******************************************************************
             30 DIT-DATENSATZ REDEFINES PMPG-INHALT-TEXT.
                35 VNR                              PIC  9(015).
                35 VERTRAG-BEGINN                   PIC  X(010).
                35 FAELLIG-VON                      PIC  X(010).
                35 AVERM                            PIC  9(007).
                35 PGEB-GES                         PIC  9(002).
                35 KUNDE-NNAME                      PIC  X(030).
                35 KUNDE-VNAME                      PIC  X(020).
                35 ABR-DAT-PG                       PIC  X(010).
                35 PIN                              PIC  9(007).
                35 ZAHLWEISE                        PIC  9(002).
                35 VERTRAG-WAEHRUNG                 PIC  X(003).
                35 PGV-PG                           PIC  X(004).
                35 WPKN                             PIC  X(012).
                35 PGV-KENNZEICHEN                  PIC  X(001).
                35 PROV-SATZ                        PIC  9(003)V999.
                35 PROV-BETRAG                      PIC S9(007)V99.
                35 PPFL-SUMME                       PIC S9(007)V99.
                35 SPARBETRAG                       PIC S9(007)V99.
                35 VL-KENNZEICHEN                   PIC  X(002).
                35 FILLER                           PIC  X(013).
      ******************************************************************
      * DRPA1430: GES. 64                                              *
      ******************************************************************
             30 HYPO-DATENSATZ REDEFINES PMPG-INHALT-TEXT.
                35 VNR                      PIC  9(015).
                35 FAELLIG-VON              PIC  X(010).
                35 AVERM                    PIC  9(007).
                35 PGEB-GES                 PIC  9(002).
                35 KUNDE-NNAME              PIC  X(030).
                35 KUNDE-VNAME              PIC  X(030).
                35 ABR-DAT-PG               PIC  X(010).
                35 VERTRAG-WAEHRUNG         PIC  X(003).
                35 PGV-PG                   PIC  X(004).
                35 PROD-BEZ                 PIC  X(015).
                35 PROV-SATZ                PIC S9(004)V9(03) COMP-3.
                35 PROV-BETRAG              PIC S9(007)V9(02) COMP-3.
                35 PPFL-SUMME               PIC S9(007)V9(02) COMP-3.
                35 FILLER                   PIC  X(019).
