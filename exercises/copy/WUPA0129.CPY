      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUPA0129.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:\MF\CPY\WUPA0129.CPv  $
      *
      *   Rev 1.1   20 Jul 2001 10:31:10   T13EX31
      *APA00143 - DB2-Datenmigration Euro-Umstellung
      *
      *   Rev 1.0   30 May 2001 08:29:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * TBPA____                                                       *
      ******************************************************************
         05  DCL-TBPA0129.
           10 PDPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PVER-VNR             PIC X(15).
           10 PAMS-ABR-SORT-NR     PIC X(15).
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 PMPT-FAELLIG-VON     PIC X(10).
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PVER-VERSION         PIC S9(4) USAGE COMP.
           10 PVER-KUNDE-NAME      PIC X(25).
           10 PVER-KUNDE-VNAME     PIC X(20).
           10 PMBE-RFOLGE-NR       PIC S9(4) USAGE COMP.
           10 PMPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPT-VERSION         PIC S9(4) USAGE COMP.
           10 PMPT-GSMT-PROV       PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPT-PPFL-BEITRAG    PIC S9(9)V9(2) USAGE COMP-3.
           10 PVPT-VT-PRIO         PIC S9(4) USAGE COMP.
           10 PEWA-ID              PIC S9(15)V USAGE COMP-3.
           10 PDPT-PRODART         PIC S9(4) USAGE COMP.
           10 PDPT-DGV             PIC S9(4) USAGE COMP.
           10 PMPT-EINH            PIC S9(6)V9(2) USAGE COMP-3.
           10 KNR-BERCH-WAEHRUNG   PIC S9(4) USAGE COMP.
           10 KNR-KZ-EH-MAX        PIC S9(4) USAGE COMP.
           10 KNR-BUCHUNGS-STAND   PIC S9(4) USAGE COMP.
           10 PEST-AVERM           PIC S9(9) USAGE COMP.
           10 PEST-VM-GESCHFT-KZ   PIC X(1).
           10 MAX-INDEX-PE         PIC S9(4) USAGE COMP.
           10 PE-STRUKTUR-DATEN.
              49 PE-STRUKTUR-DATEN-LEN  PIC S9(4) USAGE COMP.
              49 PE-STRUKTUR-DATEN-TEXT  PIC X(800).
      ******************************************************************
           10 PAMS-STRUKTUR-TABELLE REDEFINES PE-STRUKTUR-DATEN.
              32 PAMS-STRUKTUR-TABELLE-LAENGE       PIC S9(004) BINARY.
              32 FILLER                          OCCURS 20
                                                 INDEXED BY IX-PAMS-ST.
       35 PAMS-STRUKTUR-DATEN.
          40 PEBD-ST-NR                  PIC S9(009) BINARY.
          40 PEBD-DIFF-PROV-STF          PIC S9(004) BINARY.
          40 KNR-EMP-TYP                 PIC S9(004) BINARY.
          40 PDPE-BUCH-RELEVANT          PIC X.
          40 PDPE-EINH                   PIC S9(6)V99    PACKED-DECIMAL.
          40 PDPE-NT-PBTRG-BRWG          PIC S9(009)V99  PACKED-DECIMAL.
          40 PDPE-PROV-RCK-BRWG          PIC S9(009)V99  PACKED-DECIMAL.
          40 KNR-AUSZ-WAEHRUNG           PIC S9(004)     BINARY.
          40 PDPE-NT-PBTRG-AZWG          PIC S9(009)V99  PACKED-DECIMAL.
          40 PDPE-PROV-RCK-AZWG          PIC S9(009)V99  PACKED-DECIMAL.
