      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUPA0122.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUPA0122.CPv  $
      *
      *   Rev 2.1   20 Jul 2001 10:30:22   T13EX31
      *APA00143 - DB2-Datenmigration Euro-Umstellung
      *
      *   Rev 2.0   08 Jun 2001 08:35:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * TBPA0122                                                       *
      ******************************************************************
         05  DCL-TBPA0122.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PDPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPT-VERSION         PIC S9(4) USAGE COMP.
           10 PEST-ID              PIC S9(15)V USAGE COMP-3.
           10 PDPT-BUCH-RFOLGE     PIC S9(4) USAGE COMP.
           10 PDPT-MA-ABR-DAT      PIC X(10).
           10 KNR-BUCHUNGS-STAND   PIC S9(4) USAGE COMP.
           10 KNR-BUCHUNGSART      PIC S9(4) USAGE COMP.
           10 KNR-BUCHUNGS-GRUND   PIC S9(4) USAGE COMP.
           10 KNR-KZ-EH-MAX        PIC S9(4) USAGE COMP.
           10 KNR-BERCH-WAEHRUNG   PIC S9(4) USAGE COMP.
           10 PDPT-DGV             PIC S9(4) USAGE COMP.
           10 PDPT-PRODART         PIC S9(4) USAGE COMP.
           10 PDPT-FIRST-INSERT    PIC X(26).
           10 PDPT-FIRST-KENNUNG   PIC X(8).
           10 PDPT-LAST-UPDATE     PIC X(26).
           10 PDPT-KENNUNG         PIC X(8).
           10 PDPT-PDPE-DATEN.
              14 PDPT-PDPE-DATEN-LEN  PIC S9(4) USAGE COMP.
              14 PDPT-PDPE-DATEN-TEXT  PIC X(720).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
           10 RD-PDPT-PDPE-DATEN REDEFINES PDPT-PDPE-DATEN.
                27 PDPE-DATEN-LEN  PIC S9(4) USAGE COMP.
                27 PDPE-TAB1                   OCCURS 20
                                              INDEXED BY IX-PDPE1.
                30 PDPE-DATEN.
010898*
010898*          35 PDPT-ID                          PIC S9(015)
010898*                                                PACKED-DECIMAL.
010898*          35 PDPE-POSITIONS-NR                PIC S9(004) BINARY.
010898           35 PDPE-ST-NR                       PIC S9(009) BINARY.
                 35 PDPE-NT-PBTRG-BRWG               PIC S9(009)V9(2)
                                                       PACKED-DECIMAL.
                 35 PDPE-PROV-RCK-BRWG               PIC S9(009)V9(2)
                                                       PACKED-DECIMAL.
                 35 PDPE-EINH                        PIC S9(006)V9(2)
                                                       PACKED-DECIMAL.
                 35 PDPE-NT-PBTRG-AZWG               PIC S9(009)V9(2)
                                                       PACKED-DECIMAL.
                 35 PDPE-PROV-RCK-AZWG               PIC S9(009)V9(2)
                                                       PACKED-DECIMAL.
                 35 KNR-AUSZ-WAEHRUNG                PIC S9(004) BINARY.
                 35 PDPE-BUCH-RELEVANT               PIC  X(001).
