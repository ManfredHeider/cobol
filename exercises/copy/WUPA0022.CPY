      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUPA0022.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUPA0022.CPv  $
      *
      *   Rev 2.1   20 Jul 2001 10:27:54   T13EX31
      *APA00143 - DB2-Datenmigration Euro-Umstellung
      *
      *   Rev 2.0   08 Jun 2001 08:34:52   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * TBPA0022                                                       *
      ******************************************************************
         05  TBPA0022.
           10 PEWA-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PEWA-PIN             PIC S9(9) USAGE COMP.
           10 PEWA-VNR             PIC X(15).
           10 PEWA-VOR-VNR-KZ      PIC X(1).
           10 PEWA-KUNDE-NAME      PIC X(25).
           10 PEWA-KUNDE-VNAME     PIC X(20).
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KNR-ZAHLWEISE        PIC S9(4) USAGE COMP.
           10 PEWA-DIR             PIC S9(4) USAGE COMP.
           10 PEWA-AVERM           PIC S9(9) USAGE COMP.
           10 PEWA-VM              PIC S9(9) USAGE COMP.
           10 PEWA-BEGINN          PIC X(10).
           10 PEWA-AN-AUFN-DAT     PIC X(10).
           10 PEWA-STRUK-DAT       PIC X(10).
           10 PEWA-AN-ARB-DAT      PIC X(10).
           10 PEWA-PG-WV-DAT       PIC X(10).
           10 PEWA-PG-MELD-DAT     PIC X(10).
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PEWA-VSTAT           PIC X(4).
           10 PEWA-VSTAT-UNR       PIC S9(4) USAGE COMP.
           10 PEWA-FEHLER-ZAHL     PIC S9(4) USAGE COMP.
           10 KNR-PEWA-ZUSTAND     PIC S9(4) USAGE COMP.
           10 KNR-STORNO-KZ        PIC S9(4) USAGE COMP.
           10 PEWA-STORNO-DAT      PIC X(10).
           10 KNR-RUECKMELD-STAT   PIC S9(4) USAGE COMP.
           10 PEWA-RUECKMELD-DAT   PIC X(10).
           10 PEWA-ANZ-MELD-PG     PIC S9(4) USAGE COMP.
           10 PEWA-PEST-VSTAT      PIC X(4).
           10 PEWA-LAST-UPDATE     PIC X(26).
           10 PEWA-KENNUNG         PIC X(8).
           10 PEWAH-LAST-UPDATE    PIC X(26).
           10 PEWAH-KENNUNG        PIC X(8).
           10 PEWA-PROV-STRUK.
              47 PEWA-PROV-STRUK-LEN  PIC S9(4) USAGE COMP.
              47 PEWA-PROV-STRUK-TEXT  PIC X(320).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 34      *
      ******************************************************************
              47 Z-STRUKTUR-TABELLE REDEFINES PEWA-PROV-STRUK-TEXT.
                 48 Z-STRUKTUR-ZEILE         OCCURS 20.
                    49 Z-MA-ST-NR            PIC  9(007).
                    49 Z-EMP-STUFE           PIC  X(004).
                    49 Z-DIFF-PROV-STUFE     PIC  9(003).
                    49 Z-KNR-EMP-TYP         PIC  9(002).

