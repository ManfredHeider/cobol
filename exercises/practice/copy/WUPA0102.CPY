      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUPA0102.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUPA0102.CPv  $
      *
      *   Rev 2.1   20 Jul 2001 10:28:26   T13EX31
      *APA00143 - DB2-Datenmigration Euro-Umstellung
      *
      *   Rev 2.0   08 Jun 2001 08:34:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * TBPA0102                                                       *
      ******************************************************************
         05  TBPA0102.
           10 PMPG-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPG-VERSION         PIC S9(4) USAGE COMP.
           10 PMPG-LFD-NR-VNR      PIC S9(9) USAGE COMP.
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PMPE-VNR             PIC X(15).
           10 PMPE-VERTR-BEGINN    PIC X(10).
           10 PMPE-VERTR-ABLAUF    PIC X(10).
           10 PMPE-FAELLIG-VON     PIC X(10).
           10 PMPE-FAELLIG-BIS     PIC X(10).
           10 PMPE-PG-VBNR         PIC S9(9) USAGE COMP.
           10 PMPE-KUNDE-NAME      PIC X(25).
           10 PMPE-KUNDE-VNAME     PIC X(20).
           10 PMPE-GSMT-PROV-DAT   PIC X(10).
           10 PMPE-PG-ABR-DAT      PIC X(10).
           10 PMPE-PG-EINL-DAT     PIC X(10).
           10 PMPE-PIN             PIC S9(9) USAGE COMP.
           10 KNR-ZAHLWEISE        PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 PMPE-VSTAT           PIC X(4).
           10 PMPE-LAST-UPDATE     PIC X(26).
           10 PMPE-KENNUNG         PIC X(8).
           10 PMPE-PG-PM-DATEN.
              12 PMPE-PG-PM-DATEN-LEN  PIC S9(4) USAGE COMP.
              12 PMPE-PG-PM-DATEN-TEXT  PIC X(300).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 23      *
      ******************************************************************
           12 ALLG-PG-DATEN REDEFINES PMPE-PG-PM-DATEN-TEXT PIC X(300).
290599*      10 ALLG-AMV-DATEN REDEFINES ALLG-PG-DATEN IN Z-DRPA0288.
290599       12 ALLG-AMV-DATEN REDEFINES ALLG-PG-DATEN.
                15 TAB-AMV-VVNR                     OCCURS 10.
                   20 AMV-VVNR                      PIC  X(015).
                15 FILLER                           PIC  X(150).
      *
070400       12 ALLG-BADENIA-DATEN REDEFINES ALLG-PG-DATEN.
070400          15 BAD-VERTR-SPLIT-KZ               PIC  X(001).
070400          15 FILLER                           PIC  X(299).
      *
070700       12 ALLG-III-DATEN REDEFINES ALLG-PG-DATEN.
070700          15 FILLER                           PIC  X(300).
      *
150101       12 ALLG-BFG-DATEN REDEFINES ALLG-PG-DATEN.
150101          15 FILLER                           PIC  X(299).
150101          15 BFG-ERSTELL-KZ                   PIC  X(001).
      *
210201       12 ALLG-COBA-DATEN REDEFINES ALLG-PG-DATEN.
210201          15 FILLER                           PIC  X(295).
210201          15 AENDERUNGSGRUND                  PIC  X(002).
210201          15 COBA-ERSTELL-KZ                  PIC  X(001).
210201          15 FILLER                           PIC  X(002).
      *
280201       12 ALLG-AML-DARL-DATEN REDEFINES ALLG-PG-DATEN.
280201          15 FILLER                           PIC  X(297).
280201          15 AML-ERSTELL-KZ                   PIC  X(001).
280201          15 FILLER                           PIC  X(002).
      *
280201       12 ALLG-CENTRAL-DATEN REDEFINES ALLG-PG-DATEN.
280201          15 HAFTZEITBEGINN                   PIC  X(010).
280201          15 HAFTZEITDAUER                    PIC  X(003).
280201          15 FAELLIGKEITSDAUER                PIC  X(003).
280201          15 FILLER                           PIC  X(284).
      *
280201       12 ALLG-BFG-BANK-DATEN REDEFINES ALLG-PG-DATEN.
280201          15 FILLER                           PIC  X(297).
280201          15 BFG-B-ERSTELL-KZ                 PIC  X(001).
280201          15 FILLER                           PIC  X(002).
      *
280301       12 ALLG-DGI-DATEN REDEFINES ALLG-PG-DATEN.
280301          15 FILLER                           PIC  X(294).
280301          15 EINMAL-ANL-KZ                    PIC  X(001).
280301          15 UMSATZSCHL                       PIC  X(002).
280301          15 DGI-ERSTELL-KZ                   PIC  X(001).
280301          15 FILLER                           PIC  X(002).
      *
090401       12 ALLG-BAWAG-DATEN REDEFINES ALLG-PG-DATEN.
090401          15 FILLER                           PIC  X(297).
090401          15 BAWAG-ERSTELL-KZ                 PIC  X(001).
090401          15 FILLER                           PIC  X(002).
