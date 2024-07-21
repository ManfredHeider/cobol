      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUPA0103.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:\MF\CPY\WUPA0103.CPv  $
      *
      *   Rev 1.1   20 Jul 2001 10:28:44   T13EX31
      *APA00143 - DB2-Datenmigration Euro-Umstellung
      *
      *   Rev 1.0   30 May 2001 08:29:08   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * TBPA0103                                                       *
      ******************************************************************
         05  WU-TBPA0103.
           10 PMPP-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPG-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 PMPP-PGV             PIC S9(4) USAGE COMP.
           10 PMPP-PPFL-BEITRAG    PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPP-PPFL-SUMME      PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPP-PG-BEITRAG      PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPP-PG-SUMME        PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPP-PG-PROV-STZ     PIC S9(3)V9(3) USAGE COMP-3.
           10 PMPP-PG-GSMT-PROV    PIC S9(9)V9(2) USAGE COMP-3.
           10 PRKO-KOND-NR-GSP     PIC S9(4) USAGE COMP.
           10 PMPP-PG-PROD-DATEN   PIC X(30).
             10 PROD-PG-DATEN REDEFINES PMPP-PG-PROD-DATEN PIC X(30).
290599*      10 PROD-AMV-DATEN REDEFINES PROD-PG-DATEN IN Z-DRPA0288.
290599       10 PROD-AMV-DATEN REDEFINES PROD-PG-DATEN.
260997*         15 AMV-JAHRES-NT-PRAEMIE            PIC S9(010)V9(5)
260997          15 AMV-JAHRES-NT-PRAEMIE            PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
230998          15 AMV-TARIF                        PIC  X(010).
260997*         15 FILLER                           PIC  X(022).
260997*         15 FILLER                           PIC  X(024).
230998          15 FILLER                           PIC  X(014).

070400       10 PROD-BADENIA-DATEN REDEFINES PROD-PG-DATEN.
070400          15 BADENIA-TARIF                    PIC  9(003).
070400          15 FILLER                           PIC  X(027).
      *
070700       10 PROD-III-DATEN REDEFINES PROD-PG-DATEN.
070700          15 SPARPLAN-NR                      PIC  9(002).
070700          15 WPKN                             PIC  X(012).
070700          15 FILLER                           PIC  X(016).
      *
251000       10 PROD-DIT-DATEN REDEFINES PROD-PG-DATEN.
251000          15 PGV-KZ                           PIC  X(001).
251000          15 DIT-WPKN                         PIC  X(012).
130201          15 VL-KENNZEICHEN                   PIC  X(002).
251000          15 FILLER                           PIC  X(015).
      *
091100       10 PROD-HYPO-DATEN REDEFINES PROD-PG-DATEN.
091100          15 HYPO-PROD-BEZ                    PIC  X(015).
091100          15 FILLER                           PIC  X(015).
      *
150101       10 PROD-BFG-DATEN REDEFINES PROD-PG-DATEN.
150101          15 BFG-WERTPAPIER-KNR               PIC  X(015).
150101          15 BFG-PGV-KZ                       PIC  X(001).
150101          15 UMSATZ-KZ                        PIC  X(002).
150101          15 PG-PGV                           PIC  X(004).
150101          15 FILLER                           PIC  X(008).
      *
210201       10 PROD-COBA-DATEN REDEFINES PROD-PG-DATEN.
210201          15 PG-TARIF                         PIC  X(015).
210201          15 PG-PGV                           PIC  X(004).
210201          15 FILLER                           PIC  X(001).
210201          15 PROV-GRUND                       PIC  X(001).
210201          15 VORGANG-KZ                       PIC  X(001).
210201          15 QUALITAETS-KZ                    PIC  X(001).
210201          15 PROV-TYP                         PIC  X(001).
210201          15 FILLER                           PIC  X(006).
      *
280201       10 PROD-AML-DARL-DATEN REDEFINES PROD-PG-DATEN.
280201          15 PG-TARIF                         PIC  X(015).
280201          15 PG-PGV                           PIC  X(004).
280201          15 FILLER                           PIC  X(011).
      *
280201       10 PROD-CENTRAL-DATEN REDEFINES PROD-PG-DATEN.
280201          15 PG-TARIF                         PIC  X(003).
280201          15 PG-PGV                           PIC  X(004).
280201          15 FILLER                           PIC  X(023).
      *
280201       10 PROD-BFG-BANK-DATEN REDEFINES PROD-PG-DATEN.
280201          15 PG-TARIF                         PIC  X(015).
280201          15 PG-PGV                           PIC  X(004).
280201          15 FILLER                           PIC  X(001).
280201          15 UMSATZART                        PIC  X(001).
280201          15 UMSATZ-KZ                        PIC  X(001).
280201          15 FILLER                           PIC  X(008).
      *
280301       10 PROD-DGI-DATEN REDEFINES PROD-PG-DATEN.
280301          15 PG-TARIF                         PIC  X(015).
280301          15 PG-PGV                           PIC  X(004).
280301          15 FILLER                           PIC  X(001).
280301          15 STORNO-KZ                        PIC  X(001).
280301          15 UMSATZ-KZ                        PIC  X(001).
280301          15 FILLER                           PIC  X(008).
      *
090401       10 PROD-BAWAG-DATEN REDEFINES PROD-PG-DATEN.
090401          15 PG-TARIF                         PIC  X(015).
090401          15 PG-PGV                           PIC  X(004).
090401          15 FILLER                           PIC  X(011).
           10 PMPP-LAST-UPDATE     PIC X(26).
           10 PMPP-KENNUNG         PIC X(8).
      ******************************************************************
