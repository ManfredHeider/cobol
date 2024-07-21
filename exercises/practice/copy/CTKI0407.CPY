      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10/06/10
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0407.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/CTKI0407.CPv  $
      *
      *   Rev 2.8   Jun 17 2020 10:21:04   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.7   Jun 23 2016 09:21:50   t13ae31
      *AKI01438: G10 SUH Angleichung DTA OZP
      *
      *   Rev 2.6   Feb 17 2011 14:26:52   t13ex27
      *AKI01022: DB-Änderungen für R. 5.28
      *
      *   Rev 2.5   Jun 28 2010 10:14:44   t13ex48
      *AKI00970: DB-Änderungen für Rel. 5.12: SUH
      *
      *   Rev 2.0   Jun 10 2010 13:58:08   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE EINZELVERTRAG TABLE
CCC004*    ( VEZVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VEZVETLFDNR                    SMALLINT NOT NULL,
CCC004*      VEZHOSTID                      CHAR(15) NOT NULL,
CCC004*      VEZSPARTE_KTY                  SMALLINT NOT NULL,
CCC004*      VEZSPARTE_KNR                  DECIMAL(15, 0) NOT NULL,
CCC004*      VEZTARPROD_KTY                 SMALLINT NOT NULL,
CCC004*      VEZTARPROD_KNR                 DECIMAL(15, 0) NOT NULL,
CCC004*      VEZBEGINN                      DATE,
CCC004*      VEZABLAUF                      DATE,
CCC004*      VEZSTATUS                      SMALLINT NOT NULL,
CCC004*      VEZBBEITRAGZAHLW               DECIMAL(10, 2) NOT NULL,
CCC004*      VEZSUMME                       DECIMAL(10, 0) NOT NULL,
CCC004*      VEZVERTRARTEZV                 INTEGER NOT NULL,
CCC004*      VEZAENDERUNGSGRD               CHAR(50) NOT NULL,
CCC004*      VEZAENDDAT                     DATE,
CCC004*      VEZNJBEITRAG                   DECIMAL(10, 2) NOT NULL,
CCC004*      VEZBJBEITRAG                   DECIMAL(10, 2) NOT NULL,
CCC004*      VEZNBEITRAGZAHLW               DECIMAL(10, 2) NOT NULL,
CCC004*      VEZBEARBEITDAT                 DATE,
CCC004*      VEZGESCHAEFTSZW                CHAR(2) NOT NULL,
CCC004*      VEZPRODUKTID                   SMALLINT NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      VEZAENDGRUND_KTY               SMALLINT NOT NULL,
CCC004*      VEZAENDGRUND_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      VEZRABNJBEITRAG                DECIMAL(10, 2),
CCC004*      VEZSUMKOMPBEITR                DECIMAL(10, 2),
CCC004*      VEZREOBPRODNR                  INTEGER,
CCC004*      VEZPRODBEZ_KTY                 SMALLINT NOT NULL,
CCC004*      VEZPRODBEZ_KNR                 DECIMAL(15, 0),
CCC004*      VEZFLEXNSP                     DECIMAL(11, 2),
CCC004*      VEZFLEXNSPGW                   CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE EINZELVERTRAG               *
      ******************************************************************
       01  EINZELVERTRAG-GESAMT.
        05 NI-EINZELVERTRAG-TAB.
           10 NI-EINZELVERTRAG-ELE PIC S9(04) COMP OCCURS 31 TIMES.
        05 NI-EINZELVERTRAG REDEFINES NI-EINZELVERTRAG-TAB.
           10 NI-VEZVERTRAGSID      PIC S9(4) COMP.
           10 NI-VEZVETLFDNR        PIC S9(4) COMP.
           10 NI-VEZHOSTID          PIC S9(4) COMP.
           10 NI-VEZSPARTE-KTY      PIC S9(4) COMP.
           10 NI-VEZSPARTE-KNR      PIC S9(4) COMP.
           10 NI-VEZTARPROD-KTY     PIC S9(4) COMP.
           10 NI-VEZTARPROD-KNR     PIC S9(4) COMP.
           10 NI-VEZBEGINN          PIC S9(4) COMP.
           10 NI-VEZABLAUF          PIC S9(4) COMP.
           10 NI-VEZSTATUS          PIC S9(4) COMP.
           10 NI-VEZBBEITRAGZAHLW   PIC S9(4) COMP.
           10 NI-VEZSUMME           PIC S9(4) COMP.
           10 NI-VEZVERTRARTEZV     PIC S9(4) COMP.
           10 NI-VEZAENDERUNGSGRD   PIC S9(4) COMP.
           10 NI-VEZAENDDAT         PIC S9(4) COMP.
           10 NI-VEZNJBEITRAG       PIC S9(4) COMP.
           10 NI-VEZBJBEITRAG       PIC S9(4) COMP.
           10 NI-VEZNBEITRAGZAHLW   PIC S9(4) COMP.
           10 NI-VEZBEARBEITDAT     PIC S9(4) COMP.
           10 NI-VEZGESCHAEFTSZW    PIC S9(4) COMP.
           10 NI-VEZPRODUKTID       PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-VEZAENDGRUND-KTY   PIC S9(4) COMP.
           10 NI-VEZAENDGRUND-KNR   PIC S9(4) COMP.
           10 NI-VEZRABNJBEITRAG    PIC S9(4) COMP.
           10 NI-VEZSUMKOMPBEITR    PIC S9(4) COMP.
           10 NI-VEZREOBPRODNR      PIC S9(4) COMP.
           10 NI-VEZPRODBEZ-KTY     PIC S9(4) COMP.
           10 NI-VEZPRODBEZ-KNR     PIC S9(4) COMP.
           10 NI-VEZFLEXNSP         PIC S9(4) COMP.
           10 NI-VEZFLEXNSPGW       PIC S9(4) COMP.
        05 DCL-EINZELVERTRAG.
           10 VEZVERTRAGSID        PIC X(13).
           10 VEZVETLFDNR          PIC S9(4) USAGE COMP.
           10 VEZHOSTID            PIC X(15).
           10 VEZSPARTE-KTY        PIC S9(4) USAGE COMP.
           10 VEZSPARTE-KNR        PIC S9(15)V USAGE COMP-3.
           10 VEZTARPROD-KTY       PIC S9(4) USAGE COMP.
           10 VEZTARPROD-KNR       PIC S9(15)V USAGE COMP-3.
           10 VEZBEGINN            PIC X(10).
           10 VEZABLAUF            PIC X(10).
           10 VEZSTATUS            PIC S9(4) USAGE COMP.
           10 VEZBBEITRAGZAHLW     PIC S9(8)V9(2) USAGE COMP-3.
           10 VEZSUMME             PIC S9(10)V USAGE COMP-3.
           10 VEZVERTRARTEZV       PIC S9(9) USAGE COMP.
           10 VEZAENDERUNGSGRD     PIC X(50).
           10 VEZAENDDAT           PIC X(10).
           10 VEZNJBEITRAG         PIC S9(8)V9(2) USAGE COMP-3.
           10 VEZBJBEITRAG         PIC S9(8)V9(2) USAGE COMP-3.
           10 VEZNBEITRAGZAHLW     PIC S9(8)V9(2) USAGE COMP-3.
           10 VEZBEARBEITDAT       PIC X(10).
           10 VEZGESCHAEFTSZW      PIC X(2).
           10 VEZPRODUKTID         PIC S9(4) USAGE COMP.
           10 LETZTEAENDERUNG      PIC X(26).
           10 VEZAENDGRUND-KTY     PIC S9(4) USAGE COMP.
           10 VEZAENDGRUND-KNR     PIC S9(15)V USAGE COMP-3.
           10 VEZRABNJBEITRAG      PIC S9(8)V9(2) USAGE COMP-3.
           10 VEZSUMKOMPBEITR      PIC S9(8)V9(2) USAGE COMP-3.
           10 VEZREOBPRODNR        PIC S9(9) USAGE COMP.
           10 VEZPRODBEZ-KTY       PIC S9(4) USAGE COMP.
           10 VEZPRODBEZ-KNR       PIC S9(15)V USAGE COMP-3.
           10 VEZFLEXNSP           PIC S9(9)V9(2) USAGE COMP-3.
           10 VEZFLEXNSPGW         PIC X(1).
      ************************** ENDE-DCLGEN *************************

