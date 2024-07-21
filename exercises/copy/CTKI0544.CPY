      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24/06/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0544.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0544.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 09:44:28   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 15:04:38   T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE GDKAKTTARIFOU TABLE
CCC004*    ( AKOANGEBOTSNAME                CHAR(10) NOT NULL,
CCC004*      AKOVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      AKOPERSONLFDNR                 SMALLINT NOT NULL,
CCC004*      AKOTARIFTEIL_ID                CHAR(10) NOT NULL,
CCC004*      AKOSTICHTAG                    DATE NOT NULL,
CCC004*      AKOTARIFNAME                   CHAR(40) NOT NULL,
CCC004*      AKOGESAMT_BEITRAG              DECIMAL(9, 2) NOT NULL,
CCC004*      AKOSELBSTBEHALT                DECIMAL(9, 2) NOT NULL,
CCC004*      AKOVERS_LEISTUNG               DECIMAL(9, 2) NOT NULL,
CCC004*      AKOGESETZL_ZUSCHL              DECIMAL(9, 2) NOT NULL,
CCC004*      AKOZEITL_BEFR_LIM              DECIMAL(9, 2) NOT NULL,
CCC004*      AKOSUM_BEITRZUSCHL             DECIMAL(9, 2) NOT NULL,
CCC004*      AKOPAUSCH_LEIST_1              DECIMAL(9, 2) NOT NULL,
CCC004*      AKOPAUSCH_LEIST_2              DECIMAL(9, 2) NOT NULL,
CCC004*      AKOPAUSCH_LEIST_3              DECIMAL(9, 2) NOT NULL,
CCC004*      AKOPAUSCH_LEIST_4              DECIMAL(9, 2) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GDKAKTTARIFOU               *
      ******************************************************************
       01  GDKAKTTARIFOU-GESAMT.
        05 NI-GDKAKTTARIFOU-TAB.
           10 NI-GDKAKTTARIFOU-ELE PIC S9(04) COMP OCCURS 17 TIMES.
        05 NI-GDKAKTTARIFOU REDEFINES NI-GDKAKTTARIFOU-TAB.
           10 NI-AKOANGEBOTSNAME    PIC S9(4) COMP.
           10 NI-AKOVERTRAGSID      PIC S9(4) COMP.
           10 NI-AKOPERSONLFDNR     PIC S9(4) COMP.
           10 NI-AKOTARIFTEIL-ID    PIC S9(4) COMP.
           10 NI-AKOSTICHTAG        PIC S9(4) COMP.
           10 NI-AKOTARIFNAME       PIC S9(4) COMP.
           10 NI-AKOGESAMT-BEITRAG  PIC S9(4) COMP.
           10 NI-AKOSELBSTBEHALT    PIC S9(4) COMP.
           10 NI-AKOVERS-LEISTUNG   PIC S9(4) COMP.
           10 NI-AKOGESETZL-ZUSCHL  PIC S9(4) COMP.
           10 NI-AKOZEITL-BEFR-LIM  PIC S9(4) COMP.
           10 NI-AKOSUM-BEITRZUSCHL PIC S9(4) COMP.
           10 NI-AKOPAUSCH-LEIST-1  PIC S9(4) COMP.
           10 NI-AKOPAUSCH-LEIST-2  PIC S9(4) COMP.
           10 NI-AKOPAUSCH-LEIST-3  PIC S9(4) COMP.
           10 NI-AKOPAUSCH-LEIST-4  PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-GDKAKTTARIFOU.
           10 AKOANGEBOTSNAME      PIC X(10).
           10 AKOVERTRAGSID        PIC X(13).
           10 AKOPERSONLFDNR       PIC S9(4) USAGE COMP.
           10 AKOTARIFTEIL-ID      PIC X(10).
           10 AKOSTICHTAG          PIC X(10).
           10 AKOTARIFNAME         PIC X(40).
           10 AKOGESAMT-BEITRAG    PIC S9(7)V9(2) USAGE COMP-3.
           10 AKOSELBSTBEHALT      PIC S9(7)V9(2) USAGE COMP-3.
           10 AKOVERS-LEISTUNG     PIC S9(7)V9(2) USAGE COMP-3.
           10 AKOGESETZL-ZUSCHL    PIC S9(7)V9(2) USAGE COMP-3.
           10 AKOZEITL-BEFR-LIM    PIC S9(7)V9(2) USAGE COMP-3.
           10 AKOSUM-BEITRZUSCHL   PIC S9(7)V9(2) USAGE COMP-3.
           10 AKOPAUSCH-LEIST-1    PIC S9(7)V9(2) USAGE COMP-3.
           10 AKOPAUSCH-LEIST-2    PIC S9(7)V9(2) USAGE COMP-3.
           10 AKOPAUSCH-LEIST-3    PIC S9(7)V9(2) USAGE COMP-3.
           10 AKOPAUSCH-LEIST-4    PIC S9(7)V9(2) USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************

