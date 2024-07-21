      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24/06/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0545.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0545.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 09:45:10   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 15:06:35   T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE GDKAKTTARIFMU TABLE
CCC004*    ( AKMANGEBOTSNAME                CHAR(10) NOT NULL,
CCC004*      AKMVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      AKMPERSONLFDNR                 SMALLINT NOT NULL,
CCC004*      AKMTARIFTEIL_ID                CHAR(10) NOT NULL,
CCC004*      AKMANGEBOTSNR                  SMALLINT NOT NULL,
CCC004*      AKMSTICHTAG                    DATE NOT NULL,
CCC004*      AKMTARIFNAME                   CHAR(40) NOT NULL,
CCC004*      AKMTARIFSTATUS                 CHAR(20) NOT NULL,
CCC004*      AKMGESAMT_BEITRAG              DECIMAL(9, 2) NOT NULL,
CCC004*      AKMSELBSTBEHALT                DECIMAL(9, 2) NOT NULL,
CCC004*      AKMVERS_LEISTUNG               DECIMAL(9, 2) NOT NULL,
CCC004*      AKMGESETZL_ZUSCHL              DECIMAL(9, 2) NOT NULL,
CCC004*      AKMZEITL_BEFR_LIM              DECIMAL(9, 2) NOT NULL,
CCC004*      AKMSUM_BEITRZUSCHL             DECIMAL(9, 2) NOT NULL,
CCC004*      AKMPAUSCH_LEIST_1              DECIMAL(9, 2) NOT NULL,
CCC004*      AKMPAUSCH_LEIST_2              DECIMAL(9, 2) NOT NULL,
CCC004*      AKMPAUSCH_LEIST_3              DECIMAL(9, 2) NOT NULL,
CCC004*      AKMPAUSCH_LEIST_4              DECIMAL(9, 2) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GDKAKTTARIFMU               *
      ******************************************************************
       01  GDKAKTTARIFMU-GESAMT.
        05 NI-GDKAKTTARIFMU-TAB.
           10 NI-GDKAKTTARIFMU-ELE PIC S9(04) COMP OCCURS 19 TIMES.
        05 NI-GDKAKTTARIFMU REDEFINES NI-GDKAKTTARIFMU-TAB.
           10 NI-AKMANGEBOTSNAME    PIC S9(4) COMP.
           10 NI-AKMVERTRAGSID      PIC S9(4) COMP.
           10 NI-AKMPERSONLFDNR     PIC S9(4) COMP.
           10 NI-AKMTARIFTEIL-ID    PIC S9(4) COMP.
           10 NI-AKMANGEBOTSNR      PIC S9(4) COMP.
           10 NI-AKMSTICHTAG        PIC S9(4) COMP.
           10 NI-AKMTARIFNAME       PIC S9(4) COMP.
           10 NI-AKMTARIFSTATUS     PIC S9(4) COMP.
           10 NI-AKMGESAMT-BEITRAG  PIC S9(4) COMP.
           10 NI-AKMSELBSTBEHALT    PIC S9(4) COMP.
           10 NI-AKMVERS-LEISTUNG   PIC S9(4) COMP.
           10 NI-AKMGESETZL-ZUSCHL  PIC S9(4) COMP.
           10 NI-AKMZEITL-BEFR-LIM  PIC S9(4) COMP.
           10 NI-AKMSUM-BEITRZUSCHL PIC S9(4) COMP.
           10 NI-AKMPAUSCH-LEIST-1  PIC S9(4) COMP.
           10 NI-AKMPAUSCH-LEIST-2  PIC S9(4) COMP.
           10 NI-AKMPAUSCH-LEIST-3  PIC S9(4) COMP.
           10 NI-AKMPAUSCH-LEIST-4  PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-GDKAKTTARIFMU.
           10 AKMANGEBOTSNAME      PIC X(10).
           10 AKMVERTRAGSID        PIC X(13).
           10 AKMPERSONLFDNR       PIC S9(4) USAGE COMP.
           10 AKMTARIFTEIL-ID      PIC X(10).
           10 AKMANGEBOTSNR        PIC S9(4) USAGE COMP.
           10 AKMSTICHTAG          PIC X(10).
           10 AKMTARIFNAME         PIC X(40).
           10 AKMTARIFSTATUS       PIC X(20).
           10 AKMGESAMT-BEITRAG    PIC S9(7)V9(2) USAGE COMP-3.
           10 AKMSELBSTBEHALT      PIC S9(7)V9(2) USAGE COMP-3.
           10 AKMVERS-LEISTUNG     PIC S9(7)V9(2) USAGE COMP-3.
           10 AKMGESETZL-ZUSCHL    PIC S9(7)V9(2) USAGE COMP-3.
           10 AKMZEITL-BEFR-LIM    PIC S9(7)V9(2) USAGE COMP-3.
           10 AKMSUM-BEITRZUSCHL   PIC S9(7)V9(2) USAGE COMP-3.
           10 AKMPAUSCH-LEIST-1    PIC S9(7)V9(2) USAGE COMP-3.
           10 AKMPAUSCH-LEIST-2    PIC S9(7)V9(2) USAGE COMP-3.
           10 AKMPAUSCH-LEIST-3    PIC S9(7)V9(2) USAGE COMP-3.
           10 AKMPAUSCH-LEIST-4    PIC S9(7)V9(2) USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************

