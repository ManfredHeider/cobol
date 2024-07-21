      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.02.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0322.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTPA0322.CPv  $
      *
      *   Rev 2.6   Mar 07 2016 13:06:20   t13ae37
      *APA02208: Anpassungen Korrekturprogramm USt
      *
      *   Rev 2.5   Feb 03 2016 15:00:12   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.4   Nov 24 2015 09:30:46   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.3   Oct 12 2004 11:37:58   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.2   Oct 12 2004 08:18:16   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.1   Feb 17 2004 08:29:16   t13ex44
      *APA00218 - Auslagerung
      *
      *   Rev 2.0   Feb 04 2004 08:36:46   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0322)                                         *
      *        LIBRARY(T130005.PA010000.SRCDCL(CTPA0322))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0322 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PDPT_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMPT_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMPT_VERSION                   SMALLINT NOT NULL,
CCC004*      PEST_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PDPT_BUCH_RFOLGE               SMALLINT NOT NULL,
CCC004*      PDPT_MA_ABR_DAT                DATE NOT NULL,
CCC004*      KNR_BUCHUNGS_STAND             SMALLINT NOT NULL,
CCC004*      KNR_BUCHUNGSART                SMALLINT NOT NULL,
CCC004*      KNR_BUCHUNGS_GRUND             SMALLINT NOT NULL,
CCC004*      KNR_KZ_EH_MAX                  SMALLINT NOT NULL,
CCC004*      KNR_BERCH_WAEHRUNG             SMALLINT NOT NULL,
CCC004*      PDPT_DGV                       SMALLINT NOT NULL,
CCC004*      PDPT_PRODART                   SMALLINT NOT NULL,
CCC004*      PDPT_FIRST_INSERT              TIMESTAMP NOT NULL,
CCC004*      PDPT_FIRST_KENNUNG             CHAR(8) NOT NULL,
CCC004*      PDPT_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PDPT_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      PDPT_PDPE_DATEN                VARCHAR(1220) NOT NULL,
CCC004*      PDPT_UST_JAHR                  SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0322                           *
      ******************************************************************
       01  DCL-TBPA0322.
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
              49 PDPT-PDPE-DATEN-LEN  PIC S9(4) USAGE COMP.
241115*       49 PDPT-PDPE-DATEN-TEXT  PIC X(720).
241115        49 PDPT-PDPE-DATEN-TEXT  PIC X(1220).
010216     10 PDPT-UST-JAHR        PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
 
 

