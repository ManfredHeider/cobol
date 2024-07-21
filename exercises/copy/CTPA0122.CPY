      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0122.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTPA0122.CPv  $
      *
      *   Rev 2.6   Feb 03 2016 14:59:56   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.5   Jul 16 2015 15:10:42   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.4   Feb 17 2000 17:13:02   T13EX13
      *APA00081 - Pgeb-Ges in SQL-Statements
      *ergaenzt
      *
      *   Rev 2.3   May 28 1999 16:50:46   T13EX25
      *APA00042, Korrektur Komma
      *
      *   Rev 2.2   May 27 1999 15:09:10   T13EX25
      *APA00042, Produebernahme
      *
      *   Rev 2.1   Mar 15 1999 12:33:52   T01JM2E
      *NT-Umstellung
      *
      *   Rev 2.0   Feb 27 1999 14:26:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   04 Aug 1997 13:12:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0122)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0122 TABLE
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
      * COBOL DECLARATION FOR TABLE TBPA0122                           *
      ******************************************************************
       01  TBPA0122-GESAMT.
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
              49 PDPT-PDPE-DATEN-LEN  PIC S9(4) USAGE COMP.
      *       49 PDPT-PDPE-DATEN-TEXT  PIC X(720).
              49 PDPT-PDPE-DATEN-TEXT  PIC X(1220).
290116     10 PDPT-UST-JAHR            PIC S9(4) BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
 
 

