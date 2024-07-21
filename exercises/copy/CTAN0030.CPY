      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0030.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTAN0030.CPv  $
      *
      *   Rev 2.4   Feb 11 2020 12:05:48   t13ae34
      *FAN01178: eAntrag Geiger Direktkauf- Telefonnummer ist im Di
      *alog nicht aufgeführt!
      *
      *   Rev 2.3   Nov 25 2019 11:24:38   t13ae34
      *AAN01396: Antragserfassung Geiger Gold in einem System
      *
      *   Rev 2.2   Nov 21 2019 13:43:12   t13ae34
      *AAN01396: Antragserfassung Geiger Gold in einem System
      *
      *   Rev 2.1   Nov 13 2019 18:39:02   t13ae34
      *AAN01396: Antragserfassung Geiger Gold in einem System
      *
      *   Rev 2.0   Oct 22 2019 11:59:10   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      ******************************************************************
      * DCLGEN TABLE(T13VBS.TBAN0030)                                  *
      *        LIBRARY(T130005.ANTRAG.AAN01396(CTAN0030))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAN0030 TABLE
CCC004*    ( GEWS_GGSP                      CHAR(4) NOT NULL,
CCC004*      GEWS_PIN                       INTEGER NOT NULL,
CCC004*      GEWS_OKI_DATUM                 TIMESTAMP NOT NULL,
CCC004*      GEWS_MANDANT                   CHAR(10) NOT NULL,
CCC004*      GEWS_AVERM_NAME                CHAR(60) NOT NULL,
CCC004*      GEWS_INNENDIENST               CHAR(1) NOT NULL,
CCC004*      GEWS_AVERM                     INTEGER NOT NULL,
CCC004*      GEWS_VM_NAME                   CHAR(60) NOT NULL,
CCC004*      GEWS_VM                        INTEGER NOT NULL,
CCC004*      GEWS_BANK_TYP                  CHAR(5) NOT NULL,
CCC004*      GEWS_IBAN                      CHAR(34) NOT NULL,
CCC004*      GEWS_KONTOINHABER              CHAR(60) NOT NULL,
CCC004*      GEWS_KUNDE_ANREDE              CHAR(5) NOT NULL,
CCC004*      GEWS_KUNDE_TITEL               CHAR(20) NOT NULL,
CCC004*      GEWS_KUNDE_VORNAME             CHAR(26) NOT NULL,
CCC004*      GEWS_KUNDE_NAME                CHAR(26) NOT NULL,
CCC004*      GEWS_GEBURT_DATUM              DATE NOT NULL,
CCC004*      GEWS_STRASSE                   CHAR(45) NOT NULL,
CCC004*      GEWS_PLZ                       CHAR(6) NOT NULL,
CCC004*      GEWS_ORT                       CHAR(30) NOT NULL,
CCC004*      GEWS_LAND                      CHAR(30) NOT NULL,
CCC004*      GEWS_ZEIT_INS                  TIMESTAMP NOT NULL,
CCC004*      GEWS_KENNUNG_INS               CHAR(8) NOT NULL,
CCC004*      GEWS_ZEIT_UPD                  TIMESTAMP NOT NULL,
CCC004*      GEWS_KENNUNG_UPD               CHAR(8) NOT NULL,
CCC004*      GEWS_ZAHLWEISE                 SMALLINT NOT NULL,
CCC004*      GEWS_START_DATUM               DATE NOT NULL,
CCC004*      GEWS_ENDE_DATUM                DATE NOT NULL,
CCC004*      GEWS_BETRAG                    DECIMAL(10, 2) NOT NULL,
CCC004*      GEWS_EINMALBETRAG              DECIMAL(10, 2) NOT NULL,
CCC004*      GEWS_KUNDE_TELEFON             CHAR(30) NOT NULL,
CCC004*      GEWS_KUNDE_EMAIL               CHAR(50) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE        TBAN0030                    *
      ******************************************************************
       01  TBAN0030-GESAMT.
           10 GEWS-GGSP            PIC X(4).
           10 GEWS-PIN             PIC S9(9) USAGE COMP.
           10 GEWS-OKI-DATUM       PIC X(26).
           10 GEWS-MANDANT         PIC X(10).
           10 GEWS-AVERM-NAME      PIC X(60).
           10 GEWS-INNENDIENST     PIC X(1).
           10 GEWS-AVERM           PIC S9(9) USAGE COMP.
           10 GEWS-VM-NAME         PIC X(60).
           10 GEWS-VM              PIC S9(9) USAGE COMP.
           10 GEWS-BANK-TYP        PIC X(5).
           10 GEWS-IBAN            PIC X(34).
           10 GEWS-KONTOINHABER    PIC X(60).
           10 GEWS-KUNDE-ANREDE    PIC X(5).
           10 GEWS-KUNDE-TITEL     PIC X(20).
           10 GEWS-KUNDE-VORNAME   PIC X(26).
           10 GEWS-KUNDE-NAME      PIC X(26).
           10 GEWS-GEBURT-DATUM    PIC X(10).
           10 GEWS-STRASSE         PIC X(45).
           10 GEWS-PLZ             PIC X(6).
           10 GEWS-ORT             PIC X(30).
           10 GEWS-LAND            PIC X(30).
           10 GEWS-ZEIT-INS        PIC X(26).
           10 GEWS-KENNUNG-INS     PIC X(8).
           10 GEWS-ZEIT-UPD        PIC X(26).
           10 GEWS-KENNUNG-UPD     PIC X(8).
           10 GEWS-ZAHLWEISE       PIC S9(4) USAGE COMP.
           10 GEWS-START-DATUM     PIC X(10).
           10 GEWS-ENDE-DATUM      PIC X(10).
           10 GEWS-BETRAG          PIC S9(8)V9(2) USAGE COMP-3.
           10 GEWS-EINMALBETRAG    PIC S9(8)V9(2) USAGE COMP-3.
           10 GEWS-KUNDE-TELEFON   PIC X(30).
           10 GEWS-KUNDE-EMAIL     PIC X(50).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 32      *
      ******************************************************************



