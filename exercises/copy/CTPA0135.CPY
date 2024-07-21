      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0135.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTPA0135.CPv  $
      *
      *   Rev 2.1   Oct 14 2004 14:05:00   t13ex44
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.0   Oct 12 2004 11:20:54   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0135)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0313 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PRINTE                         DECIMAL(15, 0) NOT NULL,
CCC004*      PVKB_GUELTIG_AB                TIMESTAMP NOT NULL,
CCC004*      PVKB_KD_BETREUER               INTEGER NOT NULL,
CCC004*      PVKB_F_GUELTIG_AB              DATE NOT NULL,
CCC004*      PVKB_DTA_KZ                    CHAR(1) NOT NULL,
CCC004*      PVKB_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PVKB_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0135                           *
      ******************************************************************
       01  DCL-TBPA0135.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PVKB-GUELTIG-AB      PIC X(26).
           10 PVKB-KD-BETREUER     PIC S9(9) USAGE COMP.
           10 PVKB-F-GUELTIG-AB    PIC X(10).
           10 PVKB-DTA-KZ          PIC X(1).
           10 PVKB-LAST-UPDATE     PIC X(26).
           10 PVKB-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
