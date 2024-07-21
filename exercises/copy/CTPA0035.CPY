      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0035.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTPA0035.CPv  $
      *
      *   Rev 2.1   Jul 16 2015 15:04:18   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.0   Jul 14 2015 07:57:10   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0035)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0035 TABLE
CCC004*    ( PASU_VBNR            INTEGER NOT NULL,
CCC004*      PASU_ZIEL_STATUS     SMALLINT NOT NULL,
CCC004*      PASU_LFDNR           SMALLINT NOT NULL,
CCC004*      PASU_JAHR            SMALLINT NOT NULL,
CCC004*      KNR_PASU_ZUSTAND     SMALLINT NOT NULL,
CCC004*      PASU_AUFGABE_TS      TIMESTAMP NOT NULL,
CCC004*      PASU_ZUSTAND_TS      TIMESTAMP NOT NULL,
CCC004*      PASU_ZUSTAND_KENNUNG CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0035                     *
      ******************************************************************
       01  TBPA0035-GESAMT.
         05  DCL-TBPA0035.
           10 PASU-VBNR            PIC S9(9) USAGE COMP.
           10 PASU-ZIEL-STATUS     PIC S9(4) USAGE COMP.
           10 PASU-LFDNR           PIC S9(4) USAGE COMP.
           10 PASU-JAHR            PIC S9(4) USAGE COMP.
           10 KNR-PASU-ZUSTAND     PIC S9(4) USAGE COMP.
           10 PASU-AUFGABE-TS      PIC X(26).
           10 PASU-ZUSTAND-TS      PIC X(26).
           10 PASU-ZUSTAND-KENNUNG PIC X(08).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************

