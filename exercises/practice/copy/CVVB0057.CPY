      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0057.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0057.CPv  $
      *
      *   Rev 2.1   Feb 15 2006 09:12:46   t13ex43
      *ASL00224: PGM - Dimension 7 Aufbaubonus
      *
      *   Rev 2.0   Jan 30 2006 16:39:44   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    30.01.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * ASL00224!30.01.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0057) ABBA                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0057)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0057 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL,
CCC004*      ABBA_ANSPR_VOM              DATE NOT NULL,
CCC004*      ABBA_PART_VBNR              INTEGER NOT NULL,
CCC004*      KNR_ANSPR_GRUND             SMALLINT NOT NULL,
CCC004*      KTY_ANSPR_GRUND             SMALLINT NOT NULL,
CCC004*      ABBA_LAST_UPDATE            TIMESTAMP NOT NULL,
CCC004*      KTY_WAEHRUNG                SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                SMALLINT NOT NULL,
CCC004*      ABBA_PART_BETRAG            DECIMAL(11, 2) NOT NULL,
CCC004*      ABBA_PART_EH                DECIMAL(10, 1) NOT NULL,
CCC004*      ABBA_PART_SEIT              DATE NOT NULL,
CCC004*      ABBA_BETREUT_SEIT           DATE NOT NULL,
CCC004*      KTY_ANSPR_STATUS            SMALLINT NOT NULL,
CCC004*      KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0057
      ******************************************************************
       01  VIVB0057-GESAMT.
        05  NI-VIVB0057-TAB.
           10 NI-VIVB0057-ELE      PIC S9(04) BINARY OCCURS  14 TIMES.
        05  NI-VIVB0057            REDEFINES   NI-VIVB0057-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-ABBA-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-ABBA-PART-VBNR           PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-ABBA-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-ABBA-PART-BETRAG         PIC S9(04)       BINARY.
           10 NI-ABBA-PART-EH             PIC S9(04)       BINARY.
           10 NI-ABBA-PART-SEIT           PIC S9(04)       BINARY.
           10 NI-ABBA-BETREUT-SEIT        PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

        05  DCL-VIVB0057.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 ABBA-ANSPR-VOM              PIC X(10).
           10 ABBA-PART-VBNR              PIC S9(09)       BINARY.
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 ABBA-LAST-UPDATE            PIC X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 ABBA-PART-BETRAG            PIC S9(09)V9(02) USAGE COMP-3.
           10 ABBA-PART-EH                PIC S9(09)V9(01) USAGE COMP-3.
           10 ABBA-PART-SEIT              PIC X(10).
           10 ABBA-BETREUT-SEIT           PIC X(10).
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
