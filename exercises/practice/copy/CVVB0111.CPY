      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0111.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0111.CPv  $
      *
      *   Rev 2.1   25 Feb 2008 09:57:16   t13ex43
      *ASL00292: PGM - Direktions-Aufbau-Plan
      *
      *   Rev 2.0   28 Jan 2008 15:32:48   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    28.01.2008                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * ASL00292!28.01.08!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0111) DAPA                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0111)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0111 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL,
CCC004*      DAPA_ANSPR_VOM              DATE NOT NULL,
CCC004*      DAPA_PART_VBNR              INTEGER NOT NULL,
CCC004*      KNR_ANSPR_GRUND             SMALLINT NOT NULL,
CCC004*      KTY_ANSPR_GRUND             SMALLINT NOT NULL,
CCC004*      DAPA_LAST_UPDATE            TIMESTAMP NOT NULL,
CCC004*      KTY_WAEHRUNG                SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                SMALLINT NOT NULL,
CCC004*      DAPA_VBNR_TYP               CHAR(01) NOT NULL,
CCC004*      DAPA_STUFE                  CHAR(04) NOT NULL,
CCC004*      DAPA_BEF_DAT                DATE NOT NULL,
CCC004*      DAPA_EH_GESAMT              DECIMAL(10, 1) NOT NULL,
CCC004*      DAPA_EH_EIGEN               DECIMAL(10, 1) NOT NULL,
CCC004*      DAPA_EQF_GESAMT             DECIMAL(05, 2) NOT NULL,
CCC004*      DAPA_RATE                   CHAR(01) NOT NULL,
CCC004*      DAPA_ANSPR_BETRAG           DECIMAL(11, 2) NOT NULL,
CCC004*      KTY_ANSPR_STATUS            SMALLINT NOT NULL,
CCC004*      KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0111
      ******************************************************************
       01  VIVB0111-GESAMT.
        05  NI-VIVB0111-TAB.
           10 NI-VIVB0111-ELE      PIC S9(04) BINARY OCCURS  18 TIMES.
        05  NI-VIVB0111            REDEFINES   NI-VIVB0111-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-DAPA-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-DAPA-PART-VBNR           PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-DAPA-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-DAPA-VBNR-TYP            PIC S9(04)       BINARY.
           10 NI-DAPA-STUFE               PIC S9(04)       BINARY.
           10 NI-DAPA-BEF-DAT             PIC S9(04)       BINARY.
           10 NI-DAPA-EH-GESAMT           PIC S9(04)       BINARY.
           10 NI-DAPA-EH-EIGEN            PIC S9(04)       BINARY.
           10 NI-DAPA-EQF-GESAMT          PIC S9(04)       BINARY.
           10 NI-DAPA-RATE                PIC S9(04)       BINARY.
           10 NI-DAPA-ANSPR-BETRAG        PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

        05  DCL-VIVB0111.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 DAPA-ANSPR-VOM              PIC X(10).
           10 DAPA-PART-VBNR              PIC S9(09)       BINARY.
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 DAPA-LAST-UPDATE            PIC X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 DAPA-VBNR-TYP               PIC X(01).
           10 DAPA-STUFE                  PIC X(04).
           10 DAPA-BEF-DAT                PIC X(10).
           10 DAPA-EH-GESAMT              PIC S9(09)V9(01) USAGE COMP-3.
           10 DAPA-EH-EIGEN               PIC S9(09)V9(01) USAGE COMP-3.
           10 DAPA-EQF-GESAMT             PIC S9(03)V9(02) USAGE COMP-3.
           10 DAPA-RATE                   PIC X(01).
           10 DAPA-ANSPR-BETRAG           PIC S9(09)V9(02) USAGE COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
