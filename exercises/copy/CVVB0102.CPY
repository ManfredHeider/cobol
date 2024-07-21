      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0102.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0102.CPv  $
      *
      *   Rev 2.1   08 Nov 2006 16:51:38   t13ae35
      *AIS00060: Summenblatt: Füllen SONSTBUCH
      *
      *   Rev 2.0   24 Oct 2006 11:04:36   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    ERSTELLT: J.Hering                                         *
      *    DATUM:    26.10.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AIS00060!26.10.06! 2.1! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ******************************************************************
      * DCLGEN TABLE(VIVB0102) SONSTB                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0102)               *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0102 TABLE
CCC004*    ( BUCH_DAT_VON                DATE            NOT NULL,
CCC004*      MA_ST_NR                    INTEGER         NOT NULL,
CCC004*      KTY_BUCH_TYP                SMALLINT        NOT NULL,
CCC004*      KNR_BUCH_TYP                SMALLINT        NOT NULL,
CCC004*      KTY_WAEHRUNG                SMALLINT        NOT NULL,
CCC004*      KNR_WAEHRUNG                SMALLINT        NOT NULL,
CCC004*      BETRAG                      DECIMAL (11, 2) NOT NULL,
CCC004*      BUCH_TEXT                   CHAR(40)        NOT NULL,
CCC004*      KONTO_TYP                   CHAR(01)        NOT NULL,
CCC004*      BELEGART                    CHAR(03)        NOT NULL,
CCC004*      BELEG_NR                    CHAR(07)        NOT NULL,
CCC004*      TURNUS                      CHAR(06)        NOT NULL,
CCC004*      BELEG_DAT                   CHAR(08)        NOT NULL,
CCC004*      BUCH_DAT                    CHAR(08)        NOT NULL,
CCC004*      KOSTEN_STELLE               CHAR(08)        NOT NULL,
CCC004*      GEGEN_KONTO                 CHAR(12)        NOT NULL,
CCC004*      SHKZ                        CHAR(01)        NOT NULL,
CCC004*      BUCH_LAST_UPDATE            CHAR(26)        NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0102
      ******************************************************************
       01  VIVB0102-GESAMT.
        05  NI-VIVB0102-TAB.
           10 NI-VIVB0102-ELE      PIC S9(04) BINARY OCCURS  18 TIMES.
        05  NI-VIVB0102            REDEFINES   NI-VIVB0102-TAB.
           10 NI-BUCH-DAT-VON             PIC S9(04)       BINARY.
           10 NI-MA-ST-NR               PIC S9(04)       BINARY.
           10 NI-KTY-BUCH-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-BUCH-TYP           PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG           PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG           PIC S9(04)       BINARY.
           10 NI-BETRAG                 PIC S9(04)       BINARY.
           10 NI-BUCH-TEXT              PIC S9(04)       BINARY.
           10 NI-KONTO-TYP              PIC S9(04)       BINARY.
           10 NI-BELEGART               PIC S9(04)       BINARY.
           10 NI-BELEG-NR               PIC S9(04)       BINARY.
           10 NI-TURNUS                 PIC S9(04)       BINARY.
           10 NI-BELEG-DAT              PIC S9(04)       BINARY.
           10 NI-BUCH-DAT               PIC S9(04)       BINARY.
           10 NI-KOSTEN-STELLE          PIC S9(04)       BINARY.
           10 NI-GEGEN-KONTO            PIC S9(04)       BINARY.
           10 NI-SHKZ                   PIC S9(04)       BINARY.
           10 NI-BUCH-LAST-UPDATE       PIC S9(04)       BINARY.

        05  DCL-VIVB0102.
           10 BUCH-DAT-VON              PIC X(10).
           10 MA-ST-NR                  PIC S9(09)       BINARY.
           10 KTY-BUCH-TYP              PIC S9(04)       BINARY.
           10 KNR-BUCH-TYP              PIC S9(04)       BINARY.
           10 KTY-WAEHRUNG              PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG              PIC S9(04)       BINARY.
           10 BETRAG                    PIC S9(09)V9(02) COMP-3.
           10 BUCH-TEXT                 PIC X(40).
           10 KONTO-TYP                 PIC X(01).
           10 BELEGART                  PIC X(03).
           10 BELEG-NR                  PIC X(07).
           10 TURNUS                    PIC X(06).
           10 BELEG-DAT                 PIC X(08).
           10 BUCH-DAT                  PIC X(08).
           10 KOSTEN-STELLE             PIC X(08).
           10 GEGEN-KONTO               PIC X(12).
           10 SHKZ                      PIC X(01).
           10 BUCH-LAST-UPDATE          PIC X(26).

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************

