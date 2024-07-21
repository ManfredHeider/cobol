      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0094.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0094.CPv  $
      *
      *   Rev 2.1   Aug 12 2004 11:43:20   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung
      *
      *   Rev 2.0   Jun 23 2004 14:51:46   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    ERSTELLT: T.MUND                                           *
      *    DATUM:    25.06.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00184!25.06.04! 2.1! NEU                       ! T.MUND   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ******************************************************************
      * DCLGEN TABLE(VIVB0094) LZUB                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0094)               *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0094 TABLE
CCC004*    ( LZUB_ST_NR                  INTEGER NOT NULL,
CCC004*      KNR_OLZ_TYP                 SMALLINT NOT NULL,
CCC004*      KTY_OLZ_TYP                 SMALLINT NOT NULL,
CCC004*      LZUB_STICHTAG               DATE NOT NULL,
CCC004*      LZUB_ANSPR_VOM              DATE NOT NULL,
CCC004*      LZUB_LAST_UPDATE            TIMESTAMP NOT NULL,
CCC004*      LZUB_MS_NR                  SMALLINT NOT NULL,
CCC004*      LZUB_VBP_NR                 SMALLINT NOT NULL,
CCC004*      LZUB_12MON_UMSATZ           DECIMAL(10, 1) NOT NULL,
CCC004*      LZUB_03MON_UMSATZ           DECIMAL(10, 1) NOT NULL,
CCC004*      LZUB_FAKTOR                 DECIMAL(5, 2) NOT NULL,
CCC004*      LZUB_ANZ_MONATE             SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0094
      ******************************************************************
       01  VIVB0094-GESAMT.
        05  NI-VIVB0094-TAB.
           10 NI-VIVB0094-ELE      PIC S9(04) BINARY OCCURS  12 TIMES.
        05  NI-VIVB0094            REDEFINES   NI-VIVB0094-TAB.
           10 NI-LZUB-ST-NR               PIC S9(04)       BINARY.
           10 NI-KTY-OLZ-TYP              PIC S9(04)       BINARY.
           10 NI-KNR-OLZ-TYP              PIC S9(04)       BINARY.
           10 NI-LZUB-STICHTAG            PIC S9(04)       BINARY.
           10 NI-LZUB-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-LZUB-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-LZUB-MS-NR               PIC S9(04)       BINARY.
           10 NI-LZUB-PS-NR               PIC S9(04)       BINARY.
           10 NI-LZUB-12MON-UMSATZ        PIC S9(04)       BINARY.
           10 NI-LZUB-03MON-UMSATZ        PIC S9(04)       BINARY.
           10 NI-LZUB-FAKTOR              PIC S9(04)       BINARY.
           10 NI-LZUB-ANZ-MONATE          PIC S9(04)       BINARY.

        05  DCL-VIVB0094.
           10 LZUB-ST-NR                  PIC S9(09)       BINARY.
           10 KNR-OLZ-TYP                 PIC S9(04)       BINARY.
           10 KTY-OLZ-TYP                 PIC S9(04)       BINARY.
           10 LZUB-STICHTAG               PIC X(10).
           10 LZUB-ANSPR-VOM              PIC X(10).
           10 LZUB-LAST-UPDATE            PIC X(26).
           10 LZUB-MS-NR                  PIC S9(04)       BINARY.
           10 LZUB-PS-NR                  PIC S9(04)       BINARY.
           10 LZUB-12MON-UMSATZ           PIC S9(09)V9     COMP-3.
           10 LZUB-03MON-UMSATZ           PIC S9(09)V9     COMP-3.
           10 LZUB-FAKTOR                 PIC S9(03)V99    COMP-3.
           10 LZUB-ANZ-MONATE             PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************

