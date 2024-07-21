      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0093.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0093.CPv  $
      *
      *   Rev 2.1   Aug 12 2004 11:43:02   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung
      *
      *   Rev 2.0   Jun 23 2004 14:51:56   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    ERSTELLT: T.MUND                                           *
      *    DATUM:    25.06.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00153!27.11.02! 2.1! NEU                       ! J.HERING *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ******************************************************************
      * DCLGEN TABLE(VIVB0093) LZUA                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0093)               *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0093 TABLE
CCC004*    ( LZUA_ST_NR                  INTEGER NOT NULL,
CCC004*      KNR_OLZ_TYP                 SMALLINT NOT NULL,
CCC004*      KTY_OLZ_TYP                 SMALLINT NOT NULL,
CCC004*      LZUA_ANSPR_VOM              DATE NOT NULL,
CCC004*      KNR_ANSPR_GRUND             SMALLINT NOT NULL,
CCC004*      KTY_ANSPR_GRUND             SMALLINT NOT NULL,
CCC004*      LZUA_LAST_UPDATE            TIMESTAMP NOT NULL,
CCC004*      KTY_WAEHRUNG                SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                SMALLINT NOT NULL,
CCC004*      LZUA_ANSPR_BETRAG           DECIMAL(11, 2) NOT NULL,
CCC004*      KTY_ANSPR_STATUS            SMALLINT NOT NULL,
CCC004*      KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0093
      ******************************************************************
       01  VIVB0093-GESAMT.
        05  NI-VIVB0093-TAB.
           10 NI-VIVB0093-ELE      PIC S9(04) BINARY OCCURS  12 TIMES.
        05  NI-VIVB0093            REDEFINES   NI-VIVB0093-TAB.
           10 NI-LZUA-ST-NR               PIC S9(04)       BINARY.
           10 NI-KTY-OLZ-TYP              PIC S9(04)       BINARY.
           10 NI-KNR-OLZ-TYP              PIC S9(04)       BINARY.
           10 NI-LZUA-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-LZUA-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-LZUA-ANSPR-BETRAG        PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

        05  DCL-VIVB0093.
           10 LZUA-ST-NR                  PIC S9(09)       BINARY.
           10 KNR-OLZ-TYP                 PIC S9(04)       BINARY.
           10 KTY-OLZ-TYP                 PIC S9(04)       BINARY.
           10 LZUA-ANSPR-VOM              PIC X(10).
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 LZUA-LAST-UPDATE            PIC X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 LZUA-ANSPR-BETRAG           PIC S9(09)V9(02) USAGE COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
