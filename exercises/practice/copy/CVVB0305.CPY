      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0305.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0305.CPv  $
      *
      *   Rev 2.1   May 27 2009 14:16:20   t13ex43
      *AAS00815: PGM - Neukundenbonus
      *
      *   Rev 2.0   May 06 2009 14:17:18   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    27.05.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAS00815!27.05.09!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0305) NKWBA                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0305)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0305 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , KNR_WB_TYP                  SMALLINT NOT NULL
CCC004*    , NKWBA_ANSPR_VOM             DATE NOT NULL
CCC004*    , NKWBA_PART_VBNR             INTEGER NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KTY_WB_TYP                  SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , NKWBA_LAST_UPDAT            TIMESTAMP NOT NULL
CCC004*    , NKWBA_ANZ                   SMALLINT
CCC004*    , NKWBA_EH_EING               DECIMAL(10, 1)
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , NKWBA_ANSPR_BETR            DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DECLARATION FOR VIEW VIVB0305                           *
      *****************************************************************

       01  VIVB0305-GESAMT.
        05 NI-VIVB0305-TAB.
           10 NI-VIVB0305-ELE      PIC S9(04)  BINARY OCCURS  15.
        05 NI-VIVB0305             REDEFINES   NI-VIVB0305-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-KNR-WB-TYP               PIC S9(04)       BINARY.
           10 NI-NKWBA-ANSPR-VOM          PIC S9(04)       BINARY.
           10 NI-NKWBA-PART-VBNR          PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-WB-TYP               PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-NKWBA-LAST-UPDAT         PIC S9(04)       BINARY.
           10 NI-NKWBA-ANZ                PIC S9(04)       BINARY.
           10 NI-NKWBA-EH-EING            PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-NKWBA-ANSPR-BETR         PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

        05 DCL-VIVB0305.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 KNR-WB-TYP                  PIC S9(04)       BINARY.
           10 NKWBA-ANSPR-VOM             PIC  X(10).
           10 NKWBA-PART-VBNR             PIC S9(09)       BINARY.
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-WB-TYP                  PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 NKWBA-LAST-UPDAT            PIC  X(26).
           10 NKWBA-ANZ                   PIC S9(04)       BINARY.
           10 NKWBA-EH-EING               PIC S9(09)V9(01) COMP-3.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 NKWBA-ANSPR-BETR            PIC S9(09)V9(02) COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.

      *****************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS  15    *
      *****************************************************************
