      *---------------------------------------------------------------*
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VERSIONMANAGER
      *            DATUM      :  31.08.2020
      *            KATEGORIE  :  COPYSTRECKE
      *---------------------------------------------------------------*
      *    COPYRIGHT (C) 2020 ATLAS DIENSTLEISTUNGEN F. V. GMBH
      *---------------------------------------------------------------*
      *    $WORKFILE:   CVVB0137.CPY  $
      *    $REVISION:   2.0  $
      *
      *    $LOG:   L:/MF/CVVB0137.CPV  $
      *
      *   REV 2.0   AUG 31 2020 14:14:56   U130213
      *ARCHIVERSTELLUNG
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: -                                                *
      *    DATUM:    31.08.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME      *
      * --------+--------+----+---------------------------+---------- *
      * -       |31.08.20|2.1 | NEU                       | -         *
      *         |        |    |                           |           *
      *         |        |    |                           |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0137) EHBASISB                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0137)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0137 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , EHBASISB_MONAT              DATE NOT NULL
CCC004*    , AVERM                       INTEGER NOT NULL
CCC004*    , PROD_ART                    SMALLINT NOT NULL
CCC004*    , EIGEN_KZ                    CHAR(01) NOT NULL
CCC004*    , STRUK_BER_KZ                CHAR(01) NOT NULL
CCC004*    , EHBASISB_EH                 DECIMAL(11, 1) NOT NULL
CCC004*    , LAST_UPDATE                 TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0137                          *
      *****************************************************************

       01  VIVB0137-GESAMT.
        05 NI-VIVB0137-TAB.
           10 NI-VIVB0137-ELE      PIC S9(04)  BINARY OCCURS   8.
        05 NI-VIVB0137             REDEFINES   NI-VIVB0137-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-EHBASISB-MONAT           PIC S9(04)       BINARY.
           10 NI-AVERM                    PIC S9(04)       BINARY.
           10 NI-PROD-ART                 PIC S9(04)       BINARY.
           10 NI-EIGEN-KZ                 PIC S9(04)       BINARY.
           10 NI-STRUK-BER-KZ             PIC S9(04)       BINARY.
           10 NI-EHBASISB-EH              PIC S9(04)       BINARY.
           10 NI-LAST-UPDATE              PIC S9(04)       BINARY.

        05 DCL-VIVB0137.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 EHBASISB-MONAT              PIC  X(10).
           10 AVERM                       PIC S9(09)       BINARY.
           10 PROD-ART                    PIC S9(04)       BINARY.
           10 EIGEN-KZ                    PIC  X(01).
           10 STRUK-BER-KZ                PIC  X(01).
           10 EHBASISB-EH                 PIC S9(10)V9(01) COMP-3.
           10 LAST-UPDATE                 PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************


