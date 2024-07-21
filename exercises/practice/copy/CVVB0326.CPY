      *****************************************************************
      *    ERSTELLT: -                                                *
      *    DATUM:    23.09.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01580|23.09.19|2.1 | NEU                       | TTH       *
      *         |        |    |                           |           *
      *         |        |    |                           |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0326) SWBAS                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0326)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0326 TABLE
CCC004*    ( SWBAS_BETR_NR               INTEGER NOT NULL
CCC004*    , SWBAS_UNTST_NR              INTEGER NOT NULL
CCC004*    , SWBAS_DAT_WECHSEL           DATE NOT NULL
CCC004*    , KTY_UST_TYP                 SMALLINT NOT NULL
CCC004*    , KNR_UST_TYP                 SMALLINT NOT NULL
CCC004*    , SWBAS_KORR_AB               DATE NOT NULL
CCC004*    , SWBAS_KORR_BIS              DATE NOT NULL
CCC004*    , SWBAS_KORR_ENDE             DATE NOT NULL
CCC004*    , SWBAS_BASIS_AB              DATE NOT NULL
CCC004*    , SWBAS_BASIS_BIS             DATE NOT NULL
CCC004*    , KTY_ZUSTRUK_TYP             SMALLINT NOT NULL
CCC004*    , KNR_ZUSTRUK_TYP             SMALLINT NOT NULL
CCC004*    , SWBAS_DIR_NEU               SMALLINT NOT NULL
CCC004*    , SWBAS_EBENE                 SMALLINT NOT NULL
CCC004*    , KTY_KORR_TYP                SMALLINT NOT NULL
CCC004*    , KNR_KORR_TYP                SMALLINT NOT NULL
CCC004*    , KNR_KORR_DAT                DATE NOT NULL
CCC004*    , SWBAS_EH_BASIS_12           DECIMAL(10, 1) NOT NULL
CCC004*    , SWBAS_ABR_MONATE            SMALLINT NOT NULL
CCC004*    , SWBAS_EH_MONWERT            DECIMAL(10, 1) NOT NULL
CCC004*    , SWBAS_LAST_UPDATE           TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0326                          *
      *****************************************************************

       01  VIVB0326-GESAMT.
        05 NI-VIVB0326-TAB.
           10 NI-VIVB0326-ELE      PIC S9(04)  BINARY OCCURS  21.
        05 NI-VIVB0326             REDEFINES   NI-VIVB0326-TAB.
           10 NI-SWBAS-BETR-NR            PIC S9(04)       BINARY.
           10 NI-SWBAS-UNTST-NR           PIC S9(04)       BINARY.
           10 NI-SWBAS-DAT-WECHSEL        PIC S9(04)       BINARY.
           10 NI-KTY-UST-TYP              PIC S9(04)       BINARY.
           10 NI-KNR-UST-TYP              PIC S9(04)       BINARY.
           10 NI-SWBAS-KORR-AB            PIC S9(04)       BINARY.
           10 NI-SWBAS-KORR-BIS           PIC S9(04)       BINARY.
           10 NI-SWBAS-KORR-ENDE          PIC S9(04)       BINARY.
           10 NI-SWBAS-BASIS-AB           PIC S9(04)       BINARY.
           10 NI-SWBAS-BASIS-BIS          PIC S9(04)       BINARY.
           10 NI-KTY-ZUSTRUK-TYP          PIC S9(04)       BINARY.
           10 NI-KNR-ZUSTRUK-TYP          PIC S9(04)       BINARY.
           10 NI-SWBAS-DIR-NEU            PIC S9(04)       BINARY.
           10 NI-SWBAS-EBENE              PIC S9(04)       BINARY.
           10 NI-KTY-KORR-TYP             PIC S9(04)       BINARY.
           10 NI-KNR-KORR-TYP             PIC S9(04)       BINARY.
           10 NI-KNR-KORR-DAT             PIC S9(04)       BINARY.
           10 NI-SWBAS-EH-BASIS-12        PIC S9(04)       BINARY.
           10 NI-SWBAS-ABR-MONATE         PIC S9(04)       BINARY.
           10 NI-SWBAS-EH-MONWERT         PIC S9(04)       BINARY.
           10 NI-SWBAS-LAST-UPDATE        PIC S9(04)       BINARY.

        05 DCL-VIVB0326.
           10 SWBAS-BETR-NR               PIC S9(09)       BINARY.
           10 SWBAS-UNTST-NR              PIC S9(09)       BINARY.
           10 SWBAS-DAT-WECHSEL           PIC  X(10).
           10 KTY-UST-TYP                 PIC S9(04)       BINARY.
           10 KNR-UST-TYP                 PIC S9(04)       BINARY.
           10 SWBAS-KORR-AB               PIC  X(10).
           10 SWBAS-KORR-BIS              PIC  X(10).
           10 SWBAS-KORR-ENDE             PIC  X(10).
           10 SWBAS-BASIS-AB              PIC  X(10).
           10 SWBAS-BASIS-BIS             PIC  X(10).
           10 KTY-ZUSTRUK-TYP             PIC S9(04)       BINARY.
           10 KNR-ZUSTRUK-TYP             PIC S9(04)       BINARY.
           10 SWBAS-DIR-NEU               PIC S9(04)       BINARY.
           10 SWBAS-EBENE                 PIC S9(04)       BINARY.
           10 KTY-KORR-TYP                PIC S9(04)       BINARY.
           10 KNR-KORR-TYP                PIC S9(04)       BINARY.
           10 KNR-KORR-DAT                PIC  X(10).
           10 SWBAS-EH-BASIS-12           PIC S9(09)V9(01) COMP-3.
           10 SWBAS-ABR-MONATE            PIC S9(04)       BINARY.
           10 SWBAS-EH-MONWERT            PIC S9(09)V9(01) COMP-3.
           10 SWBAS-LAST-UPDATE           PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  21                    *
      *****************************************************************







