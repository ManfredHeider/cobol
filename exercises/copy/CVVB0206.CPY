      *****************************************************************
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    28.09.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01103!28.09.10!2.1 ! NEU                       ! WILLIG    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0206) QEDA                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0206)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0206 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , ED_ST_NR                    INTEGER NOT NULL
CCC004*    , QEDA_ANSPR_VOM              DATE NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , QEDA_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , QEDA_ANSPR_BETRAG           DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0206                           *
      *****************************************************************

       01  VIVB0206-GESAMT.
        05 NI-VIVB0206-TAB.
           10 NI-VIVB0206-ELE      PIC S9(04)  BINARY OCCURS  11.
        05 NI-VIVB0206             REDEFINES   NI-VIVB0206-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-ED-ST-NR                 PIC S9(04)       BINARY.
           10 NI-QEDA-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-QEDA-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-QEDA-ANSPR-BETRAG        PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

        05 DCL-VIVB0206.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 ED-ST-NR                    PIC S9(09)       BINARY.
           10 QEDA-ANSPR-VOM              PIC  X(10).
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 QEDA-LAST-UPDATE            PIC  X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 QEDA-ANSPR-BETRAG           PIC S9(09)V9(02) COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
