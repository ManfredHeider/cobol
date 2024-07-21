      *****************************************************************
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    25.03.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01103!25.03.11!2.1 ! NEU                       ! WILLIG    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0205) QEDD                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0205)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0205 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , ED_ST_NR                    INTEGER NOT NULL
CCC004*    , QEDD_ED_ZUSATZ              SMALLINT NOT NULL
CCC004*    , ED_GENERATION               SMALLINT NOT NULL
CCC004*    , QEDD_DATUM                  DATE NOT NULL
CCC004*    , QEDD_ED_EINH                DECIMAL(10, 1) NOT NULL
CCC004*    , QEDD_EINH_MAX               DECIMAL(10, 1) NOT NULL
CCC004*    , QEDD_EINH_GEW               DECIMAL(10, 1) NOT NULL
CCC004*    , QEDD_ANSPR_BETRAG           DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , DL_NET_GES_EH               DECIMAL(10, 1) NOT NULL
CCC004*    , DL_EH_MAX_PROZ              DECIMAL(06, 3)
CCC004*    , ED_EINH_FAKTOR              DECIMAL(06, 3) NOT NULL
CCC004*    , QUARTAL                     CHAR(17) NOT NULL
CCC004*    , QEDD_LAST_UPDATE            TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0205                           *
      *****************************************************************

       01  VIVB0205-GESAMT.
        05 NI-VIVB0205-TAB.
           10 NI-VIVB0205-ELE      PIC S9(04)  BINARY OCCURS  16.
        05 NI-VIVB0205             REDEFINES   NI-VIVB0205-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-ED-ST-NR                 PIC S9(04)       BINARY.
           10 NI-QEDD-ED-ZUSATZ           PIC S9(04)       BINARY.
           10 NI-ED-GENERATION            PIC S9(04)       BINARY.
           10 NI-QEDD-DATUM               PIC S9(04)       BINARY.
           10 NI-QEDD-ED-EINH             PIC S9(04)       BINARY.
           10 NI-QEDD-EINH-MAX            PIC S9(04)       BINARY.
           10 NI-QEDD-EINH-GEW            PIC S9(04)       BINARY.
           10 NI-QEDD-ANSPR-BETRAG        PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-DL-NET-GES-EH            PIC S9(04)       BINARY.
           10 NI-DL-EH-MAX-PROZ           PIC S9(04)       BINARY.
           10 NI-ED-EINH-FAKTOR           PIC S9(04)       BINARY.
           10 NI-QUARTAL                  PIC S9(04)       BINARY.
           10 NI-QEDD-LAST-UPDATE         PIC S9(04)       BINARY.

        05 DCL-VIVB0205.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 ED-ST-NR                    PIC S9(09)       BINARY.
           10 QEDD-ED-ZUSATZ              PIC S9(04)       BINARY.
           10 ED-GENERATION               PIC S9(04)       BINARY.
           10 QEDD-DATUM                  PIC  X(10).
           10 QEDD-ED-EINH                PIC S9(09)V9(01) COMP-3.
           10 QEDD-EINH-MAX               PIC S9(09)V9(01) COMP-3.
           10 QEDD-EINH-GEW               PIC S9(09)V9(01) COMP-3.
           10 QEDD-ANSPR-BETRAG           PIC S9(09)V9(02) COMP-3.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 DL-NET-GES-EH               PIC S9(09)V9(01) COMP-3.
           10 DL-EH-MAX-PROZ              PIC S9(03)V9(03) COMP-3.
           10 ED-EINH-FAKTOR              PIC S9(03)V9(03) COMP-3.
           10 QUARTAL                     PIC  X(17).
           10 QEDD-LAST-UPDATE            PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  16                    *
      *****************************************************************
