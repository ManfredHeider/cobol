      *****************************************************************
      *    ERSTELLT: T13AE15                                          *
      *    DATUM:    28.04.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VLA-1953|28.04.21|2.1 | NEU                       | T13AE15   *
      *         |        |    |                           |           *
      *         |        |    |                           |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0141) EDW2                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0141)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0141 TABLE
CCC004*    ( EDW2_VBNR                   INTEGER NOT NULL
CCC004*    , EDW2_BEZUGSNR               INTEGER NOT NULL
CCC004*    , EDW2_DAT_VON                TIMESTAMP NOT NULL
CCC004*    , EDW2_DAT_BIS                DATE NOT NULL
CCC004*    , KTY_EDW2_TYP                SMALLINT NOT NULL
CCC004*    , KNR_EDW2_TYP                SMALLINT NOT NULL
CCC004*    , EDW2_LAST_UPDATE            TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0141                          *
      *****************************************************************

       01  VIVB0141-GESAMT.
        05 NI-VIVB0141-TAB.
           10 NI-VIVB0141-ELE      PIC S9(04)  BINARY OCCURS   7.
        05 NI-VIVB0141             REDEFINES   NI-VIVB0141-TAB.
           10 NI-EDW2-VBNR                PIC S9(04)       BINARY.
           10 NI-EDW2-BEZUGSNR            PIC S9(04)       BINARY.
           10 NI-EDW2-DAT-VON             PIC S9(04)       BINARY.
           10 NI-EDW2-DAT-BIS             PIC S9(04)       BINARY.
           10 NI-KTY-EDW2-TYP             PIC S9(04)       BINARY.
           10 NI-KNR-EDW2-TYP             PIC S9(04)       BINARY.
           10 NI-EDW2-LAST-UPDATE         PIC S9(04)       BINARY.

        05 DCL-VIVB0141.
           10 EDW2-VBNR                   PIC S9(09)       BINARY.
           10 EDW2-BEZUGSNR               PIC S9(09)       BINARY.
           10 EDW2-DAT-VON                PIC  X(26).
           10 EDW2-DAT-BIS                PIC  X(10).
           10 KTY-EDW2-TYP                PIC S9(04)       BINARY.
           10 KNR-EDW2-TYP                PIC S9(04)       BINARY.
           10 EDW2-LAST-UPDATE            PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   7                    *
      *****************************************************************