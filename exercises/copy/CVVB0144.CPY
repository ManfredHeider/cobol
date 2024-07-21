      *****************************************************************
      *    ERSTELLT: THI / TTH                                        *
      *    DATUM:    22.06.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-1742 !22.06.21! NEU                           ! THI / TTH *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0144) BWUEB                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0144)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0144 TABLE
CCC004*    ( BWUEB_BETR_ALT              INTEGER NOT NULL
CCC004*    , BWUEB_UEBH_NR               INTEGER NOT NULL
CCC004*    , BWUEB_DAT_VON               DATE NOT NULL
CCC004*    , BWUEB_DAT_BIS               DATE NOT NULL
CCC004*    , BWUEB_DAT_ERSTELL           DATE NOT NULL
CCC004*    , BWUEB_DAT_ENDE              DATE NOT NULL
CCC004*    , BWUEB_HERKUNFT              CHAR(01) NOT NULL
CCC004*    , BWUEB_BETR_NEU              INTEGER NOT NULL
CCC004*    , BWUEB_STAT                  SMALLINT NOT NULL
CCC004*    , BWUEB_STAT_DATUM            DATE NOT NULL
CCC004*    , BWUEB_LAST_UPDATE           TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0144                          *
      *****************************************************************

       01  VIVB0144-GESAMT.
        05 NI-VIVB0144-TAB.
           10 NI-VIVB0144-ELE      PIC S9(04)  BINARY OCCURS  11.
        05 NI-VIVB0144             REDEFINES   NI-VIVB0144-TAB.
           10 NI-BWUEB-BETR-ALT           PIC S9(04)       BINARY.
           10 NI-BWUEB-UEBH-NR            PIC S9(04)       BINARY.
           10 NI-BWUEB-DAT-VON            PIC S9(04)       BINARY.
           10 NI-BWUEB-DAT-BIS            PIC S9(04)       BINARY.
           10 NI-BWUEB-DAT-ERSTELL        PIC S9(04)       BINARY.
           10 NI-BWUEB-DAT-ENDE           PIC S9(04)       BINARY.
           10 NI-BWUEB-HERKUNFT           PIC S9(04)       BINARY.
           10 NI-BWUEB-BETR-NEU           PIC S9(04)       BINARY.
           10 NI-BWUEB-STAT               PIC S9(04)       BINARY.
           10 NI-BWUEB-STAT-DATUM         PIC S9(04)       BINARY.
           10 NI-BWUEB-LAST-UPDATE        PIC S9(04)       BINARY.

        05 DCL-VIVB0144.
           10 BWUEB-BETR-ALT              PIC S9(09)       BINARY.
           10 BWUEB-UEBH-NR               PIC S9(09)       BINARY.
           10 BWUEB-DAT-VON               PIC  X(10).
           10 BWUEB-DAT-BIS               PIC  X(10).
           10 BWUEB-DAT-ERSTELL           PIC  X(10).
           10 BWUEB-DAT-ENDE              PIC  X(10).
           10 BWUEB-HERKUNFT              PIC  X(01).
           10 BWUEB-BETR-NEU              PIC S9(09)       BINARY.
           10 BWUEB-STAT                  PIC S9(04)       BINARY.
           10 BWUEB-STAT-DATUM            PIC  X(10).
           10 BWUEB-LAST-UPDATE           PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
