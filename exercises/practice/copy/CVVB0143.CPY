      *****************************************************************
      *    ERSTELLT: THI / TTH                                        *
      *    DATUM:    22.06.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-1742 !22.06.21! NEU                           ! THI / TTH *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0143) BWKS                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0143)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0143 TABLE
CCC004*    ( BWKS_BETR_NR                INTEGER NOT NULL
CCC004*    , BWKS_UNTST_NR               INTEGER NOT NULL
CCC004*    , BWKS_DAT_VON                DATE NOT NULL
CCC004*    , BWKS_DAT_BIS                DATE NOT NULL
CCC004*    , BWKS_DAT_ERSTELL            DATE NOT NULL
CCC004*    , BWKS_DAT_ENDE               DATE NOT NULL
CCC004*    , BWKS_HERKUNFT               CHAR(01) NOT NULL
CCC004*    , BWKS_EBENE                  SMALLINT NOT NULL
CCC004*    , BWKS_ABZUGS_ART             SMALLINT NOT NULL
CCC004*    , BWKS_DIR_NEU                SMALLINT NOT NULL
CCC004*    , BWKS_DIR_ALT                SMALLINT NOT NULL
CCC004*    , BWKS_BETR_ALT               INTEGER NOT NULL
CCC004*    , BWKS_ZUSTRUK_TYP            SMALLINT NOT NULL
CCC004*    , BWKS_STAT                   SMALLINT NOT NULL
CCC004*    , BWKS_STAT_DATUM             DATE NOT NULL
CCC004*    , BWKS_LAST_UPDATE            TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0143                          *
      *****************************************************************

       01  VIVB0143-GESAMT.
        05 NI-VIVB0143-TAB.
           10 NI-VIVB0143-ELE      PIC S9(04)  BINARY OCCURS  16.
        05 NI-VIVB0143             REDEFINES   NI-VIVB0143-TAB.
           10 NI-BWKS-BETR-NR             PIC S9(04)       BINARY.
           10 NI-BWKS-UNTST-NR            PIC S9(04)       BINARY.
           10 NI-BWKS-DAT-VON             PIC S9(04)       BINARY.
           10 NI-BWKS-DAT-BIS             PIC S9(04)       BINARY.
           10 NI-BWKS-DAT-ERSTELL         PIC S9(04)       BINARY.
           10 NI-BWKS-DAT-ENDE            PIC S9(04)       BINARY.
           10 NI-BWKS-HERKUNFT            PIC S9(04)       BINARY.
           10 NI-BWKS-EBENE               PIC S9(04)       BINARY.
           10 NI-BWKS-ABZUGS-ART          PIC S9(04)       BINARY.
           10 NI-BWKS-DIR-NEU             PIC S9(04)       BINARY.
           10 NI-BWKS-DIR-ALT             PIC S9(04)       BINARY.
           10 NI-BWKS-BETR-ALT            PIC S9(04)       BINARY.
           10 NI-BWKS-ZUSTRUK-TYP         PIC S9(04)       BINARY.
           10 NI-BWKS-STAT                PIC S9(04)       BINARY.
           10 NI-BWKS-STAT-DATUM          PIC S9(04)       BINARY.
           10 NI-BWKS-LAST-UPDATE         PIC S9(04)       BINARY.

        05 DCL-VIVB0143.
           10 BWKS-BETR-NR                PIC S9(09)       BINARY.
           10 BWKS-UNTST-NR               PIC S9(09)       BINARY.
           10 BWKS-DAT-VON                PIC  X(10).
           10 BWKS-DAT-BIS                PIC  X(10).
           10 BWKS-DAT-ERSTELL            PIC  X(10).
           10 BWKS-DAT-ENDE               PIC  X(10).
           10 BWKS-HERKUNFT               PIC  X(01).
           10 BWKS-EBENE                  PIC S9(04)       BINARY.
           10 BWKS-ABZUGS-ART             PIC S9(04)       BINARY.
           10 BWKS-DIR-NEU                PIC S9(04)       BINARY.
           10 BWKS-DIR-ALT                PIC S9(04)       BINARY.
           10 BWKS-BETR-ALT               PIC S9(09)       BINARY.
           10 BWKS-ZUSTRUK-TYP            PIC S9(04)       BINARY.
           10 BWKS-STAT                   PIC S9(04)       BINARY.
           10 BWKS-STAT-DATUM             PIC  X(10).
           10 BWKS-LAST-UPDATE            PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  16                    *
      *****************************************************************
