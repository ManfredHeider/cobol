      *****************************************************************
      *    ERSTELLT: U130213                                          *
      *    DATUM:    08.07.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-2980 !08.07.21! NEU                           ! U130213   *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0145) BWBAS                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0145)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0145 TABLE
CCC004*    ( BWBAS_UNTST_NR              INTEGER NOT NULL
CCC004*    , BWBAS_DAT_VON               DATE NOT NULL
CCC004*    , BWBAS_MS_NR                 SMALLINT NOT NULL
CCC004*    , BWBAS_DAT_ERSTELL           DATE NOT NULL
CCC004*    , BWBAS_ZUSTRUK_TYP           SMALLINT NOT NULL
CCC004*    , BWBAS_HERKUNFT              CHAR(01) NOT NULL
CCC004*    , BWBAS_BETR_NR               INTEGER NOT NULL
CCC004*    , BWBAS_EBENE                 SMALLINT NOT NULL
CCC004*    , BWBAS_DIR_NEU               SMALLINT NOT NULL
CCC004*    , BWBAS_DIR_ALT               SMALLINT NOT NULL
CCC004*    , BWBAS_STAT                  SMALLINT NOT NULL
CCC004*    , BWBAS_LAST_UPDATE           TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0145                          *
      *****************************************************************

       01  VIVB0145-GESAMT.
        05 NI-VIVB0145-TAB.
           10 NI-VIVB0145-ELE      PIC S9(04)  BINARY OCCURS  12.
        05 NI-VIVB0145             REDEFINES   NI-VIVB0145-TAB.
           10 NI-BWBAS-UNTST-NR           PIC S9(04)       BINARY.
           10 NI-BWBAS-DAT-VON            PIC S9(04)       BINARY.
           10 NI-BWBAS-MS-NR              PIC S9(04)       BINARY.
           10 NI-BWBAS-DAT-ERSTELL        PIC S9(04)       BINARY.
           10 NI-BWBAS-ZUSTRUK-TYP        PIC S9(04)       BINARY.
           10 NI-BWBAS-HERKUNFT           PIC S9(04)       BINARY.
           10 NI-BWBAS-BETR-NR            PIC S9(04)       BINARY.
           10 NI-BWBAS-EBENE              PIC S9(04)       BINARY.
           10 NI-BWBAS-DIR-NEU            PIC S9(04)       BINARY.
           10 NI-BWBAS-DIR-ALT            PIC S9(04)       BINARY.
           10 NI-BWBAS-STAT               PIC S9(04)       BINARY.
           10 NI-BWBAS-LAST-UPDATE        PIC S9(04)       BINARY.

        05 DCL-VIVB0145.
           10 BWBAS-UNTST-NR              PIC S9(09)       BINARY.
           10 BWBAS-DAT-VON               PIC  X(10).
           10 BWBAS-MS-NR                 PIC S9(04)       BINARY.
           10 BWBAS-DAT-ERSTELL           PIC  X(10).
           10 BWBAS-ZUSTRUK-TYP           PIC S9(04)       BINARY.
           10 BWBAS-HERKUNFT              PIC  X(01).
           10 BWBAS-BETR-NR               PIC S9(09)       BINARY.
           10 BWBAS-EBENE                 PIC S9(04)       BINARY.
           10 BWBAS-DIR-NEU               PIC S9(04)       BINARY.
           10 BWBAS-DIR-ALT               PIC S9(04)       BINARY.
           10 BWBAS-STAT                  PIC S9(04)       BINARY.
           10 BWBAS-LAST-UPDATE           PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *
      *****************************************************************
