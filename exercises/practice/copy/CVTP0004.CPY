      *****************************************************************
      *    ERSTELLT: S. WALB                                          *
      *    DATUM:    24.01.2018                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01479!24.01.18!2.1 ! NEU                       ! S. WALB   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(TBTP0004) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVTP0004)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE TBTP0004 TABLE
CCC004*    ( GENUM_AM_AG_NR              INTEGER NOT NULL
CCC004*    , GENUM_LFD_NR                SMALLINT NOT NULL
CCC004*    , GENUM_NACHNAME              CHAR(40) NOT NULL
CCC004*    , GENUM_VORNAME               CHAR(20) NOT NULL
CCC004*    , KTY_ANREDE                  SMALLINT NOT NULL
CCC004*    , KNR_ANREDE                  SMALLINT NOT NULL
CCC004*    , GENUM_AD_GES_TITEL          CHAR(10) NOT NULL
CCC004*    , GENUM_EU_REG_NR             CHAR(30) NOT NULL
CCC004*    , GENUM_PLZ                   CHAR(09) NOT NULL
CCC004*    , GENUM_ORT                   CHAR(40) NOT NULL
CCC004*    , GENUM_STR                   CHAR(40) NOT NULL
CCC004*    , GENUM_NAME                  CHAR(40) NOT NULL
CCC004*    , GENUM_DIR                   SMALLINT NOT NULL
CCC004*    , GENUM_DIR_NAME              CHAR(40) NOT NULL
CCC004*    , GENUM_MSNR                  SMALLINT NOT NULL
CCC004*    , GENUM_STUFE                 CHAR(30) NOT NULL
CCC004*    , GENUM_PRAXIS                SMALLINT NOT NULL
CCC004*    , GENUM_AMAG_NR_B             INTEGER NOT NULL
CCC004*    , GENUM_NAME_B                CHAR(40) NOT NULL
CCC004*    , GENUM_STUFE_B               CHAR(30) NOT NULL
CCC004*    , GENUM_PRAXIS_B              SMALLINT NOT NULL
CCC004*    , GENUM_STEUER_NR             CHAR(20) NOT NULL
CCC004*    , GENUM_IBAN                  CHAR(34) NOT NULL
CCC004*    , GENUM_BANK                  CHAR(40) NOT NULL
CCC004*    , GENUM_GEB_DAT               DATE NOT NULL
CCC004*    , GENUM_EIN_DAT               DATE NOT NULL
CCC004*    , GENUM_EIGENREG_KZ           CHAR(01) NOT NULL
CCC004*    , GENUM_UMSTEIG_TYP           CHAR(02) NOT NULL
CCC004*    , GENUM_EHEM_ANGEST           CHAR(01) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VITP0004                           *
      *****************************************************************

       01  TBTP0004-GESAMT.
        05 NI-TBTP0004-TAB.
           10 NI-TBTP0004-ELE      PIC S9(04)  BINARY OCCURS  29.
        05 NI-TBTP0004             REDEFINES   NI-TBTP0004-TAB.
           10 NI-GENUM-AM-AG-NR           PIC S9(04)       BINARY.
           10 NI-GENUM-LFD-NR             PIC S9(04)       BINARY.
           10 NI-GENUM-NACHNAME           PIC S9(04)       BINARY.
           10 NI-GENUM-VORNAME            PIC S9(04)       BINARY.
           10 NI-KTY-ANREDE               PIC S9(04)       BINARY.
           10 NI-KNR-ANREDE               PIC S9(04)       BINARY.
           10 NI-GENUM-AD-GES-TITEL       PIC S9(04)       BINARY.
           10 NI-GENUM-EU-REG-NR          PIC S9(04)       BINARY.
           10 NI-GENUM-PLZ                PIC S9(04)       BINARY.
           10 NI-GENUM-ORT                PIC S9(04)       BINARY.
           10 NI-GENUM-STR                PIC S9(04)       BINARY.
           10 NI-GENUM-NAME               PIC S9(04)       BINARY.
           10 NI-GENUM-DIR                PIC S9(04)       BINARY.
           10 NI-GENUM-DIR-NAME           PIC S9(04)       BINARY.
           10 NI-GENUM-MSNR               PIC S9(04)       BINARY.
           10 NI-GENUM-STUFE              PIC S9(04)       BINARY.
           10 NI-GENUM-PRAXIS             PIC S9(04)       BINARY.
           10 NI-GENUM-AMAG-NR-B          PIC S9(04)       BINARY.
           10 NI-GENUM-NAME-B             PIC S9(04)       BINARY.
           10 NI-GENUM-STUFE-B            PIC S9(04)       BINARY.
           10 NI-GENUM-PRAXIS-B           PIC S9(04)       BINARY.
           10 NI-GENUM-STEUER-NR          PIC S9(04)       BINARY.
           10 NI-GENUM-IBAN               PIC S9(04)       BINARY.
           10 NI-GENUM-BANK               PIC S9(04)       BINARY.
           10 NI-GENUM-GEB-DAT            PIC S9(04)       BINARY.
           10 NI-GENUM-EIN-DAT            PIC S9(04)       BINARY.
           10 NI-GENUM-EIGENREG-KZ        PIC S9(04)       BINARY.
           10 NI-GENUM-UMSTEIG-TYP        PIC S9(04)       BINARY.
           10 NI-GENUM-EHEM-ANGEST        PIC S9(04)       BINARY.

        05 DCL-TBTP0004.
           10 GENUM-AM-AG-NR              PIC S9(09)       BINARY.
           10 GENUM-LFD-NR                PIC S9(04)       BINARY.
           10 GENUM-NACHNAME              PIC  X(40).
           10 GENUM-VORNAME               PIC  X(20).
           10 KTY-ANREDE                  PIC S9(04)       BINARY.
           10 KNR-ANREDE                  PIC S9(04)       BINARY.
           10 GENUM-AD-GES-TITEL          PIC  X(10).
           10 GENUM-EU-REG-NR             PIC  X(30).
           10 GENUM-PLZ                   PIC  X(09).
           10 GENUM-ORT                   PIC  X(40).
           10 GENUM-STR                   PIC  X(40).
           10 GENUM-NAME                  PIC  X(40).
           10 GENUM-DIR                   PIC S9(04)       BINARY.
           10 GENUM-DIR-NAME              PIC  X(40).
           10 GENUM-MSNR                  PIC S9(04)       BINARY.
           10 GENUM-STUFE                 PIC  X(30).
           10 GENUM-PRAXIS                PIC S9(04)       BINARY.
           10 GENUM-AMAG-NR-B             PIC S9(09)       BINARY.
           10 GENUM-NAME-B                PIC  X(40).
           10 GENUM-STUFE-B               PIC  X(30).
           10 GENUM-PRAXIS-B              PIC S9(04)       BINARY.
           10 GENUM-STEUER-NR             PIC  X(20).
           10 GENUM-IBAN                  PIC  X(34).
           10 GENUM-BANK                  PIC  X(40).
           10 GENUM-GEB-DAT               PIC  X(10).
           10 GENUM-EIN-DAT               PIC  X(10).
           10 GENUM-EIGENREG-KZ           PIC  X(01).
           10 GENUM-UMSTEIG-TYP           PIC  X(02).
           10 GENUM-EHEM-ANGEST           PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  29                    *
      *****************************************************************

