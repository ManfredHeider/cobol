      *****************************************************************
      *    ERSTELLT: -                                                *
      *    DATUM:    26.04.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * -        !26.04.22! NEU                           ! -         *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBC046) KLZA                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBC046)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBC046 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , KLZA_ANSPR_VOM              DATE NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KLZA_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , KLZA_BEITRAG                DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KLZA_ANSPR_BETRAG           DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT
CCC004*    , KNR_ANSPR_STATUS            SMALLINT
CCC004*    , KLZA_CENTRAL_KZ             CHAR(01) NOT NULL
CCC004*    , KLZA_EH_NNE                 DECIMAL(10, 1)
CCC004*    , KLZA_JEQF                   DECIMAL(05, 2)
CCC004*    , KLZA_ERF_INDX               CHAR(01)
CCC004*    , KLZA_AS_CLUB_KZ             CHAR(01)
CCC004*    , KLZA_BES_345_KZ             CHAR(01)
CCC004*    , KLZA_STRUKTUR               SMALLINT
CCC004*    , KLZA_PRAXIS                 SMALLINT
CCC004*    , KLZA_AUSTRITT_KZ            CHAR(01)
CCC004*    , KLZA_BES_229_KZ             CHAR(01)
CCC004*    , KLZA_BV_FEHLT_KZ            CHAR(01)
CCC004*    , KLZA_PFAEND_KZ              CHAR(01)
CCC004*    , KLZA_SPERRE_KZ              CHAR(01)
CCC004*    , KLZA_SPERRE_TEXT            CHAR(40)
CCC004*    , KLZA_EH_NNE_SOLL            DECIMAL(10, 1)
CCC004*    , KLZA_BQF                    DECIMAL(05, 2)
CCC004*    , KLZA_SACH_EG_BASIS          DECIMAL(10, 1)
CCC004*    , KLZA_SACH_EG_VERGL          DECIMAL(10, 1)
CCC004*    , KLZA_SACH_EG_STEIG          DECIMAL(05, 2)
CCC004*    , KLZA_ZUGANG                 SMALLINT
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBC046                          *
      *****************************************************************

       01  VIVBC046-GESAMT.
        05 NI-VIVBC046-TAB.
           10 NI-VIVBC046-ELE      PIC S9(04)  BINARY OCCURS  31.
        05 NI-VIVBC046             REDEFINES   NI-VIVBC046-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-KLZA-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KLZA-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KLZA-BEITRAG             PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KLZA-ANSPR-BETRAG        PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KLZA-CENTRAL-KZ          PIC S9(04)       BINARY.
           10 NI-KLZA-EH-NNE              PIC S9(04)       BINARY.
           10 NI-KLZA-JEQF                PIC S9(04)       BINARY.
           10 NI-KLZA-ERF-INDX            PIC S9(04)       BINARY.
           10 NI-KLZA-AS-CLUB-KZ          PIC S9(04)       BINARY.
           10 NI-KLZA-BES-345-KZ          PIC S9(04)       BINARY.
           10 NI-KLZA-STRUKTUR            PIC S9(04)       BINARY.
           10 NI-KLZA-PRAXIS              PIC S9(04)       BINARY.
           10 NI-KLZA-AUSTRITT-KZ         PIC S9(04)       BINARY.
           10 NI-KLZA-BES-229-KZ          PIC S9(04)       BINARY.
           10 NI-KLZA-BV-FEHLT-KZ         PIC S9(04)       BINARY.
           10 NI-KLZA-PFAEND-KZ           PIC S9(04)       BINARY.
           10 NI-KLZA-SPERRE-KZ           PIC S9(04)       BINARY.
           10 NI-KLZA-SPERRE-TEXT         PIC S9(04)       BINARY.
           10 NI-KLZA-EH-NNE-SOLL         PIC S9(04)       BINARY.
           10 NI-KLZA-BQF                 PIC S9(04)       BINARY.
           10 NI-KLZA-SACH-EG-BASIS       PIC S9(04)       BINARY.
           10 NI-KLZA-SACH-EG-VERGL       PIC S9(04)       BINARY.
           10 NI-KLZA-SACH-EG-STEIG       PIC S9(04)       BINARY.
           10 NI-KLZA-ZUGANG              PIC S9(04)       BINARY.

        05 DCL-VIVBC046.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 KLZA-ANSPR-VOM              PIC  X(10).
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KLZA-LAST-UPDATE            PIC  X(26).
           10 KLZA-BEITRAG                PIC S9(09)V9(02) COMP-3.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 KLZA-ANSPR-BETRAG           PIC S9(09)V9(02) COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KLZA-CENTRAL-KZ             PIC  X(01).
           10 KLZA-EH-NNE                 PIC S9(09)V9(01) COMP-3.
           10 KLZA-JEQF                   PIC S9(03)V9(02) COMP-3.
           10 KLZA-ERF-INDX               PIC  X(01).
           10 KLZA-AS-CLUB-KZ             PIC  X(01).
           10 KLZA-BES-345-KZ             PIC  X(01).
           10 KLZA-STRUKTUR               PIC S9(04)       BINARY.
           10 KLZA-PRAXIS                 PIC S9(04)       BINARY.
           10 KLZA-AUSTRITT-KZ            PIC  X(01).
           10 KLZA-BES-229-KZ             PIC  X(01).
           10 KLZA-BV-FEHLT-KZ            PIC  X(01).
           10 KLZA-PFAEND-KZ              PIC  X(01).
           10 KLZA-SPERRE-KZ              PIC  X(01).
           10 KLZA-SPERRE-TEXT            PIC  X(40).
           10 KLZA-EH-NNE-SOLL            PIC S9(09)V9(01) COMP-3.
           10 KLZA-BQF                    PIC S9(03)V9(02) COMP-3.
           10 KLZA-SACH-EG-BASIS          PIC S9(09)V9(01) COMP-3.
           10 KLZA-SACH-EG-VERGL          PIC S9(09)V9(01) COMP-3.
           10 KLZA-SACH-EG-STEIG          PIC S9(03)V9(02) COMP-3.
           10 KLZA-ZUGANG                 PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  31                    *
      *****************************************************************
