      *****************************************************************
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    17.12.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAS00928!17.12.10!2.1 ! NEU                       ! WILLIG    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBD043) CLBED                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBD043)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBD043 TABLE
CCC004*    ( KTY_CLUB_TYP                SMALLINT NOT NULL
CCC004*    , KNR_CLUB_TYP                SMALLINT NOT NULL
CCC004*    , KTY_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , KNR_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , CLUB_KRITERIUM              SMALLINT NOT NULL
CCC004*    , CLUB_MS_NR                  SMALLINT
CCC004*    , CLUB_VBP_NR                 SMALLINT
CCC004*    , CLUB_ANZ_KDN                SMALLINT
CCC004*    , ANZ_KDN_2_PLUS              SMALLINT
CCC004*    , NET_NEU_EIG_12MON           DECIMAL(10, 1)
CCC004*    , NET_EIG_GES_12MON           DECIMAL(10, 1)
CCC004*    , MAX_EQF_EIG                 DECIMAL(05, 2)
CCC004*    , BEITRAG_VOL                 DECIMAL(11, 2)
CCC004*    , VB_SEIT_MIN_MON             SMALLINT
CCC004*    , AS_SEIT_MIN_MON             SMALLINT
CCC004*    , NET_EIG_GES_36MO            DECIMAL(10, 1)
CCC004*    , NET_GES_12MO                DECIMAL(10, 1)
CCC004*    , KTY_TYP_BEN_CLUB            SMALLINT
CCC004*    , KNR_TYP_BEN_CLUB            SMALLINT
CCC004*    , KTY_TYP_ENDE_CLUB           SMALLINT
CCC004*    , KNR_TYP_ENDE_CLUB           SMALLINT
CCC004*    , ED_ANZ_SOLL                 SMALLINT
CCC004*    , ANZ_PART_KAP                SMALLINT
CCC004*    , ANZ_AKT_STRUK_ADM           SMALLINT
CCC004*    , CLBED_LAST_UPDATE           TIMESTAMP NOT NULL
CCC004*    , CLBED_DAT_VON               DATE NOT NULL
CCC004*    , CLBED_DAT_BIS               DATE NOT NULL
CCC004*    , ANZ_HAUPTBEPART             SMALLINT
CCC004*    , ANZ_NEU_DIR_PART            SMALLINT
CCC004*    , MAX_EQF_GES                 DECIMAL(05, 2)
CCC004*    , NET_NEU_GES_12MO            DECIMAL(10, 1)
CCC004*    , ANZ_STERNE                  SMALLINT
CCC004*    , ANT_INVEST                  SMALLINT
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBD043                           *
      *****************************************************************

       01  VIVBD043-GESAMT.
        05 NI-VIVBD043-TAB.
           10 NI-VIVBD043-ELE      PIC S9(04)  BINARY OCCURS  33.
        05 NI-VIVBD043             REDEFINES   NI-VIVBD043-TAB.
           10 NI-KTY-CLUB-TYP             PIC S9(04)       BINARY.
           10 NI-KNR-CLUB-TYP             PIC S9(04)       BINARY.
           10 NI-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-CLUB-KRITERIUM           PIC S9(04)       BINARY.
           10 NI-CLUB-MS-NR               PIC S9(04)       BINARY.
           10 NI-CLUB-VBP-NR              PIC S9(04)       BINARY.
           10 NI-CLUB-ANZ-KDN             PIC S9(04)       BINARY.
           10 NI-ANZ-KDN-2-PLUS           PIC S9(04)       BINARY.
           10 NI-NET-NEU-EIG-12MON        PIC S9(04)       BINARY.
           10 NI-NET-EIG-GES-12MON        PIC S9(04)       BINARY.
           10 NI-MAX-EQF-EIG              PIC S9(04)       BINARY.
           10 NI-BEITRAG-VOL              PIC S9(04)       BINARY.
           10 NI-VB-SEIT-MIN-MON          PIC S9(04)       BINARY.
           10 NI-AS-SEIT-MIN-MON          PIC S9(04)       BINARY.
           10 NI-NET-EIG-GES-36MO         PIC S9(04)       BINARY.
           10 NI-NET-GES-12MO             PIC S9(04)       BINARY.
           10 NI-KTY-TYP-BEN-CLUB         PIC S9(04)       BINARY.
           10 NI-KNR-TYP-BEN-CLUB         PIC S9(04)       BINARY.
           10 NI-KTY-TYP-ENDE-CLUB        PIC S9(04)       BINARY.
           10 NI-KNR-TYP-ENDE-CLUB        PIC S9(04)       BINARY.
           10 NI-ED-ANZ-SOLL              PIC S9(04)       BINARY.
           10 NI-ANZ-PART-KAP             PIC S9(04)       BINARY.
           10 NI-ANZ-AKT-STRUK-ADM        PIC S9(04)       BINARY.
           10 NI-CLBED-LAST-UPDATE        PIC S9(04)       BINARY.
           10 NI-CLBED-DAT-VON            PIC S9(04)       BINARY.
           10 NI-CLBED-DAT-BIS            PIC S9(04)       BINARY.
           10 NI-ANZ-HAUPTBEPART          PIC S9(04)       BINARY.
           10 NI-ANZ-NEU-DIR-PART         PIC S9(04)       BINARY.
           10 NI-MAX-EQF-GES              PIC S9(04)       BINARY.
           10 NI-NET-NEU-GES-12MO         PIC S9(04)       BINARY.
           10 NI-ANZ-STERNE               PIC S9(04)       BINARY.
           10 NI-ANT-INVEST               PIC S9(04)       BINARY.

        05 DCL-VIVBD043.
           10 KTY-CLUB-TYP                PIC S9(04)       BINARY.
           10 KNR-CLUB-TYP                PIC S9(04)       BINARY.
           10 KTY-GESELL-TYP              PIC S9(04)       BINARY.
           10 KNR-GESELL-TYP              PIC S9(04)       BINARY.
           10 CLUB-KRITERIUM              PIC S9(04)       BINARY.
           10 CLUB-MS-NR                  PIC S9(04)       BINARY.
           10 CLUB-VBP-NR                 PIC S9(04)       BINARY.
           10 CLUB-ANZ-KDN                PIC S9(04)       BINARY.
           10 ANZ-KDN-2-PLUS              PIC S9(04)       BINARY.
           10 NET-NEU-EIG-12MON           PIC S9(09)V9(01) COMP-3.
           10 NET-EIG-GES-12MON           PIC S9(09)V9(01) COMP-3.
           10 MAX-EQF-EIG                 PIC S9(03)V9(02) COMP-3.
           10 BEITRAG-VOL                 PIC S9(09)V9(02) COMP-3.
           10 VB-SEIT-MIN-MON             PIC S9(04)       BINARY.
           10 AS-SEIT-MIN-MON             PIC S9(04)       BINARY.
           10 NET-EIG-GES-36MO            PIC S9(09)V9(01) COMP-3.
           10 NET-GES-12MO                PIC S9(09)V9(01) COMP-3.
           10 KTY-TYP-BEN-CLUB            PIC S9(04)       BINARY.
           10 KNR-TYP-BEN-CLUB            PIC S9(04)       BINARY.
           10 KTY-TYP-ENDE-CLUB           PIC S9(04)       BINARY.
           10 KNR-TYP-ENDE-CLUB           PIC S9(04)       BINARY.
           10 ED-ANZ-SOLL                 PIC S9(04)       BINARY.
           10 ANZ-PART-KAP                PIC S9(04)       BINARY.
           10 ANZ-AKT-STRUK-ADM           PIC S9(04)       BINARY.
           10 CLBED-LAST-UPDATE           PIC  X(26).
           10 CLBED-DAT-VON               PIC  X(10).
           10 CLBED-DAT-BIS               PIC  X(10).
           10 ANZ-HAUPTBEPART             PIC S9(04)       BINARY.
           10 ANZ-NEU-DIR-PART            PIC S9(04)       BINARY.
           10 MAX-EQF-GES                 PIC S9(03)V9(02) COMP-3.
           10 NET-NEU-GES-12MO            PIC S9(09)V9(01) COMP-3.
           10 ANZ-STERNE                  PIC S9(04)       BINARY.
           10 ANT-INVEST                  PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  33                    *
      *****************************************************************
