      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  12.09.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBE043.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVBE043.CPv  $
      *
      *   Rev 2.2   Jan 13 2020 16:35:56   t13ae30
      *AAS01307: 1. Erweiterung Cursor OZPs ZJVB0013 und ZJVB0014
      *
      *   Rev 2.1   Oct 30 2019 12:36:34   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club mit besonderen Leist
      *ungen und AS-Club (VB-System)
      *
      *   Rev 2.0   Sep 12 2019 13:28:36   T13AE30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    11.09.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VBL-964 !11.09.19!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBE043) CLBED                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBE043)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBE043 TABLE
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
CCC004*    , FAKTOR_1                    DECIMAL(05, 2)
CCC004*    , FAKTOR_2                    DECIMAL(05, 2)
CCC004*    , FAKTOR_GES                  DECIMAL(05, 2)
CCC004*    , BQF                         DECIMAL(05, 2)
CCC004*    , BED_ERF_MIN_MON             SMALLINT
CCC004*    , KAP_SEIT_MIN_MON            SMALLINT
CCC004*    , ED_MITGL                    CHAR(01)
CCC004*    , FAK_1_BASISWERT             DECIMAL(10, 1)
CCC004*    , FAK_2_BASISWERT             DECIMAL(10, 1)
CCC004*    , GES_EH_EIG_MAX              DECIMAL(10, 1)
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBE043                          *
      *****************************************************************
 
       01  VIVBE043-GESAMT.
        05 NI-VIVBE043-TAB.
           10 NI-VIVBE043-ELE      PIC S9(04)  BINARY OCCURS  43.
        05 NI-VIVBE043             REDEFINES   NI-VIVBE043-TAB.
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
           10 NI-FAKTOR-1                 PIC S9(04)       BINARY.
           10 NI-FAKTOR-2                 PIC S9(04)       BINARY.
           10 NI-FAKTOR-GES               PIC S9(04)       BINARY.
           10 NI-BQF                      PIC S9(04)       BINARY.
           10 NI-BED-ERF-MIN-MON          PIC S9(04)       BINARY.
           10 NI-KAP-SEIT-MIN-MON         PIC S9(04)       BINARY.
           10 NI-ED-MITGL                 PIC S9(04)       BINARY.
           10 NI-FAK-1-BASISWERT          PIC S9(04)       BINARY.
           10 NI-FAK-2-BASISWERT          PIC S9(04)       BINARY.
           10 NI-GES-EH-EIG-MAX           PIC S9(04)       BINARY.
 
        05 DCL-VIVBE043.
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
           10 FAKTOR-1                    PIC S9(03)V9(02) COMP-3.
           10 FAKTOR-2                    PIC S9(03)V9(02) COMP-3.
           10 FAKTOR-GES                  PIC S9(03)V9(02) COMP-3.
           10 BQF                         PIC S9(03)V9(02) COMP-3.
           10 BED-ERF-MIN-MON             PIC S9(04)       BINARY.
           10 KAP-SEIT-MIN-MON            PIC S9(04)       BINARY.
           10 ED-MITGL                    PIC  X(01).
           10 FAK-1-BASISWERT             PIC S9(09)V9(01) COMP-3.
           10 FAK-2-BASISWERT             PIC S9(09)V9(01) COMP-3.
           10 GES-EH-EIG-MAX              PIC S9(09)V9(01) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  43                    *
      *****************************************************************


