      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB043.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB043.CPv  $
      *
      *   Rev 2.1   Jan 09 2009 17:05:22   t13ae30
      *AAS00779: Allfinanz "Champions-Club" 
      *
      *   Rev 2.0   Sep 11 2008 12:31:18   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE VIVBB043 TABLE
CCC004*    (
CCC004*       KTY_CLUB_TYP              SMALLINT NOT NULL,
CCC004*       KNR_CLUB_TYP              SMALLINT NOT NULL,
CCC004*       KTY_GESELL_TYP            SMALLINT NOT NULL,
CCC004*       KNR_GESELL_TYP            SMALLINT NOT NULL,
CCC004*       CLUB_KRITERIUM            SMALLINT NOT NULL,
CCC004*       CLUB_MS_NR                SMALLINT,
CCC004*       CLUB_VBP_NR               SMALLINT,
CCC004*       CLUB_ANZ_KDN              SMALLINT,
CCC004*       ANZ_KDN_2_PLUS            SMALLINT,
CCC004*       NET_NEU_EIG_12MON         DECIMAL (10, 1),
CCC004*       NET_EIG_GES_12MON         DECIMAL (10, 1),
CCC004*       MAX_EQF_EIG               DECIMAL (5, 2),
CCC004*       BEITRAG_VOL               DECIMAL (11, 2),
CCC004*       VB_SEIT_MIN_MON           SMALLINT,
CCC004*       AS_SEIT_MIN_MON           SMALLINT,
CCC004*       NET_EIG_GES_36MO          DECIMAL (10, 1),
CCC004*       NET_GES_12MO              DECIMAL (10, 1),
CCC004*       KTY_TYP_BEN_CLUB          SMALLINT,
CCC004*       KNR_TYP_BEN_CLUB          SMALLINT,
CCC004*       KTY_TYP_ENDE_CLUB         SMALLINT,
CCC004*       KNR_TYP_ENDE_CLUB         SMALLINT,
CCC004*       ED_ANZ_SOLL               SMALLINT,
CCC004*       ANZ_PART_KAP              SMALLINT,
CCC004*       ANZ_AKT_STRUK_ADM         SMALLINT,
CCC004*       CLBED_LAST_UPDATE         TIMESTAMP NOT NULL WITH DEFAULT,
CCC004*       CLBED_DAT_VON             DATE NOT NULL WITH DEFAULT,
CCC004*       CLBED_DAT_BIS             DATE NOT NULL WITH DEFAULT,
CCC004*       ANZ_HAUPTBEPART           SMALLINT,
CCC004*       ANZ_NEU_DIR_PART          SMALLINT,
CCC004*       MAX_EQF_GES               DECIMAL (5, 2),
CCC004*       NET_NEU_GES_12MON         DECIMAL (10, 1)
CCC004*    )
CCC003*    END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB043                           *
      ******************************************************************
       01  VIVBB043-GESAMT.
        05  NI-VIVBB043-TAB.
           10 NI-VIVBB043-ELE      PIC S9(04)  COMP   OCCURS  31 TIMES.
        05  NI-VIVBB043            REDEFINES   NI-VIVBB043-TAB.
           10 NI-KTY-CLUB-TYP                PIC S9(04)  COMP.
           10 NI-KNR-CLUB-TYP                PIC S9(04)  COMP.
           10 NI-KTY-GESELL-TYP              PIC S9(04)  COMP.
           10 NI-KNR-GESELL-TYP              PIC S9(04)  COMP.
           10 NI-CLUB-KRITERIUM              PIC S9(04)  COMP.
           10 NI-CLUB-MS-NR                  PIC S9(04)  COMP.
           10 NI-CLUB-VBP-NR                 PIC S9(04)  COMP.
           10 NI-CLUB-ANZ-KDN                PIC S9(04)  COMP.
           10 NI-ANZ-KDN-2-PLUS              PIC S9(04)  COMP.
           10 NI-NET-NEU-EIG-12MON           PIC S9(04)  COMP.
           10 NI-NET-EIG-GES-12MON           PIC S9(04)  COMP.
           10 NI-MAX-EQF-EIG                 PIC S9(04)  COMP.
           10 NI-BEITRAG-VOL                 PIC S9(04)  COMP.
           10 NI-VB-SEIT-MIN-MON             PIC S9(04)  COMP.
           10 NI-AS-SEIT-MIN-MON             PIC S9(04)  COMP.
           10 NI-NET-EIG-GES-36MO            PIC S9(04)  COMP.
           10 NI-NET-GES-12MO                PIC S9(04)  COMP.
           10 NI-KTY-TYP-BEN-CLUB            PIC S9(04)  COMP.
           10 NI-KNR-TYP-BEN-CLUB            PIC S9(04)  COMP.
           10 NI-KTY-TYP-ENDE-CLUB           PIC S9(04)  COMP.
           10 NI-KNR-TYP-ENDE-CLUB           PIC S9(04)  COMP.
           10 NI-ED-ANZ-SOLL                 PIC S9(04)  COMP.
           10 NI-ANZ-PART-KAP                PIC S9(04)  COMP.
           10 NI-ANZ-AKT-STRUK-ADM           PIC S9(04)  COMP.
           10 NI-CLBED-LAST-UPDATE           PIC S9(04)  COMP.
           10 NI-CLBED-DAT-VON               PIC S9(04)  COMP.
           10 NI-CLBED-DAT-BIS               PIC S9(04)  COMP.
           10 NI-ANZ-HAUPTBEPART             PIC S9(04)  COMP.
           10 NI-ANZ-NEU-DIR-PART            PIC S9(04)  COMP.
           10 NI-MAX-EQF-GES                 PIC S9(04)  COMP.
           10 NI-NET-NEU-GES-12MON           PIC S9(04)  COMP.
        05  DCL-VIVBB043.
           10 KTY-CLUB-TYP                   PIC S9(4) COMP.
           10 KNR-CLUB-TYP                   PIC S9(4) COMP.
           10 KTY-GESELL-TYP                 PIC S9(4) COMP.
           10 KNR-GESELL-TYP                 PIC S9(4) COMP.
           10 CLUB-KRITERIUM                 PIC S9(4) COMP.
           10 CLUB-MS-NR                     PIC S9(4) COMP.
           10 CLUB-VBP-NR                    PIC S9(4) COMP.
           10 CLUB-ANZ-KDN                   PIC S9(4) COMP.
           10 ANZ-KDN-2-PLUS                 PIC S9(4) COMP.
           10 NET-NEU-EIG-12MON              PIC S9(9)V9(1) COMP-3.
           10 NET-EIG-GES-12MON              PIC S9(9)V9(1) COMP-3.
           10 MAX-EQF-EIG                    PIC S9(3)V9(2) COMP-3.
           10 BEITRAG-VOL                    PIC S9(9)V9(2) COMP-3.
           10 VB-SEIT-MIN-MON                PIC S9(4) COMP.
           10 AS-SEIT-MIN-MON                PIC S9(4) COMP.
           10 NET-EIG-GES-36MO               PIC S9(9)V9(1) COMP-3.
           10 NET-GES-12MO                   PIC S9(9)V9(1) COMP-3.
           10 KTY-TYP-BEN-CLUB               PIC S9(4) COMP.
           10 KNR-TYP-BEN-CLUB               PIC S9(4) COMP.
           10 KTY-TYP-ENDE-CLUB              PIC S9(4) COMP.
           10 KNR-TYP-ENDE-CLUB              PIC S9(4) COMP.
           10 ED-ANZ-SOLL                    PIC S9(4) COMP.
           10 ANZ-PART-KAP                   PIC S9(4) COMP.
           10 ANZ-AKT-STRUK-ADM              PIC S9(4) COMP.
           10 CLBED-LAST-UPDATE              PIC X(26).
           10 CLBED-DAT-VON                  PIC X(10).
           10 CLBED-DAT-BIS                  PIC X(10).
           10 ANZ-HAUPTBEPART                PIC S9(4) COMP.
           10 ANZ-NEU-DIR-PART               PIC S9(4) COMP.
           10 MAX-EQF-GES                    PIC S9(3)V9(2) COMP-3.
           10 NET-NEU-GES-12MON              PIC S9(9)V9(1) COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 31      *
      ******************************************************************
