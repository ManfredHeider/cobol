      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB060.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVBB060.CPv  $
      *
      *   Rev 2.2   Jan 12 2009 15:43:58   t13ae30
      *AAS00779: Allfinanz "Champions-Club" 
      *
      *   Rev 2.1   Jan 09 2009 17:05:42   t13ae30
      *AAS00779: Allfinanz "Champions-Club" 
      *
      *   Rev 2.0   Sep 15 2008 12:18:12   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVBB060                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBVB0060 (CLPRUEF)                              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    01.09.2008                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00779!01.09.08!2.1 ! NEU                       ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*
      * DCLGEN TABLE(TBTP0005)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB060))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBB060 TABLE
CCC004*    (
CCC004*      MA_ST_NR                   INTEGER         NOT NULL,
CCC004*      KTY_CLUB_TYP               SMALLINT        NOT NULL,
CCC004*      KNR_CLUB_TYP               SMALLINT        NOT NULL,
CCC004*      CLUB_KRITERIUM             SMALLINT        NOT NULL,
CCC004*      CLUB_MS_NR                 SMALLINT                ,
CCC004*      CL_MS_NR_ERF               CHAR    (1)             ,
CCC004*      CLUB_VBP_NR                SMALLINT                ,
CCC004*      CL_VBP_NR_ERF              CHAR    (1)             ,
CCC004*      CLUB_ANZ_KDN               SMALLINT                ,
CCC004*      CL_ANZ_KDN_ERF             CHAR    (1)             ,
CCC004*      ANZ_KDN_2_PLUS             SMALLINT                ,
CCC004*      CL_ANZ_KDN2_ERF            CHAR    (1)             ,
CCC004*      NET_NEU_EIG_12MON          DECIMAL (10, 1)         ,
CCC004*      CL_NN_EIG12MO_ERF          CHAR    (1)             ,
CCC004*      MAX_EQF_EIG                DECIMAL (5, 2)          ,
CCC004*      CL_MAX_EQFEIG_ERF          CHAR    (1)             ,
CCC004*      BEITRAG_VOL                DECIMAL (11, 2)         ,
CCC004*      CL_BEITRAG_ERF             CHAR    (1)             ,
CCC004*      VB_SEIT_MIN_MON            SMALLINT                ,
CCC004*      CL_VB_SEIT_ERF             CHAR    (1)             ,
CCC004*      AS_SEIT_MIN_MON            SMALLINT                ,
CCC004*      CL_AS_SEIT_ERF             CHAR    (1)             ,
CCC004*      NET_EIG_GES_36MO           DECIMAL (10, 1)         ,
CCC004*      CL_NEGES36MO_ERF           CHAR    (1)             ,
CCC004*      NET_GES_12MO               DECIMAL (10, 1)         ,
CCC004*      CL_NGES12MO_ERF            CHAR    (1)             ,
CCC004*      KTY_TYP_BEN_CLUB           SMALLINT                ,
CCC004*      KNR_TYP_BEN_CLUB           SMALLINT                ,
CCC004*      CL_BEN_CLUB_ERF            CHAR    (1)             ,
CCC004*      KTY_TYP_BEEND_CLUB         SMALLINT                ,
CCC004*      KNR_TYP_BEEND_CLUB         SMALLINT                ,
CCC004*      CL_BEEND_CLUB_ERF          CHAR    (1)             ,
CCC004*      ED_ANZ_SOLL                SMALLINT                ,
CCC004*      CL_EDANZ_ERF               CHAR    (1)             ,
CCC004*      ANZ_PART_KAP               SMALLINT                ,
CCC004*      CL_ANZPART_ERF             CHAR    (1)             ,
CCC004*      ANZ_AKT_STRUK_ADM          SMALLINT                ,
CCC004*      CL_AKTSTRUK_ERF            CHAR    (1)             ,
CCC004*      EIG_GES_12MON              DECIMAL (10, 1)         ,
CCC004*      CL_GES_EIG12MO_ERF         CHAR    (1)             ,
CCC004*      CLBED_DAT_VON              DATE            NOT NULL,
CCC004*      MAX_EQF_GES                DECIMAL (5, 2)          ,
CCC004*      CL_MX_EQFGES_ERF           CHAR    (1)             ,
CCC004*      ANZ_HAUPTBEPART            SMALLINT                ,
CCC004*      CL_HAUPTPART_ERF           CHAR    (1)             ,
CCC004*      ANZ_NEU_DIR_PART           SMALLINT                ,
CCC004*      CL_NEU_DPART_ERF           CHAR    (1)             ,
CCC004*      NET_NEU_GES_12MO           DECIMAL (10, 1)         ,
CCC004*      CL_NNGES12MO_ERF           CHAR    (1)
CCC004*        )
CCC003*       END-EXEC.

      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0060                           *
      ******************************************************************
       01  VIVBB060-GESAMT.
           05  NI-VIVBB060-TAB.
              10 NI-VIVBB060-ELE  PIC S9(04) BINARY OCCURS 49 TIMES.
           05  NI-VIVBB060        REDEFINES  NI-VIVBB060-TAB.
              10 NI-MA-ST-NR                PIC S9(04)       BINARY.
              10 NI-KTY-CL-TYP              PIC S9(04)       BINARY.
              10 NI-KNR-CL-TYP              PIC S9(04)       BINARY.
              10 NI-CLUB-KRITERIUM          PIC S9(04)       BINARY.
              10 NI-CLUB-MS-NR              PIC S9(04)       BINARY.
              10 NI-CL-MS-NR-ERF            PIC S9(04)       BINARY.
              10 NI-CLUB-VBP-NR             PIC S9(04)       BINARY.
              10 NI-CL-VBP-NR-ERF           PIC S9(04)       BINARY.
              10 NI-CLUB-ANZ-KDN            PIC S9(04)       BINARY.
              10 NI-CL-ANZ-KDN-ERF          PIC S9(04)       BINARY.
              10 NI-ANZ-KDN-2-PLUS          PIC S9(04)       BINARY.
              10 NI-CL-ANZ-KDN2-ERF         PIC S9(04)       BINARY.
              10 NI-NET-NEU-EIG-12MON       PIC S9(04)       BINARY.
              10 NI-CL-NN-EIG12MO-ERF       PIC S9(04)       BINARY.
              10 NI-MAX-EQF-EIG             PIC S9(04)       BINARY.
              10 NI-CL-MAX-EQFEIG-ERF       PIC S9(04)       BINARY.
              10 NI-BEITRAG-VOL             PIC S9(04)       BINARY.
              10 NI-CL-BEITRAG-ERF          PIC S9(04)       BINARY.
              10 NI-VB-SEIT-MIN-MON         PIC S9(04)       BINARY.
              10 NI-CL-VB-SEIT-ERF          PIC S9(04)       BINARY.
              10 NI-AS-SEIT-MIN-MON         PIC S9(04)       BINARY.
              10 NI-CL-AS-SEIT-ERF          PIC S9(04)       BINARY.
              10 NI-NET-EIG-GES-36MO        PIC S9(04)       BINARY.
              10 NI-CL-NEGES36MO-ERF        PIC S9(04)       BINARY.
              10 NI-NET-GES-12MO            PIC S9(04)       BINARY.
              10 NI-CL-NGES12MO-ERF         PIC S9(04)       BINARY.
              10 NI-KTY-TYP-BEN-CLUB        PIC S9(04)       BINARY.
              10 NI-KNR-TYP-BEN-CLUB        PIC S9(04)       BINARY.
              10 NI-CL-BEN-CLUB-ERF         PIC S9(04)       BINARY.
              10 NI-KTY-TYP-BEEND-CLUB      PIC S9(04)       BINARY.
              10 NI-KNR-TYP-BEEND-CLUB      PIC S9(04)       BINARY.
              10 NI-CL-BEEND-CLUB-ERF       PIC S9(04)       BINARY.
              10 NI-ED-ANZ-SOLL             PIC S9(04)       BINARY.
              10 NI-CL-EDANZ-ERF            PIC S9(04)       BINARY.
              10 NI-ANZ-PART-KAP            PIC S9(04)       BINARY.
              10 NI-CL-ANZPART-ERF          PIC S9(04)       BINARY.
              10 NI-ANZ-AKT-STRUK-ADM       PIC S9(04)       BINARY.
              10 NI-CL-AKTSTRUK-ERF         PIC S9(04)       BINARY.
              10 NI-EIG-GES-12MO            PIC S9(04)       BINARY.
              10 NI-CL-GES-EIG12MO-ERF      PIC S9(04)       BINARY.
              10 NI-CLBED-DAT-VON           PIC S9(04)       BINARY.
              10 NI-MAX-EQF-GES             PIC S9(04)       BINARY.
              10 NI-CL-MAX-EQFGES-ERF       PIC S9(04)       BINARY.
              10 NI-ANZ-HAUPTBER-PART       PIC S9(04)       BINARY.
              10 NI-CL-HAUPTBER-PART-ERF    PIC S9(04)       BINARY.
              10 NI-ANZ-NEU-DIR-PART        PIC S9(04)       BINARY.
              10 NI-CL-ANZNEU-DIRPART-ER    PIC S9(04)       BINARY.
              10 NI-NET-NEU-GES-12MO        PIC S9(04)       BINARY.
              10 NI-CL-NNGES12MO-ERF        PIC S9(04)       BINARY.
           05  DCL-VIVBB060.
              10 MA-ST-NR                       PIC S9(9)    COMP.
              10 KTY-CLUB-TYP                   PIC S9(4)    COMP.
              10 KNR-CLUB-TYP                   PIC S9(4)    COMP.
              10 CLUB-KRITERIUM                 PIC S9(4)    COMP.
              10 CLUB-MS-NR                     PIC S9(4)    COMP.
              10 CL-MS-NR-ERF                   PIC X.
              10 CLUB-VBP-NR                    PIC S9(4)    COMP.
              10 CL-VBP-NR-ERF                  PIC X.
              10 CLUB-ANZ-KDN                   PIC S9(4)    COMP.
              10 CL-ANZ-KDN-ERF                 PIC X.
              10 ANZ-KDN-2-PLUS                 PIC S9(4)    COMP.
              10 CL-ANZ-KDN2-ERF                PIC X.
              10 NET-NEU-EIG-12MON              PIC S9(9)V9  COMP-3.
              10 CL-NN-EIG12MO-ERF              PIC X.
              10 MAX-EQF-EIG                    PIC S9(3)V9  COMP-3.
              10 CL-MAX-EQFEIG-ERF              PIC X.
              10 BEITRAG-VOL                    PIC S9(9)V9  COMP-3.
              10 CL-BEITRAG-ERF                 PIC X.
              10 VB-SEIT-MIN-MON                PIC S9(4)    COMP.
              10 CL-VB-SEIT-ERF                 PIC X.
              10 AS-SEIT-MIN-MON                PIC S9(4)    COMP.
              10 CL-AS-SEIT-ERF                 PIC X.
              10 NET-EIG-GES-36MO               PIC S9(9)V9  COMP-3.
              10 CL-NEGES36MO-ERF               PIC X.
              10 NET-GES-12MO                   PIC S9(9)V9  COMP-3.
              10 CL-NGES12MO-ERF                PIC X.
              10 KTY-TYP-BEN-CLUB               PIC S9(4)    COMP.
              10 KNR-TYP-BEN-CLUB               PIC S9(4)    COMP.
              10 CL-BEN-CLUB-ERF                PIC X.
              10 KTY-TYP-BEEND-CLUB             PIC S9(4)    COMP.
              10 KNR-TYP-BEEND-CLUB             PIC S9(4)    COMP.
              10 CL-BEEND-CLUB-ERF              PIC X.
              10 ED-ANZ-SOLL                    PIC S9(4)    COMP.
              10 CL-EDANZ-ERF                   PIC X.
              10 ANZ-PART-KAP                   PIC S9(4)    COMP.
              10 CL-ANZPART-ERF                 PIC X.
              10 ANZ-AKT-STRUK-ADM              PIC S9(4)    COMP.
              10 CL-AKTSTRUK-ERF                PIC X.
              10 EIG-GES-12MO                   PIC S9(9)V9  COMP-3.
              10 CL-GES-EIG12MO-ERF             PIC X.
              10 CLBED-DAT-VON                  PIC X(10).
              10 MAX-EQF-GES                    PIC S9(3)V9  COMP-3.
              10 CL-MAX-EQFGES-ERF              PIC X.
              10 ANZ-HAUPTBER-PART              PIC S9(4)    COMP.
              10 CL-HAUPTBER-PART-ERF           PIC X.
              10 ANZ-NEU-DIR-PART               PIC S9(4)    COMP.
              10 CL-ANZNEU-DIRPART-ER           PIC X.
              10 NET-NEU-GES-12MO               PIC S9(9)V9  COMP-3.
              10 CL-NNGES12MO-ERF               PIC X.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 49      *
      ******************************************************************
