      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0013.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTAN0013.CPv  $
      *
      *   Rev 2.6   Jul 25 2013 10:52:48   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.5   May 15 2012 10:40:20   t13ae23
      *AAN01051: Neu Generali  "RISK Care" mit R6.16
      *
      *   Rev 2.4   Dec 14 2011 17:28:44   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.3   Dec 14 2011 16:51:12   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.2   May 22 2009 09:51:06   t13ae23
      *FAN00884: Beitragsfelder in TBAN0013 zu klein
      *
      *   Rev 2.1   Nov 24 2008 14:03:06   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.0   Sep 16 2008 09:39:20   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: CTAN0013                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0013 GENERALI-SACH    *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00824!25.11.08!2.1  !NEU                        !RAHNER   *
      * FAN00884!22.05.09!2.2  !VERGRÖßERN BEITRAGSFELDER  !RAHNER   *
      * AAN01033!14.12.11!2.3  !DATEN FÜR GELDWÄSCHE ECT   !RAHNER   *
      * AAN01051!15.05.12!2.5  !INTERNE NUMMERN GENERALI   !RAHNER   *
      * AAN01073!25.07.13!2.6  !SEPA                       !RAHNER   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*

CCC005*    EXEC SQL DECLARE TBAN0013 TABLE
CCC004*    (
CCC004*       PGEB_GES                        SMALLINT
CCC004*                                      NOT NULL,
CCC004*       GENS_PIN                        INT
CCC004*                                      NOT NULL,
CCC004*       GENS_HVANTNUM                   CHAR (9)
CCC004*                                      NOT NULL,
CCC004*       GENS_AVERM                      INT
CCC004*                                      NOT NULL,
CCC004*       GENS_AVERM_PZ                   CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       GENS_AVERM_E                    INT
CCC004*                                      NOT NULL,
CCC004*       GENS_AVERM_PZ_E                 CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       GENS_VB_GES                     SMALLINT
CCC004*                                      NOT NULL,
CCC004*       GENS_VMNR                       INT
CCC004*                                      NOT NULL,
CCC004*       GENS_VMNR_PZ                    CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       GENS_VM_VORNA                   CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VM_ZUNA                    CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VMNR_E                     INT
CCC004*                                      NOT NULL,
CCC004*       GENS_VMPZ_E                     CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       GENS_VM_VORNA_E                 CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VM_ZUNA_E                  CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VB_GW                      INT
CCC004*                                      NOT NULL,
CCC004*       GENS_VB_GW_PZ                   CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       GENS_VB_GW_E                    INT
CCC004*                                      NOT NULL,
CCC004*       GENS_VB_GW_PZ_E                 CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       GENS_DIR                        SMALLINT
CCC004*                                      NOT NULL,
CCC004*       GENS_LAST_UPDATE                TIMESTAMP
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       GENS_KENNUNG_A                  CHAR (8)
CCC004*                                      NOT NULL,
CCC004*       GENS_KD_BEST_ID                 DECIMAL (18, 0)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_AKD_T                   CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_AKD_T_E                 CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_VORNA                   CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_VORNA_E                 CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_ZUNA                    CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_ZUNA_E                  CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_SVNR                    DECIMAL (10, 0)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_SVNR_E                  DECIMAL (10, 0)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_STR                     CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_STR_E                   CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_PLZ                     CHAR (5)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_PLZ_E                   CHAR (5)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_ORT                    CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_ORT_E                  CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_LKZ                     CHAR (3)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_LKZ_E                   CHAR (3)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_KNR                     DECIMAL (15, 0)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_KNR_E                   DECIMAL (15, 0)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_BLZ                     DECIMAL (15, 0)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_BLZ_E                   DECIMAL (15, 0)
CCC004*                                      NOT NULL,
CCC004*       GENS_VD_P_1                     SMALLINT
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P1                   DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P1                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P1_E                 DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P1_E                 DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_NJP_P1                  DECIMAL (6, 2)
CCC004*       GENS_VD_NJP_P1                  DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_EH_P1                   DECIMAL (6, 2)
CCC004*       GENS_VD_EH_P1                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
CCC004*       GENS_VD_P_2                     SMALLINT
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P2                   DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P2                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P2_E                 DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P2_E                 DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_NJP_P2                  DECIMAL (6, 2)
CCC004*       GENS_VD_NJP_P2                  DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_EH_P2                   DECIMAL (6, 2)
CCC004*       GENS_VD_EH_P2                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
CCC004*       GENS_VD_P_3                     SMALLINT
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P3                   DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P3                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P3_E                 DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P3_E                 DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_NJP_P3                  DECIMAL (6, 2)
CCC004*       GENS_VD_NJP_P3                  DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_EH_P3                   DECIMAL (6, 2)
CCC004*       GENS_VD_EH_P3                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
CCC004*       GENS_VD_P_4                     SMALLINT
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P4                   DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P4                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P4_E                 DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P4_E                 DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_NJP_P4                  DECIMAL (6, 2)
CCC004*       GENS_VD_NJP_P4                  DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_EH_P4                   DECIMAL (6, 2)
CCC004*       GENS_VD_EH_P4                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
CCC004*       GENS_VD_P_5                     SMALLINT
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P5                   DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P5                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P5_E                 DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P5_E                 DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_NJP_P5                  DECIMAL (6, 2)
CCC004*       GENS_VD_NJP_P5                  DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_EH_P5                   DECIMAL (6, 2)
CCC004*       GENS_VD_EH_P5                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
CCC004*       GENS_VD_P_6                     SMALLINT
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P6                   DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P6                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P6_E                 DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P6_E                 DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_NJP_P6                  DECIMAL (6, 2)
CCC004*       GENS_VD_NJP_P6                  DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_EH_P6                   DECIMAL (6, 2)
CCC004*       GENS_VD_EH_P6                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
CCC004*       GENS_VD_P_7                     SMALLINT
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P7                   DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P7                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P7_E                 DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P7_E                 DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_NJP_P7                  DECIMAL (6, 2)
CCC004*       GENS_VD_NJP_P7                  DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_EH_P7                   DECIMAL (6, 2)
CCC004*       GENS_VD_EH_P7                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
CCC004*       GENS_VD_P_8                     SMALLINT
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P8                   DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P8                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_BP_P8_E                 DECIMAL (6, 2)
CCC004*       GENS_VD_BP_P8_E                 DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_NJP_P8                  DECIMAL (6, 2)
CCC004*       GENS_VD_NJP_P8                  DECIMAL (8, 2)
CCC004*                                      NOT NULL,
220509*       GENS_VD_EH_P8                   DECIMAL (6, 2)
CCC004*       GENS_VD_EH_P8                   DECIMAL (8, 2)
CCC004*                                      NOT NULL,
CCC004*       GENS_BEGINN                     DATE
CCC004*                                      NOT NULL,
CCC004*       GENS_BEGINN_E                   DATE
CCC004*                                      NOT NULL,
CCC004*       GENS_ENDE                       DATE
CCC004*                                      NOT NULL,
CCC004*       GENS_ENDE_E                     DATE
CCC004*                                      NOT NULL,
CCC004*       GENS_ZAHLW                      SMALLINT
CCC004*                                      NOT NULL,
CCC004*       GENS_ZAHLW_E                    SMALLINT
CCC004*                                      NOT NULL,
CCC004*       GENS_WAEHRUNG                   CHAR (3)
CCC004*                                      NOT NULL,
CCC004*       GENS_KFZ_KZ                     CHAR (10)
CCC004*                                      NOT NULL,
CCC004*       GENS_KFZ_KZ_E                   CHAR (10)
CCC004*                                      NOT NULL,
CCC004*       GENS_FGST_NR                    CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_FGST_NR_E                  CHAR (26)
CCC004*                                      NOT NULL,
CCC004*       GENS_KFZ_B_NR                   CHAR (20)
CCC004*                                      NOT NULL,
CCC004*       GENS_KFZ_B_NR_E                 CHAR (20)
CCC004*                                      NOT NULL,
CCC004*       GENS_AN_ART                     CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       GENS_AN_ART_E                   CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       GENS_AN_DAT                     DATE
CCC004*                                      NOT NULL,
CCC004*       GENS_AN_DAT_E                   DATE
CCC004*                                      NOT NULL,
CCC004*       GENS_VERSION                    CHAR (5)
CCC004*                                      NOT NULL,
CCC004*       GENS_FILLER                     CHAR (100)
CCC004*                                      NOT NULL,
CCC004*       GENS_XML                        VARCHAR (30000)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_STAA                    CHAR (3)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_STAA_E                  CHAR (3)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_GEB                     DATE
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_GEB_E                   DATE
CCC004*                                      NOT NULL,
CCC004*       GENS_GEN_ANT_NR                 DECIMAL (15, 0)
CCC004*                                      NOT NULL,
CCC004*       GENS_GEN_B_ANT_NR               DECIMAL (15, 0)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_IBAN                    CHAR (34)
CCC004*                                      NOT NULL,
CCC004*       GENS_VN_IBAN_E                  CHAR (34)
CCC004*                                      NOT NULL
CCC004*    )
CCC003*    END-EXEC.
      *
       01  DCLTBAN0013.
           03 PGEB-GES                    PIC S9(4)       COMP.
           03 GENS-PIN                    PIC S9(9)       COMP.
           03 GENS-HVANTNUM               PIC X(9).
      *
           03 GENS-AVERM                  PIC S9(9)       COMP.
           03 GENS-AVERM-PZ               PIC X.
           03 GENS-AVERM-E                PIC S9(9)       COMP.
           03 GENS-AVERM-PZ-E             PIC X.
           03 GENS-VB-GES                 PIC S9(4)       COMP.
      *
           03 GENS-VMNR                   PIC S9(9)       COMP.
           03 GENS-VMNR-PZ                PIC X(1).
           03 GENS-VM-VORNA               PIC X(26).
           03 GENS-VM-ZUNA                PIC X(26).
           03 GENS-VMNR-E                 PIC S9(9)       COMP.
           03 GENS-VMPZ-E                 PIC X(1).
           03 GENS-VM-VORNA-E             PIC X(26).
           03 GENS-VM-ZUNA-E              PIC X(26).
      *
           03 GENS-VB-GW                  PIC S9(9)       COMP.
           03 GENS-VB-GW-PZ               PIC X.
           03 GENS-VB-GW-E                PIC S9(9)       COMP.
           03 GENS-VB-GW-PZ-E             PIC X.
      *
           03 GENS-DIR                    PIC S9(4)       COMP.
      *
           03 GENS-LAST-UPDATE            PIC X(26).
           03 GENS-KENNUNG-A              PIC X(8).
      *
           03 GENS-KD-BEST-ID             PIC S9(18)V     COMP-3.
           03 GENS-VN-AKD-T               PIC X(26).
           03 GENS-VN-AKD-T-E             PIC X(26).
           03 GENS-VN-VORNA               PIC X(26).
           03 GENS-VN-VORNA-E             PIC X(26).
           03 GENS-VN-ZUNA                PIC X(26).
           03 GENS-VN-ZUNA-E              PIC X(26).
           03 GENS-VN-SVNR                PIC S9(10)V     COMP-3.
           03 GENS-VN-SVNR-E              PIC S9(10)V     COMP-3.
           03 GENS-VN-STR                 PIC X(26).
           03 GENS-VN-STR-E               PIC X(26).
           03 GENS-VN-PLZ                 PIC X(5).
           03 GENS-VN-PLZ-E               PIC X(5).
           03 GENS-VN-ORT                 PIC X(26).
           03 GENS-VN-ORT-E               PIC X(26).
           03 GENS-VN-LKZ                 PIC X(3).
           03 GENS-VN-LKZ-E               PIC X(3).
           03 GENS-VN-KNR                 PIC S9(15)V     COMP-3.
           03 GENS-VN-KNR-E               PIC S9(15)V     COMP-3.
           03 GENS-VN-BLZ                 PIC S9(15)V     COMP-3.
           03 GENS-VN-BLZ-E               PIC S9(15)V     COMP-3.
      *
           03 GENS-VN-PRODKEY1            PIC S9(4)       COMP.
           03 GENS-VN-BTP-P1              PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-BTP-P1-E            PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-JNTP-P1             PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-EH-P1               PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-PRODKEY2            PIC S9(4)       COMP.
           03 GENS-VN-BTP-P2              PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-BTP-P2-E            PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-JNTP-P2             PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-EH-P2               PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-PRODKEY3            PIC S9(4)       COMP.
           03 GENS-VN-BTP-P3              PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-BTP-P3-E            PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-JNTP-P3             PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-EH-P3               PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-PRODKEY4            PIC S9(4)       COMP.
           03 GENS-VN-BTP-P4              PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-BTP-P4-E            PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-JNTP-P4             PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-EH-P4               PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-PRODKEY5            PIC S9(4)       COMP.
           03 GENS-VN-BTP-P5              PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-BTP-P5-E            PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-JNTP-P5             PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-EH-P5               PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-PRODKEY6            PIC S9(4)       COMP.
           03 GENS-VN-BTP-P6              PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-BTP-P6-E            PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-JNTP-P6             PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-EH-P6               PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-PRODKEY7            PIC S9(4)       COMP.
           03 GENS-VN-BTP-P7              PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-BTP-P7-E            PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-JNTP-P7             PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-EH-P7               PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-PRODKEY8            PIC S9(4)       COMP.
           03 GENS-VN-BTP-P8              PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-BTP-P8-E            PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-JNTP-P8             PIC S9(9)V9(2)  COMP-3.
           03 GENS-VN-EH-P8               PIC S9(9)V9(2)  COMP-3.
      *
           03 GENS-BEGINN                 PIC X(10).
           03 GENS-BEGINN-E               PIC X(10).
           03 GENS-ENDE                   PIC X(10).
           03 GENS-ENDE-E                 PIC X(10).
           03 GENS-ZAHLW                  PIC S9(4)       COMP.
           03 GENS-ZAHLW-E                PIC S9(4)       COMP.
           03 GENS-WAEHRUNG               PIC X(3).
      *
           03 GENS-KFZ-KZ                 PIC X(10).
           03 GENS-KFZ-KZ-E               PIC X(10).
           03 GENS-FGST-NR                PIC X(26).
           03 GENS-FGST-NR-E              PIC X(26).
           03 GENS-KFZ-B-NR               PIC X(20).
           03 GENS-KFZ-B-NR-E             PIC X(20).
      *
           03 GENS-AN-ART                 PIC X(1).
           03 GENS-AN-ART-E               PIC X(1).
           03 GENS-AN-DAT                 PIC X(10).
           03 GENS-VERSION                PIC X(5).
           03 GENS-FILLER                 PIC X(100).
           03 GENS-XML.
              05 GENS-XML-LEN             PIC S9(4) COMP.
              05 GENS-XML-TEXT            PIC X(30000).
141211     03 GENS-VN-STAA                PIC X(3).
141211     03 GENS-VN-STAA-E              PIC X(3).
141211     03 GENS-VN-GEB                 PIC X(10).
141211     03 GENS-VN-GEB-E               PIC X(10).
150512     03 GENS-GEN-ANT-NR             PIC S9(15)V     COMP-3.
150512     03 GENS-GEN-B-ANT-NR           PIC S9(15)V     COMP-3.
250713     03 GENS-VN-IBAN                PIC X(34).
250713     03 GENS-VN-IBAN-E              PIC X(34).


