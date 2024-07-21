      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB058.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB058.CPv  $
      *
      *   Rev 2.1   Dec 21 2017 10:36:58   t13ae30
      *AVB01474: 3. Statistik-Kategorie Folge Neu - Programme
      *
      *   Rev 2.0   Dec 13 2017 11:32:50   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    13.12.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01474!13.12.17!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBB058) ALIAS                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB058)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBB058 TABLE
CCC004*    ( ST_NR                       INTEGER NOT NULL
CCC004*    , PSEV_MONAT                  DATE NOT NULL
CCC004*    , KNR_PROD_SPARTE             SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KTY_PROD_SPARTE             SMALLINT NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , PSEV_EH_GS_ZU_EG            DECIMAL(10, 1) NOT NULL
CCC004*    , PSEV_EH_GS_ZU_GR            DECIMAL(10, 1) NOT NULL
CCC004*    , PSEV_EH_GS_AB_EG            DECIMAL(10, 1) NOT NULL
CCC004*    , PSEV_EH_GS_AB_GR            DECIMAL(10, 1) NOT NULL
CCC004*    , PSEV_PV_GS_NT_EG            DECIMAL(11, 2) NOT NULL
CCC004*    , PSEV_PV_GS_NT_GR            DECIMAL(11, 2) NOT NULL
CCC004*    , PSEV_PV_FG_NT_EG            DECIMAL(11, 2) NOT NULL
CCC004*    , PSEV_PV_FG_NT_GR            DECIMAL(11, 2) NOT NULL
CCC004*    , PSEV_EH_FG_ZU_EG            DECIMAL(10, 1) NOT NULL
CCC004*    , PSEV_EH_FG_ZU_GR            DECIMAL(10, 1) NOT NULL
CCC004*    , PSEV_EH_FG_AB_EG            DECIMAL(10, 1) NOT NULL
CCC004*    , PSEV_EH_FG_AB_GR            DECIMAL(10, 1) NOT NULL
CCC004*    , KTY_UST_KZ                  SMALLINT NOT NULL
CCC004*    , KNR_UST_KZ                  SMALLINT NOT NULL
CCC004*    , PV_GS_NT_EG_USTANT          DECIMAL(11, 2) NOT NULL
CCC004*    , PV_GS_NT_GR_USTANT          DECIMAL(11, 2) NOT NULL
CCC004*    , PV_FG_NT_EG_USTANT          DECIMAL(11, 2) NOT NULL
CCC004*    , PV_FG_NT_GR_USTANT          DECIMAL(11, 2) NOT NULL
CCC004*    , PV_NEUR_NT_EG               DECIMAL(11, 2) NOT NULL
CCC004*    , PV_NEUR_NT_GR               DECIMAL(11, 2) NOT NULL
CCC004*    , PV_NEUR_NT_EG_UST           DECIMAL(11, 2) NOT NULL
CCC004*    , PV_NEUR_NT_GR_UST           DECIMAL(11, 2) NOT NULL
CCC004*    , EH_NEUR_ZU_EG               DECIMAL(10, 1) NOT NULL
CCC004*    , EH_NEUR_ZU_GR               DECIMAL(10, 1) NOT NULL
CCC004*    , EH_NEUR_AB_EG               DECIMAL(10, 1) NOT NULL
CCC004*    , EH_NEUR_AB_GR               DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBB058                           *
      *****************************************************************

       01  VIVBB058-GESAMT.
        05 NI-VIVBB058-TAB.
           10 NI-VIVBB058-ELE      PIC S9(04)  BINARY OCCURS  32.
        05 NI-VIVBB058             REDEFINES   NI-VIVBB058-TAB.
           10 NI-ST-NR                    PIC S9(04)       BINARY.
           10 NI-PSEV-MONAT               PIC S9(04)       BINARY.
           10 NI-KNR-PROD-SPARTE          PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KTY-PROD-SPARTE          PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-PSEV-EH-GS-ZU-EG         PIC S9(04)       BINARY.
           10 NI-PSEV-EH-GS-ZU-GR         PIC S9(04)       BINARY.
           10 NI-PSEV-EH-GS-AB-EG         PIC S9(04)       BINARY.
           10 NI-PSEV-EH-GS-AB-GR         PIC S9(04)       BINARY.
           10 NI-PSEV-PV-GS-NT-EG         PIC S9(04)       BINARY.
           10 NI-PSEV-PV-GS-NT-GR         PIC S9(04)       BINARY.
           10 NI-PSEV-PV-FG-NT-EG         PIC S9(04)       BINARY.
           10 NI-PSEV-PV-FG-NT-GR         PIC S9(04)       BINARY.
           10 NI-PSEV-EH-FG-ZU-EG         PIC S9(04)       BINARY.
           10 NI-PSEV-EH-FG-ZU-GR         PIC S9(04)       BINARY.
           10 NI-PSEV-EH-FG-AB-EG         PIC S9(04)       BINARY.
           10 NI-PSEV-EH-FG-AB-GR         PIC S9(04)       BINARY.
           10 NI-KTY-UST-KZ               PIC S9(04)       BINARY.
           10 NI-KNR-UST-KZ               PIC S9(04)       BINARY.
           10 NI-PV-GS-NT-EG-USTANT       PIC S9(04)       BINARY.
           10 NI-PV-GS-NT-GR-USTANT       PIC S9(04)       BINARY.
           10 NI-PV-FG-NT-EG-USTANT       PIC S9(04)       BINARY.
           10 NI-PV-FG-NT-GR-USTANT       PIC S9(04)       BINARY.
           10 NI-PV-NEUR-NT-EG            PIC S9(04)       BINARY.
           10 NI-PV-NEUR-NT-GR            PIC S9(04)       BINARY.
           10 NI-PV-NEUR-NT-EG-UST        PIC S9(04)       BINARY.
           10 NI-PV-NEUR-NT-GR-UST        PIC S9(04)       BINARY.
           10 NI-EH-NEUR-ZU-EG            PIC S9(04)       BINARY.
           10 NI-EH-NEUR-ZU-GR            PIC S9(04)       BINARY.
           10 NI-EH-NEUR-AB-EG            PIC S9(04)       BINARY.
           10 NI-EH-NEUR-AB-GR            PIC S9(04)       BINARY.

        05 DCL-VIVBB058.
           10 ST-NR                       PIC S9(09)       BINARY.
           10 PSEV-MONAT                  PIC  X(10).
           10 KNR-PROD-SPARTE             PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 KTY-PROD-SPARTE             PIC S9(04)       BINARY.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 PSEV-EH-GS-ZU-EG            PIC S9(09)V9(01) COMP-3.
           10 PSEV-EH-GS-ZU-GR            PIC S9(09)V9(01) COMP-3.
           10 PSEV-EH-GS-AB-EG            PIC S9(09)V9(01) COMP-3.
           10 PSEV-EH-GS-AB-GR            PIC S9(09)V9(01) COMP-3.
           10 PSEV-PV-GS-NT-EG            PIC S9(09)V9(02) COMP-3.
           10 PSEV-PV-GS-NT-GR            PIC S9(09)V9(02) COMP-3.
           10 PSEV-PV-FG-NT-EG            PIC S9(09)V9(02) COMP-3.
           10 PSEV-PV-FG-NT-GR            PIC S9(09)V9(02) COMP-3.
           10 PSEV-EH-FG-ZU-EG            PIC S9(09)V9(01) COMP-3.
           10 PSEV-EH-FG-ZU-GR            PIC S9(09)V9(01) COMP-3.
           10 PSEV-EH-FG-AB-EG            PIC S9(09)V9(01) COMP-3.
           10 PSEV-EH-FG-AB-GR            PIC S9(09)V9(01) COMP-3.
           10 KTY-UST-KZ                  PIC S9(04)       BINARY.
           10 KNR-UST-KZ                  PIC S9(04)       BINARY.
           10 PV-GS-NT-EG-USTANT          PIC S9(09)V9(02) COMP-3.
           10 PV-GS-NT-GR-USTANT          PIC S9(09)V9(02) COMP-3.
           10 PV-FG-NT-EG-USTANT          PIC S9(09)V9(02) COMP-3.
           10 PV-FG-NT-GR-USTANT          PIC S9(09)V9(02) COMP-3.
           10 PV-NEUR-NT-EG               PIC S9(09)V9(02) COMP-3.
           10 PV-NEUR-NT-GR               PIC S9(09)V9(02) COMP-3.
           10 PV-NEUR-NT-EG-UST           PIC S9(09)V9(02) COMP-3.
           10 PV-NEUR-NT-GR-UST           PIC S9(09)V9(02) COMP-3.
           10 EH-NEUR-ZU-EG               PIC S9(09)V9(01) COMP-3.
           10 EH-NEUR-ZU-GR               PIC S9(09)V9(01) COMP-3.
           10 EH-NEUR-AB-EG               PIC S9(09)V9(01) COMP-3.
           10 EH-NEUR-AB-GR               PIC S9(09)V9(01) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  32                    *
      *****************************************************************

