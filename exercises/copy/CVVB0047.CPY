      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0047.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0047.CPv  $
      *
      *   Rev 2.1   Jun 25 2015 09:21:58   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - Änderung BVSA und
      * BVSP
      *
      *   Rev 2.0   Jun 18 2015 12:47:46   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: WALB                                             *
      *    DATUM:    18.06.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01332!18.06.15!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0047) BVSA                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0047)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0047 TABLE
CCC004*    ( VS_ZEITRAUM                 SMALLINT NOT NULL
CCC004*    , VB_NR                       INTEGER NOT NULL
CCC004*    , ERSTELL_DATUM               DATE NOT NULL
CCC004*    , STUFE_AKTUELL               SMALLINT
CCC004*    , STUFE_NEU                   SMALLINT
CCC004*    , STUFE_BEF_DAT               DATE
CCC004*    , IST_ZPKT_VON_NEU            DATE
CCC004*    , IST_ZPKT_BIS_NEU            DATE
CCC004*    , ZIEL_ZPKT_VON_NEU           DATE
CCC004*    , ZIEL_ZPKT_BIS_NEU           DATE
CCC004*    , IST_ZPKT_VON_VBP            DATE
CCC004*    , IST_ZPKT_BIS_VBP            DATE
CCC004*    , ZIEL_ZPKT_VON_VBP           DATE
CCC004*    , ZIEL_ZPKT_BIS_VBP           DATE
CCC004*    , EH_EIG_PZTR_NEU             DECIMAL(10, 1)
CCC004*    , EQF_EIG_NEU                 DECIMAL(05, 2)
CCC004*    , EH_GRU_PZTR_NEU             DECIMAL(10, 1)
CCC004*    , B_EH_GRU_PZTR_NEU           DECIMAL(10, 1)
CCC004*    , EQF_GRU_NEU                 DECIMAL(05, 2)
CCC004*    , EH_GES_PZTR_NEU             DECIMAL(10, 1)
CCC004*    , B_EH_GES_PZTR_NEU           DECIMAL(10, 1)
CCC004*    , EQF_GES_NEU                 DECIMAL(05, 2)
CCC004*    , EH_EG_MX_PZTR_NEU           DECIMAL(10, 1)
CCC004*    , EH_GES_VERBL_NEU            DECIMAL(10, 1)
CCC004*    , ANZ_PARTNER_NEU             SMALLINT
CCC004*    , HPT_ANZ_PARTN_NEU           SMALLINT
CCC004*    , ALP_ANZ_VM_OD_NEU           SMALLINT
CCC004*    , VBP_AKTUELL                 SMALLINT
CCC004*    , VBP_VORHER                  SMALLINT
CCC004*    , VBP_BEF_DAT                 DATE
CCC004*    , BEF_DAT_EH_EG_VBP           DATE
CCC004*    , VBP_NEU                     SMALLINT
CCC004*    , EH_EIG_PZTR_VBP             DECIMAL(10, 1)
CCC004*    , WEIT_EH_EIG_VBP             DECIMAL(10, 1)
CCC004*    , EQF_EIG_VBP                 DECIMAL(05, 2)
CCC004*    , ANZ_HB_PARTN_ERS            SMALLINT
CCC004*    , ANZPART_AB_RGS              SMALLINT
CCC004*    , EH_GS_NT_GES_NEU            DECIMAL(10, 1)
CCC004*    , VBP_EH_ERSTM_KZ             CHAR(01)
CCC004*    , EH_EIG_PZTR_11              DECIMAL(10, 1)
CCC004*    , EQF_EIG_11                  DECIMAL(05, 2)
CCC004*    , EH_GES_PZTR_11              DECIMAL(10, 1)
CCC004*    , B_EH_GES_PZTR_11            DECIMAL(10, 1)
CCC004*    , EQF_GES_11                  DECIMAL(05, 2)
CCC004*    , EH_EG_MX_PZTR_11            DECIMAL(10, 1)
CCC004*    , EH_GES_VERBL_11             DECIMAL(10, 1)
CCC004*    , EH_NEU_EIG_NP               DECIMAL(10, 1)
CCC004*    , B_EH_NEU_EIG_NP             DECIMAL(10, 1)
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0047                           *
      *****************************************************************

       01  VIVB0047-GESAMT.
        05 NI-VIVB0047-TAB.
           10 NI-VIVB0047-ELE      PIC S9(04)  BINARY OCCURS  48.
        05 NI-VIVB0047             REDEFINES   NI-VIVB0047-TAB.
           10 NI-VS-ZEITRAUM              PIC S9(04)       BINARY.
           10 NI-VB-NR                    PIC S9(04)       BINARY.
           10 NI-ERSTELL-DATUM            PIC S9(04)       BINARY.
           10 NI-STUFE-AKTUELL            PIC S9(04)       BINARY.
           10 NI-STUFE-NEU                PIC S9(04)       BINARY.
           10 NI-STUFE-BEF-DAT            PIC S9(04)       BINARY.
           10 NI-IST-ZPKT-VON-NEU         PIC S9(04)       BINARY.
           10 NI-IST-ZPKT-BIS-NEU         PIC S9(04)       BINARY.
           10 NI-ZIEL-ZPKT-VON-NEU        PIC S9(04)       BINARY.
           10 NI-ZIEL-ZPKT-BIS-NEU        PIC S9(04)       BINARY.
           10 NI-IST-ZPKT-VON-VBP         PIC S9(04)       BINARY.
           10 NI-IST-ZPKT-BIS-VBP         PIC S9(04)       BINARY.
           10 NI-ZIEL-ZPKT-VON-VBP        PIC S9(04)       BINARY.
           10 NI-ZIEL-ZPKT-BIS-VBP        PIC S9(04)       BINARY.
           10 NI-EH-EIG-PZTR-NEU          PIC S9(04)       BINARY.
           10 NI-EQF-EIG-NEU              PIC S9(04)       BINARY.
           10 NI-EH-GRU-PZTR-NEU          PIC S9(04)       BINARY.
           10 NI-B-EH-GRU-PZTR-NEU        PIC S9(04)       BINARY.
           10 NI-EQF-GRU-NEU              PIC S9(04)       BINARY.
           10 NI-EH-GES-PZTR-NEU          PIC S9(04)       BINARY.
           10 NI-B-EH-GES-PZTR-NEU        PIC S9(04)       BINARY.
           10 NI-EQF-GES-NEU              PIC S9(04)       BINARY.
           10 NI-EH-EG-MX-PZTR-NEU        PIC S9(04)       BINARY.
           10 NI-EH-GES-VERBL-NEU         PIC S9(04)       BINARY.
           10 NI-ANZ-PARTNER-NEU          PIC S9(04)       BINARY.
           10 NI-HPT-ANZ-PARTN-NEU        PIC S9(04)       BINARY.
           10 NI-ALP-ANZ-VM-OD-NEU        PIC S9(04)       BINARY.
           10 NI-VBP-AKTUELL              PIC S9(04)       BINARY.
           10 NI-VBP-VORHER               PIC S9(04)       BINARY.
           10 NI-VBP-BEF-DAT              PIC S9(04)       BINARY.
           10 NI-BEF-DAT-EH-EG-VBP        PIC S9(04)       BINARY.
           10 NI-VBP-NEU                  PIC S9(04)       BINARY.
           10 NI-EH-EIG-PZTR-VBP          PIC S9(04)       BINARY.
           10 NI-WEIT-EH-EIG-VBP          PIC S9(04)       BINARY.
           10 NI-EQF-EIG-VBP              PIC S9(04)       BINARY.
           10 NI-ANZ-HB-PARTN-ERS         PIC S9(04)       BINARY.
           10 NI-ANZPART-AB-RGS           PIC S9(04)       BINARY.
           10 NI-EH-GS-NT-GES-NEU         PIC S9(04)       BINARY.
           10 NI-VBP-EH-ERSTM-KZ          PIC S9(04)       BINARY.
           10 NI-EH-EIG-PZTR-11           PIC S9(04)       BINARY.
           10 NI-EQF-EIG-11               PIC S9(04)       BINARY.
           10 NI-EH-GES-PZTR-11           PIC S9(04)       BINARY.
           10 NI-B-EH-GES-PZTR-11         PIC S9(04)       BINARY.
           10 NI-EQF-GES-11               PIC S9(04)       BINARY.
           10 NI-EH-EG-MX-PZTR-11         PIC S9(04)       BINARY.
           10 NI-EH-GES-VERBL-11          PIC S9(04)       BINARY.
           10 NI-EH-NEU-EIG-NP            PIC S9(04)       BINARY.
           10 NI-B-EH-NEU-EIG-NP          PIC S9(04)       BINARY.

        05 DCL-VIVB0047.
           10 VS-ZEITRAUM                 PIC S9(04)       BINARY.
           10 VB-NR                       PIC S9(09)       BINARY.
           10 ERSTELL-DATUM               PIC  X(10).
           10 STUFE-AKTUELL               PIC S9(04)       BINARY.
           10 STUFE-NEU                   PIC S9(04)       BINARY.
           10 STUFE-BEF-DAT               PIC  X(10).
           10 IST-ZPKT-VON-NEU            PIC  X(10).
           10 IST-ZPKT-BIS-NEU            PIC  X(10).
           10 ZIEL-ZPKT-VON-NEU           PIC  X(10).
           10 ZIEL-ZPKT-BIS-NEU           PIC  X(10).
           10 IST-ZPKT-VON-VBP            PIC  X(10).
           10 IST-ZPKT-BIS-VBP            PIC  X(10).
           10 ZIEL-ZPKT-VON-VBP           PIC  X(10).
           10 ZIEL-ZPKT-BIS-VBP           PIC  X(10).
           10 EH-EIG-PZTR-NEU             PIC S9(09)V9(01) COMP-3.
           10 EQF-EIG-NEU                 PIC S9(03)V9(02) COMP-3.
           10 EH-GRU-PZTR-NEU             PIC S9(09)V9(01) COMP-3.
           10 B-EH-GRU-PZTR-NEU           PIC S9(09)V9(01) COMP-3.
           10 EQF-GRU-NEU                 PIC S9(03)V9(02) COMP-3.
           10 EH-GES-PZTR-NEU             PIC S9(09)V9(01) COMP-3.
           10 B-EH-GES-PZTR-NEU           PIC S9(09)V9(01) COMP-3.
           10 EQF-GES-NEU                 PIC S9(03)V9(02) COMP-3.
           10 EH-EG-MX-PZTR-NEU           PIC S9(09)V9(01) COMP-3.
           10 EH-GES-VERBL-NEU            PIC S9(09)V9(01) COMP-3.
           10 ANZ-PARTNER-NEU             PIC S9(04)       BINARY.
           10 HPT-ANZ-PARTN-NEU           PIC S9(04)       BINARY.
           10 ALP-ANZ-VM-OD-NEU           PIC S9(04)       BINARY.
           10 VBP-AKTUELL                 PIC S9(04)       BINARY.
           10 VBP-VORHER                  PIC S9(04)       BINARY.
           10 VBP-BEF-DAT                 PIC  X(10).
           10 BEF-DAT-EH-EG-VBP           PIC  X(10).
           10 VBP-NEU                     PIC S9(04)       BINARY.
           10 EH-EIG-PZTR-VBP             PIC S9(09)V9(01) COMP-3.
           10 WEIT-EH-EIG-VBP             PIC S9(09)V9(01) COMP-3.
           10 EQF-EIG-VBP                 PIC S9(03)V9(02) COMP-3.
           10 ANZ-HB-PARTN-ERS            PIC S9(04)       BINARY.
           10 ANZPART-AB-RGS              PIC S9(04)       BINARY.
           10 EH-GS-NT-GES-NEU            PIC S9(09)V9(01) COMP-3.
           10 VBP-EH-ERSTM-KZ             PIC  X(01).
           10 EH-EIG-PZTR-11              PIC S9(09)V9(01) COMP-3.
           10 EQF-EIG-11                  PIC S9(03)V9(02) COMP-3.
           10 EH-GES-PZTR-11              PIC S9(09)V9(01) COMP-3.
           10 B-EH-GES-PZTR-11            PIC S9(09)V9(01) COMP-3.
           10 EQF-GES-11                  PIC S9(03)V9(02) COMP-3.
           10 EH-EG-MX-PZTR-11            PIC S9(09)V9(01) COMP-3.
           10 EH-GES-VERBL-11             PIC S9(09)V9(01) COMP-3.
           10 EH-NEU-EIG-NP               PIC S9(09)V9(01) COMP-3.
           10 B-EH-NEU-EIG-NP             PIC S9(09)V9(01) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  48                    *
      *****************************************************************

