      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.10.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA058.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA058.CPv  $
      *
      *   Rev 2.1   Nov 17 2015 11:43:58   t13ae30
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - Laden St
      *atistikdaten -TWI
      *
      *   Rev 2.0   Oct 26 2015 08:04:32   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVBA058                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON VIVB0058 (PSEV)                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    26.10.2015                                       *
      *****************************************************************
      *    VERSION   1    29.10.94 10:23:29
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01347!26.10.15! 01 ! NEU                       ! TH.WILLIG*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVBA058)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA058 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      PSEV_MONAT                     DATE NOT NULL,
CCC004*      KNR_PROD_SPARTE                SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KTY_PROD_SPARTE                SMALLINT NOT NULL,
CCC004*      KTY_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      PSEV_EH_GS_ZU_EG               DECIMAL(10, 1) NOT NULL,
CCC004*      PSEV_EH_GS_ZU_GR               DECIMAL(10, 1) NOT NULL,
CCC004*      PSEV_EH_GS_AB_EG               DECIMAL(10, 1) NOT NULL,
CCC004*      PSEV_EH_GS_AB_GR               DECIMAL(10, 1) NOT NULL,
CCC004*      PSEV_PV_GS_NT_EG               DECIMAL(11, 2) NOT NULL,
CCC004*      PSEV_PV_GS_NT_GR               DECIMAL(11, 2) NOT NULL,
CCC004*      PSEV_PV_FG_NT_EG               DECIMAL(11, 2) NOT NULL,
CCC004*      PSEV_PV_FG_NT_GR               DECIMAL(11, 2) NOT NULL,
CCC004*      PSEV_EH_FG_ZU_EG               DECIMAL(10, 1) NOT NULL,
CCC004*      PSEV_EH_FG_ZU_GR               DECIMAL(10, 1) NOT NULL,
CCC004*      PSEV_EH_FG_AB_EG               DECIMAL(10, 1) NOT NULL,
CCC004*      PSEV_EH_FG_AB_GR               DECIMAL(10, 1) NOT NULL,
CCC004*      KTY_UST_KZ                     SMALLINT NOT NULL,
CCC004*      KNR_UST_KZ                     SMALLINT NOT NULL,
CCC004*      PV_GS_NT_EG_USTANT             DECIMAL(11, 2) NOT NULL,
CCC004*      PV_GS_NT_GR_USTANT             DECIMAL(11, 2) NOT NULL,
CCC004*      PV_FG_NT_EG_USTANT             DECIMAL(11, 2) NOT NULL,
CCC004*      PV_FG_NT_GR_USTANT             DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0058                           *
      ******************************************************************
       01  VIVBA058-GESAMT.
        05  NI-VIVBA058-TAB.
           10 NI-VIVBA058-ELE        PIC S9(04) COMP  OCCURS 24 TIMES.
        05  NI-VIVBA058              REDEFINES  NI-VIVBA058-TAB.
           10 NI-ST-NR               PIC S9(4) USAGE COMP.
           10 NI-PSEV-MONAT          PIC S9(4) USAGE COMP.
           10 NI-KNR-PROD-SPARTE     PIC S9(4) USAGE COMP.
           10 NI-KNR-WAEHRUNG        PIC S9(4) USAGE COMP.
           10 NI-KTY-PROD-SPARTE     PIC S9(4) USAGE COMP.
           10 NI-KTY-WAEHRUNG        PIC S9(4) USAGE COMP.
           10 NI-PSEV-EH-GS-ZU-EG    PIC S9(4) USAGE COMP.
           10 NI-PSEV-EH-GS-ZU-GR    PIC S9(4) USAGE COMP.
           10 NI-PSEV-EH-GS-AB-EG    PIC S9(4) USAGE COMP.
           10 NI-PSEV-EH-GS-AB-GR    PIC S9(4) USAGE COMP.
           10 NI-PSEV-PV-GS-NT-EG    PIC S9(4) USAGE COMP.
           10 NI-PSEV-PV-GS-NT-GR    PIC S9(4) USAGE COMP.
           10 NI-PSEV-PV-FG-NT-EG    PIC S9(4) USAGE COMP.
           10 NI-PSEV-PV-FG-NT-GR    PIC S9(4) USAGE COMP.
           10 NI-PSEV-EH-FG-ZU-EG    PIC S9(4) USAGE COMP.
           10 NI-PSEV-EH-FG-ZU-GR    PIC S9(4) USAGE COMP.
           10 NI-PSEV-EH-FG-AB-EG    PIC S9(4) USAGE COMP.
           10 NI-PSEV-EH-FG-AB-GR    PIC S9(4) USAGE COMP.
           10 NI-KTY-UST-KZ          PIC S9(4) USAGE COMP.
           10 NI-KNR-UST-KZ          PIC S9(4) USAGE COMP.
           10 NI-PV-GS-NT-EG-USTANT  PIC S9(4) USAGE COMP.
           10 NI-PV-GS-NT-GR-USTANT  PIC S9(4) USAGE COMP.
           10 NI-PV-FG-NT-EG-USTANT  PIC S9(4) USAGE COMP.
           10 NI-PV-FG-NT-GR-USTANT  PIC S9(4) USAGE COMP.

        05  DCL-VIVBA058.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 PSEV-MONAT           PIC X(10).
           10 KNR-PROD-SPARTE      PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTY-PROD-SPARTE      PIC S9(4) USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 PSEV-EH-GS-ZU-EG     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-EH-GS-ZU-GR     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-EH-GS-AB-EG     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-EH-GS-AB-GR     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-PV-GS-NT-EG     PIC S999999999V99 USAGE COMP-3.
           10 PSEV-PV-GS-NT-GR     PIC S999999999V99 USAGE COMP-3.
           10 PSEV-PV-FG-NT-EG     PIC S999999999V99 USAGE COMP-3.
           10 PSEV-PV-FG-NT-GR     PIC S999999999V99 USAGE COMP-3.
           10 PSEV-EH-FG-ZU-EG     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-EH-FG-ZU-GR     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-EH-FG-AB-EG     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-EH-FG-AB-GR     PIC S999999999V9 USAGE COMP-3.
           10 KTY-UST-KZ           PIC S9(4) USAGE COMP.
           10 KNR-UST-KZ           PIC S9(4) USAGE COMP.
           10 PV-GS-NT-EG-USTANT   PIC S999999999V99 USAGE COMP-3.
           10 PV-GS-NT-GR-USTANT   PIC S999999999V99 USAGE COMP-3.
           10 PV-FG-NT-EG-USTANT   PIC S999999999V99 USAGE COMP-3.
           10 PV-FG-NT-GR-USTANT   PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************

