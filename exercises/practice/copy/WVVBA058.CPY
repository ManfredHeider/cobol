      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.10.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA058.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA058.CPv  $
      *
      *   Rev 2.1   Nov 17 2015 11:44:08   t13ae30
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - Laden St
      *atistikdaten -TWI
      *
      *   Rev 2.0   Oct 26 2015 08:05:44   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA058                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    26.10.2015                                       *
      *    --------------------------------------------------------   *
      *
      *****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA058                           *
      ******************************************************************
        05  WB-NI-VIVBA058-TAB.
           10 WB-NI-VIVBA058-ELE     PIC S9(04) COMP  OCCURS 24 TIMES.
        05  WB-NI-VIVB0058         REDEFINES  WB-NI-VIVBA058-TAB.
           10 WB-NI-ST-NR            PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-MONAT       PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-PROD-SPARTE  PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-WAEHRUNG     PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-PROD-SPARTE  PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-WAEHRUNG     PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-GS-ZU-EG PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-GS-ZU-GR PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-GS-AB-EG PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-GS-AB-GR PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-PV-GS-NT-EG PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-PV-GS-NT-GR PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-PV-FG-NT-EG PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-PV-FG-NT-GR PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-FG-ZU-EG PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-FG-ZU-GR PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-FG-AB-EG PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-FG-AB-GR PIC S9(4) USAGE COMP.
           10 WB-KTY-UST-KZ          PIC S9(4) USAGE COMP.
           10 WB-KNR-UST-KZ          PIC S9(4) USAGE COMP.
           10 WB-PV-GS-NT-EG-USTANT  PIC S9(4) USAGE COMP.
           10 WB-PV-GS-NT-GR-USTANT  PIC S9(4) USAGE COMP.
           10 WB-PV-FG-NT-EG-USTANT  PIC S9(4) USAGE COMP.
           10 WB-PV-FG-NT-GR-USTANT  PIC S9(4) USAGE COMP.
        05  WB-VIVBA058.
           10 WB-ST-NR               PIC S9(9) USAGE COMP.
           10 WB-PSEV-MONAT          PIC X(10).
           10 WB-KNR-PROD-SPARTE     PIC S9(4) USAGE COMP.
           10 WB-KNR-WAEHRUNG        PIC S9(4) USAGE COMP.
           10 WB-KTY-PROD-SPARTE     PIC S9(4) USAGE COMP.
           10 WB-KTY-WAEHRUNG        PIC S9(4) USAGE COMP.
           10 WB-PSEV-EH-GS-ZU-EG    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-EH-GS-ZU-GR    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-EH-GS-AB-EG    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-EH-GS-AB-GR    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-PV-GS-NT-EG    PIC S999999999V99 USAGE COMP-3.
           10 WB-PSEV-PV-GS-NT-GR    PIC S999999999V99 USAGE COMP-3.
           10 WB-PSEV-PV-FG-NT-EG    PIC S999999999V99 USAGE COMP-3.
           10 WB-PSEV-PV-FG-NT-GR    PIC S999999999V99 USAGE COMP-3.
           10 WB-PSEV-EH-FG-ZU-EG    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-EH-FG-ZU-GR    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-EH-FG-AB-EG    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-EH-FG-AB-GR    PIC S999999999V9 USAGE COMP-3.
           10 WB-KTY-UST-KZ          PIC S9(4) USAGE COMP.
           10 WB-KNR-UST-KZ          PIC S9(4) USAGE COMP.
           10 WB-PV-GS-NT-EG-USTANT  PIC S999999999V99 USAGE COMP-3.
           10 WB-PV-GS-NT-GR-USTANT  PIC S999999999V99 USAGE COMP-3.
           10 WB-PV-FG-NT-EG-USTANT  PIC S999999999V99 USAGE COMP-3.
           10 WB-PV-FG-NT-GR-USTANT  PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************

