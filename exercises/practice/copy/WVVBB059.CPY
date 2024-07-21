      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.10.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB059.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB059.CPv  $
      *
      *   Rev 2.1   Nov 17 2015 11:44:26   t13ae30
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - Laden St
      *atistikdaten -TWI
      *
      *   Rev 2.0   Oct 26 2015 08:05:58   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB059                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    26.10.2015                                       *
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
      * COBOL DECLARATION FOR VIEW  VIVBB059                           *
      ******************************************************************
        05  WB-NI-VIVBB059-TAB.
           10 WB-NI-VIVBB059-ELE      PIC S9(04) COMP  OCCURS 15 TIMES.
        05  WB-NI-VIVBB059            REDEFINES  WB-NI-VIVBB059-TAB.
           10 WB-NI-ST-NR             PIC S9(4) USAGE COMP.
           10 WB-NI-AST-MONAT         PIC S9(4) USAGE COMP.
           10 WB-NI-AST-JEQF-EG       PIC S9(4) USAGE COMP.
           10 WB-NI-AST-JEQF-GR       PIC S9(4) USAGE COMP.
           10 WB-NI-AST-JEQF-GS       PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-AB-78-EG   PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-AB-78-GR   PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-BIS-78-GS  PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-VM-ZU-EG   PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-VM-ZU-GR   PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-VM-AB-EG   PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-VM-AB-GR   PIC S9(4) USAGE COMP.
           10 WB-NI-AST-RCK-STZ       PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-UST-KZ        PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-UST-KZ        PIC S9(4) USAGE COMP.
        05  WB-VIVBB059.
           10 WB-ST-NR                PIC S9(9) USAGE COMP.
           10 WB-AST-MONAT            PIC X(10).
           10 WB-AST-JEQF-EG          PIC S999V99 USAGE COMP-3.
           10 WB-AST-JEQF-GR          PIC S999V99 USAGE COMP-3.
           10 WB-AST-JEQF-GS          PIC S999V99 USAGE COMP-3.
           10 WB-AST-EH-AB-78-EG      PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-AB-78-GR      PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-BIS-78-GS     PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-VM-ZU-EG      PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-VM-ZU-GR      PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-VM-AB-EG      PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-VM-AB-GR      PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-RCK-STZ          PIC S999V99 USAGE COMP-3.
           10 WB-KTY-UST-KZ           PIC S9(4) USAGE COMP.
           10 WB-KNR-UST-KZ           PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************

