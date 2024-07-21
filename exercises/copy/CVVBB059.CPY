      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.10.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB059.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB059.CPv  $
      *
      *   Rev 2.1   Nov 17 2015 11:44:16   t13ae30
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - Laden St
      *atistikdaten -TWI
      *
      *   Rev 2.0   Oct 26 2015 08:05:50   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVBB059                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON VIVBB059 (AST)                                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    26.10.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01347!26.10.15! 01 ! NEU                       ! TH.WILLIG*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN VIEW (VIVBB059)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBB059 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      AST_MONAT                      DATE NOT NULL,
CCC004*      AST_JEQF_EG                    DECIMAL(5, 2) NOT NULL,
CCC004*      AST_JEQF_GR                    DECIMAL(5, 2) NOT NULL,
CCC004*      AST_JEQF_GS                    DECIMAL(5, 2) NOT NULL,
CCC004*      AST_EH_AB_78_EG                DECIMAL(10, 1) NOT NULL,
CCC004*      AST_EH_AB_78_GR                DECIMAL(10, 1) NOT NULL,
CCC004*      AST_EH_BIS_78_GS               DECIMAL(10, 1) NOT NULL,
CCC004*      AST_EH_VM_ZU_EG                DECIMAL(10, 1) NOT NULL,
CCC004*      AST_EH_VM_ZU_GR                DECIMAL(10, 1) NOT NULL,
CCC004*      AST_EH_VM_AB_EG                DECIMAL(10, 1) NOT NULL,
CCC004*      AST_EH_VM_AB_GR                DECIMAL(10, 1) NOT NULL,
CCC004*      AST_RCK_STZ                    DECIMAL(5, 2) NOT NULL,
CCC004*      KTY_UST_KZ                     SMALLINT NOT NULL,
CCC004*      KNR_UST_KZ                     SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR VIEW  VIVBA059                           *
      ******************************************************************
       01  VIVBB059-GESAMT.
        05  NI-VIVBB059-TAB.
           10 NI-VIVBB059-ELE      PIC S9(04) COMP  OCCURS 15 TIMES.
        05  NI-VIVBB059            REDEFINES  NI-VIVBB059-TAB.
           10 NI-ST-NR             PIC S9(4) USAGE COMP.
           10 NI-AST-MONAT         PIC S9(4) USAGE COMP.
           10 NI-AST-JEQF-EG       PIC S9(4) USAGE COMP.
           10 NI-AST-JEQF-GR       PIC S9(4) USAGE COMP.
           10 NI-AST-JEQF-GS       PIC S9(4) USAGE COMP.
           10 NI-AST-EH-AB-78-EG   PIC S9(4) USAGE COMP.
           10 NI-AST-EH-AB-78-GR   PIC S9(4) USAGE COMP.
           10 NI-AST-EH-BIS-78-GS  PIC S9(4) USAGE COMP.
           10 NI-AST-EH-VM-ZU-EG   PIC S9(4) USAGE COMP.
           10 NI-AST-EH-VM-ZU-GR   PIC S9(4) USAGE COMP.
           10 NI-AST-EH-VM-AB-EG   PIC S9(4) USAGE COMP.
           10 NI-AST-EH-VM-AB-GR   PIC S9(4) USAGE COMP.
           10 NI-AST-RCK-STZ       PIC S9(4) USAGE COMP.
           10 NI-KTY-UST-KZ        PIC S9(4) USAGE COMP.
           10 NI-KNR-UST-KZ        PIC S9(4) USAGE COMP.
        05  DCL-VIVBB059.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 AST-MONAT            PIC X(10).
           10 AST-JEQF-EG          PIC S999V99 USAGE COMP-3.
           10 AST-JEQF-GR          PIC S999V99 USAGE COMP-3.
           10 AST-JEQF-GS          PIC S999V99 USAGE COMP-3.
           10 AST-EH-AB-78-EG      PIC S999999999V9 USAGE COMP-3.
           10 AST-EH-AB-78-GR      PIC S999999999V9 USAGE COMP-3.
           10 AST-EH-BIS-78-GS     PIC S999999999V9 USAGE COMP-3.
           10 AST-EH-VM-ZU-EG      PIC S999999999V9 USAGE COMP-3.
           10 AST-EH-VM-ZU-GR      PIC S999999999V9 USAGE COMP-3.
           10 AST-EH-VM-AB-EG      PIC S999999999V9 USAGE COMP-3.
           10 AST-EH-VM-AB-GR      PIC S999999999V9 USAGE COMP-3.
           10 AST-RCK-STZ          PIC S999V99 USAGE COMP-3.
           10 KTY-UST-KZ           PIC S9(4) USAGE COMP.
           10 KNR-UST-KZ           PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************

