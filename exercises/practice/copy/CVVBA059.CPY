      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA059.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVBA059.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:10:46   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CVVBA059                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON VIVBA059 (AST)                                  * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00060000
      *    DATUM:    12.09.1995                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    14.10.95 10:14:27
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0237!12.09.95!    ! NEU                       ! SCHÖLLH. *  00140000
      *         !14.10.95! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  00144001
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN VIEW (VIVBA059)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA059 TABLE
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
CCC004*      AST_RCK_STZ                    DECIMAL(5, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR VIEW  VIVBA059                           *
      ******************************************************************
       01  VIVBA059-GESAMT.
        05  NI-VIVBA059-TAB.
           10 NI-VIVBA059-ELE      PIC S9(04) COMP  OCCURS 13 TIMES.
        05  NI-VIVBA059            REDEFINES  NI-VIVBA059-TAB.
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
        05  DCL-VIVBA059.
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
