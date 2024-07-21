      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0059.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0059.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:03:42   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CTVB0059                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON TBVB0059 (AST)                                  * 00040001
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: KUHN                                             * 00060000
      *    DATUM:    15.09.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   2    14.10.95 10:13:17
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0142!15.09.94! 01 ! NEU                       ! KUHN     *  00140000
      * AFVB0237!12.09.95!    ! ENTFÄLLT: FOLGEEINHEITEN, ! SCHÖLLH. *  00143000
      *         !        !    !  PRV-RCK-ZFL; NEU: RCK-STZ!          *  00144000
      *         !14.10.95! 02 ! PROD.UEBERGABE            ! SCHÖLLH. *  00144000
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(TBVB0059)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0059 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      AST_MONAT                      DATE NOT NULL,
CCC004*      AST_JEQF_EG                    DECIMAL(5, 2) NOT NULL,
CCC004*      AST_JEQF_GR                    DECIMAL(5, 2) NOT NULL,
CCC004*      AST_JEQF_GS                    DECIMAL(5, 2) NOT NULL,
CCC004*      AST_EH_AB_78_EG                DECIMAL(10, 1) NOT NULL,
CCC004*      AST_EH_AB_78_GR                DECIMAL(10, 1) NOT NULL,
CCC004*      AST_EH_BIS_78_GS               DECIMAL(10, 1) NOT NULL,
120995*       AST_EH_FG_ZU_EG                DECIMAL(10, 1) NOT NULL,
120995*       AST_EH_FG_ZU_GR                DECIMAL(10, 1) NOT NULL,
120995*       AST_EH_FG_AB_EG                DECIMAL(10, 1) NOT NULL,
120995*       AST_EH_FG_AB_GR                DECIMAL(10, 1) NOT NULL,
CCC004*      AST_EH_VM_ZU_EG                DECIMAL(10, 1) NOT NULL,
CCC004*      AST_EH_VM_ZU_GR                DECIMAL(10, 1) NOT NULL,
CCC004*      AST_EH_VM_AB_EG                DECIMAL(10, 1) NOT NULL,
CCC004*      AST_EH_VM_AB_GR                DECIMAL(10, 1) NOT NULL,
120995*       AST_PROV_RCK_ZFL               DECIMAL(11, 2) NOT NULL
CCC004*      AST_RCK_STZ                    DECIMAL(5, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0059                           *
      ******************************************************************
       01  TBVB0059-GESAMT.
        05  NI-TBVB0059-TAB.
120995*     10 NI-TBVB0059-ELE      PIC S9(04) COMP  OCCURS 17 TIMES.
120995     10 NI-TBVB0059-ELE      PIC S9(04) COMP  OCCURS 13 TIMES.
        05  NI-TBVB0059            REDEFINES  NI-TBVB0059-TAB.
           10 NI-ST-NR             PIC S9(4) USAGE COMP.
           10 NI-AST-MONAT         PIC S9(4) USAGE COMP.
           10 NI-AST-JEQF-EG       PIC S9(4) USAGE COMP.
           10 NI-AST-JEQF-GR       PIC S9(4) USAGE COMP.
           10 NI-AST-JEQF-GS       PIC S9(4) USAGE COMP.
           10 NI-AST-EH-AB-78-EG   PIC S9(4) USAGE COMP.
           10 NI-AST-EH-AB-78-GR   PIC S9(4) USAGE COMP.
           10 NI-AST-EH-BIS-78-GS  PIC S9(4) USAGE COMP.
120995*     10 NI-AST-EH-FG-ZU-EG   PIC S9(4) USAGE COMP.
120995*     10 NI-AST-EH-FG-ZU-GR   PIC S9(4) USAGE COMP.
120995*     10 NI-AST-EH-FG-AB-EG   PIC S9(4) USAGE COMP.
120995*     10 NI-AST-EH-FG-AB-GR   PIC S9(4) USAGE COMP.
           10 NI-AST-EH-VM-ZU-EG   PIC S9(4) USAGE COMP.
           10 NI-AST-EH-VM-ZU-GR   PIC S9(4) USAGE COMP.
           10 NI-AST-EH-VM-AB-EG   PIC S9(4) USAGE COMP.
           10 NI-AST-EH-VM-AB-GR   PIC S9(4) USAGE COMP.
120995*     10 NI-AST-PROV-RCK-ZFL  PIC S9(4) USAGE COMP.
120995     10 NI-AST-RCK-STZ       PIC S9(4) USAGE COMP.
        05  DCL-TBVB0059.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 AST-MONAT            PIC X(10).
           10 AST-JEQF-EG          PIC S999V99 USAGE COMP-3.
           10 AST-JEQF-GR          PIC S999V99 USAGE COMP-3.
           10 AST-JEQF-GS          PIC S999V99 USAGE COMP-3.
           10 AST-EH-AB-78-EG      PIC S999999999V9 USAGE COMP-3.
           10 AST-EH-AB-78-GR      PIC S999999999V9 USAGE COMP-3.
           10 AST-EH-BIS-78-GS     PIC S999999999V9 USAGE COMP-3.
120995*     10 AST-EH-FG-ZU-EG      PIC S999999999V9 USAGE COMP-3.
120995*     10 AST-EH-FG-ZU-GR      PIC S999999999V9 USAGE COMP-3.
120995*     10 AST-EH-FG-AB-EG      PIC S999999999V9 USAGE COMP-3.
120995*     10 AST-EH-FG-AB-GR      PIC S999999999V9 USAGE COMP-3.
           10 AST-EH-VM-ZU-EG      PIC S999999999V9 USAGE COMP-3.
           10 AST-EH-VM-ZU-GR      PIC S999999999V9 USAGE COMP-3.
           10 AST-EH-VM-AB-EG      PIC S999999999V9 USAGE COMP-3.
           10 AST-EH-VM-AB-GR      PIC S999999999V9 USAGE COMP-3.
120995*     10 AST-PROV-RCK-ZFL     PIC S999999999V99 USAGE COMP-3.
120995     10 AST-RCK-STZ          PIC S999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
