      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0065.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0065.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:03:56   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CTVB0065                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON TBVB0065 (GEEV)                                 * 00040001
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00060000
      *    DATUM:    12.09.1995                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    14.10.95 10:13:40
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0237!12.09.95!    ! NEU                       ! SCHÖLLH. *  00140000
      *         !14.10.95! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  00144000
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(TBVB0065)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0065 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      GEEV_MONAT                     DATE NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KTY_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      GEEV_EH_GS_ZU_EG               DECIMAL(10, 1) NOT NULL,
CCC004*      GEEV_EH_GS_ZU_GR               DECIMAL(10, 1) NOT NULL,
CCC004*      GEEV_EH_GS_AB_EG               DECIMAL(10, 1) NOT NULL,
CCC004*      GEEV_EH_GS_AB_GR               DECIMAL(10, 1) NOT NULL,
CCC004*      GEEV_PV_GS_NT_EG               DECIMAL(11, 2) NOT NULL,
CCC004*      GEEV_PV_GS_NT_GR               DECIMAL(11, 2) NOT NULL,
CCC004*      GEEV_PV_FG_NT_EG               DECIMAL(11, 2) NOT NULL,
CCC004*      GEEV_PV_FG_NT_GR               DECIMAL(11, 2) NOT NULL,
CCC004*      GEEV_EH_FG_ZU_EG               DECIMAL(10, 1) NOT NULL,
CCC004*      GEEV_EH_FG_ZU_GR               DECIMAL(10, 1) NOT NULL,
CCC004*      GEEV_EH_FG_AB_EG               DECIMAL(10, 1) NOT NULL,
CCC004*      GEEV_EH_FG_AB_GR               DECIMAL(10, 1) NOT NULL,
CCC004*      GEEV_PRV_RCK_ZFL               DECIMAL(11, 2) NOT NULL,
CCC004*      GEEV_RCK_AUSZ_Q                DECIMAL(11, 2) NOT NULL,
CCC004*      GEEV_UEBWEIS                   DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0065                           *
      ******************************************************************
       01  TBVB0065-GESAMT.
        05  NI-TBVB0065-TAB.
           10 NI-TBVB0065-ELE      PIC S9(04) COMP  OCCURS 19 TIMES.
        05  NI-TBVB0065            REDEFINES  NI-TBVB0065-TAB.
           10 NI-ST-NR             PIC S9(4) USAGE COMP.
           10 NI-GEEV-MONAT        PIC S9(4) USAGE COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(4) USAGE COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(4) USAGE COMP.
           10 NI-GEEV-EH-GS-ZU-EG  PIC S9(4) USAGE COMP.
           10 NI-GEEV-EH-GS-ZU-GR  PIC S9(4) USAGE COMP.
           10 NI-GEEV-EH-GS-AB-EG  PIC S9(4) USAGE COMP.
           10 NI-GEEV-EH-GS-AB-GR  PIC S9(4) USAGE COMP.
           10 NI-GEEV-PV-GS-NT-EG  PIC S9(4) USAGE COMP.
           10 NI-GEEV-PV-GS-NT-GR  PIC S9(4) USAGE COMP.
           10 NI-GEEV-PV-FG-NT-EG  PIC S9(4) USAGE COMP.
           10 NI-GEEV-PV-FG-NT-GR  PIC S9(4) USAGE COMP.
           10 NI-GEEV-EH-FG-ZU-EG  PIC S9(4) USAGE COMP.
           10 NI-GEEV-EH-FG-ZU-GR  PIC S9(4) USAGE COMP.
           10 NI-GEEV-EH-FG-AB-EG  PIC S9(4) USAGE COMP.
           10 NI-GEEV-EH-FG-AB-GR  PIC S9(4) USAGE COMP.
           10 NI-GEEV-PRV-RCK-ZFL  PIC S9(4) USAGE COMP.
           10 NI-GEEV-RCK-AUSZ-Q   PIC S9(4) USAGE COMP.
           10 NI-GEEV-UEBWEIS      PIC S9(4) USAGE COMP.
        05  DCL-TBVB0065.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 GEEV-MONAT           PIC X(10).
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 GEEV-EH-GS-ZU-EG     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-EH-GS-ZU-GR     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-EH-GS-AB-EG     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-EH-GS-AB-GR     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-PV-GS-NT-EG     PIC S999999999V99 USAGE COMP-3.
           10 GEEV-PV-GS-NT-GR     PIC S999999999V99 USAGE COMP-3.
           10 GEEV-PV-FG-NT-EG     PIC S999999999V99 USAGE COMP-3.
           10 GEEV-PV-FG-NT-GR     PIC S999999999V99 USAGE COMP-3.
           10 GEEV-EH-FG-ZU-EG     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-EH-FG-ZU-GR     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-EH-FG-AB-EG     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-EH-FG-AB-GR     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-PRV-RCK-ZFL     PIC S999999999V99 USAGE COMP-3.
           10 GEEV-RCK-AUSZ-Q      PIC S999999999V99 USAGE COMP-3.
           10 GEEV-UEBWEIS         PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
