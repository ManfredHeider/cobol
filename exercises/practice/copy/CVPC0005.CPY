      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVPC0005.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\CVPC0005.CPv  $
      *
      *   Rev 2.1   23 Aug 2001 11:31:04   t13ae13
      *AVB00543: Riesterbereinigung
      *
      *   Rev 2.0   21 Aug 2001 12:54:04   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00543!22.08.01!    ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*
      * DCLGEN TABLE(VIPC0005
      *        LIBRARY(P130005.BP010000.SRCDCL(CVPC0005))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIPC0005 TABLE
CCC004*    ( MA_ST_NR                       INTEGER        NOT NULL,
CCC004*      BERP_MONAT                     DATE           NOT NULL,
CCC004*      BERP_NEU_STUFE                 SMALLINT       NOT NULL,
CCC004*      BERP_ZEITRAUM_STUF             SMALLINT       NOT NULL,
CCC004*      BERP_EH_GS_PRG_BAS             DECIMAL(10, 1) NOT NULL,
CCC004*      BERP_EH_GS_GRENZE              DECIMAL(10, 1) NOT NULL,
CCC004*      BERP_EH_GS_NT_GES              DECIMAL(10, 1) NOT NULL,
CCC004*      BERP_NEU_PRAXIS_ST             SMALLINT       NOT NULL,
CCC004*      BERP_EH_EG_PRG_BAS             DECIMAL(10, 1) NOT NULL,
CCC004*      BERP_EH_EG_GRENZE              DECIMAL(10, 1) NOT NULL,
CCC004*      BERP_EH_EG_12MON               DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AES.TBPC0005                    *
      ******************************************************************
       01  VIPC0005-GESAMT.
        05  NI-VIPC0005-TAB.
           10  NI-VIPC0005-ELE     PIC S9(04)  COMP   OCCURS  11 TIMES.
        05  NI-VIPC0005            REDEFINES   NI-VIPC0005-TAB.
           10 NI-MA-ST-NR                PIC S9(4) USAGE COMP.
           10 NI-BERP-MONAT              PIC S9(4) USAGE COMP.
           10 NI-BERP-NEU-STUFE          PIC S9(4) USAGE COMP.
           10 NI-BERP-ZEITRAUM-STUF      PIC S9(4) USAGE COMP.
           10 NI-BERP-EH-GS-PRG-BAS      PIC S9(4) USAGE COMP.
           10 NI-BERP-EH-GS-GRENZE       PIC S9(4) USAGE COMP.
           10 NI-BERP-EH-GS-NT-GES       PIC S9(4) USAGE COMP.
           10 NI-BERP-NEU-PRAXIS-ST      PIC S9(4) USAGE COMP.
           10 NI-BERP-EH-EG-PRG-BAS      PIC S9(4) USAGE COMP.
           10 NI-BERP-EH-EG-GRENZE       PIC S9(4) USAGE COMP.
           10 NI-BERP-EH-EG-12MON        PIC S9(4) USAGE COMP.

        05  DCL-VIPC0005.
           10 MA-ST-NR                   PIC S9(9)      USAGE COMP.
           10 BERP-MONAT                 PIC X(10).
           10 BERP-NEU-STUFE             PIC S9(4)      USAGE COMP.
           10 BERP-ZEITRAUM-STUF         PIC S9(4)      USAGE COMP.
           10 BERP-EH-GS-PRG-BAS         PIC S9(9)V9(1) USAGE COMP-3.
           10 BERP-EH-GS-GRENZE          PIC S9(9)V9(1) USAGE COMP-3.
           10 BERP-EH-GS-NT-GES          PIC S9(9)V9(1) USAGE COMP-3.
           10 BERP-NEU-PRAXIS-ST         PIC S9(4)      USAGE COMP.
           10 BERP-EH-EG-PRG-BAS         PIC S9(9)V9(1) USAGE COMP-3.
           10 BERP-EH-EG-GRENZE          PIC S9(9)V9(1) USAGE COMP-3.
           10 BERP-EH-EG-12MON           PIC S9(9)V9(1) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************

