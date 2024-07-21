      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVPC0003.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\CVPC0003.CPv  $
      *
      *   Rev 2.1   29 Sep 2000 11:47:34   T13AE28
      *AVB00490: Einrichten neue Tabelle BEIN
      *
      *   Rev 2.0   28 Sep 2000 12:16:44   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00490!28.09.00!    ! NEU                       ! d.ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*
      * DCLGEN TABLE(VIPC0003
      *        LIBRARY(P130005.BP010000.SRCDCL(CVPC0003))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIPC0003 TABLE
CCC004*    ( MA_ST_NR                       INTEGER        NOT NULL,
CCC004*      BEIN_MONAT                     DATE           NOT NULL,
CCC004*      BEIN_NEU_STUFE                 SMALLINT       NOT NULL,
CCC004*      BEIN_ZEITRAUM_STUF             SMALLINT       NOT NULL,
CCC004*      BEIN_EH_GS_INV_BAS             DECIMAL(10, 1) NOT NULL,
CCC004*      BEIN_EH_GS_INV_GES             DECIMAL(10, 1) NOT NULL,
CCC004*      BEIN_EH_GS_GRENZE              DECIMAL(10, 1) NOT NULL,
CCC004*      BEIN_EH_GS_NT_GES              DECIMAL(10, 1) NOT NULL,
CCC004*      BEIN_NEU_PRAXIS_ST             SMALLINT       NOT NULL,
CCC004*      BEIN_EH_EG_INV_BAS             DECIMAL(10, 1) NOT NULL,
CCC004*      BEIN_EH_EG_INV_GES             DECIMAL(10, 1) NOT NULL,
CCC004*      BEIN_EH_EG_GRENZE              DECIMAL(10, 1) NOT NULL,
CCC004*      BEIN_EH_EG_12MON               DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AES.TBPC0003                    *
      ******************************************************************
       01  VIPC0003-GESAMT.
        05  NI-VIPC0003-TAB.
           10  NI-VIPC0003-ELE     PIC S9(04)  COMP   OCCURS  13 TIMES.
        05  NI-VIPC0003            REDEFINES   NI-VIPC0003-TAB.
           10 NI-MA-ST-NR                PIC S9(4) USAGE COMP.
           10 NI-BEIN-MONAT              PIC S9(4) USAGE COMP.
           10 NI-BEIN-NEU-STUFE          PIC S9(4) USAGE COMP.
           10 NI-BEIN-ZEITRAUM-STUF      PIC S9(4) USAGE COMP.
           10 NI-BEIN-EH-GS-INV-BAS      PIC S9(4) USAGE COMP.
           10 NI-BEIN-EH-GS-INV-GES      PIC S9(4) USAGE COMP.
           10 NI-BEIN-EH-GS-GRENZE       PIC S9(4) USAGE COMP.
           10 NI-BEIN-EH-GS-NT-GES       PIC S9(4) USAGE COMP.
           10 NI-BEIN-NEU-PRAXIS-ST      PIC S9(4) USAGE COMP.
           10 NI-BEIN-EH-EG-INV-BAS      PIC S9(4) USAGE COMP.
           10 NI-BEIN-EH-EG-INV-GES      PIC S9(4) USAGE COMP.
           10 NI-BEIN-EH-EG-GRENZE       PIC S9(4) USAGE COMP.
           10 NI-BEIN-EH-EG-12MON        PIC S9(4) USAGE COMP.

        05  DCL-VIPC0003.
           10 MA-ST-NR                   PIC S9(9)      USAGE COMP.
           10 BEIN-MONAT                 PIC X(10).
           10 BEIN-NEU-STUFE             PIC S9(4)      USAGE COMP.
           10 BEIN-ZEITRAUM-STUF         PIC S9(4)      USAGE COMP.
           10 BEIN-EH-GS-INV-BAS         PIC S9(9)V9(1) USAGE COMP-3.
           10 BEIN-EH-GS-INV-GES         PIC S9(9)V9(1) USAGE COMP-3.
           10 BEIN-EH-GS-GRENZE          PIC S9(9)V9(1) USAGE COMP-3.
           10 BEIN-EH-GS-NT-GES          PIC S9(9)V9(1) USAGE COMP-3.
           10 BEIN-NEU-PRAXIS-ST         PIC S9(4)      USAGE COMP.
           10 BEIN-EH-EG-INV-BAS         PIC S9(9)V9(1) USAGE COMP-3.
           10 BEIN-EH-EG-INV-GES         PIC S9(9)V9(1) USAGE COMP-3.
           10 BEIN-EH-EG-GRENZE          PIC S9(9)V9(1) USAGE COMP-3.
           10 BEIN-EH-EG-12MON           PIC S9(9)V9(1) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************

