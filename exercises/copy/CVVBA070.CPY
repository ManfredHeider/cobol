      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA070.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVBA070.CPv  $
      *
      *   Rev 2.2   Mar 22 2006 14:54:48   t13ae28
      *ASL00216: VSWZ-neuer DCLGEN etc.
      *
      *   Rev 2.1   Mar 06 2006 15:50:54   t13ae30
      *ASL00216: VSWZ-neuer DCLGEN etc.
      *
      *   Rev 2.0   Jan 19 2006 13:54:30   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVBA070                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON VIVBA070 (KIRH)                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    19.01.2006                                       *
      *****************************************************************
      *    VERSION   1    19.01.06
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00216!19.01.06! 01 ! NEU                       !TH.WILLIG *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVBA070)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA070 TABLE
CCC004*    ( KIRH_ALTER         SMALLINT       NOT NULL,
CCC004*      KNR_GESCHL         SMALLINT       NOT NULL,
CCC004*      KNR_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      KIRH_LAST_UPDATE   TIMESTAMP      NOT NULL,
CCC004*      KTY_GESCHL         SMALLINT       NOT NULL,
CCC004*      KTY_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      KIRH_VERS_SUM      DECIMAL(11, 2) NOT NULL,
CCC004*      KIRH_ABL_SUM       DECIMAL(11, 2) NOT NULL,
CCC004*      KIRH_RV_RENTE      DECIMAL(11, 2) NOT NULL,
CCC004*      KIRH_RV_ABF        DECIMAL(11, 2) NOT NULL,
CCC004*      KIRH_RV_BURENTE    DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA070                           *
      ******************************************************************
       01  VIVBA070-GESAMT.
        05  NI-VIVBA070-TAB.
           10 NI-VIVBA070-ELE      PIC S9(04) COMP  OCCURS 11 TIMES.
        05  NI-VIVBA070            REDEFINES  NI-VIVBA070-TAB.
           10 NI-KIRH-ALTER        PIC S9(4)    USAGE COMP.
           10 NI-KNR-GESCHL        PIC S9(4)    USAGE COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-KIRH-LAST-UPDATE  PIC S9(4)    USAGE COMP.
           10 NI-KTY-GESCHL        PIC S9(4)    USAGE COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-KIRH-VERS-SUM     PIC S9(4)    USAGE COMP.
           10 NI-KIRH-ABL-SUM      PIC S9(4)    USAGE COMP.
           10 NI-KIRH-RV-RENTE     PIC S9(4)    USAGE COMP.
           10 NI-KIRH-RV-ABF       PIC S9(4)    USAGE COMP.
           10 NI-KIRH-RV-BURENTE   PIC S9(4)    USAGE COMP.
        05  DCL-VIVBA070.
           10 KIRH-ALTER           PIC S9(4)    USAGE COMP.
           10 KNR-GESCHL           PIC S9(4)    USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KIRH-LAST-UPDATE     PIC X(26).
           10 KTY-GESCHL           PIC S9(4)    USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KIRH-VERS-SUM        PIC S9(9)V99 USAGE COMP-3.
           10 KIRH-ABL-SUM         PIC S9(9)V99 USAGE COMP-3.
           10 KIRH-RV-RENTE        PIC S9(9)V99 USAGE COMP-3.
           10 KIRH-RV-ABF          PIC S9(9)V99 USAGE COMP-3.
           10 KIRH-RV-BURENTE      PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
