      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB070.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB070.CPv  $
      *
      *   Rev 2.1   11 Apr 2007 16:15:38   t13ae30
      *ASL00247: Verlängerung der Zusatzleistungen und VSW
      *
      *   Rev 2.0   Feb 15 2007 16:55:52   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVBB070                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON VIVBB070 (KIRH)                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    15.02.2007                                       *
      *****************************************************************
      *    VERSION   1    15.02.07
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00247!15.02.07! 01 ! NEU                       !TH.WILLIG *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVBB070)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBB070 TABLE
CCC004*    ( KIRH_ALTER         SMALLINT       NOT NULL,
CCC004*      KNR_GESCHL         SMALLINT       NOT NULL,
CCC004*      KIRH_KAT           SMALLINT       NOT NULL,
CCC004*      KNR_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      KIRH_LAST_UPDATE   TIMESTAMP      NOT NULL,
CCC004*      KTY_GESCHL         SMALLINT       NOT NULL,
CCC004*      KTY_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      KIRH_VERS_SUM      DECIMAL(11, 2) NOT NULL,
CCC004*      KIRH_ABL_SUM       DECIMAL(11, 2) NOT NULL,
CCC004*      KIRH_RV_RENTE      DECIMAL(11, 2) NOT NULL,
CCC004*      KIRH_RV_ABF        DECIMAL(11, 2) NOT NULL,
CCC004*      KIRH_RV_BURENTE    DECIMAL(11, 2) NOT NULL,
CCC004*      KIRH_VERSB_LV      DECIMAL(11, 2) NOT NULL,
CCC004*      KIRH_VERSB_RV      DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB070                           *
      ******************************************************************
       01  VIVBB070-GESAMT.
        05  NI-VIVBB070-TAB.
           10 NI-VIVBB070-ELE      PIC S9(04) COMP  OCCURS 14 TIMES.
        05  NI-VIVBB070            REDEFINES  NI-VIVBB070-TAB.
           10 NI-KIRH-ALTER        PIC S9(4)    USAGE COMP.
           10 NI-KNR-GESCHL        PIC S9(4)    USAGE COMP.
           10 NI-KIRH-KAT          PIC S9(4)    USAGE COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-KIRH-LAST-UPDATE  PIC S9(4)    USAGE COMP.
           10 NI-KTY-GESCHL        PIC S9(4)    USAGE COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-KIRH-VERS-SUM     PIC S9(4)    USAGE COMP.
           10 NI-KIRH-ABL-SUM      PIC S9(4)    USAGE COMP.
           10 NI-KIRH-RV-RENTE     PIC S9(4)    USAGE COMP.
           10 NI-KIRH-RV-ABF       PIC S9(4)    USAGE COMP.
           10 NI-KIRH-RV-BURENTE   PIC S9(4)    USAGE COMP.
           10 NI-KIRH-VERSB-LV     PIC S9(4)    USAGE COMP.
           10 NI-KIRH-VERSB-RV     PIC S9(4)    USAGE COMP.
        05  DCL-VIVBB070.
           10 KIRH-ALTER           PIC S9(4)    USAGE COMP.
           10 KNR-GESCHL           PIC S9(4)    USAGE COMP.
           10 KIRH-KAT             PIC S9(4)    USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KIRH-LAST-UPDATE     PIC X(26).
           10 KTY-GESCHL           PIC S9(4)    USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KIRH-VERS-SUM        PIC S9(9)V99 USAGE COMP-3.
           10 KIRH-ABL-SUM         PIC S9(9)V99 USAGE COMP-3.
           10 KIRH-RV-RENTE        PIC S9(9)V99 USAGE COMP-3.
           10 KIRH-RV-ABF          PIC S9(9)V99 USAGE COMP-3.
           10 KIRH-RV-BURENTE      PIC S9(9)V99 USAGE COMP-3.
           10 KIRH-VERSB-LV        PIC S9(9)V99 USAGE COMP-3.
           10 KIRH-VERSB-RV        PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
