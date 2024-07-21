      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA070.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVBA070.CPv  $
      *
      *   Rev 2.2   Mar 22 2006 14:54:58   t13ae28
      *ASL00216: VSWZ-neuer DCLGEN etc.
      *
      *   Rev 2.1   Mar 06 2006 15:51:08   t13ae30
      *ASL00216: VSWZ-neuer DCLGEN etc.
      *
      *   Rev 2.0   Jan 19 2006 13:54:52   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA070                                      *
      *    --------------------------------------------------------   *
      *    WB-BEREICH VON VIVBA070 (KIRH)                             *
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
      * COBOL DECLARATION FOR TABLE VIVBA070                           *
      ******************************************************************
       01  WB-VIVBA070-GESAMT.
        05  WB-NI-VIVBA070-TAB.
           10 WB-NI-VIVBA070-ELE     PIC S9(04) COMP  OCCURS 11 TIMES.
        05  WB-NI-VIVBA070           REDEFINES  NI-VIVBA070-TAB.
           10 WB-NI-KIRH-ALTER       PIC S9(4)    USAGE COMP.
           10 WB-NI-KNR-GESCHL       PIC S9(4)    USAGE COMP.
           10 WB-NI-KNR-WAEHRUNG     PIC S9(4)    USAGE COMP.
           10 WB-NI-KIRH-LAST-UPDATE PIC S9(4)    USAGE COMP.
           10 WB-NI-KTY-GESCHL       PIC S9(4)    USAGE COMP.
           10 WB-NI-KTY-WAEHRUNG     PIC S9(4)    USAGE COMP.
           10 WB-NI-KIRH-VERS-SUM    PIC S9(4)    USAGE COMP.
           10 WB-NI-KIRH-ABL-SUM     PIC S9(4)    USAGE COMP.
           10 WB-NI-KIRH-RV-RENTE    PIC S9(4)    USAGE COMP.
           10 WB-NI-KIRH-RV-ABF      PIC S9(4)    USAGE COMP.
           10 WB-NI-KIRH-RV-BURENTE  PIC S9(4)    USAGE COMP.
        05  WB-VIVBA070.
           10 WB-KIRH-ALTER          PIC S9(4)    USAGE COMP.
           10 WB-KNR-GESCHL          PIC S9(4)    USAGE COMP.
           10 WB-KNR-WAEHRUNG        PIC S9(4)    USAGE COMP.
           10 WB-KIRH-LAST-UPDATE    PIC X(26).
           10 WB-KTY-HESCHL          PIC S9(4)    USAGE COMP.
           10 WB-KTY-WAEHRUNG        PIC S9(4)    USAGE COMP.
           10 WB-KIRH-VERS-SUM       PIC S9(9)V99 USAGE COMP-3.
           10 WB-KIRH-ABL-SUM        PIC S9(9)V99 USAGE COMP-3.
           10 WB-KIRH-RV-RENTE       PIC S9(9)V99 USAGE COMP-3.
           10 WB-KIRH-RV-ABF         PIC S9(9)V99 USAGE COMP-3.
           10 WB-KIRH-RV-BURENTE     PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
