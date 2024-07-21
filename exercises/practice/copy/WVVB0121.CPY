      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0121.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0121.CPv  $
      *
      *   Rev 2.1   Nov 21 2011 09:57:48   t13ae13
      *AAS00984: Beförderungsbedingungen 2007 auswertbar
      *
      *   Rev 2.0   Nov 08 2011 11:48:52   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0121                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    10.11.2011                                       *
      *    STAND:    10.11.2011                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00984!10.11.11!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0121                           *
      ******************************************************************
        05  WB-NI-VIVB0121-TAB.
           10  WB-NI-VIVB0121-ELE  PIC S9(04) COMP   OCCURS 12  TIMES.
        05  WB-NI-VIVB0121         REDEFINES  WB-NI-VIVB0121-TAB.
           10 WB-NI-MA-ST-NR          PIC S9(04) COMP.
           10 WB-NI-MS-NR             PIC S9(04) COMP.
           10 WB-NI-BEFINFO-DAT-VON   PIC S9(04) COMP.
           10 WB-NI-BEFINFO-LAST-UPD  PIC S9(04) COMP.
           10 WB-NI-KTY-BEFBED-TYP    PIC S9(04) COMP.
           10 WB-NI-KNR-BEFBED-TYP    PIC S9(04) COMP.
           10 WB-NI-BEFINFO-E-PR-ALT  PIC S9(04) COMP.
           10 WB-NI-BEFINFO-S-PR-ALT  PIC S9(04) COMP.
           10 WB-NI-BEFINFO-E-PR-07   PIC S9(04) COMP.
           10 WB-NI-BEFINFO-S-PR-07   PIC S9(04) COMP.
           10 WB-NI-BEFINFO-E-PR-11   PIC S9(04) COMP.
           10 WB-NI-BEFINFO-S-PR-11   PIC S9(04) COMP.
        05  WB-VIVB0121.
           10 WB-MA-ST-NR             PIC S9(09) USAGE COMP VALUE ZERO.
           10 WB-MS-NR                PIC S9(04) USAGE COMP VALUE ZERO.
           10 WB-BEFINFO-DAT-VON      PIC X(10)             VALUE SPACE.
           10 WB-BEFINFO-LAST-UPD     PIC X(26)             VALUE SPACE.
           10 WB-KTY-BEFBED-TYP       PIC S9(04) USAGE COMP VALUE ZERO.
           10 WB-KNR-BEFBED-TYP       PIC S9(04) USAGE COMP VALUE ZERO.
           10 WB-BEFINFO-E-PR-ALT     PIC X(01)             VALUE SPACE.
           10 WB-BEFINFO-S-PR-ALT     PIC X(01)             VALUE SPACE.
           10 WB-BEFINFO-E-PR-07      PIC X(01)             VALUE SPACE.
           10 WB-BEFINFO-S-PR-07      PIC X(01)             VALUE SPACE.
           10 WB-BEFINFO-E-PR-11      PIC X(01)             VALUE SPACE.
           10 WB-BEFINFO-S-PR-11      PIC X(01)             VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
