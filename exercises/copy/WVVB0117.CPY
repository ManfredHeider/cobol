      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0117.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0117.CPv  $
      *
      *   Rev 2.1   Apr 30 2010 12:11:56   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung - Tabellen
      *
      *   Rev 2.0   Mar 06 2009 09:13:44   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0117                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    22.01.2010                                       *
      *    STAND:    22.01.2010                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01031!22.01.10!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0117                           *
      ******************************************************************
        05  WB-NI-VIVB0117-TAB.
           10  WB-NI-VIVB0117-ELE  PIC S9(04) COMP   OCCURS 10  TIMES.
        05  WB-NI-VIVB0117         REDEFINES  WB-NI-VIVB0117-TAB.
           10 WB-NI-EDW-DL-ALT       PIC S9(04) COMP.
           10 WB-NI-EDW-UNT-ST-NR    PIC S9(04) COMP.
           10 WB-NI-EDW-LAST-UPDATE  PIC S9(04) COMP.
           10 WB-NI-EDW-DL-NEU       PIC S9(04) COMP.
           10 WB-NI-EDW-DAT-VON      PIC S9(04) COMP.
           10 WB-NI-EDW-DAT-BIS      PIC S9(04) COMP.
           10 WB-NI-KTY-EDW-STATUS   PIC S9(04) COMP.
           10 WB-NI-KNR-EDW-STATUS   PIC S9(04) COMP.
           10 WB-NI-EDW-DIR-NR-ALT   PIC S9(04) COMP.
           10 WB-NI-EDW-DIR-NR-NEU   PIC S9(04) COMP.
        05  WB-VIVB0117.
           10 WB-EDW-DL-ALT          PIC S9(09) USAGE COMP VALUE ZERO.
           10 WB-EDW-UNT-ST-NR       PIC S9(09) USAGE COMP VALUE ZERO.
           10 WB-EDW-LAST-UPDATE     PIC X(26)             VALUE SPACE.
           10 WB-EDW-DL-NEU          PIC S9(09) USAGE COMP VALUE ZERO.
           10 WB-EDW-DAT-VON         PIC X(10)             VALUE SPACE.
           10 WB-EDW-DAT-BIS         PIC X(10)             VALUE SPACE.
           10 WB-KTY-EDW-STATUS      PIC S9(04) USAGE COMP VALUE ZERO.
           10 WB-KNR-EDW-STATUS      PIC S9(04) USAGE COMP VALUE ZERO.
           10 WB-EDW-DIR-NR-ALT      PIC S9(04) USAGE COMP VALUE ZERO.
           10 WB-EDW-DIR-NR-NEU      PIC S9(04) USAGE COMP VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
