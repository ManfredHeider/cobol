      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0122.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0122.CPv  $
      *
      *   Rev 2.1   Jan 04 2012 11:02:44   t13ae13
      *AVB01200: Erfassung von Branche und Unternehmen bei Neueinst
      *ellungen
      *
      *   Rev 2.0   Dec 01 2011 11:50:40   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0122                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    01.12.2011                                       *
      *    STAND:    01.12.2011                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01200!01.12.11!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0122                           *
      ******************************************************************
        05  WB-NI-VIVB0122-TAB.
           10  WB-NI-VIVB0122-ELE  PIC S9(04) COMP   OCCURS 8  TIMES.
        05  WB-NI-VIVB0122         REDEFINES  WB-NI-VIVB0122-TAB.
           10 WB-NI-MA-ST-NR          PIC S9(04) COMP.
           10 WB-NI-BRAUNTZ-DAT-VON   PIC S9(04) COMP.
           10 WB-NI-BRAUNTZ-LAST-UPD  PIC S9(04) COMP.
           10 WB-NI-KTY-BRANCHE       PIC S9(04) COMP.
           10 WB-NI-KNR-BRANCHE       PIC S9(04) COMP.
           10 WB-NI-KTY-UNT-IN-BRA    PIC S9(04) COMP.
           10 WB-NI-KNR-UNT-IN-BRA    PIC S9(04) COMP.
           10 WB-NI-BRAUNTZ-KUNDE     PIC S9(04) COMP.
        05  WB-VIVB0122.
           10 WB-MA-ST-NR             PIC S9(09) USAGE COMP VALUE ZERO.
           10 WB-BRAUNTZ-DAT-VON      PIC X(10)             VALUE SPACE.
           10 WB-BRAUNTZ-LAST-UPD     PIC X(26)             VALUE SPACE.
           10 WB-KTY-BRANCHE          PIC S9(04) USAGE COMP VALUE ZERO.
           10 WB-KNR-BRANCHE          PIC S9(04) USAGE COMP VALUE ZERO.
           10 WB-KTY-UNT-IN-BRA       PIC S9(04) USAGE COMP VALUE ZERO.
           10 WB-KNR-UNT-IN-BRA       PIC S9(04) USAGE COMP VALUE ZERO.
           10 WB-BRAUNTZ-KUNDE        PIC X(01)             VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
