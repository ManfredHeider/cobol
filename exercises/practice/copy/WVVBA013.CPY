      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.04.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA013.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA013.CPv  $
      *
      *   Rev 2.1   26 Apr 2007 15:44:36   t13ae30
      *AVB00908: Neuer VB-Vertrag 2007
      *
      *   Rev 2.0   24 Apr 2007 15:23:40   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA013                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    24.04.2007                                       *
      *    STAND:    24.04.2007                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA013                           *
      ******************************************************************
        05  WB-NI-VIVBA013-TAB.
           10  WB-NI-VIVBA013-ELE  PIC S9(04)  COMP   OCCURS 10  TIMES.
        05  WB-NI-VIVBA013           REDEFINES   WB-NI-VIVBA013-TAB.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-AV-DAT-VON       PIC S9(04)  COMP.
           10 WB-NI-AV-LAST-UPDATE   PIC S9(04)  COMP.
           10 WB-NI-KTY-AV-TYP       PIC S9(04)  COMP.
           10 WB-NI-KNR-AV-TYP       PIC S9(04)  COMP.
           10 WB-NI-AV-VERTRAGSDAT   PIC S9(04)  COMP.
           10 WB-NI-AV-DAT-BIS       PIC S9(04)  COMP.
           10 WB-NI-AV-UMSCHREIB-DAT PIC S9(04)  COMP.
           10 WB-NI-AV-UMSCHREIB-PTN PIC S9(04)  COMP.
           10 WB-NI-AV-RUECKSCH-DAT  PIC S9(04)  COMP.
        05  WB-VIVBA013.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-AV-DAT-VON        PIC X(26)                VALUE SPACE.
           10 WB-AV-LAST-UPDATE    PIC X(26)                VALUE SPACE.
           10 WB-KTY-AV-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-AV-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-AV-VERTRAGSDAT    PIC X(10)                VALUE SPACE.
           10 WB-AV-DAT-BIS        PIC X(10)                VALUE SPACE.
           10 WB-AV-UMSCHREIB-DAT  PIC X(10)                VALUE SPACE.
           10 WB-AV-UMSCHREIB-PTN  PIC X(40)                VALUE SPACE.
           10 WB-AV-RUECKSCH-DAT   PIC X(10)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
