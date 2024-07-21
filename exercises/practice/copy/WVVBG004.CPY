      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBG004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBG004.CPv  $
      *
      *   Rev 2.1   14 Nov 2006 12:53:06   t13ae13
      *AVB00874: VB21 - Gewerbeberechtigung bei Antragserfassung DV
      *AG-Wien
      *
      *   Rev 2.0   26 Oct 2006 12:59:10   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBG004                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    26.10.2006                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00874!26.10.06!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBG004                           *
      ******************************************************************
        05  WB-NI-VIVBG004-TAB.
           10 WB-NI-VIVBG004-ELE     PIC S9(04)  COMP  OCCURS  35 TIMES.
        05  WB-NI-VIVBG004           REDEFINES   WB-NI-VIVBG004-TAB.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-MA-LAST-UPDATE   PIC S9(04)  COMP.
           10 WB-NI-KTY-MA-TYP       PIC S9(04)  COMP.
           10 WB-NI-KNR-MA-TYP       PIC S9(04)  COMP.
           10 WB-NI-MA-BDV-MITGLIED  PIC S9(04)  COMP.
           10 WB-NI-MA-STRAFE        PIC S9(04)  COMP.
           10 WB-NI-MA-VERSANDSTOP   PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-ZVII-ABL   PIC S9(04)  COMP.
           10 WB-NI-MA-RCK-STZ       PIC S9(04)  COMP.
           10 WB-NI-MA-RCK-STZ-DAT   PIC S9(04)  COMP.
           10 WB-NI-MA-FUEH-ZEUG-DAT PIC S9(04)  COMP.
           10 WB-NI-MA-SCHUFA-DAT    PIC S9(04)  COMP.
           10 WB-NI-MA-34C-DAT       PIC S9(04)  COMP.
           10 WB-NI-MA-AFO           PIC S9(04)  COMP.
           10 WB-NI-MA-DARLEHEN      PIC S9(04)  COMP.
           10 WB-NI-MA-AVAG-DVAG     PIC S9(04)  COMP.
           10 WB-NI-MA-KOMPASS-AVAG  PIC S9(04)  COMP.
           10 WB-NI-MA-AUTOTELEFON   PIC S9(04)  COMP.
           10 WB-NI-MA-TELETEXT      PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-NR         PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-DAT-BIS-R  PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-DAT-BIS    PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-DAT-VON    PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-KUE-DAT    PIC S9(04)  COMP.
           10 WB-NI-MA-ED-ZUSATZ     PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-VERL       PIC S9(04)  COMP.
           10 WB-NI-MA-FG-DAT        PIC S9(04)  COMP.
           10 WB-NI-MA-127-DAT       PIC S9(04)  COMP.
           10 WB-NI-MA-VA-DAT        PIC S9(04)  COMP.
           10 WB-NI-MA-ANWERBER      PIC S9(04)  COMP.
           10 WB-NI-MA-BEFBED-NEU    PIC S9(04)  COMP.
           10 WB-NI-MA-STEUER-NR     PIC S9(04)  COMP.
           10 WB-NI-MA-GEWERBE-DAT   PIC S9(04)  COMP.
           10 WB-NI-MA-GRUNDSTUECKE  PIC S9(04)  COMP.
           10 WB-NI-MA-127-UNBES-DAT PIC S9(04)  COMP.
        05  WB-VIVBG004.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-MA-LAST-UPDATE    PIC X(26)                VALUE SPACE.
           10 WB-KTY-MA-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-MA-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-MA-BDV-MITGLIED   PIC X(1)                 VALUE SPACE.
           10 WB-MA-STRAFE         PIC X(1)                 VALUE SPACE.
           10 WB-MA-VERSANDSTOP    PIC X(1)                 VALUE SPACE.
           10 WB-MA-ZV-ZVII-ABL    PIC X(1)                 VALUE SPACE.
           10 WB-MA-RCK-STZ        PIC S999V99 COMP-3       VALUE ZERO.
           10 WB-MA-RCK-STZ-DAT    PIC X(10)                VALUE SPACE.
           10 WB-MA-FUEH-ZEUG-DAT  PIC X(10)                VALUE SPACE.
           10 WB-MA-SCHUFA-DAT     PIC X(10)                VALUE SPACE.
           10 WB-MA-34C-DAT        PIC X(10)                VALUE SPACE.
           10 WB-MA-AFO            PIC X(1)                 VALUE SPACE.
           10 WB-MA-DARLEHEN       PIC X(1)                 VALUE SPACE.
           10 WB-MA-AVAG-DVAG      PIC X(10)                VALUE SPACE.
           10 WB-MA-KOMPASS-AVAG   PIC X(10)                VALUE SPACE.
           10 WB-MA-AUTOTELEFON    PIC X(26)                VALUE SPACE.
           10 WB-MA-TELETEXT       PIC X(26)                VALUE SPACE.
           10 WB-MA-ZV-NR          PIC X(1)                 VALUE SPACE.
           10 WB-MA-ZV-DAT-BIS-R   PIC X(10)                VALUE SPACE.
           10 WB-MA-ZV-DAT-BIS     PIC X(10)                VALUE SPACE.
           10 WB-MA-ZV-DAT-VON     PIC X(10)                VALUE SPACE.
           10 WB-MA-ZV-KUE-DAT     PIC X(10)                VALUE SPACE.
           10 WB-MA-ED-ZUSATZ      PIC S9(04)  COMP         VALUE ZERO.
           10 WB-MA-ZV-VERL        PIC X(1)                 VALUE SPACE.
           10 WB-MA-FG-DAT         PIC X(10)                VALUE SPACE.
           10 WB-MA-127-DAT        PIC X(10)                VALUE SPACE.
           10 WB-MA-VA-DAT         PIC X(10)                VALUE SPACE.
           10 WB-MA-ANWERBER       PIC S9(9)   COMP         VALUE ZERO.
           10 WB-MA-BEFBED-NEU     PIC X(10)                VALUE SPACE.
           10 WB-MA-STEUER-NR      PIC X(15)                VALUE SPACE.
           10 WB-MA-GEWERBE-DAT    PIC X(10)                VALUE SPACE.
           10 WB-MA-GRUNDSTUECKE   PIC X(1)                 VALUE SPACE.
           10 WB-MA-127-UNBES-DAT  PIC X(10)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 35      *
      ******************************************************************
