      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBE004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBE004.CPv  $
      *
      *   Rev 2.1   Nov 02 2004 16:49:26   t13ex43
      *AVB00771: Z-PGMe - Steuernummer auf der VB-Abrechnung
      *
      *   Rev 2.0   Nov 02 2004 16:03:26   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBE004                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    02.11.2004                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00771!02.11.04!2.1 ! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBE004                           *
      ******************************************************************
        05  WB-NI-VIVBE004-TAB.
           10 WB-NI-VIVBE004-ELE     PIC S9(04)  COMP  OCCURS  32 TIMES.
        05  WB-NI-VIVBE004           REDEFINES   WB-NI-VIVBE004-TAB.
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
        05  WB-VIVBE004.
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
           10 WB-MA-AUTOTELEFON    PIC X(14)                VALUE SPACE.
           10 WB-MA-TELETEXT       PIC X(14)                VALUE SPACE.
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 32      *
      ******************************************************************
