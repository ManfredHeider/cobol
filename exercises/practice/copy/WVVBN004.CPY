      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBN004.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WVVBN004.CPv  $
      *
      *   Rev 2.3   Jan 22 2013 10:41:54   t13ae13
      *AVB01252: §34f  - MA
      *
      *   Rev 2.2   Jan 10 2013 09:06:22   t13ae13
      *AVB01252: §34f  - MA
      *
      *   Rev 2.1   Nov 09 2012 16:05:58   t13ae13
      *AVB01252: §34f  - MA
      *
      *   Rev 2.0   Sep 13 2012 09:29:58   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBN004                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    13.09.2012                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01252!13.09.12!2.1 ! NEU                       ! S. Walb  *
      * AVB01252!07.01.13!2.2 ! Neues Feld MA-34C-ANT-SCH ! S. Walb  *
      * AVB01272!21.01.13!2.3 ! Neues Feld MA-34F-REG-NR  ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBN004                           *
      ******************************************************************
        05  WB-NI-VIVBN004-TAB.
           10 WB-NI-VIVBN004-ELE      PIC S9(04)  COMP  OCCURS 79 TIMES.
        05  WB-NI-VIVBN004            REDEFINES   WB-NI-VIVBN004-TAB.
           10 WB-NI-MA-ST-NR          PIC S9(04)  COMP.
           10 WB-NI-MA-LAST-UPDATE    PIC S9(04)  COMP.
           10 WB-NI-KTY-MA-TYP        PIC S9(04)  COMP.
           10 WB-NI-KNR-MA-TYP        PIC S9(04)  COMP.
           10 WB-NI-MA-BDV-MITGLIED   PIC S9(04)  COMP.
           10 WB-NI-MA-STRAFE         PIC S9(04)  COMP.
           10 WB-NI-MA-VERSANDSTOP    PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-ZVII-ABL    PIC S9(04)  COMP.
           10 WB-NI-MA-RCK-STZ        PIC S9(04)  COMP.
           10 WB-NI-MA-RCK-STZ-DAT    PIC S9(04)  COMP.
           10 WB-NI-MA-FUEH-ZEUG-DAT  PIC S9(04)  COMP.
           10 WB-NI-MA-SCHUFA-DAT     PIC S9(04)  COMP.
           10 WB-NI-MA-34C-DAT        PIC S9(04)  COMP.
           10 WB-NI-MA-AFO            PIC S9(04)  COMP.
           10 WB-NI-MA-DARLEHEN       PIC S9(04)  COMP.
           10 WB-NI-MA-AVAG-DVAG      PIC S9(04)  COMP.
           10 WB-NI-MA-KOMPASS-AVAG   PIC S9(04)  COMP.
           10 WB-NI-MA-AUTOTELEFON    PIC S9(04)  COMP.
           10 WB-NI-MA-TELETEXT       PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-NR          PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-DAT-BIS-R   PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-DAT-BIS     PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-KUE-DAT     PIC S9(04)  COMP.
           10 WB-NI-MA-ED-ZUSATZ      PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-VERL        PIC S9(04)  COMP.
           10 WB-NI-MA-FG-DAT         PIC S9(04)  COMP.
           10 WB-NI-MA-127-DAT        PIC S9(04)  COMP.
           10 WB-NI-MA-VA-DAT         PIC S9(04)  COMP.
           10 WB-NI-MA-ANWERBER       PIC S9(04)  COMP.
           10 WB-NI-MA-BEFBED-NEU     PIC S9(04)  COMP.
           10 WB-NI-MA-STEUER-NR      PIC S9(04)  COMP.
           10 WB-NI-MA-GEWERBE-DAT    PIC S9(04)  COMP.
           10 WB-NI-MA-GRUNDSTUECKE   PIC S9(04)  COMP.
           10 WB-NI-MA-127-UNBES-DAT  PIC S9(04)  COMP.
           10 WB-NI-MA-BEFBED-WIEN-07 PIC S9(04)  COMP.
           10 WB-NI-MA-REGISTER-NR    PIC S9(04)  COMP.
           10 WB-NI-MA-SCHULDNER-VERZ PIC S9(04)  COMP.
           10 WB-NI-MA-GEW-ZENTR-REG  PIC S9(04)  COMP.
           10 WB-NI-MA-INSOLVENZ-VERZ PIC S9(04)  COMP.
           10 WB-NI-MA-AM-AGENT-NR    PIC S9(04)  COMP.
           10 WB-NI-MA-EINTR-DAT-AM   PIC S9(04)  COMP.
           10 WB-NI-MA-HRD-ORT        PIC S9(04)  COMP.
           10 WB-NI-MA-HRD-EINTR      PIC S9(04)  COMP.
           10 WB-NI-MA-HROE-EINTR-VA  PIC S9(04)  COMP.
           10 WB-NI-MA-HROE-EINTR-VB  PIC S9(04)  COMP.
           10 WB-NI-MA-BEFBED-DVAG-07 PIC S9(04)  COMP.
           10 WB-NI-KTY-NOTFALL-TYP   PIC S9(04)  COMP.
           10 WB-NI-KNR-NOTFALL-TYP   PIC S9(04)  COMP.
           10 WB-NI-MA-34C-AMT        PIC S9(04)  COMP.
           10 WB-NI-MA-34C-STRASSE    PIC S9(04)  COMP.
           10 WB-NI-MA-34C-PLZ        PIC S9(04)  COMP.
           10 WB-NI-MA-34C-ORT        PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-ABSICHERG   PIC S9(04)  COMP.
           10 WB-NI-MA-FG-VOM         PIC S9(04)  COMP.
           10 WB-NI-MA-127-VOM        PIC S9(04)  COMP.
           10 WB-NI-MA-VA-VOM         PIC S9(04)  COMP.
           10 WB-NI-MA-FUEH-ZEUG-VOM  PIC S9(04)  COMP.
           10 WB-NI-MA-SCHUFA-VOM     PIC S9(04)  COMP.
           10 WB-NI-MA-GEW-Z-REG-VOM  PIC S9(04)  COMP.
           10 WB-NI-MA-ANLAGE-BER     PIC S9(04)  COMP.
           10 WB-NI-MA-HROE-EINTR-FG  PIC S9(04)  COMP.
           10 WB-NI-MA-VSH-POLNR      PIC S9(04)  COMP.
           10 WB-NI-MA-VSH-DAT        PIC S9(04)  COMP.
           10 WB-NI-MA-VSH-VOM        PIC S9(04)  COMP.
           10 WB-NI-MA-HROE-EINTR-WG  PIC S9(04)  COMP.
           10 WB-NI-MA-WG-DAT         PIC S9(04)  COMP.
           10 WB-NI-MA-WG-VOM         PIC S9(04)  COMP.
           10 WB-NI-MA-BEFBED-2011    PIC S9(04)  COMP.
           10 WB-NI-MA-34F-DAT        PIC S9(04)  COMP.
           10 WB-NI-MA-34F-ANT-SCH    PIC S9(04)  COMP.
           10 WB-NI-MA-34F-AFO        PIC S9(04)  COMP.
           10 WB-NI-MA-34F-ANL-BER    PIC S9(04)  COMP.
           10 WB-NI-MA-34F-AMT        PIC S9(04)  COMP.
           10 WB-NI-MA-34F-STRASSE    PIC S9(04)  COMP.
           10 WB-NI-MA-34F-PLZ        PIC S9(04)  COMP.
           10 WB-NI-MA-34F-ORT        PIC S9(04)  COMP.
           10 WB-NI-MA-34C-ANT-SCH    PIC S9(04)  COMP.
           10 WB-NI-MA-34F-REG-NR     PIC S9(04)  COMP.
       05  WB-VIVBN004.
           10 WB-MA-ST-NR             PIC S9(9)   COMP      VALUE ZERO.
           10 WB-MA-LAST-UPDATE       PIC X(26)             VALUE SPACE.
           10 WB-KTY-MA-TYP           PIC S9(4)   COMP      VALUE ZERO.
           10 WB-KNR-MA-TYP           PIC S9(4)   COMP      VALUE ZERO.
           10 WB-MA-BDV-MITGLIED      PIC X(1)              VALUE SPACE.
           10 WB-MA-STRAFE            PIC X(1)              VALUE SPACE.
           10 WB-MA-VERSANDSTOP       PIC X(1)              VALUE SPACE.
           10 WB-MA-ZV-ZVII-ABL       PIC X(1)              VALUE SPACE.
           10 WB-MA-RCK-STZ           PIC S999V99 COMP-3    VALUE ZERO.
           10 WB-MA-RCK-STZ-DAT       PIC X(10)             VALUE SPACE.
           10 WB-MA-FUEH-ZEUG-DAT     PIC X(10)             VALUE SPACE.
           10 WB-MA-SCHUFA-DAT        PIC X(10)             VALUE SPACE.
           10 WB-MA-34C-DAT           PIC X(10)             VALUE SPACE.
           10 WB-MA-AFO               PIC X(1)              VALUE SPACE.
           10 WB-MA-DARLEHEN          PIC X(1)              VALUE SPACE.
           10 WB-MA-AVAG-DVAG         PIC X(10)             VALUE SPACE.
           10 WB-MA-KOMPASS-AVAG      PIC X(10)             VALUE SPACE.
           10 WB-MA-AUTOTELEFON       PIC X(26)             VALUE SPACE.
           10 WB-MA-TELETEXT          PIC X(26)             VALUE SPACE.
           10 WB-MA-ZV-NR             PIC X(1)              VALUE SPACE.
           10 WB-MA-ZV-DAT-BIS-R      PIC X(10)             VALUE SPACE.
           10 WB-MA-ZV-DAT-BIS        PIC X(10)             VALUE SPACE.
           10 WB-MA-ZV-DAT-VON        PIC X(10)             VALUE SPACE.
           10 WB-MA-ZV-KUE-DAT        PIC X(10)             VALUE SPACE.
           10 WB-MA-ED-ZUSATZ         PIC S9(04)  COMP      VALUE ZERO.
           10 WB-MA-ZV-VERL           PIC X(1)              VALUE SPACE.
           10 WB-MA-FG-DAT            PIC X(10)             VALUE SPACE.
           10 WB-MA-127-DAT           PIC X(10)             VALUE SPACE.
           10 WB-MA-VA-DAT            PIC X(10)             VALUE SPACE.
           10 WB-MA-ANWERBER          PIC S9(9)   COMP      VALUE ZERO.
           10 WB-MA-BEFBED-NEU        PIC X(10)             VALUE SPACE.
           10 WB-MA-STEUER-NR         PIC X(15)             VALUE SPACE.
           10 WB-MA-GEWERBE-DAT       PIC X(10)             VALUE SPACE.
           10 WB-MA-GRUNDSTUECKE      PIC X(1)              VALUE SPACE.
           10 WB-MA-127-UNBES-DAT     PIC X(10)             VALUE SPACE.
           10 WB-MA-BEFBED-WIEN-07    PIC X(10)             VALUE SPACE.
           10 WB-MA-REGISTER-NR       PIC X(30)             VALUE SPACE.
           10 WB-MA-SCHULDNER-VERZ    PIC X(10)             VALUE SPACE.
           10 WB-MA-GEW-ZENTR-REG     PIC X(10)             VALUE SPACE.
           10 WB-MA-INSOLVENZ-VERZ    PIC X(10)             VALUE SPACE.
           10 WB-MA-AM-AGENT-NR       PIC S9(9)   COMP      VALUE ZERO.
           10 WB-MA-EINTR-DAT-AM      PIC X(10)             VALUE SPACE.
           10 WB-MA-HRD-ORT           PIC X(40)             VALUE SPACE.
           10 WB-MA-HRD-EINTR         PIC X(8)              VALUE SPACE.
           10 WB-MA-HROE-EINTR-VA     PIC X(25)             VALUE SPACE.
           10 WB-MA-HROE-EINTR-VB     PIC X(25)             VALUE SPACE.
           10 WB-MA-BEFBED-DVAG-07    PIC X(10)             VALUE SPACE.
           10 WB-KTY-NOTFALL-TYP      PIC S9(4)   COMP      VALUE ZERO.
           10 WB-KNR-NOTFALL-TYP      PIC S9(4)   COMP      VALUE ZERO.
           10 WB-MA-34C-AMT           PIC X(40)             VALUE SPACE.
           10 WB-MA-34C-STRASSE       PIC X(40)             VALUE SPACE.
           10 WB-MA-34C-PLZ           PIC X(09)             VALUE SPACE.
           10 WB-MA-34C-ORT           PIC X(40)             VALUE SPACE.
           10 WB-MA-ZV-ABSICHERG      PIC S9(9)V99 COMP-3   VALUE ZERO.
           10 WB-MA-FG-VOM            PIC X(10)             VALUE SPACE.
           10 WB-MA-127-VOM           PIC X(10)             VALUE SPACE.
           10 WB-MA-VA-VOM            PIC X(10)             VALUE SPACE.
           10 WB-MA-FUEH-ZEUG-VOM     PIC X(10)             VALUE SPACE.
           10 WB-MA-SCHUFA-VOM        PIC X(10)             VALUE SPACE.
           10 WB-MA-GEW-Z-REG-VOM     PIC X(10)             VALUE SPACE.
           10 WB-MA-ANLAGE-BER        PIC X(1)              VALUE SPACE.
           10 WB-MA-HROE-EINTR-FG     PIC X(25)             VALUE SPACE.
           10 WB-MA-VSH-POLNR         PIC X(25)             VALUE SPACE.
           10 WB-MA-VSH-DAT           PIC X(10)             VALUE SPACE.
           10 WB-MA-VSH-VOM           PIC X(10)             VALUE SPACE.
           10 WB-MA-HROE-EINTR-WG     PIC X(25)             VALUE SPACE.
           10 WB-MA-WG-DAT            PIC X(10)             VALUE SPACE.
           10 WB-MA-WG-VOM            PIC X(10)             VALUE SPACE.
           10 WB-MA-BEFBED-2011       PIC X(10)             VALUE SPACE.
           10 WB-MA-34F-DAT           PIC X(10)             VALUE SPACE.
           10 WB-MA-34F-ANT-SCH       PIC X(01)             VALUE SPACE.
           10 WB-MA-34F-AFO           PIC X(01)             VALUE SPACE.
           10 WB-MA-34F-ANL-BER       PIC X(01)             VALUE SPACE.
           10 WB-MA-34F-AMT           PIC X(40)             VALUE SPACE.
           10 WB-MA-34F-STRASSE       PIC X(40)             VALUE SPACE.
           10 WB-MA-34F-PLZ           PIC X(09)             VALUE SPACE.
           10 WB-MA-34F-ORT           PIC X(40)             VALUE SPACE.
           10 WB-MA-34C-ANT-SCH       PIC X(01)             VALUE SPACE.
           10 WB-MA-34F-REG-NR        PIC X(15)             VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 79      *
      ******************************************************************
