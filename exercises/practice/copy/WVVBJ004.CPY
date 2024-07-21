      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.02.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBJ004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBJ004.CPv  $
      *
      *   Rev 2.1   Apr 23 2008 11:17:26   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - M
      *A (JMS)
      *
      *   Rev 2.0   Feb 29 2008 11:05:42   t13ae48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBJ004                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.MAESS                                          *
      *    DATUM:    29.02.2008                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00875!29.02.08!2.1 ! NEU                       ! J.MAESS  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBJ004                           *
      ******************************************************************
        05  WB-NI-VIVBJ004-TAB.
           10 WB-NI-VIVBJ004-ELE      PIC S9(04)  COMP  OCCURS 53 TIMES.
        05  WB-NI-VIVBJ004            REDEFINES   WB-NI-VIVBJ004-TAB.
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
       05  WB-VIVBJ004.
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 53      *
      ******************************************************************
