      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.06.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBI004.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVBI004.CPv  $
      *
      *   Rev 2.2   13 Sep 2007 10:42:52   t13ae13
      *AVB00932: neue Felder im VB 21
      *
      *   Rev 2.1   18 Jul 2007 08:39:16   t13ae13
      *AVB00923: neue Aufstiegsbedingungen DVAG 2007 - MA erweitern
      *
      *   Rev 2.0   18 Jun 2007 11:32:32   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBI004                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    18.06.2007                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00905!18.06.07!2.1 ! NEU                       ! TH.WILLIG*
      * AVB00923!        !    ! Neue Befbed 2007 für DVAG !          *
      * AVB00932!24.08.07!2.2 ! Feldverlängerung HRD_EINTR! S. Walb  *
      *         !04.09.07!    ! u. Feldumbenennung        !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBI004                           *
      ******************************************************************
        05  WB-NI-VIVBI004-TAB.
           10 WB-NI-VIVBI004-ELE      PIC S9(04)  COMP  OCCURS 47 TIMES.
        05  WB-NI-VIVBI004            REDEFINES   WB-NI-VIVBI004-TAB.
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
040907*    10 WB-NI-MA-HROE-EINTR-01  PIC S9(04)  COMP.
040907*    10 WB-NI-MA-HROE-EINTR-02  PIC S9(04)  COMP.
040907     10 WB-NI-MA-HROE-EINTR-VA  PIC S9(04)  COMP.
040907     10 WB-NI-MA-HROE-EINTR-VB  PIC S9(04)  COMP.
           10 WB-NI-MA-BEFBED-DVAG-07 PIC S9(04)  COMP.
        05  WB-VIVBI004.
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
           10 WB-MA-AM-AGENT-NR       PIC S9(9)  COMP       VALUE ZERO.
           10 WB-MA-EINTR-DAT-AM      PIC X(10)             VALUE SPACE.
           10 WB-MA-HRD-ORT           PIC X(40)             VALUE SPACE.
240807*    10 WB-MA-HRD-EINTR         PIC X(5)              VALUE SPACE.
240807     10 WB-MA-HRD-EINTR         PIC X(8)              VALUE SPACE.
040907*    10 WB-MA-HROE-EINTR-01     PIC X(25)             VALUE SPACE.
040907*    10 WB-MA-HROE-EINTR-02     PIC X(25)             VALUE SPACE.
040907     10 WB-MA-HROE-EINTR-VA     PIC X(25)             VALUE SPACE.
040907     10 WB-MA-HROE-EINTR-VB     PIC X(25)             VALUE SPACE.
           10 WB-MA-BEFBED-DVAG-07    PIC X(10)             VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      ******************************************************************
