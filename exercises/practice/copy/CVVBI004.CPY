      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.06.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBI004.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVBI004.CPv  $
      *
      *   Rev 2.2   13 Sep 2007 10:42:46   t13ae13
      *AVB00932: neue Felder im VB 21
      *
      *   Rev 2.1   18 Jul 2007 08:39:10   t13ae13
      *AVB00923: neue Aufstiegsbedingungen DVAG 2007 - MA erweitern
      *
      *   Rev 2.0   18 Jun 2007 11:32:16   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVBI004                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
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
      ******************************************************************
      * DCLGEN TABLE(VIVBI004)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBI004))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBI004 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      MA_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      KTY_MA_TYP                     SMALLINT NOT NULL,
CCC004*      KNR_MA_TYP                     SMALLINT NOT NULL,
CCC004*      MA_BDV_MITGLIED                CHAR(1) NOT NULL,
CCC004*      MA_STRAFE                      CHAR(1) NOT NULL,
CCC004*      MA_VERSANDSTOP                 CHAR(1) NOT NULL,
CCC004*      MA_ZV_ZVII_ABL                 CHAR(1) NOT NULL,
CCC004*      MA_RCK_STZ                     DECIMAL(5, 2) NOT NULL,
CCC004*      MA_RCK_STZ_DAT                 DATE NOT NULL,
CCC004*      MA_FUEH_ZEUG_DAT               DATE,
CCC004*      MA_SCHUFA_DAT                  DATE,
CCC004*      MA_34C_DAT                     DATE,
CCC004*      MA_AFO                         CHAR(1) NOT NULL,
CCC004*      MA_DARLEHEN                    CHAR(1) NOT NULL,
CCC004*      MA_AVAG_DVAG                   DATE,
CCC004*      MA_KOMPASS_AVAG                DATE,
CCC004*      MA_AUTOTELEFON                 CHAR(26) NOT NULL,
CCC004*      MA_TELETEXT                    CHAR(26) NOT NULL,
CCC004*      MA_ZV_NR                       CHAR(1) NOT NULL,
CCC004*      MA_ZV_DAT_BIS_R                DATE,
CCC004*      MA_ZV_DAT_BIS                  DATE,
CCC004*      MA_ZV_DAT_VON                  DATE,
CCC004*      MA_ZV_KUE_DAT                  DATE,
CCC004*      MA_ED_ZUSATZ                   SMALLINT NOT NULL,
CCC004*      MA_ZV_VERL                     CHAR(1) NOT NULL,
CCC004*      MA_FG_DAT                      DATE,
CCC004*      MA_127_DAT                     DATE,
CCC004*      MA_VA_DAT                      DATE,
CCC004*      MA_ANWERBER                    INTEGER NOT NULL,
CCC004*      MA_BEFBED_NEU                  DATE,
CCC004*      MA_STEUER_NR                   CHAR(15),
CCC004*      MA_GEWERBE_DAT                 INTEGER NOT NULL,
CCC004*      MA_GRUNDSTUECKE                DATE,
CCC004*      MA_127_UNBES_DAT               DATE,
CCC004*      MA_BEFBED_WIEN_07              DATE,
CCC004*      MA_REGISTER_NR                 CHAR(30),
CCC004*      MA_SCHULDNER_VERZ              DATE,
CCC004*      MA_GEW_ZENTR_REG               DATE,
CCC004*      MA_INSOLVENZ_VERZ              DATE,
CCC004*      MA_AM_AGENT_NR                 INTEGER,
CCC004*      MA_EINTR_DAT_AM                DATE,
CCC004*      MA_HRD_ORT                     CHAR(40),
240807*      MA_HRD_EINTR                   CHAR(5),
CCC004*      MA_HRD_EINTR                   CHAR(8),
040907*      MA_HROE_EINTR_01               CHAR(25),
040907*      MA_HROE_EINTR_02               CHAR(25),
CCC004*      MA_HROE_EINTR_VA               CHAR(25),
CCC004*      MA_HROE_EINTR_VB               CHAR(25),
CCC004*      MA_BEFBED_DVAG_07              DATE
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBI004                           *
      ******************************************************************
       01  VIVBI004-GESAMT.
        05  NI-VIVBI004-TAB.
           10  NI-VIVBI004-ELE     PIC S9(04)  COMP   OCCURS  47 TIMES.
        05  NI-VIVBI004            REDEFINES   NI-VIVBI004-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-MA-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-KTY-MA-TYP        PIC S9(04)  COMP.
           10 NI-KNR-MA-TYP        PIC S9(04)  COMP.
           10 NI-MA-BDV-MITGLIED   PIC S9(04)  COMP.
           10 NI-MA-STRAFE         PIC S9(04)  COMP.
           10 NI-MA-VERSANDSTOP    PIC S9(04)  COMP.
           10 NI-MA-ZV-ZVII-ABL    PIC S9(04)  COMP.
           10 NI-MA-RCK-STZ        PIC S9(04)  COMP.
           10 NI-MA-RCK-STZ-DAT    PIC S9(04)  COMP.
           10 NI-MA-FUEH-ZEUG-DAT  PIC S9(04)  COMP.
           10 NI-MA-SCHUFA-DAT     PIC S9(04)  COMP.
           10 NI-MA-34C-DAT        PIC S9(04)  COMP.
           10 NI-MA-AFO            PIC S9(04)  COMP.
           10 NI-MA-DARLEHEN       PIC S9(04)  COMP.
           10 NI-MA-AVAG-DVAG      PIC S9(04)  COMP.
           10 NI-MA-KOMPASS-AVAG   PIC S9(04)  COMP.
           10 NI-MA-AUTOTELEFON    PIC S9(04)  COMP.
           10 NI-MA-TELETEXT       PIC S9(04)  COMP.
           10 NI-MA-ZV-NR          PIC S9(04)  COMP.
           10 NI-MA-ZV-DAT-BIS-R   PIC S9(04)  COMP.
           10 NI-MA-ZV-DAT-BIS     PIC S9(04)  COMP.
           10 NI-MA-ZV-DAT-VON     PIC S9(04)  COMP.
           10 NI-MA-ZV-KUE-DAT     PIC S9(04)  COMP.
           10 NI-MA-ED-ZUSATZ      PIC S9(04)  COMP.
           10 NI-MA-ZV-VERL        PIC S9(04)  COMP.
           10 NI-MA-FG-DAT         PIC S9(04)  COMP.
           10 NI-MA-127-DAT        PIC S9(04)  COMP.
           10 NI-MA-VA-DAT         PIC S9(04)  COMP.
           10 NI-MA-ANWERBER       PIC S9(04)  COMP.
           10 NI-MA-BEFBED-NEU     PIC S9(04)  COMP.
           10 NI-MA-STEUER-NR      PIC S9(04)  COMP.
           10 NI-MA-GEWERBE-DAT    PIC S9(04)  COMP.
           10 NI-MA-GRUNDSTUECKE   PIC S9(04)  COMP.
           10 NI-MA-127-UNBES-DAT  PIC S9(04)  COMP.
           10 NI-MA-BEFBED-WIEN-07 PIC S9(04)  COMP.
           10 NI-MA-REGISTER-NR    PIC S9(04)  COMP.
           10 NI-MA-SCHULDNER-VERZ PIC S9(04)  COMP.
           10 NI-MA-GEW-ZENTR-REG  PIC S9(04)  COMP.
           10 NI-MA-INSOLVENZ-VERZ PIC S9(04)  COMP.
           10 NI-MA-AM-AGENT-NR    PIC S9(04)  COMP.
           10 NI-MA-EINTR-DAT-AM   PIC S9(04)  COMP.
           10 NI-MA-HRD-ORT        PIC S9(04)  COMP.
           10 NI-MA-HRD-EINTR      PIC S9(04)  COMP.
040907*    10 NI-MA-HROE-EINTR-01  PIC S9(04)  COMP.
040907*    10 NI-MA-HROE-EINTR-02  PIC S9(04)  COMP.
040907     10 NI-MA-HROE-EINTR-VA  PIC S9(04)  COMP.
040907     10 NI-MA-HROE-EINTR-VB  PIC S9(04)  COMP.
           10 NI-MA-BEFBED-DVAG-07 PIC S9(04)  COMP.
        05  DCL-VIVBI004.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 MA-LAST-UPDATE       PIC X(26).
           10 KTY-MA-TYP           PIC S9(4) USAGE COMP.
           10 KNR-MA-TYP           PIC S9(4) USAGE COMP.
           10 MA-BDV-MITGLIED      PIC X(1).
           10 MA-STRAFE            PIC X(1).
           10 MA-VERSANDSTOP       PIC X(1).
           10 MA-ZV-ZVII-ABL       PIC X(1).
           10 MA-RCK-STZ           PIC S999V99 USAGE COMP-3.
           10 MA-RCK-STZ-DAT       PIC X(10).
           10 MA-FUEH-ZEUG-DAT     PIC X(10).
           10 MA-SCHUFA-DAT        PIC X(10).
           10 MA-34C-DAT           PIC X(10).
           10 MA-AFO               PIC X(1).
           10 MA-DARLEHEN          PIC X(1).
           10 MA-AVAG-DVAG         PIC X(10).
           10 MA-KOMPASS-AVAG      PIC X(10).
           10 MA-AUTOTELEFON       PIC X(26).
           10 MA-TELETEXT          PIC X(26).
           10 MA-ZV-NR             PIC X(1).
           10 MA-ZV-DAT-BIS-R      PIC X(10).
           10 MA-ZV-DAT-BIS        PIC X(10).
           10 MA-ZV-DAT-VON        PIC X(10).
           10 MA-ZV-KUE-DAT        PIC X(10).
           10 MA-ED-ZUSATZ         PIC S9(4) USAGE COMP.
           10 MA-ZV-VERL           PIC X(1).
           10 MA-FG-DAT            PIC X(10).
           10 MA-127-DAT           PIC X(10).
           10 MA-VA-DAT            PIC X(10).
           10 MA-ANWERBER          PIC S9(9) USAGE COMP.
           10 MA-BEFBED-NEU        PIC X(10).
           10 MA-STEUER-NR         PIC X(15).
           10 MA-GEWERBE-DAT       PIC X(10).
           10 MA-GRUNDSTUECKE      PIC X(1).
           10 MA-127-UNBES-DAT     PIC X(10).
           10 MA-BEFBED-WIEN-07    PIC X(10).
           10 MA-REGISTER-NR       PIC X(30).
           10 MA-SCHULDNER-VERZ    PIC X(10).
           10 MA-GEW-ZENTR-REG     PIC X(10).
           10 MA-INSOLVENZ-VERZ    PIC X(10).
           10 MA-AM-AGENT-NR       PIC S9(9) USAGE COMP.
           10 MA-EINTR-DAT-AM      PIC X(10).
           10 MA-HRD-ORT           PIC X(40).
240807*    10 MA-HRD-EINTR         PIC X(5).
240807     10 MA-HRD-EINTR         PIC X(8).
040907*    10 MA-HROE-EINTR-01     PIC X(25).
040907*    10 MA-HROE-EINTR-02     PIC X(25).
040907     10 MA-HROE-EINTR-VA     PIC X(25).
040907     10 MA-HROE-EINTR-VB     PIC X(25).
           10 MA-BEFBED-DVAG-07    PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      ******************************************************************
