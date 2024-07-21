      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.08.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBK004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBK004.CPv  $
      *
      *   Rev 2.1   Oct 14 2008 11:20:28   t13ae13
      *AVB00981: Erweiterung der Host-Stammdaten für österr. VB
      *
      *   Rev 2.0   Aug 21 2008 11:03:04   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVBK004                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    21.08.2008                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00981!21.08.08!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVBK004)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBK004))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBK004 TABLE
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
CCC004*      MA_GRUNDSTUECKE                CHAR(1),
CCC004*      MA_127_UNBES_DAT               DATE,
CCC004*      MA_BEFBED_WIEN_07              DATE,
CCC004*      MA_REGISTER_NR                 CHAR(30),
CCC004*      MA_SCHULDNER_VERZ              DATE,
CCC004*      MA_GEW_ZENTR_REG               DATE,
CCC004*      MA_INSOLVENZ_VERZ              DATE,
CCC004*      MA_AM_AGENT_NR                 INTEGER,
CCC004*      MA_EINTR_DAT_AM                DATE,
CCC004*      MA_HRD_ORT                     CHAR(40),
CCC004*      MA_HRD_EINTR                   CHAR(8),
CCC004*      MA_HROE_EINTR_VA               CHAR(25),
CCC004*      MA_HROE_EINTR_VB               CHAR(25),
CCC004*      MA_BEFBED_DVAG_07              DATE,
CCC004*      KTY_NOTFALL_TYP                SMALLINT,
CCC004*      KNR_NOTFALL_TYP                SMALLINT,
CCC004*      MA_34C_AMT                     CHAR(40),
CCC004*      MA_34C_STRASSE                 CHAR(40),
CCC004*      MA_34C_PLZ                     CHAR(09),
CCC004*      MA_34C_ORT                     CHAR(40),
CCC004*      MA_ZV_ABSICHERG                DECIMAL(11, 2),
CCC004*      MA_FG_VOM                      DATE,
CCC004*      MA_127_VOM                     DATE,
CCC004*      MA_VA_VOM                      DATE,
CCC004*      MA_FUEH_ZEUG_VOM               DATE,
CCC004*      MA_SCHUFA_VOM                  DATE,
CCC004*      MA_GEW_Z_REG_VOM               DATE,
CCC004*      MA_ANLAGE_BER                  CHAR(1),
CCC004*      MA_HROE_EINTR_FG               CHAR(25)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBK004                          *
      ******************************************************************
       01  VIVBK004-GESAMT.
        05  NI-VIVBK004-TAB.
           10  NI-VIVBK004-ELE     PIC S9(04)  COMP   OCCURS  62 TIMES.
        05  NI-VIVBK004           REDEFINES   NI-VIVBK004-TAB.
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
           10 NI-MA-HROE-EINTR-VA  PIC S9(04)  COMP.
           10 NI-MA-HROE-EINTR-VB  PIC S9(04)  COMP.
           10 NI-MA-BEFBED-DVAG-07 PIC S9(04)  COMP.
           10 NI-KTY-NOTFALL-TYP   PIC S9(04)  COMP.
           10 NI-KNR-NOTFALL-TYP   PIC S9(04)  COMP.
           10 NI-MA-34C-AMT        PIC S9(04)  COMP.
           10 NI-MA-34C-STRASSE    PIC S9(04)  COMP.
           10 NI-MA-34C-PLZ        PIC S9(04)  COMP.
           10 NI-MA-34C-ORT        PIC S9(04)  COMP.
           10 NI-MA-ZV-ABSICHERG   PIC S9(04)  COMP.
           10 NI-MA-FG-VOM         PIC S9(04)  COMP.
           10 NI-MA-127-VOM        PIC S9(04)  COMP.
           10 NI-MA-VA-VOM         PIC S9(04)  COMP.
           10 NI-MA-FUEH-ZEUG-VOM  PIC S9(04)  COMP.
           10 NI-MA-SCHUFA-VOM     PIC S9(04)  COMP.
           10 NI-MA-GEW-Z-REG-VOM  PIC S9(04)  COMP.
           10 NI-MA-ANLAGE-BER     PIC S9(04)  COMP.
           10 NI-MA-HROE-EINTR-FG  PIC S9(04)  COMP.
        05  DCL-VIVBK004.
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
           10 MA-HRD-EINTR         PIC X(8).
           10 MA-HROE-EINTR-VA     PIC X(25).
           10 MA-HROE-EINTR-VB     PIC X(25).
           10 MA-BEFBED-DVAG-07    PIC X(10).
           10 KTY-NOTFALL-TYP      PIC S9(4) USAGE COMP.
           10 KNR-NOTFALL-TYP      PIC S9(4) USAGE COMP.
           10 MA-34C-AMT           PIC X(40).
           10 MA-34C-STRASSE       PIC X(40).
           10 MA-34C-PLZ           PIC X(09).
           10 MA-34C-ORT           PIC X(40).
           10 MA-ZV-ABSICHERG      PIC S9(9)V99 USAGE COMP-3.
           10 MA-FG-VOM            PIC X(10).
           10 MA-127-VOM           PIC X(10).
           10 MA-VA-VOM            PIC X(10).
           10 MA-FUEH-ZEUG-VOM     PIC X(10).
           10 MA-SCHUFA-VOM        PIC X(10).
           10 MA-GEW-Z-REG-VOM     PIC X(10).
           10 MA-ANLAGE-BER        PIC X(1).
           10 MA-HROE-EINTR-FG     PIC X(25).

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 62      *
      ******************************************************************
