      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.02.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBJ004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBJ004.CPv  $
      *
      *   Rev 2.1   Apr 23 2008 11:17:16   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - M
      *A (JMS)
      *
      *   Rev 2.0   Feb 29 2008 11:05:30   t13ae48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVBJ004                                      *
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
      ******************************************************************
      * DCLGEN TABLE(VIVBJ004)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBJ004))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBJ004 TABLE
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
CCC004*      MA_34C_ORT                     CHAR(40)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBJ004                          *
      ******************************************************************
       01  VIVBJ004-GESAMT.
        05  NI-VIVBJ004-TAB.
           10  NI-VIVBJ004-ELE     PIC S9(04)  COMP   OCCURS  53 TIMES.
        05  NI-VIVBJ004           REDEFINES   NI-VIVBJ004-TAB.
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
        05  DCL-VIVBJ004.
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

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 53      *
      ******************************************************************
