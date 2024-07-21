      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.11.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBO004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBO004.CPv  $
      *
      *   Rev 2.1   Nov 09 2015 10:51:48   t13ae13
      *AVB01357: Umgestaltung GISA / Steuernummer
      *
      *   Rev 2.0   Nov 02 2015 10:20:02   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: WALB                                             *
      *    DATUM:    05.11.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01357!05.11.15!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBO004) MA                                     *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBO004)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBO004 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , MA_LAST_UPDATE              TIMESTAMP NOT NULL
CCC004*    , KTY_MA_TYP                  SMALLINT NOT NULL
CCC004*    , KNR_MA_TYP                  SMALLINT NOT NULL
CCC004*    , MA_BDV_MITGLIED             CHAR(01) NOT NULL
CCC004*    , MA_STRAFE                   CHAR(01) NOT NULL
CCC004*    , MA_VERSANDSTOP              CHAR(01) NOT NULL
CCC004*    , MA_ZV_ZVII_ABL              CHAR(01) NOT NULL
CCC004*    , MA_RCK_STZ                  DECIMAL(05, 2) NOT NULL
CCC004*    , MA_RCK_STZ_DAT              DATE NOT NULL
CCC004*    , MA_FUEH_ZEUG_DAT            DATE
CCC004*    , MA_SCHUFA_DAT               DATE
CCC004*    , MA_34C_DAT                  DATE
CCC004*    , MA_AFO                      CHAR(01) NOT NULL
CCC004*    , MA_DARLEHEN                 CHAR(01) NOT NULL
CCC004*    , MA_AVAG_DVAG                DATE
CCC004*    , MA_KOMPASS_AVAG             DATE
CCC004*    , MA_AUTOTELEFON              CHAR(26) NOT NULL
CCC004*    , MA_TELETEXT                 CHAR(26) NOT NULL
CCC004*    , MA_ZV_NR                    CHAR(01) NOT NULL
CCC004*    , MA_ZV_DAT_BIS_R             DATE
CCC004*    , MA_ZV_DAT_BIS               DATE
CCC004*    , MA_ZV_DAT_VON               DATE
CCC004*    , MA_ZV_KUE_DAT               DATE
CCC004*    , MA_ED_ZUSATZ                SMALLINT NOT NULL
CCC004*    , MA_ZV_VERL                  CHAR(01) NOT NULL
CCC004*    , MA_FG_DAT                   DATE
CCC004*    , MA_127_DAT                  DATE
CCC004*    , MA_VA_DAT                   DATE
CCC004*    , MA_ANWERBER                 INTEGER
CCC004*    , MA_BEFBED_NEU               DATE
CCC004*    , MA_STEUER_NR                CHAR(15)
CCC004*    , MA_GEWERBE_DAT              DATE
CCC004*    , MA_GRUNDSTUECKE             CHAR(01) NOT NULL
CCC004*    , MA_127_UNBES_DAT            DATE
CCC004*    , MA_BEFBED_WIEN_07           DATE
CCC004*    , MA_REGISTER_NR              CHAR(30)
CCC004*    , MA_SCHULDNER_VERZ           DATE
CCC004*    , MA_GEW_ZENTR_REG            DATE
CCC004*    , MA_INSOLVENZ_VERZ           DATE
CCC004*    , MA_AM_AGENT_NR              INTEGER
CCC004*    , MA_EINTR_DAT_AM             DATE
CCC004*    , MA_HRD_ORT                  CHAR(40)
CCC004*    , MA_HRD_EINTR                CHAR(08)
CCC004*    , MA_HROE_EINTR_VA            CHAR(25)
CCC004*    , MA_HROE_EINTR_VB            CHAR(25)
CCC004*    , MA_BEFBED_DVAG_07           DATE
CCC004*    , KTY_NOTFALL_TYP             SMALLINT NOT NULL
CCC004*    , KNR_NOTFALL_TYP             SMALLINT NOT NULL
CCC004*    , MA_34C_AMT                  CHAR(40)
CCC004*    , MA_34C_STRASSE              CHAR(40)
CCC004*    , MA_34C_PLZ                  CHAR(09)
CCC004*    , MA_34C_ORT                  CHAR(40)
CCC004*    , MA_ZV_ABSICHERG             DECIMAL(11, 2)
CCC004*    , MA_FG_VOM                   DATE
CCC004*    , MA_127_VOM                  DATE
CCC004*    , MA_VA_VOM                   DATE
CCC004*    , MA_FUEH_ZEUG_VOM            DATE
CCC004*    , MA_SCHUFA_VOM               DATE
CCC004*    , MA_GEW_Z_REG_VOM            DATE
CCC004*    , MA_ANLAGE_BER               CHAR(01) NOT NULL
CCC004*    , MA_HROE_EINTR_FG            CHAR(25)
CCC004*    , MA_VSH_POLNR                CHAR(25)
CCC004*    , MA_VSH_DAT                  DATE
CCC004*    , MA_VSH_VOM                  DATE
CCC004*    , MA_HROE_EINTR_WG            CHAR(25)
CCC004*    , MA_WG_DAT                   DATE
CCC004*    , MA_WG_VOM                   DATE
CCC004*    , MA_BEFBED_2011              DATE
CCC004*    , MA_34F_DAT                  DATE
CCC004*    , MA_34F_ANT_SCH              CHAR(01) NOT NULL
CCC004*    , MA_34F_AFO                  CHAR(01) NOT NULL
CCC004*    , MA_34F_ANL_BER              CHAR(01) NOT NULL
CCC004*    , MA_34F_AMT                  CHAR(40)
CCC004*    , MA_34F_STRASSE              CHAR(40)
CCC004*    , MA_34F_PLZ                  CHAR(09)
CCC004*    , MA_34F_ORT                  CHAR(40)
CCC004*    , MA_34C_ANT_SCH              CHAR(01) NOT NULL
CCC004*    , MA_34F_REG_NR               CHAR(15)
CCC004*    , MA_34I_DAT                  DATE
CCC004*    , MA_34I_AMT                  CHAR(40)
CCC004*    , MA_34I_STRASSE              CHAR(40)
CCC004*    , MA_34I_PLZ                  CHAR(09)
CCC004*    , MA_34I_ORT                  CHAR(40)
CCC004*    , MA_34I_REG_NR               CHAR(15)
CCC004*    , MA_SOZ_VSNR_WIEN            CHAR(15)
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBO004                           *
      *****************************************************************

       01  VIVBO004-GESAMT.
        05 NI-VIVBO004-TAB.
           10 NI-VIVBO004-ELE      PIC S9(04)  BINARY OCCURS  86.
        05 NI-VIVBO004             REDEFINES   NI-VIVBO004-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-MA-LAST-UPDATE           PIC S9(04)       BINARY.
           10 NI-KTY-MA-TYP               PIC S9(04)       BINARY.
           10 NI-KNR-MA-TYP               PIC S9(04)       BINARY.
           10 NI-MA-BDV-MITGLIED          PIC S9(04)       BINARY.
           10 NI-MA-STRAFE                PIC S9(04)       BINARY.
           10 NI-MA-VERSANDSTOP           PIC S9(04)       BINARY.
           10 NI-MA-ZV-ZVII-ABL           PIC S9(04)       BINARY.
           10 NI-MA-RCK-STZ               PIC S9(04)       BINARY.
           10 NI-MA-RCK-STZ-DAT           PIC S9(04)       BINARY.
           10 NI-MA-FUEH-ZEUG-DAT         PIC S9(04)       BINARY.
           10 NI-MA-SCHUFA-DAT            PIC S9(04)       BINARY.
           10 NI-MA-34C-DAT               PIC S9(04)       BINARY.
           10 NI-MA-AFO                   PIC S9(04)       BINARY.
           10 NI-MA-DARLEHEN              PIC S9(04)       BINARY.
           10 NI-MA-AVAG-DVAG             PIC S9(04)       BINARY.
           10 NI-MA-KOMPASS-AVAG          PIC S9(04)       BINARY.
           10 NI-MA-AUTOTELEFON           PIC S9(04)       BINARY.
           10 NI-MA-TELETEXT              PIC S9(04)       BINARY.
           10 NI-MA-ZV-NR                 PIC S9(04)       BINARY.
           10 NI-MA-ZV-DAT-BIS-R          PIC S9(04)       BINARY.
           10 NI-MA-ZV-DAT-BIS            PIC S9(04)       BINARY.
           10 NI-MA-ZV-DAT-VON            PIC S9(04)       BINARY.
           10 NI-MA-ZV-KUE-DAT            PIC S9(04)       BINARY.
           10 NI-MA-ED-ZUSATZ             PIC S9(04)       BINARY.
           10 NI-MA-ZV-VERL               PIC S9(04)       BINARY.
           10 NI-MA-FG-DAT                PIC S9(04)       BINARY.
           10 NI-MA-127-DAT               PIC S9(04)       BINARY.
           10 NI-MA-VA-DAT                PIC S9(04)       BINARY.
           10 NI-MA-ANWERBER              PIC S9(04)       BINARY.
           10 NI-MA-BEFBED-NEU            PIC S9(04)       BINARY.
           10 NI-MA-STEUER-NR             PIC S9(04)       BINARY.
           10 NI-MA-GEWERBE-DAT           PIC S9(04)       BINARY.
           10 NI-MA-GRUNDSTUECKE          PIC S9(04)       BINARY.
           10 NI-MA-127-UNBES-DAT         PIC S9(04)       BINARY.
           10 NI-MA-BEFBED-WIEN-07        PIC S9(04)       BINARY.
           10 NI-MA-REGISTER-NR           PIC S9(04)       BINARY.
           10 NI-MA-SCHULDNER-VERZ        PIC S9(04)       BINARY.
           10 NI-MA-GEW-ZENTR-REG         PIC S9(04)       BINARY.
           10 NI-MA-INSOLVENZ-VERZ        PIC S9(04)       BINARY.
           10 NI-MA-AM-AGENT-NR           PIC S9(04)       BINARY.
           10 NI-MA-EINTR-DAT-AM          PIC S9(04)       BINARY.
           10 NI-MA-HRD-ORT               PIC S9(04)       BINARY.
           10 NI-MA-HRD-EINTR             PIC S9(04)       BINARY.
           10 NI-MA-HROE-EINTR-VA         PIC S9(04)       BINARY.
           10 NI-MA-HROE-EINTR-VB         PIC S9(04)       BINARY.
           10 NI-MA-BEFBED-DVAG-07        PIC S9(04)       BINARY.
           10 NI-KTY-NOTFALL-TYP          PIC S9(04)       BINARY.
           10 NI-KNR-NOTFALL-TYP          PIC S9(04)       BINARY.
           10 NI-MA-34C-AMT               PIC S9(04)       BINARY.
           10 NI-MA-34C-STRASSE           PIC S9(04)       BINARY.
           10 NI-MA-34C-PLZ               PIC S9(04)       BINARY.
           10 NI-MA-34C-ORT               PIC S9(04)       BINARY.
           10 NI-MA-ZV-ABSICHERG          PIC S9(04)       BINARY.
           10 NI-MA-FG-VOM                PIC S9(04)       BINARY.
           10 NI-MA-127-VOM               PIC S9(04)       BINARY.
           10 NI-MA-VA-VOM                PIC S9(04)       BINARY.
           10 NI-MA-FUEH-ZEUG-VOM         PIC S9(04)       BINARY.
           10 NI-MA-SCHUFA-VOM            PIC S9(04)       BINARY.
           10 NI-MA-GEW-Z-REG-VOM         PIC S9(04)       BINARY.
           10 NI-MA-ANLAGE-BER            PIC S9(04)       BINARY.
           10 NI-MA-HROE-EINTR-FG         PIC S9(04)       BINARY.
           10 NI-MA-VSH-POLNR             PIC S9(04)       BINARY.
           10 NI-MA-VSH-DAT               PIC S9(04)       BINARY.
           10 NI-MA-VSH-VOM               PIC S9(04)       BINARY.
           10 NI-MA-HROE-EINTR-WG         PIC S9(04)       BINARY.
           10 NI-MA-WG-DAT                PIC S9(04)       BINARY.
           10 NI-MA-WG-VOM                PIC S9(04)       BINARY.
           10 NI-MA-BEFBED-2011           PIC S9(04)       BINARY.
           10 NI-MA-34F-DAT               PIC S9(04)       BINARY.
           10 NI-MA-34F-ANT-SCH           PIC S9(04)       BINARY.
           10 NI-MA-34F-AFO               PIC S9(04)       BINARY.
           10 NI-MA-34F-ANL-BER           PIC S9(04)       BINARY.
           10 NI-MA-34F-AMT               PIC S9(04)       BINARY.
           10 NI-MA-34F-STRASSE           PIC S9(04)       BINARY.
           10 NI-MA-34F-PLZ               PIC S9(04)       BINARY.
           10 NI-MA-34F-ORT               PIC S9(04)       BINARY.
           10 NI-MA-34C-ANT-SCH           PIC S9(04)       BINARY.
           10 NI-MA-34F-REG-NR            PIC S9(04)       BINARY.
           10 NI-MA-34I-DAT               PIC S9(04)       BINARY.
           10 NI-MA-34I-AMT               PIC S9(04)       BINARY.
           10 NI-MA-34I-STRASSE           PIC S9(04)       BINARY.
           10 NI-MA-34I-PLZ               PIC S9(04)       BINARY.
           10 NI-MA-34I-ORT               PIC S9(04)       BINARY.
           10 NI-MA-34I-REG-NR            PIC S9(04)       BINARY.
           10 NI-MA-SOZ-VSNR-WIEN         PIC S9(04)       BINARY.

        05 DCL-VIVBO004.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 MA-LAST-UPDATE              PIC  X(26).
           10 KTY-MA-TYP                  PIC S9(04)       BINARY.
           10 KNR-MA-TYP                  PIC S9(04)       BINARY.
           10 MA-BDV-MITGLIED             PIC  X(01).
           10 MA-STRAFE                   PIC  X(01).
           10 MA-VERSANDSTOP              PIC  X(01).
           10 MA-ZV-ZVII-ABL              PIC  X(01).
           10 MA-RCK-STZ                  PIC S9(03)V9(02) COMP-3.
           10 MA-RCK-STZ-DAT              PIC  X(10).
           10 MA-FUEH-ZEUG-DAT            PIC  X(10).
           10 MA-SCHUFA-DAT               PIC  X(10).
           10 MA-34C-DAT                  PIC  X(10).
           10 MA-AFO                      PIC  X(01).
           10 MA-DARLEHEN                 PIC  X(01).
           10 MA-AVAG-DVAG                PIC  X(10).
           10 MA-KOMPASS-AVAG             PIC  X(10).
           10 MA-AUTOTELEFON              PIC  X(26).
           10 MA-TELETEXT                 PIC  X(26).
           10 MA-ZV-NR                    PIC  X(01).
           10 MA-ZV-DAT-BIS-R             PIC  X(10).
           10 MA-ZV-DAT-BIS               PIC  X(10).
           10 MA-ZV-DAT-VON               PIC  X(10).
           10 MA-ZV-KUE-DAT               PIC  X(10).
           10 MA-ED-ZUSATZ                PIC S9(04)       BINARY.
           10 MA-ZV-VERL                  PIC  X(01).
           10 MA-FG-DAT                   PIC  X(10).
           10 MA-127-DAT                  PIC  X(10).
           10 MA-VA-DAT                   PIC  X(10).
           10 MA-ANWERBER                 PIC S9(09)       BINARY.
           10 MA-BEFBED-NEU               PIC  X(10).
           10 MA-STEUER-NR                PIC  X(15).
           10 MA-GEWERBE-DAT              PIC  X(10).
           10 MA-GRUNDSTUECKE             PIC  X(01).
           10 MA-127-UNBES-DAT            PIC  X(10).
           10 MA-BEFBED-WIEN-07           PIC  X(10).
           10 MA-REGISTER-NR              PIC  X(30).
           10 MA-SCHULDNER-VERZ           PIC  X(10).
           10 MA-GEW-ZENTR-REG            PIC  X(10).
           10 MA-INSOLVENZ-VERZ           PIC  X(10).
           10 MA-AM-AGENT-NR              PIC S9(09)       BINARY.
           10 MA-EINTR-DAT-AM             PIC  X(10).
           10 MA-HRD-ORT                  PIC  X(40).
           10 MA-HRD-EINTR                PIC  X(08).
           10 MA-HROE-EINTR-VA            PIC  X(25).
           10 MA-HROE-EINTR-VB            PIC  X(25).
           10 MA-BEFBED-DVAG-07           PIC  X(10).
           10 KTY-NOTFALL-TYP             PIC S9(04)       BINARY.
           10 KNR-NOTFALL-TYP             PIC S9(04)       BINARY.
           10 MA-34C-AMT                  PIC  X(40).
           10 MA-34C-STRASSE              PIC  X(40).
           10 MA-34C-PLZ                  PIC  X(09).
           10 MA-34C-ORT                  PIC  X(40).
           10 MA-ZV-ABSICHERG             PIC S9(09)V9(02) COMP-3.
           10 MA-FG-VOM                   PIC  X(10).
           10 MA-127-VOM                  PIC  X(10).
           10 MA-VA-VOM                   PIC  X(10).
           10 MA-FUEH-ZEUG-VOM            PIC  X(10).
           10 MA-SCHUFA-VOM               PIC  X(10).
           10 MA-GEW-Z-REG-VOM            PIC  X(10).
           10 MA-ANLAGE-BER               PIC  X(01).
           10 MA-HROE-EINTR-FG            PIC  X(25).
           10 MA-VSH-POLNR                PIC  X(25).
           10 MA-VSH-DAT                  PIC  X(10).
           10 MA-VSH-VOM                  PIC  X(10).
           10 MA-HROE-EINTR-WG            PIC  X(25).
           10 MA-WG-DAT                   PIC  X(10).
           10 MA-WG-VOM                   PIC  X(10).
           10 MA-BEFBED-2011              PIC  X(10).
           10 MA-34F-DAT                  PIC  X(10).
           10 MA-34F-ANT-SCH              PIC  X(01).
           10 MA-34F-AFO                  PIC  X(01).
           10 MA-34F-ANL-BER              PIC  X(01).
           10 MA-34F-AMT                  PIC  X(40).
           10 MA-34F-STRASSE              PIC  X(40).
           10 MA-34F-PLZ                  PIC  X(09).
           10 MA-34F-ORT                  PIC  X(40).
           10 MA-34C-ANT-SCH              PIC  X(01).
           10 MA-34F-REG-NR               PIC  X(15).
           10 MA-34I-DAT                  PIC  X(10).
           10 MA-34I-AMT                  PIC  X(40).
           10 MA-34I-STRASSE              PIC  X(40).
           10 MA-34I-PLZ                  PIC  X(09).
           10 MA-34I-ORT                  PIC  X(40).
           10 MA-34I-REG-NR               PIC  X(15).
           10 MA-SOZ-VSNR-WIEN            PIC  X(15).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  86                    *
      *****************************************************************

