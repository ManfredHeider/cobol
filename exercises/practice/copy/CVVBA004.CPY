      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA004.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVBA004.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:09:06   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVBA004)                                         *
      *        LIBRARY(T130005.DB2PILOT.SRCDCL(CVVBA004))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA004 TABLE
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
CCC004*      MA_AUTOTELEFON                 CHAR(14) NOT NULL,
CCC004*      MA_TELETEXT                    CHAR(14) NOT NULL,
CCC004*      MA_ZV_NR                       CHAR(1) NOT NULL,
CCC004*      MA_ZV_DAT_BIS_R                DATE,
CCC004*      MA_ZV_DAT_BIS                  DATE,
CCC004*      MA_ZV_DAT_VON                  DATE,
CCC004*      MA_ZV_KUE_DAT                  DATE,
CCC004*      MA_ED_ZUSATZ                   SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA004                           *
      ******************************************************************
       01  VIVBA004-GESAMT.
        05  NI-VIVBA004-TAB.
           10  NI-VIVBA004-ELE     PIC S9(04)  COMP   OCCURS  25 TIMES.
        05  NI-VIVBA004            REDEFINES   NI-VIVBA004-TAB.
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
        05  DCL-VIVBA004.
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
           10 MA-AUTOTELEFON       PIC X(14).
           10 MA-TELETEXT          PIC X(14).
           10 MA-ZV-NR             PIC X(1).
           10 MA-ZV-DAT-BIS-R      PIC X(10).
           10 MA-ZV-DAT-BIS        PIC X(10).
           10 MA-ZV-DAT-VON        PIC X(10).
           10 MA-ZV-KUE-DAT        PIC X(10).
           10 MA-ED-ZUSATZ         PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 25      *
      ******************************************************************
