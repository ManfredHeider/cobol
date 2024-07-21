000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   CVVBC004.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/CVVBC004.CPv  $
000140*
000150*   Rev 2.1   Feb 14 2002 14:41:58   t13ae30
000160*AVB00527 - Erfassen Gewerbeschein für DVAG-Wien
000170*
000180*   Rev 2.0   Dec 27 2001 14:35:32   p13sp20
000190*Archiverstellung
000200*
000210******************************************************************
000220* DCLGEN TABLE(VIVBC004)                                         *
000230*        LIBRARY(T130005.DB2PILOT.SRCDCL(CVVBC004))              *
000240*        ACTION(REPLACE)                                         *
000250*        QUOTE                                                   *
000260* ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
000270******************************************************************
CCC005*    EXEC SQL DECLARE VIVBC004 TABLE
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
CCC004*      MA_ED_ZUSATZ                   SMALLINT NOT NULL,
CCC004*      MA_ZV_VERL                     CHAR(1) NOT NULL,
CCC004*      MA_FG_DAT                      DATE,
CCC004*      MA_127_DAT                     DATE,
CCC004*      MA_VA_DAT                      DATE,
CCC004*      MA_ANWERBER                    INTEGER NOT NULL
CCC003*    ) END-EXEC.
000600******************************************************************
000610* COBOL DECLARATION FOR TABLE VIVBC004                           *
000620******************************************************************
000630 01  VIVBC004-GESAMT.
000640  05  NI-VIVBC004-TAB.
000650     10  NI-VIVBC004-ELE     PIC S9(04)  COMP   OCCURS  30 TIMES.
000660  05  NI-VIVBC004            REDEFINES   NI-VIVBC004-TAB.
000670     10 NI-MA-ST-NR          PIC S9(04)  COMP.
000680     10 NI-MA-LAST-UPDATE    PIC S9(04)  COMP.
000690     10 NI-KTY-MA-TYP        PIC S9(04)  COMP.
000700     10 NI-KNR-MA-TYP        PIC S9(04)  COMP.
000710     10 NI-MA-BDV-MITGLIED   PIC S9(04)  COMP.
000720     10 NI-MA-STRAFE         PIC S9(04)  COMP.
000730     10 NI-MA-VERSANDSTOP    PIC S9(04)  COMP.
000740     10 NI-MA-ZV-ZVII-ABL    PIC S9(04)  COMP.
000750     10 NI-MA-RCK-STZ        PIC S9(04)  COMP.
000760     10 NI-MA-RCK-STZ-DAT    PIC S9(04)  COMP.
000770     10 NI-MA-FUEH-ZEUG-DAT  PIC S9(04)  COMP.
000780     10 NI-MA-SCHUFA-DAT     PIC S9(04)  COMP.
000790     10 NI-MA-34C-DAT        PIC S9(04)  COMP.
000800     10 NI-MA-AFO            PIC S9(04)  COMP.
000810     10 NI-MA-DARLEHEN       PIC S9(04)  COMP.
000820     10 NI-MA-AVAG-DVAG      PIC S9(04)  COMP.
000830     10 NI-MA-KOMPASS-AVAG   PIC S9(04)  COMP.
000840     10 NI-MA-AUTOTELEFON    PIC S9(04)  COMP.
000850     10 NI-MA-TELETEXT       PIC S9(04)  COMP.
000860     10 NI-MA-ZV-NR          PIC S9(04)  COMP.
000870     10 NI-MA-ZV-DAT-BIS-R   PIC S9(04)  COMP.
000880     10 NI-MA-ZV-DAT-BIS     PIC S9(04)  COMP.
000890     10 NI-MA-ZV-DAT-VON     PIC S9(04)  COMP.
000900     10 NI-MA-ZV-KUE-DAT     PIC S9(04)  COMP.
000910     10 NI-MA-ED-ZUSATZ      PIC S9(04)  COMP.
000920     10 NI-MA-ZV-VERL        PIC S9(04)  COMP.
000930     10 NI-MA-FG-DAT         PIC S9(04)  COMP.
000940     10 NI-MA-127-DAT        PIC S9(04)  COMP.
000950     10 NI-MA-VA-DAT         PIC S9(04)  COMP.
000960     10 NI-MA-ANWERBER       PIC S9(04)  COMP.
000970  05  DCL-VIVBC004.
000980     10 MA-ST-NR             PIC S9(9) USAGE COMP.
000990     10 MA-LAST-UPDATE       PIC X(26).
001000     10 KTY-MA-TYP           PIC S9(4) USAGE COMP.
001010     10 KNR-MA-TYP           PIC S9(4) USAGE COMP.
001020     10 MA-BDV-MITGLIED      PIC X(1).
001030     10 MA-STRAFE            PIC X(1).
001040     10 MA-VERSANDSTOP       PIC X(1).
001050     10 MA-ZV-ZVII-ABL       PIC X(1).
001060     10 MA-RCK-STZ           PIC S999V99 USAGE COMP-3.
001070     10 MA-RCK-STZ-DAT       PIC X(10).
001080     10 MA-FUEH-ZEUG-DAT     PIC X(10).
001090     10 MA-SCHUFA-DAT        PIC X(10).
001100     10 MA-34C-DAT           PIC X(10).
001110     10 MA-AFO               PIC X(1).
001120     10 MA-DARLEHEN          PIC X(1).
001130     10 MA-AVAG-DVAG         PIC X(10).
001140     10 MA-KOMPASS-AVAG      PIC X(10).
001150     10 MA-AUTOTELEFON       PIC X(14).
001160     10 MA-TELETEXT          PIC X(14).
001170     10 MA-ZV-NR             PIC X(1).
001180     10 MA-ZV-DAT-BIS-R      PIC X(10).
001190     10 MA-ZV-DAT-BIS        PIC X(10).
001200     10 MA-ZV-DAT-VON        PIC X(10).
001210     10 MA-ZV-KUE-DAT        PIC X(10).
001220     10 MA-ED-ZUSATZ         PIC S9(4) USAGE COMP.
001230     10 MA-ZV-VERL           PIC X(1).
001240     10 MA-FG-DAT            PIC X(10).
001250     10 MA-127-DAT           PIC X(10).
001260     10 MA-VA-DAT            PIC X(10).
001270     10 MA-ANWERBER          PIC S9(9) USAGE COMP.

001290******************************************************************
001300* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 30      *
001310******************************************************************
