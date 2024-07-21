      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3565.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTMA3565.CPv  $
      *
      *   Rev 2.2   Apr 12 2016 11:26:08   t13ac20
      *AMA00268: USt: LUCY II - KOMA0508-Zugriffe - Copies
      *
      *   Rev 2.1   Jun 28 2013 11:18:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3565
CCC005*    EXEC SQL DECLARE OVSTAMMDATEN00 TABLE
CCC004*     (VERSION              CHAR(1)              NOT NULL
CCC004*     ,VG                   SMALLINT             NOT NULL
CCC004*     ,VBNR                 INTEGER              NOT NULL
CCC004*     ,HIST_KZN             CHAR(1)              NOT NULL
CCC004*     ,GUELTIG_AB           DATE                 NOT NULL
CCC004*     ,GUELTIG_BIS          DATE                 NOT NULL
CCC004*     ,KTO_FKT              CHAR(2)              NOT NULL
CCC004*     ,EROEFF_DAT           DATE                 NOT NULL
CCC004*     ,SCHLIE_DAT           DATE                 NOT NULL
CCC004*     ,EINR_DAT             DATE                 NOT NULL
CCC004*     ,GEB_DAT              DATE                 NOT NULL
CCC004*     ,DIR_NR               SMALLINT             NOT NULL
CCC004*     ,STRUK_STUFE          SMALLINT             NOT NULL
CCC004*     ,STRUK_STUFE_TEXT     CHAR(4)              NOT NULL
CCC004*     ,STELLEN_TYP          SMALLINT             NOT NULL
CCC004*     ,PRAXIS_STUFE         SMALLINT             NOT NULL
CCC004*     ,UMST_NR              CHAR(15)             NOT NULL
CCC004*     ,REGISTER_NR          CHAR(30)             NOT NULL
CCC004*     ,AM_AGENT_NR          INTEGER              NOT NULL
CCC004*     ,VERRECHNKTO_KZ       CHAR(1)              NOT NULL
CCC004*     ,MA_ALLFI_STATUS      CHAR(1)              NOT NULL
CCC004*     ,MA_KEINE_ERF_PROV    CHAR(1)              NOT NULL
CCC004*     ,AVERM_KZN            CHAR(1)              NOT NULL
CCC004*     ,MA_CKV_WECHSEL       CHAR(1)              NOT NULL
CCC004*     ,MA_CKV_ERFPROV       CHAR(1)              NOT NULL
CCC004*     ,SCHLIE_DAT_ORIG      DATE                 NOT NULL
CCC004*     ,STRUK_STUFE_ORIG     SMALLINT             NOT NULL
CCC004*     ,MA_SCHW_PARTNER      CHAR(1)              NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL
CCC004*     ,KNR_USTAT_TYP        SMALLINT             NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3565
        01 OVSTAMMDATEN00-SATZ.
         10 VERSION                           PIC X(1).
         10 VG                                PIC S9(4) COMP.
         10 VBNR                              PIC S9(9) COMP.
         10 HIST-KZN                          PIC X(1).
         10 GUELTIG-AB                        PIC X(10).
         10 GUELTIG-BIS                       PIC X(10).
         10 KTO-FKT                           PIC X(2).
         10 EROEFF-DAT                        PIC X(10).
         10 SCHLIE-DAT                        PIC X(10).
         10 EINR-DAT                          PIC X(10).
         10 GEB-DAT                           PIC X(10).
         10 DIR-NR                            PIC S9(4) COMP.
         10 STRUK-STUFE                       PIC S9(4) COMP.
         10 STRUK-STUFE-TEXT                  PIC X(4).
         10 STELLEN-TYP                       PIC S9(4) COMP.
         10 PRAXIS-STUFE                      PIC S9(4) COMP.
         10 UMST-NR                           PIC X(15).
         10 REGISTER-NR                       PIC X(30).
         10 AM-AGENT-NR                       PIC S9(9) COMP.
         10 VERRECHNKTO-KZ                    PIC X(1).
         10 MA-ALLFI-STATUS                   PIC X(1).
         10 MA-KEINE-ERF-PROV                 PIC X(1).
         10 AVERM-KZN                         PIC X(1).
         10 MA-CKV-WECHSEL                    PIC X(1).
         10 MA-CKV-ERFPROV                    PIC X(1).
         10 SCHLIE-DAT-ORIG                   PIC X(10).
         10 STRUK-STUFE-ORIG                  PIC S9(4) COMP.
         10 MA-SCHW-PARTNER                   PIC X(1).
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).
         10 KNR-USTAT-TYP                     PIC S9(4) COMP.

