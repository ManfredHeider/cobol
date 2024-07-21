      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0022.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTPA0022.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:26:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   06 Aug 1998 13:03:56   T01JM2Y
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.1   20 Oct 1997 17:33:44   T13EX22
      *AFPA0018 - DCLGEN fuer TBPA0022
      *
      *   Rev 1.0   04 Jul 1997 08:01:08   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0022)                                         *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0022 TABLE
CCC004*    ( PEWA_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PEWA_PIN                       INTEGER NOT NULL,
CCC004*      PEWA_VNR                       CHAR(15) NOT NULL,
CCC004*      PEWA_VOR_VNR_KZ                CHAR(1) NOT NULL,
CCC004*      PEWA_KUNDE_NAME                CHAR(25) NOT NULL,
CCC004*      PEWA_KUNDE_VNAME               CHAR(20) NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KNR_ZAHLWEISE                  SMALLINT NOT NULL,
CCC004*      PEWA_DIR                       SMALLINT NOT NULL,
CCC004*      PEWA_AVERM                     INTEGER NOT NULL,
CCC004*      PEWA_VM                        INTEGER NOT NULL,
CCC004*      PEWA_BEGINN                    DATE NOT NULL,
CCC004*      PEWA_AN_AUFN_DAT               DATE NOT NULL,
CCC004*      PEWA_STRUK_DAT                 DATE NOT NULL,
CCC004*      PEWA_AN_ARB_DAT                DATE NOT NULL,
CCC004*      PEWA_PG_WV_DAT                 DATE NOT NULL,
CCC004*      PEWA_PG_MELD_DAT               DATE NOT NULL,
CCC004*      PDTA_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PEWA_VSTAT                     CHAR(4) NOT NULL,
CCC004*      PEWA_VSTAT_UNR                 SMALLINT NOT NULL,
CCC004*      PEWA_FEHLER_ZAHL               SMALLINT NOT NULL,
CCC004*      KNR_PEWA_ZUSTAND               SMALLINT NOT NULL,
CCC004*      KNR_STORNO_KZ                  SMALLINT NOT NULL,
CCC004*      PEWA_STORNO_DAT                DATE NOT NULL,
CCC004*      KNR_RUECKMELD_STAT             SMALLINT NOT NULL,
CCC004*      PEWA_RUECKMELD_DAT             DATE NOT NULL,
CCC004*      PEWA_ANZ_MELD_PG               SMALLINT NOT NULL,
CCC004*      PEWA_PEST_VSTAT                CHAR(4) NOT NULL,
CCC004*      PEWA_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PEWA_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      PEWAH_LAST_UPDATE              TIMESTAMP NOT NULL,
CCC004*      PEWAH_KENNUNG                  CHAR(8) NOT NULL,
CCC004*      PEWA_PROV_STRUK                VARCHAR(320) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0022                           *
      ******************************************************************
       01  TBPA0022-GESAMT.
         05  DCL-TBPA0022.
           10 PEWA-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PEWA-PIN             PIC S9(9) USAGE COMP.
           10 PEWA-VNR             PIC X(15).
           10 PEWA-VOR-VNR-KZ      PIC X(1).
           10 PEWA-KUNDE-NAME      PIC X(25).
           10 PEWA-KUNDE-VNAME     PIC X(20).
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KNR-ZAHLWEISE        PIC S9(4) USAGE COMP.
           10 PEWA-DIR             PIC S9(4) USAGE COMP.
           10 PEWA-AVERM           PIC S9(9) USAGE COMP.
           10 PEWA-VM              PIC S9(9) USAGE COMP.
           10 PEWA-BEGINN          PIC X(10).
           10 PEWA-AN-AUFN-DAT     PIC X(10).
           10 PEWA-STRUK-DAT       PIC X(10).
           10 PEWA-AN-ARB-DAT      PIC X(10).
           10 PEWA-PG-WV-DAT       PIC X(10).
           10 PEWA-PG-MELD-DAT     PIC X(10).
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PEWA-VSTAT           PIC X(4).
           10 PEWA-VSTAT-UNR       PIC S9(4) USAGE COMP.
           10 PEWA-FEHLER-ZAHL     PIC S9(4) USAGE COMP.
           10 KNR-PEWA-ZUSTAND     PIC S9(4) USAGE COMP.
           10 KNR-STORNO-KZ        PIC S9(4) USAGE COMP.
           10 PEWA-STORNO-DAT      PIC X(10).
           10 KNR-RUECKMELD-STAT   PIC S9(4) USAGE COMP.
           10 PEWA-RUECKMELD-DAT   PIC X(10).
           10 PEWA-ANZ-MELD-PG     PIC S9(4) USAGE COMP.
           10 PEWA-PEST-VSTAT      PIC X(4).
           10 PEWA-LAST-UPDATE     PIC X(26).
           10 PEWA-KENNUNG         PIC X(8).
           10 PEWAH-LAST-UPDATE    PIC X(26).
           10 PEWAH-KENNUNG        PIC X(8).
           10 PEWA-PROV-STRUK.
              49 PEWA-PROV-STRUK-LEN  PIC S9(4) USAGE COMP.
              49 PEWA-PROV-STRUK-TEXT  PIC X(320).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 34      *
      ******************************************************************
