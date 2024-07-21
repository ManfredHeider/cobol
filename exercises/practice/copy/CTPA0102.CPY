      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0102.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTPA0102.CPv  $
      *
      *   Rev 2.6   May 14 2018 11:12:26   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.5   Mar 28 2017 10:43:04   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs
      *
      *   Rev 2.4   Mar 28 2017 08:21:42   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs
      *
      *   Rev 2.3   Jul 16 2015 15:03:40   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.2   Mar 10 2014 16:52:16   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.1   Jul 26 2013 11:43:50   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.0   Feb 27 1999 14:26:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:52:12   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:40:08   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !2.0 ! NEUERSTELLUNG             !          *
      * APA01539!26.07.13!2.1 ! ERWEITERT FÜR KALYPSO     ! RAHNER   *
      * APA01932!27.03.15!2.2 ! NEUE FELDER  HINZU        ! STORCH   *
      * APA02353!24.02.17!2.5 ! NEUE FELDER Teleson       ! Buch     *
      * APA02579!24.04.18!2.6 ! GENERALI LEBEN            ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
     ******************************************************************
      * DCLGEN TABLE(TBPA0102)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0102 TABLE
CCC004*    ( PMPG_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMPG_VERSION                   SMALLINT NOT NULL,
CCC004*      PMPG_LFD_NR_VNR                INTEGER NOT NULL,
CCC004*      PDTA_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PMPE_VNR                       CHAR(15) NOT NULL,
CCC004*      PMPE_VERTR_BEGINN              DATE NOT NULL,
CCC004*      PMPE_VERTR_ABLAUF              DATE NOT NULL,
CCC004*      PMPE_FAELLIG_VON               DATE NOT NULL,
CCC004*      PMPE_FAELLIG_BIS               DATE NOT NULL,
CCC004*      PMPE_PG_VBNR                   INTEGER NOT NULL,
CCC004*      PMPE_KUNDE_NAME                CHAR(25) NOT NULL,
CCC004*      PMPE_KUNDE_VNAME               CHAR(20) NOT NULL,
CCC004*      PMPE_GSMT_PROV_DAT             DATE NOT NULL,
CCC004*      PMPE_PG_ABR_DAT                DATE NOT NULL,
CCC004*      PMPE_PG_EINL_DAT               DATE NOT NULL,
CCC004*      PMPE_PIN                       INTEGER NOT NULL,
CCC004*      KNR_ZAHLWEISE                  SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      PMPE_VSTAT                     CHAR(4) NOT NULL,
CCC004*      PMPE_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PMPE_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      PMPE_PG_PM_DATEN               VARCHAR(300) NOT NULL,
CCC004*      PMPE_BZ_NAME                   CHAR(25) NOT NULL,
CCC004*      PMPE_BZ_VNAME                  CHAR(20) NOT NULL,
CCC004*      PMPE_VP_NAME                   CHAR(25) NOT NULL,
CCC004*      PMPE_VP_VNAME                  CHAR(20) NOT NULL,
CCC004*      PMPE_KZ_EIGENV                 CHAR(1) NOT NULL,
CCC004*      KNR_FEIV_KZ                    SMALLINT NOT NULL,
CCC004*      KNR_VERM_GES                   SMALLINT NOT NULL,
CCC004*      PMPE_UMW_NR                    DECIMAL(15, 0) NOT NULL,
CCC004*      PMPE_UNR                       SMALLINT NOT NULL,
CCC004*      PMPE_SCHICHT_NR                SMALLINT NOT NULL,
CCC004*      PMPE_INKST_DAT                 DATE NOT NULL,
CCC004*      KNR_INK_ART                    SMALLINT NOT NULL,
CCC004*      PMPE_SAM_NR                    INTEGER NOT NULL,
CCC004*      KNR_RAB_ST                     SMALLINT NOT NULL,
CCC004*      PMPE_VVG_KZ                    CHAR(1) NOT NULL,
CCC004*      KNR_DEPOT_KZ                   SMALLINT NOT NULL,
CCC004*      PMPE_PROV_SCHL                 CHAR(5) NOT NULL,
CCC004*      PMPE_BEW_DAT                   DATE NOT NULL,
CCC004*      PMPE_URSPR_POL_DAT             DATE NOT NULL,
CCC004*      PMPE_POL_DAT                   DATE NOT NULL,
CCC004*      PMPE_BESVV                     DECIMAL(1, 0) NOT NULL,
CCC004*      PMPE_VN_STRASSE                CHAR(35) NOT NULL,
CCC004*      PMPE_VN_PLZ                    CHAR(5) NOT NULL,
CCC004*      PMPE_VN_ORT                    CHAR(35) NOT NULL,
CCC004*      PMPE_RATENBEITRAG              DECIMAL(11, 2) NOT NULL,
CCC004*      PMPE_PG_RECH_NR                CHAR(30) NOT NULL,
CCC004*      PMPE_HIST_UNR                  SMALLINT NOT NULL,
CCC004*      KNR_TLSN_PARTNER               SMALLINT NOT NULL,
CCC004*      PMPE_GEL_KENNUNG               CHAR(6) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0102                           *
      ******************************************************************
       01  TBPA0102-GESAMT.
         05 DCL-TBPA0102.
            10 PMPG-ID              PIC S9(15)V USAGE COMP-3.
            10 PMPG-VERSION         PIC S9(4)   USAGE COMP.
            10 PMPG-LFD-NR-VNR      PIC S9(9)   USAGE COMP.
            10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
            10 PGEB-GES             PIC S9(4)   USAGE COMP.
            10 PMPE-VNR             PIC X(15).
            10 PMPE-VERTR-BEGINN    PIC X(10).
            10 PMPE-VERTR-ABLAUF    PIC X(10).
            10 PMPE-FAELLIG-VON     PIC X(10).
            10 PMPE-FAELLIG-BIS     PIC X(10).
            10 PMPE-PG-VBNR         PIC S9(9)   USAGE COMP.
            10 PMPE-KUNDE-NAME      PIC X(25).
            10 PMPE-KUNDE-VNAME     PIC X(20).
            10 PMPE-GSMT-PROV-DAT   PIC X(10).
            10 PMPE-PG-ABR-DAT      PIC X(10).
            10 PMPE-PG-EINL-DAT     PIC X(10).
            10 PMPE-PIN             PIC S9(9)   USAGE COMP.
            10 KNR-ZAHLWEISE        PIC S9(4)   USAGE COMP.
            10 KNR-WAEHRUNG         PIC S9(4)   USAGE COMP.
            10 PMPE-VSTAT           PIC X(4).
            10 PMPE-LAST-UPDATE     PIC X(26).
            10 PMPE-KENNUNG         PIC X(8).
            10 PMPE-PG-PM-DATEN.
               49 PMPE-PG-PM-DATEN-LEN  PIC S9(4) USAGE COMP.
               49 PMPE-PG-PM-DATEN-TEXT PIC X(300).
            10 PMPE-BZ-NAME         PIC X(25).
            10 PMPE-BZ-VNAME        PIC X(20).
            10 PMPE-VP-NAME         PIC X(25).
            10 PMPE-VP-VNAME        PIC X(20).
            10 PMPE-KZ-EIGENV       PIC X(1).
            10 KNR-FEIV-KZ          PIC S9(4)   USAGE COMP.
            10 KNR-VERM-GES         PIC S9(4)   USAGE COMP.
            10 PMPE-UMW-NR          PIC S9(15)V USAGE COMP-3.
            10 PMPE-UNR             PIC S9(4)   USAGE COMP.
            10 PMPE-SCHICHT-NR      PIC S9(4)   USAGE COMP.
            10 PMPE-INKST-DAT       PIC X(10).
            10 KNR-INK-ART          PIC S9(4)   USAGE COMP.
            10 PMPE-SAM-NR          PIC S9(9)   USAGE COMP.
            10 KNR-RAB-ST           PIC S9(4)   USAGE COMP.
            10 PMPE-VVG-KZ          PIC X(1).
            10 KNR-DEPOT-KZ         PIC S9(4)   USAGE COMP.
            10 PMPE-PROV-SCHL       PIC X(5).
            10 PMPE-BEW-DAT         PIC X(10).
            10 PMPE-URSPR-POL-DAT   PIC X(10).
            10 PMPE-POL-DAT         PIC X(10).
            10 PMPE-BESVV           PIC S9(1)V  USAGE COMP-3.
            10 PMPE-VN-STRASSE      PIC X(35).
            10 PMPE-VN-PLZ          PIC X(5).
            10 PMPE-VN-ORT          PIC X(35).
            10 PMPE-RATENBEITRAG    PIC S9(9)V9(2) USAGE COMP-3.
240217*     10 PMPE-PG-RECH-NR      PIC X(12).
240217      10 PMPE-PG-RECH-NR      PIC X(30).
            10 PMPE-HIST-UNR        PIC S9(4)   USAGE COMP.
240217      10 KNR-TLSN-PARTNER     PIC S9(4)   USAGE COMP.
240418      10 PMPE-GEL-KENNUNG     PIC X(6).
      ******************************************************************

