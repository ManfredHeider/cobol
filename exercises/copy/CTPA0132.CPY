      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0132.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/CTPA0132.CPv  $
      *
      *   Rev 2.9   May 14 2018 11:12:48   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.8   Mar 28 2017 08:22:12   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs
      *
      *   Rev 2.7   Jul 16 2015 15:04:00   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.6   Mar 10 2014 16:53:14   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.5   Jul 26 2013 11:44:34   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.4   Mar 25 2013 11:57:12   t13ex67
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.3   Mar 20 2013 16:19:28   i13az40
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.1   05 Feb 2001 10:19:32   T13AE29
      *APA0118: Einf?hrung der Gesellschaften 46 (BfG Invest) und 47 (Bf
      *
      *   Rev 2.0   04 Jan 2001 11:48:12   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 !NAME      *
      * -------------------------------------------------------------*
      * APA01539!20.03.13!2.2 ! KALYPSO ERGAENZUNGEN      !HARALANOVA*
      * APA01932!27.03.15!2.3 ! NEUE FELDER HINZU         ! STORCH   *
      * APA02353!03.03.17!2.8 ! NEUE FELDER Teleson       !Buch      *
      * APA02579!24.04.18!2.9 ! GENERALI LEBEN            ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(TBPA0132)                                         *
      *        LIBRARY(T130005.PA010000.SRCDCL(CTPA0132))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0132 TABLE
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
CCC004*      PMPEH_LAST_UPDATE              TIMESTAMP NOT NULL,
CCC004*      PMPEH_KENNUNG                  CHAR(8) NOT NULL,
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
CCC004*      PMPE_PG_RECH_NR                CHAR(12) NOT NULL,
CCC004*      PMPE_HIST_UNR                  SMALLINT NOT NULL,
CCC004*      KNR_TLSN_PARTNER               SMALLINT NOT NULL,
CCC004*      PMPE_GEL_KENNUNG               CHAR(6) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0132                           *
      ******************************************************************
       01  TBPA0132-GESAMT.
         05  DCL-TBPA0132.
           10 PMPG-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPG-VERSION         PIC S9(4) USAGE COMP.
           10 PMPG-LFD-NR-VNR      PIC S9(9) USAGE COMP.
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PMPE-VNR             PIC X(15).
           10 PMPE-VERTR-BEGINN    PIC X(10).
           10 PMPE-VERTR-ABLAUF    PIC X(10).
           10 PMPE-FAELLIG-VON     PIC X(10).
           10 PMPE-FAELLIG-BIS     PIC X(10).
           10 PMPE-PG-VBNR         PIC S9(9) USAGE COMP.
           10 PMPE-KUNDE-NAME      PIC X(25).
           10 PMPE-KUNDE-VNAME     PIC X(20).
           10 PMPE-GSMT-PROV-DAT   PIC X(10).
           10 PMPE-PG-ABR-DAT      PIC X(10).
           10 PMPE-PG-EINL-DAT     PIC X(10).
           10 PMPE-PIN             PIC S9(9) USAGE COMP.
           10 KNR-ZAHLWEISE        PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 PMPE-VSTAT           PIC X(4).
           10 PMPE-LAST-UPDATE     PIC X(26).
           10 PMPE-KENNUNG         PIC X(8).
           10 PMPEH-LAST-UPDATE    PIC X(26).
           10 PMPEH-KENNUNG        PIC X(8).
           10 PMPE-PG-PM-DATEN.
              49 PMPE-PG-PM-DATEN-LEN  PIC S9(4) USAGE COMP.
              49 PMPE-PG-PM-DATEN-TEXT  PIC X(300).
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
           10 PMPE-PG-RECH-NR      PIC X(12).
           10 PMPE-HIST-UNR        PIC S9(4)   USAGE COMP.
030317     10 KNR-TLSN-PARTNER     PIC S9(4)   USAGE COMP.
240418     10 PMPE-GEL-KENNUNG     PIC X(6).
      ******************************************************************

