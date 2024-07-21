      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0012.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/CTAN0012.CPv  $
      *
      *   Rev 2.13   Mar 31 2016 08:21:50   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.12   Mar 22 2016 18:17:06   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.11   Mar 09 2016 11:55:02   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.10   Mar 02 2016 19:14:26   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.9   Feb 17 2016 17:33:34   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.8   Jan 22 2016 08:02:32   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.7   Jul 25 2013 10:52:44   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.6   May 15 2012 10:40:12   t13ae23
      *AAN01051: Neu Generali  "RISK Care" mit R6.16
      *
      *   Rev 2.5   Dec 14 2011 17:03:50   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 26
      *
      *   Rev 2.4   May 27 2009 16:35:46   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26
      *- Beitrag
      *sfeld öffnen -
      *
      *   Rev 2.2   Nov 24 2008 14:21:00   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.1   Nov 24 2008 11:52:00   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.0   Jul 08 2008 10:39:54   i13ex34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CTAN0012                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0012 GENERALI-LEBEN   *
      *    ACHTUNG! ->  COPY WEICHT VON GENERIERTEM DCLGEN AB!       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00824!24.11.08!2.1  !NEU                        !RAHNER   *
      * AAN00824!23.05.09!2.2  !BEITRAGSFELD ÖFFNEN        !RAHNER   *
      * AAN01033!14.12.11!2.3  !DATEN GELDWÄSCHE ECT.      !RAHNER   *
      * AAN01051!15.05.12!2.5  !INTERNE NUMMERN GENERALI   !RAHNER   *
      * AAN01073!25.07.13!2.7  !SEPA                       !RAHNER   *
      * AAN01230!29.10.15!2.8  !PDF ALS BLOB               !HEIDER   *
      * AAN01230!02.03.16!2.10 !OHNE BLOB, MIT DATEINAME   !HEIDER   *
      * AAN01230!07.03.16!2.11 !MIT BLOB, MIT DATEINAME    !HEIDER   *
      * AAN01230!21.03.16!2.12 !NULLINDIKATOREN            !HEIDER   *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
CCC005*    EXEC SQL DECLARE TBAN0012 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      GENL_HVANTNUM                  CHAR(9) NOT NULL,
CCC004*      GENL_AVERM                     INTEGER NOT NULL,
CCC004*      GENL_PIN                       INTEGER NOT NULL,
CCC004*      GENL_AVERM_PZ                  CHAR(1) NOT NULL,
CCC004*      GENL_AVERM_E                   INTEGER NOT NULL,
CCC004*      GENL_AVERM_PZ_E                CHAR(1) NOT NULL,
CCC004*      GENL_VB_GES                    SMALLINT NOT NULL,
CCC004*      GENL_VMNR                      INTEGER NOT NULL,
CCC004*      GENL_VMNR_PZ                   CHAR(1) NOT NULL,
CCC004*      GENL_VM_VORNA                  CHAR(26) NOT NULL,
CCC004*      GENL_VM_ZUNA                   CHAR(26) NOT NULL,
CCC004*      GENL_VMNR_E                    INTEGER NOT NULL,
CCC004*      GENL_VMPZ_E                    CHAR(1) NOT NULL,
CCC004*      GENL_VM_VORNA_E                CHAR(26) NOT NULL,
CCC004*      GENL_VM_ZUNA_E                 CHAR(26) NOT NULL,
CCC004*      GENL_VB_GW                     INTEGER NOT NULL,
CCC004*      GENL_VB_GW_PZ                  CHAR(1) NOT NULL,
CCC004*      GENL_VB_GW_E                   INTEGER NOT NULL,
CCC004*      GENL_VB_GW_PZ_E                CHAR(1) NOT NULL,
CCC004*      GENL_DIR                       SMALLINT NOT NULL,
CCC004*      GENL_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      GENL_KENNUNG_A                 CHAR(8) NOT NULL,
CCC004*      GENL_KD_BEST_ID                DECIMAL(18, 0) NOT NULL,
CCC004*      GENL_VN_AKD_T                  CHAR(26) NOT NULL,
CCC004*      GENL_VN_AKD_T_E                CHAR(26) NOT NULL,
CCC004*      GENL_VN_VORNA                  CHAR(26) NOT NULL,
CCC004*      GENL_VN_VORNA_E                CHAR(26) NOT NULL,
CCC004*      GENL_VN_ZUNA                   CHAR(26) NOT NULL,
CCC004*      GENL_VN_ZUNA_E                 CHAR(26) NOT NULL,
CCC004*      GENL_VN_GBDAT                  DATE NOT NULL,
CCC004*      GENL_VN_SVNR                   DECIMAL(10, 0) NOT NULL,
CCC004*      GENL_VN_SVNR_E                 DECIMAL(10, 0) NOT NULL,
CCC004*      GENL_VN_STR                    CHAR(26) NOT NULL,
CCC004*      GENL_VN_STR_E                  CHAR(26) NOT NULL,
CCC004*      GENL_VN_PLZ                    CHAR(5) NOT NULL,
CCC004*      GENL_VN_PLZ_E                  CHAR(5) NOT NULL,
CCC004*      GENL_VN_ORT                    CHAR(26) NOT NULL,
CCC004*      GENL_VN_ORT_E                  CHAR(26) NOT NULL,
CCC004*      GENL_VN_LKZ                    CHAR(3) NOT NULL,
CCC004*      GENL_VN_LKZ_E                  CHAR(3) NOT NULL,
CCC004*      GENL_VN_KNR                    DECIMAL(15, 0) NOT NULL,
CCC004*      GENL_VN_KNR_E                  DECIMAL(15, 0) NOT NULL,
CCC004*      GENL_VN_BLZ                    DECIMAL(15, 0) NOT NULL,
CCC004*      GENL_VN_BLZ_E                  DECIMAL(15, 0) NOT NULL,
CCC004*      GENL_VD_PRODKEY                SMALLINT NOT NULL,
CCC004*      GENL_VD_KZ_ZUW                 CHAR(1) NOT NULL,
CCC004*      GENL_VD_KZ_BUZB                CHAR(1) NOT NULL,
CCC004*      GENL_VD_KZ_BUZR                CHAR(1) NOT NULL,
CCC004*      GENL_VD_KZ_PO                  CHAR(1) NOT NULL,
CCC004*      GENL_VD_KZ_UTZ                 CHAR(1) NOT NULL,
CCC004*      GENL_BEGINN                    DATE NOT NULL,
CCC004*      GENL_BEGINN_E                  DATE NOT NULL,
CCC004*      GENL_PZ_DAUER                  SMALLINT NOT NULL,
CCC004*      GENL_PZ_DAUER_E                SMALLINT NOT NULL,
CCC004*      GENL_SUMME                     DECIMAL(9, 0) NOT NULL,
CCC004*      GENL_SUMME_E                   DECIMAL(9, 0) NOT NULL,
CCC004*      GENL_BEITRAG                   DECIMAL(11, 2) NOT NULL,
CCC004*      GENL_JAHR_NTP                  DECIMAL(11, 2) NOT NULL,
CCC004*      GENL_JAHR_NTP_E                DECIMAL(11, 2) NOT NULL,
CCC004*      GENL_PRSUM                     DECIMAL(11, 2) NOT NULL,
CCC004*      GENL_PRSUM_E                   DECIMAL(11, 2) NOT NULL,
CCC004*      GENL_ZAHLW                     SMALLINT NOT NULL,
CCC004*      GENL_ZAHLW_E                   SMALLINT NOT NULL,
CCC004*      GENL_WAEHRUNG                  CHAR(3) NOT NULL,
CCC004*      GENL_VD_EINH                   DECIMAL(9, 2) NOT NULL,
CCC004*      GENL_VD_EINH_E                 DECIMAL(9, 2) NOT NULL,
CCC004*      GENL_AN_ART                    CHAR(1) NOT NULL,
CCC004*      GENL_AN_ART_E                  CHAR(1) NOT NULL,
CCC004*      GENL_AN_DAT                    DATE NOT NULL,
CCC004*      GENL_AN_DAT_E                  DATE NOT NULL,
CCC004*      GENL_VERSION                   CHAR(5) NOT NULL,
CCC004*      GENL_FILLER                    CHAR(100) NOT NULL,
CCC004*      GENL_XML                       VARCHAR(30000) NOT NULL,
CCC004*      GENL_BEITRAG_E                 DECIMAL(11, 2) NOT NULL,
CCC004*      GENL_VN_STAA                   CHAR(3) NOT NULL,
CCC004*      GENL_VN_STAA_E                 CHAR(3) NOT NULL,
CCC004*      GENL_VN_BGRP                   CHAR(40) NOT NULL,
CCC004*      GENL_VN_BGRP_E                 CHAR(40) NOT NULL,
CCC004*      GENL_VN_BRAN                   CHAR(40) NOT NULL,
CCC004*      GENL_VN_BRAN_E                 CHAR(40) NOT NULL,
CCC004*      GENL_VN_PEP                    CHAR(1) NOT NULL,
CCC004*      GENL_VN_PEP_E                  CHAR(1) NOT NULL,
CCC004*      GENL_GEN_ANT_NR                DECIMAL(15, 0) NOT NULL,
CCC004*      GENL_GEN_B_ANT_NR              DECIMAL(15, 0) NOT NULL,
CCC004*      GENL_VN_IBAN                   CHAR(34) NOT NULL,
CCC004*      GENL_VN_IBAN_E                 CHAR(34) NOT NULL,
CCC004*      GENL_EA_DATEINAME              CHAR(50),
CCC004*      GENL_PDF                       BLOB(1048576)
CCC004*    )
CCC003*    END-EXEC.
      *
       01  DCLTBAN0012.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 GENL-HVANTNUM        PIC X(9).
           10 GENL-AVERM           PIC S9(9) USAGE COMP.
           10 GENL-PIN             PIC S9(9) USAGE COMP.
           10 GENL-AVERM-PZ        PIC X(1).
           10 GENL-AVERM-E         PIC S9(9) USAGE COMP.
           10 GENL-AVERM-PZ-E      PIC X(1).
           10 GENL-VB-GES          PIC S9(4) USAGE COMP.
           10 GENL-VMNR            PIC S9(9) USAGE COMP.
           10 GENL-VMNR-PZ         PIC X(1).
           10 GENL-VM-VORNA        PIC X(26).
           10 GENL-VM-ZUNA         PIC X(26).
           10 GENL-VMNR-E          PIC S9(9) USAGE COMP.
           10 GENL-VMPZ-E          PIC X(1).
           10 GENL-VM-VORNA-E      PIC X(26).
           10 GENL-VM-ZUNA-E       PIC X(26).
           10 GENL-VB-GW           PIC S9(9) USAGE COMP.
           10 GENL-VB-GW-PZ        PIC X(1).
           10 GENL-VB-GW-E         PIC S9(9) USAGE COMP.
           10 GENL-VB-GW-PZ-E      PIC X(1).
           10 GENL-DIR             PIC S9(4) USAGE COMP.
           10 GENL-LAST-UPDATE     PIC X(26).
           10 GENL-KENNUNG-A       PIC X(8).
           10 GENL-KD-BEST-ID      PIC S9(18)V USAGE COMP-3.
           10 GENL-VN-AKD-T        PIC X(26).
           10 GENL-VN-AKD-T-E      PIC X(26).
           10 GENL-VN-VORNA        PIC X(26).
           10 GENL-VN-VORNA-E      PIC X(26).
           10 GENL-VN-ZUNA         PIC X(26).
           10 GENL-VN-ZUNA-E       PIC X(26).
           10 GENL-VN-GBDAT        PIC X(10).
           10 GENL-VN-SVNR         PIC S9(10)V USAGE COMP-3.
           10 GENL-VN-SVNR-E       PIC S9(10)V USAGE COMP-3.
           10 GENL-VN-STR          PIC X(26).
           10 GENL-VN-STR-E        PIC X(26).
           10 GENL-VN-PLZ          PIC X(5).
           10 GENL-VN-PLZ-E        PIC X(5).
           10 GENL-VN-ORT          PIC X(26).
           10 GENL-VN-ORT-E        PIC X(26).
           10 GENL-VN-LKZ          PIC X(3).
           10 GENL-VN-LKZ-E        PIC X(3).
           10 GENL-VN-KNR          PIC S9(15)V USAGE COMP-3.
           10 GENL-VN-KNR-E        PIC S9(15)V USAGE COMP-3.
           10 GENL-VN-BLZ          PIC S9(15)V USAGE COMP-3.
           10 GENL-VN-BLZ-E        PIC S9(15)V USAGE COMP-3.
           10 GENL-VD-PRODKEY      PIC S9(4) USAGE COMP.
           10 GENL-VD-KZ-ZUW       PIC X(1).
           10 GENL-VD-KZ-BUZB      PIC X(1).
           10 GENL-VD-KZ-BUZR      PIC X(1).
           10 GENL-VD-KZ-PO        PIC X(1).
           10 GENL-VD-KZ-UTZ       PIC X(1).
           10 GENL-BEGINN          PIC X(10).
           10 GENL-BEGINN-E        PIC X(10).
           10 GENL-PZ-DAUER        PIC S9(4) USAGE COMP.
           10 GENL-PZ-DAUER-E      PIC S9(4) USAGE COMP.
           10 GENL-SUMME           PIC S9(9)V USAGE COMP-3.
           10 GENL-SUMME-E         PIC S9(9)V USAGE COMP-3.
           10 GENL-BEITRAG         PIC S9(9)V9(2) USAGE COMP-3.
           10 GENL-JAHR-NTP        PIC S9(9)V9(2) USAGE COMP-3.
           10 GENL-JAHR-NTP-E      PIC S9(9)V9(2) USAGE COMP-3.
           10 GENL-PRSUM           PIC S9(9)V9(2) USAGE COMP-3.
           10 GENL-PRSUM-E         PIC S9(9)V9(2) USAGE COMP-3.
           10 GENL-ZAHLW           PIC S9(4) USAGE COMP.
           10 GENL-ZAHLW-E         PIC S9(4) USAGE COMP.
           10 GENL-WAEHRUNG        PIC X(3).
           10 GENL-VD-EINH         PIC S9(7)V9(2) USAGE COMP-3.
           10 GENL-VD-EINH-E       PIC S9(7)V9(2) USAGE COMP-3.
           10 GENL-AN-ART          PIC X(1).
           10 GENL-AN-ART-E        PIC X(1).
           10 GENL-AN-DAT          PIC X(10).
           10 GENL-AN-DAT-E        PIC X(10).
           10 GENL-VERSION         PIC X(5).
           10 GENL-FILLER          PIC X(100).
           10 GENL-XML.
              15 GENL-XML-LEN      PIC S9(4) USAGE COMP.
              15 GENL-XML-TEXT     PIC X(30000).
           10 GENL-BEITRAG-E       PIC S9(9)V9(2) USAGE COMP-3.
           10 GENL-VN-STAA         PIC X(3).
           10 GENL-VN-STAA-E       PIC X(3).
           10 GENL-VN-BGRP         PIC X(40).
           10 GENL-VN-BGRP-E       PIC X(40).
           10 GENL-VN-BRAN         PIC X(40).
           10 GENL-VN-BRAN-E       PIC X(40).
           10 GENL-VN-PEP          PIC X(1).
           10 GENL-VN-PEP-E        PIC X(1).
           10 GENL-GEN-ANT-NR      PIC S9(15)V USAGE COMP-3.
           10 GENL-GEN-B-ANT-NR    PIC S9(15)V USAGE COMP-3.
           10 GENL-VN-IBAN         PIC X(34).
           10 GENL-VN-IBAN-E       PIC X(34).
020316     10 GENL-EA-DATEINAME    PIC X(50).
210316     10 NI-GENL-EA-DATEINAME PIC S9(4) COMP.
210316        88 NI-GENL-EA-DATEINAME-NULL-YES VALUE -1 -2.
210316        88 NI-GENL-EA-DATEINAME-NULL-NO  VALUE +0 -0.
291015*    10 GENL-PDF             USAGE SQL TYPE IS BLOB-LOCATOR.
           10 GENL-PDF             USAGE SQL TYPE IS BLOB(1048576).
210316     10 NI-GENL-PDF          PIC S9(4) COMP.
210316        88 NI-GENL-PDF-NULL-YES          VALUE -1 -2.
210316        88 NI-GENL-PDF-NULL-NO           VALUE +0 -0.
 
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 89      *
      ******************************************************************
      *
      ******* ENDE   COPY-MEMBER ***************************************



