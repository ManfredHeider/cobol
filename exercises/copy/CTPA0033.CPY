      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0033.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTPA0033.CPv  $
      *
      *   Rev 2.3   Apr 07 2016 10:18:00   t13ae39
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng TBPA033 ASR
      *
      *   Rev 2.2   Mar 10 2014 16:52:08   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.1   Dec 11 2013 15:36:52   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell -
      *Tabelle TB
      *PA0033 PRKL -
      *
      *   Rev 2.0   Oct 01 2013 10:30:00   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------------------------------------------------------------*
      *         !        !2.0 ! NEUERSTELLUNG              !          *
      * APA01539!11.12.13!2.1 ! ERWEITERT FÜR KALYPSO      ! RAHNER   *
      * APA01526!10.03.14!2.2 ! ERWEITERT FÜR LUCY         ! RAHNER   *
      * APA02220!15.02.16!2.3 ! GLOBALER DATENTYP PROZENT  ! STRAUSS  *
      *         !        !    ! 4 VOR- UND NK-STELLEN      !          *
      *         !        !    !                            !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      ******************************************************************
      * DCLGEN TABLE(TBPA0033)                                         *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0033 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PRKL_TARSCHL_GRUPPE            INTEGER NOT NULL,
CCC004*      PRKL_TARSCHL                   INTEGER NOT NULL,
CCC004*      KNR_TARIF_KZ                   SMALLINT NOT NULL,
CCC004*      PRKL_LZ_VON                    SMALLINT NOT NULL,
CCC004*      PRKL_LZ_BIS                    SMALLINT NOT NULL,
CCC004*      PRKL_GUELTIG_VON               DATE NOT NULL,
CCC004*      PRKL_GUELTIG_BIS               DATE NOT NULL,
CCC004*      PRKL_KOND_NR                   SMALLINT NOT NULL,
CCC004*      PRKL_GRUND_PROV_MA             DECIMAL(8, 4) NOT NULL,
CCC004*      PRKL_PROV_SATZ_VWF             DECIMAL(8, 4) NOT NULL,
CCC004*      PRKL_PROV_SATZ_VTF             DECIMAL(8, 4) NOT NULL,
CCC004*      PRKL_KUERZUNG_ART              CHAR(1) NOT NULL,
CCC004*      PRKL_KUERZUNG_AUF              SMALLINT NOT NULL,
CCC004*      KNR_PVT_KZ                     SMALLINT NOT NULL,
CCC004*      PRKL_PROV_MIN                  INTEGER NOT NULL,
CCC004*      PRKL_PROV_MAX                  INTEGER NOT NULL,
CCC004*      KNR_KOND_REGEL_GRP             SMALLINT NOT NULL,
CCC004*      KNR_KOND_REGEL_GSP             SMALLINT NOT NULL,
CCC004*      PRKL_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PRKL_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0033                           *
      ******************************************************************
       01  TBPA0033-GESAMT.
         05  DCL-TBPA0033.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 PRKL-TARSCHL-GRUPPE  PIC S9(9) USAGE COMP.
           10 PRKL-TARSCHL         PIC S9(9) USAGE COMP.
           10 KNR-TARIF-KZ         PIC S9(4) USAGE COMP.
           10 PRKL-LZ-VON          PIC S9(4) USAGE COMP.
           10 PRKL-LZ-BIS          PIC S9(4) USAGE COMP.
           10 PRKL-GUELTIG-VON     PIC X(10).
           10 PRKL-GUELTIG-BIS     PIC X(10).
           10 PRKL-KOND-NR         PIC S9(4) USAGE COMP.
           10 PRKL-GRUND-PROV-MA   PIC S9(4)V9(4) USAGE COMP-3.
           10 PRKL-PROV-SATZ-VWF   PIC S9(4)V9(4) USAGE COMP-3.
           10 PRKL-PROV-SATZ-VTF   PIC S9(4)V9(4) USAGE COMP-3.
           10 PRKL-KUERZUNG-ART    PIC X(1).
           10 PRKL-KUERZUNG-AUF    PIC S9(4) USAGE COMP.
           10 KNR-PVT-KZ           PIC S9(4) USAGE COMP.
           10 PRKL-PROV-MIN        PIC S9(9) USAGE COMP.
           10 PRKL-PROV-MAX        PIC S9(9) USAGE COMP.
           10 KNR-KOND-REGEL-GRP   PIC S9(4) USAGE COMP.
           10 KNR-KOND-REGEL-GSP   PIC S9(4) USAGE COMP.
           10 PRKL-LAST-UPDATE     PIC X(26).
           10 PRKL-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 22      *
      ******************************************************************

