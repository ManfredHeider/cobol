      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0117.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/CTPA0117.CPv  $
      *
      *   Rev 2.9   Feb 28 2017 09:21:42   T13ex24
      *APA02275: DAISY-Erweiterungen zu FlexN - PEG  Module
      *
      *   Rev 2.8   Feb 10 2017 15:47:44   t13ex24
      *APA02275: DAISY-Erweiterungen zu FlexN - PEG  Module
      *
      *   Rev 2.7   Apr 07 2016 09:59:48   t13ae46
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlung
      *
      *   Rev 2.6   Jul 16 2015 15:03:50   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.5   Mar 10 2014 16:52:58   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.4   Jul 26 2013 11:44:22   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.3   Jun 04 1999 19:15:24   T13EX16
      *Umwandlungsfehler da Komma im DCLGEN fehlt
      *
      *   Rev 2.2   Jun 04 1999 15:19:06   T13EX21
      *
      *
      *   Rev 2.1   May 27 1999 14:43:22   T13EX25
      *APA00042, Korrektur V2.0
      *
      *   Rev 2.0   May 27 1999 14:33:22   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   04 Aug 1997 13:12:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00042!27.05.99!2.0 ! NEUERSTELLUNG             ! JÄGER    *
      * APA01539!26.07.13!2.2 ! ERWEITERT FÜR KALYPSO     ! RAHNER   *
      * APA01932!01.04.15!2.3 ! NEUE ATTRIBUTE            ! STORCH   *
      * APA02220!02.03.16!2.7 ! 4 VOR-UND NACHKOMMASTELLEN! WANG     *
      *         !        !    ! /PMPT_MA_GRD_PR_STZ       !          *
      * APA02275!30.01.17!2.8 ! ERWEITERT UM FlexN-Felder ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(TBPA0117)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0117 TABLE
CCC004*    ( PMPT_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMPT_VERSION                   SMALLINT NOT NULL,
CCC004*      PMPT_V_GUELTIG_AB              TIMESTAMP NOT NULL,
CCC004*      PMPT_V_GUELTIG_BIS             DATE NOT NULL,
CCC004*      PMPT_V_KENNUNG                 CHAR(8) NOT NULL,
CCC004*      PVPT_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PVPT_VERSION                   SMALLINT NOT NULL,
CCC004*      PMPR_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMPR_VERSION                   SMALLINT NOT NULL,
CCC004*      PMBE_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PRINTE                         DECIMAL(15, 0) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      KNR_PMPT_ART                   SMALLINT NOT NULL,
CCC004*      PMPT_VSTAT                     CHAR(4) NOT NULL,
CCC004*      PMPT_FAELLIG_VON               DATE NOT NULL,
CCC004*      PMPT_FAELLIG_BIS               DATE NOT NULL,
CCC004*      PMPT_PPFL_BEITRAG              DECIMAL(11, 2) NOT NULL,
CCC004*      PMPT_PPFL_SUMME                DECIMAL(11, 2) NOT NULL,
CCC004*      PMPT_GSMT_PROV                 DECIMAL(11, 2) NOT NULL,
CCC004*      PMPT_GRUND_PROV                DECIMAL(11, 2) NOT NULL,
CCC004*      PMPT_EINH                      DECIMAL(8, 2) NOT NULL,
CCC004*      PRKO_KOND_NR_GRP               SMALLINT NOT NULL,
CCC004*      PRKO_KOND_NR_EINH              SMALLINT NOT NULL,
CCC004*      PRKO_KOND_NR_KDB               SMALLINT NOT NULL,
CCC004*      PMPT_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PMPT_KENNUNG                   CHAR(8) NOT NULL,
020316*      PMPT_MA_GRD_PR_STZ             DECIMAL(6, 3) NOT NULL,
CCC004*      PMPT_MA_GRD_PR_STZ             DECIMAL(8, 4) NOT NULL,
CCC004*      PMPT_PG_UST_BETRAG             DECIMAL(11, 2) NOT NULL,
CCC004*      PMPT_PG_UST_SATZ               DECIMAL(6, 3) NOT NULL,
CCC004*      PMPT_VSP_KOMBI_NR              INTEGER NOT NULL,
CCC004*      PMPT_VSP_KOMBI_BZ              CHAR(3) NOT NULL,
CCC004*      PMPT_VSP_FN_BER                CHAR(1) NOT NULL,
CCC004*      PMPT_VSP_FN_REL                CHAR(1) NOT NULL,
CCC004*      PMPT_VSP_FN_FP_KZ              CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0117                           *
      ******************************************************************
       01  TBPA0117-GESAMT.
         05  DCL-TBPA0117.
           10 PMPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPT-VERSION         PIC S9(4) USAGE COMP.
           10 PMPT-V-GUELTIG-AB    PIC X(26).
           10 PMPT-V-GUELTIG-BIS   PIC X(10).
           10 PMPT-V-KENNUNG       PIC X(8).
           10 PVPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PVPT-VERSION         PIC S9(4) USAGE COMP.
           10 PMPR-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPR-VERSION         PIC S9(4) USAGE COMP.
           10 PMBE-ID              PIC S9(15)V USAGE COMP-3.
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 KNR-PMPT-ART         PIC S9(4) USAGE COMP.
           10 PMPT-VSTAT           PIC X(4).
           10 PMPT-FAELLIG-VON     PIC X(10).
           10 PMPT-FAELLIG-BIS     PIC X(10).
           10 PMPT-PPFL-BEITRAG    PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPT-PPFL-SUMME      PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPT-GSMT-PROV       PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPT-GRUND-PROV      PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPT-EINH            PIC S9(6)V9(2) USAGE COMP-3.
           10 PRKO-KOND-NR-GRP     PIC S9(4) USAGE COMP.
           10 PRKO-KOND-NR-EINH    PIC S9(4) USAGE COMP.
           10 PRKO-KOND-NR-KDB     PIC S9(4) USAGE COMP.
           10 PMPT-LAST-UPDATE     PIC X(26).
           10 PMPT-KENNUNG         PIC X(8).
020316*    10 PMPT-MA-GRD-PR-STZ   PIC S9(3)V9(3) USAGE COMP-3.
020316     10 PMPT-MA-GRD-PR-STZ   PIC S9(4)V9(4) USAGE COMP-3.
010415     10 PMPT-PG-UST-BETRAG   PIC S9(9)V9(2) USAGE COMP-3.
010415     10 PMPT-PG-UST-SATZ     PIC S9(3)V9(3) USAGE COMP-3.
300117     10 PMPT-VSP-KOMBI-NR    PIC S9(05) USAGE COMP.
300117     10 PMPT-VSP-KOMBI-BZ    PIC X(3).
300117     10 PMPT-VSP-FN-BER      PIC X(1).
300117     10 PMPT-VSP-FN-REL      PIC X(1).
300117     10 PMPT-VSP-FN-FP-KZ    PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 30      *
      ******************************************************************
 

