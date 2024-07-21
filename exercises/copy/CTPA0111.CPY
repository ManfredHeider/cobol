      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0111.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/CTPA0111.CPv  $
      *
      *   Rev 2.10   Mar 10 2014 16:52:38   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.9   Jul 26 2013 11:44:06   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.8   Jul 30 2003 12:07:30   t13ex44
      *APA00245: zum Xten Mal neu wg. Auftrags-DB-Fehlern (SCH...)
      *
      *   Rev 2.3   Jul 24 2003 16:34:36   t13ex44
      *APA00245: NEU: PVPT-KDBETR-REDUZ
      *
      *   Rev 2.2   Jun 04 1999 19:15:18   T13EX16
      *Umwandlungsfehler da Komma im DCLGEN fehlt
      *
      *   Rev 2.1   Jun 04 1999 15:18:58   T13EX21
      *APA00012 - Korrektur Reihenfolge
      *
      *   Rev 2.0   May 27 1999 14:30:20   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   04 Aug 1997 13:12:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00042!27.05.99!2.0 ! NEUERSTELLUNG             !          *
      * APA0012 !04.06.99!2.1 ! KORREKTUR REIHENFOLGE     !          *
      *         !        !2.2 ! KOMMA IM DCLGEN FEHLT     !          *
      * APA00245!24.07.03!2.3 ! NEU: PVPT-KDBETR-REDUZ    !          *
      * APA00245!30.07.03!2.X ! C+L                       !          *
260713* APA01539!26.07.13!2.9 ! ERWEITERT FÜR KALYPSO     ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(TBPA0111)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0111 TABLE
CCC004*    ( PVPT_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PVPT_VERSION                   SMALLINT NOT NULL,
CCC004*      PVPT_F_GUELTIG_AB              DATE NOT NULL,
CCC004*      PVPT_F_GUELTIG_BIS             DATE NOT NULL,
CCC004*      PVPT_V_GUELTIG_AB              TIMESTAMP NOT NULL,
CCC004*      PVPT_V_GUELTIG_BIS             DATE NOT NULL,
CCC004*      PVPT_V_KENNUNG                 CHAR(8) NOT NULL,
CCC004*      KNR_PVPT_ART                   SMALLINT NOT NULL,
CCC004*      PRINTE                         DECIMAL(15, 0) NOT NULL,
CCC004*      PVER_VERSION                   SMALLINT NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PVPT_VT_PRIO                   SMALLINT NOT NULL,
CCC004*      PVPT_PPFL_BEGINN               DATE NOT NULL,
CCC004*      PVPT_PPFL_ENDE                 DATE NOT NULL,
CCC004*      PVPT_BEWERT_BETRAG             DECIMAL(11, 2) NOT NULL,
CCC004*      PVPT_PTEIL_BEGINN              DATE NOT NULL,
CCC004*      PVPT_PTEIL_ABLAUF              DATE NOT NULL,
CCC004*      PVPT_LAUFZEIT                  SMALLINT NOT NULL,
CCC004*      PRKO_KOND_NR_EE                SMALLINT NOT NULL,
CCC004*      PVPT_EE_ENTSCHEID              CHAR(1) NOT NULL,
CCC004*      KNR_EVS_GRUND                  SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      PEWA_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PVPT_PIN                       INTEGER NOT NULL,
CCC004*      PEST_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PVPT_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PVPT_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      PVPT_KDBETR_REDUZ              CHAR(1) NOT NULL,
CCC004*      PVPT_HAUPTTARIF_KZ             CHAR(1) NOT NULL,
CCC004*      PVPT_BUZTAR                    SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0111                           *
      ******************************************************************
       01  TBPA0111-GESAMT.
         05  DCL-TBPA0111.
           10 PVPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PVPT-VERSION         PIC S9(4)   USAGE COMP.
           10 PVPT-F-GUELTIG-AB    PIC X(10).
           10 PVPT-F-GUELTIG-BIS   PIC X(10).
           10 PVPT-V-GUELTIG-AB    PIC X(26).
           10 PVPT-V-GUELTIG-BIS   PIC X(10).
           10 PVPT-V-KENNUNG       PIC X(8).
           10 KNR-PVPT-ART         PIC S9(4)   USAGE COMP.
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PVER-VERSION         PIC S9(4) USAGE COMP.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 PVPT-VT-PRIO         PIC S9(4) USAGE COMP.
           10 PVPT-PPFL-BEGINN     PIC X(10).
           10 PVPT-PPFL-ENDE       PIC X(10).
           10 PVPT-BEWERT-BETRAG   PIC S9(9)V9(2) USAGE COMP-3.
           10 PVPT-PTEIL-BEGINN    PIC X(10).
           10 PVPT-PTEIL-ABLAUF    PIC X(10).
           10 PVPT-LAUFZEIT        PIC S9(4) USAGE COMP.
           10 PRKO-KOND-NR-EE      PIC S9(4) USAGE COMP.
           10 PVPT-EE-ENTSCHEID    PIC X(1).
           10 KNR-EVS-GRUND        PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 PEWA-ID              PIC S9(15)V USAGE COMP-3.
           10 PVPT-PIN             PIC S9(9)   USAGE COMP.
           10 PEST-ID              PIC S9(15)V USAGE COMP-3.
           10 PVPT-LAST-UPDATE     PIC X(26).
           10 PVPT-KENNUNG         PIC X(8).
           10 PVPT-KDBETR-REDUZ    PIC X(1).
           10 PVPT-HAUPTTARIF-KZ   PIC X(1).
           10 PVPT-BUZTAR          PIC S9(4)  USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 31      *
      ******************************************************************
