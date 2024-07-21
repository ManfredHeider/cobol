      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTAN0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:05:10   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.1   07 Aug 1998 11:46:42   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   09 Sep 1997 09:13:16   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTAN0001                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN FüR TABELLE TBAN0001 - VVNR                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    SEPTEMBER 1997                                        *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00167!        ! 01 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(TBAN0001)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAN0001 TABLE
CCC004*    ( VVNR_PIN                       INTEGER NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      VVNR_VOR_VNR                   CHAR(15) NOT NULL,
CCC004*      KNR_PGRP                       SMALLINT NOT NULL,
CCC004*      KTY_PGRP                       SMALLINT NOT NULL,
CCC004*      VVNR_KD_BETREUER               INTEGER NOT NULL,
CCC004*      VVNR_AUTOMATISCH               CHAR(1) NOT NULL,
CCC004*      VVNR_HINWEIS                   CHAR(4) NOT NULL,
CCC004*      VVNR_BEITRAG                   DECIMAL(11, 2) NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KTY_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      VVNR_ERSTELLUNG                TIMESTAMP NOT NULL,
CCC004*      VVNR_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      VVNR_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAN0001                           *
      ******************************************************************
       01  TBAN0001-GESAMT.
           05  DCL-TBAN0001.
           10 VVNR-PIN             PIC S9(9) USAGE COMP.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 VVNR-VOR-VNR         PIC X(15).
           10 KNR-PGRP             PIC S9(4) USAGE COMP.
           10 KTY-PGRP             PIC S9(4) USAGE COMP.
           10 VVNR-KD-BETREUER     PIC S9(9) USAGE COMP.
           10 VVNR-AUTOMATISCH     PIC X(1).
           10 VVNR-HINWEIS         PIC X(4).
           10 VVNR-BEITRAG         PIC S9(9)V9(2) USAGE COMP-3.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 VVNR-ERSTELLUNG      PIC X(26).
           10 VVNR-LAST-UPDATE     PIC X(26).
           10 VVNR-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
