      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0034.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTPA0034.CPv  $
      *
      *   Rev 2.2   Jul 16 2015 15:06:48   t13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Source
      *n und Copies ASR
      *
      *   Rev 2.1   Mar 21 2014 09:17:22   t13ae39
      *APA01766: GESAMTABRECHNUNGSPROTOKOLL DAISY: zus. Kontrolliste
      *
      *   Rev 2.0   Mar 13 2014 13:30:26   t13ae39
      *Archiverstellung
      *
      ******************************************************************
      *     COPY-MEMBER: CTPA0034                                      *
      *     FUNKTION   : DCLGEN FÜR TABELLE TBPA0034 KOSU              *
      ******************************************************************
      *  AENDERUNGEN:                                                  *
      *  AUFTRAG ! DATUM  ! VER ! AENDERUNG                 ! NAME     *
      *  --------------------------------------------------------------*
      *  APA01790!13.03.14! 2.0 ! NEU. MTL KONTROLLSUMMEN   ! STRAUSS  *
      *          !        !     ! FÜR VERARB. 1.+2.SST-MA   !          *
      *  APA01932!23.04.15! 2.1 ! LUCY: UST FÜR GES. 90 DV  ! STRAUSS  *
      *          !        !     !                           !          *
      *                                                                *
      *  ENDE-LOGBUCH                                                  *
      ******************************************************************
      * DCLGEN TABLE(T13DIS.TBPA0034)                                  *
      *        LIBRARY(T130005.DI010000.SRCDCL(CTPA0034))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE T13DIS.TBPA0034 TABLE
CCC004*    ( KNR_MANDANT                    SMALLINT NOT NULL,
CCC004*      KNR_VGES                       SMALLINT NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      KOSU_PROVISION                 DECIMAL(11, 2) NOT NULL,
CCC004*      KOSU_AUSZAHLUNG                DECIMAL(11, 2) NOT NULL,
CCC004*      KOSU_RUECKSTELLUNG             DECIMAL(11, 2) NOT NULL,
CCC004*      KOSU_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      KOSU_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      KOSU_UST_GESAMT                DECIMAL(11, 2) NOT NULL,
CCC004*      KOSU_UST_AUSZAHLUNG            DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13DIS.TBPA0034                    *
      ******************************************************************
       01  DCLTBPA0034.
           10 KNR-MANDANT          PIC S9(4) USAGE COMP.
           10 KNR-VGES             PIC S9(4) USAGE COMP.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 KOSU-PROVISION       PIC S9(9)V9(2) USAGE COMP-3.
           10 KOSU-AUSZAHLUNG      PIC S9(9)V9(2) USAGE COMP-3.
           10 KOSU-RUECKSTELLUNG   PIC S9(9)V9(2) USAGE COMP-3.
           10 KOSU-LAST-UPDATE     PIC X(26).
           10 KOSU-KENNUNG         PIC X(8).
           10 KOSU-UST-GESAMT      PIC S9(9)V9(2) USAGE COMP-3.
           10 KOSU-UST-AUSZAHLUNG  PIC S9(9)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
      *---------------------------------------------------------------*

