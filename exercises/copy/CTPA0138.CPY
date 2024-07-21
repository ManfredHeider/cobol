      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0138.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTPA0138.CPv  $
      *
      *   Rev 2.1   Jul 16 2015 15:06:02   t13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Source
      *n und Copies ASR
      *
      *   Rev 2.0   Apr 23 2015 16:41:44   t13ae39
      *Archiverstellung
      *
      ******************************************************************
      *     COPY-MEMBER: CTPA1038                                      *
      *     FUNKTION   : DCLGEN FÜR TABELLE TBPA0138 PABR              *
      ******************************************************************
      *  AENDERUNGEN:                                                  *
      *  AUFTRAG ! DATUM  ! VER ! AENDERUNG                 ! NAME     *
      *  --------------------------------------------------------------*
      *  APA01932!23.04.15! 2.0 ! LUCY: UST FÜR GES. 90 DV  ! STRAUSS  *
      *          !        !     ! ABRECHN-NR(KORR.BUCH)=>AC !          *
      *          !        !     !                           !          *
      *                                                                *
      *  ENDE-LOGBUCH                                                  *
      ******************************************************************
      * DCLGEN TABLE(T13DIS.TBPA0138)                                  *
      *        LIBRARY(T130005.DI010000.SRCDCL(CTPA0138))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE T13DIS.TBPA0138 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PDPT_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PABR_VBNR                      INTEGER NOT NULL,
CCC004*      PABR_ABRECH_NR                 CHAR(15) NOT NULL,
CCC004*      PDPT_ID_KORR                   DECIMAL(15, 0) NOT NULL,
CCC004*      PABR_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PABR_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13DIS.TBPA0138                    *
      ******************************************************************
       01  DCL-TBPA0138.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PDPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PABR-VBNR            PIC S9(9) USAGE COMP.
           10 PABR-ABRECH-NR       PIC X(15).
           10 PDPT-ID-KORR         PIC S9(15)V USAGE COMP-3.
           10 PABR-LAST-UPDATE     PIC X(26).
           10 PABR-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
      *---------------------------------------------------------------*

