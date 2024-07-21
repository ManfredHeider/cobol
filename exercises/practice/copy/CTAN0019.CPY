      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0019.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTAN0019.CPv  $
      *
      *   Rev 2.1   Jul 05 2013 10:23:52   t13ae23
      *AAN01127: Erfassung Investanträge ohne §34f
      *
      *   Rev 2.0   Jul 03 2013 14:39:32   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CTAN0019                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0019 WHITELIST §34F   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN01127!05.07.13!2.1  !NEU                        !RAHNER   *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
CCC005*    EXEC SQL DECLARE TBAN0019 TABLE
CCC004*    ( ANWL_VBNR                      INTEGER NOT NULL,
CCC004*      ANWL_NACHNAME                  CHAR(40) NOT NULL,
CCC004*      ANWL_VORNAME                   CHAR(30) NOT NULL,
CCC004*      ANWL_LAST_UPDATE               TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAN0019                           *
      ******************************************************************
       01  DCLTBAN0019.
           10 ANWL-VBNR            PIC S9(9) USAGE COMP.
           10 ANWL-NACHNAME        PIC X(40).
           10 ANWL-VORNAME         PIC X(30).
           10 ANWL-LAST-UPDATE     PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
