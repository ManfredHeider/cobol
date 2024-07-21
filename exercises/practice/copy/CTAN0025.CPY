      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.02.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0025.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTAN0025.CPv  $
      *
      *   Rev 2.2   Mar 31 2016 08:21:58   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.1   Mar 02 2016 19:04:42   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.0   Feb 16 2016 14:32:22   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: CTAN0025                                      *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0025 EAB               *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME     *
      * --------------------------------------------------------------*
      * AAN01230!16.02.16!2.1  !NEU                        !HEIDER    *
      *         !        !     !                           !          *
      *         !        !     !                           !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE TBAN0025 TABLE
CCC004*    ( EAB_DATEINAME                  CHAR(50) NOT NULL,
CCC004*      EAB_LFD_NR                     SMALLINT NOT NULL,
CCC004*      EAB_TEILDATEN                  VARCHAR(32000) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAN0025                           *
      ******************************************************************
       01  DCL-TBAN0025.
           10 EAB-DATEINAME        PIC X(50).
           10 EAB-LFD-NR           PIC S9(4) USAGE COMP.
           10 EAB-TEILDATEN.
              49 EAB-TEILDATEN-LEN
                 PIC S9(4) USAGE COMP.
              49 EAB-TEILDATEN-TEXT
                 PIC X(32000).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************


