      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0136.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTPA0136.CPv  $
      *
      *   Rev 2.1   Jun 14 2010 10:03:00   t13ae23
      *APA01000: PIN-Änderungen AMV nach Aachen
      *
      *   Rev 2.0   Feb 09 2010 14:59:14   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0136)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0136 TABLE
CCC004*    (
CCC004*      PGEB_GES                       SMALLINT  NOT NULL,
CCC004*      PPIA_VNR                       CHAR (15) NOT NULL,
CCC004*      PPIA_PIN                       INTEGER   NOT NULL,
CCC004*      PPIA_DTA_KZ                    CHAR (1)  NOT NULL,
CCC004*      PPIA_KENNUNG                   CHAR (8)  NOT NULL,
CCC004*      PPIA_LAST_UPDATE               TIMESTAMP NOT NULL
CCC004*    )
CCC003*      END-EXEC.

      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0136                           *
      ******************************************************************
       01  DCL-TBPA0136.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PPIA-VNR             PIC X(15).
           10 PPIA-PIN             PIC S9(9) USAGE COMP.
           10 PPIA-DTA-KZ          PIC X(1).
           10 PPIA-KENNUNG         PIC X(8).
           10 PPIA-LAST-UPDATE     PIC X(26).

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
