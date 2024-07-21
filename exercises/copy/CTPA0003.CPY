      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0003.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTPA0003.CPv  $
      *
      *   Rev 2.2   Feb 13 2019 12:34:34   t13ex44
      *APA02808: Reaktivierung Auslagerung
      *
      *   Rev 2.1   Dec 17 2004 10:04:18   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 04 2002 15:31:30   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0003)                                         *
      *        LIBRARY(T130005.PA010000.SRCDCL(CTPA0003))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0003 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PASA_ERSTELLT                  TIMESTAMP NOT NULL,
CCC004*      PRINTE_VON                     DECIMAL(15, 0) NOT NULL,
CCC004*      PRINTE_BIS                     DECIMAL(15, 0) NOT NULL,
CCC004*      PASA_PRIO                      SMALLINT NOT NULL,
CCC004*      PASA_STATUS                    CHAR(1) NOT NULL,
CCC004*      PASA_VORHALTZEIT               SMALLINT NOT NULL,
CCC004*      PASA_START                     TIMESTAMP NOT NULL,
CCC004*      PASA_ENDE                      TIMESTAMP NOT NULL,
CCC004*      PASA_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE        TBPA0003                    *
      ******************************************************************
       01  DCL-TBPA0003.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PASA-ERSTELLT        PIC X(26).
           10 PRINTE-VON           PIC S9(15)V USAGE COMP-3.
           10 PRINTE-BIS           PIC S9(15)V USAGE COMP-3.
           10 PASA-PRIO            PIC S9(4) USAGE COMP.
           10 PASA-STATUS          PIC X(1).
           10 PASA-VORHALTZEIT     PIC S9(4) USAGE COMP.
           10 PASA-START           PIC X(26).
           10 PASA-ENDE            PIC X(26).
           10 PASA-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************

