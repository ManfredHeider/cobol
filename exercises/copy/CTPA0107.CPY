      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0107.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\CTPA0107.CPv  $
      *
      *   Rev 2.2   03 Jul 2000 14:36:14   T13EX44
      *Qualifier entfernt
      *
      *   Rev 2.1   16 May 2000 09:19:46   T13EX23
      *APA00090 - Neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 14:26:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   04 Aug 1997 13:11:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      ******************************************************************
      * DCLGEN TABLE(T13PAS.TBPA0107)                                  *
      *        LIBRARY(T130005.PA010000.DCLGEN)                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CTPA0107)                                     *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0107 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PINZ_PIN                       INTEGER NOT NULL,
CCC004*      PRINTE                         DECIMAL(15, 0) NOT NULL,
CCC004*      PINZ_PG_VBNR                   INTEGER NOT NULL,
CCC004*      PINZ_GUELTIG_AB                DATE  NOT NULL,
CCC004*      PINZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PINZ_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13PAS.TBPA0107                    *
      ******************************************************************
       01  TBPA0107-GESAMT.
         05  DCL-TBPA0107.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PINZ-PIN             PIC S9(9) USAGE COMP.
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PINZ-PG-VBNR         PIC S9(9) USAGE COMP.
           10 PINZ-GUELTIG-AB      PIC X(10).
           10 PINZ-LAST-UPDATE     PIC X(26).
           10 PINZ-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
