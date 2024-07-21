      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0108.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTPA0108.CPv  $
      *
      *   Rev 2.3   Jun 09 2020 11:34:40   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.2   Feb 17 2000 17:12:18   T13EX13
      *APA00081 - Pgeb-Ges in SQL-Statements
      *ergaenzt
      *
      *   Rev 2.1   Sep 06 1999 15:01:26   T13EX21
      *FPA00078 - Reihenfolge korrigiert
      *
      *   Rev 2.0   May 27 1999 14:27:54   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   04 Aug 1997 13:12:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0108)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0108 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PRINTE_VERTRAG                 DECIMAL(15, 0) NOT NULL,
CCC004*      PRINTE_VORVERTRAG              DECIMAL(15, 0) NOT NULL,
CCC004*      PVOR_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PVOR_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      PGEB_GES_VORVERTRAG            SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0108                           *
      ******************************************************************
       01  TBPA0108-GESAMT.
         05  DCL-TBPA0108.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PRINTE-VERTRAG       PIC S9(15)V USAGE COMP-3.
           10 PRINTE-VORVERTRAG    PIC S9(15)V USAGE COMP-3.
           10 PVOR-LAST-UPDATE     PIC X(26).
           10 PVOR-KENNUNG         PIC X(8).
170220     10 PGEB-GES-VORVERTRAG  PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************

