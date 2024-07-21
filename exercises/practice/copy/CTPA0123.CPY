      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0123.cpy  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:\MF\CPY\ctpa0123.cpv  $
      *
      *   Rev 2.4   Feb 17 2000 17:13:08   T13EX13
      *APA00081 - Pgeb-Ges in SQL-Statements
      *ergaenzt
      *
      *   Rev 2.3   Sep 03 1999 09:31:06   T13EX21
      *FPA00078 - Last-Update und Kennung an 
      *das Ende gesetzt
      *
      *   Rev 2.2   Jun 07 1999 08:11:32   T13EX25
      *Korrektur wg. leerem PVCS-Kopf
      *
      *   Rev 2.1   May 27 1999 15:10:34   T13EX25
      *APA00042, Produebernahme
      *
      *   Rev 2.0   Feb 27 1999 14:26:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   04 Aug 1997 13:12:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0123)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0123 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PMBE_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMBE_VERSION                   SMALLINT NOT NULL,
CCC004*      PMDS_PMBE_ID                   DECIMAL(15, 0) NOT NULL,
CCC004*      PMDS_PMBE_VERSION              SMALLINT NOT NULL,
CCC004*      PMDS_GUELTIG_AB                TIMESTAMP NOT NULL,
CCC004*      PMDS_GUELTIG_BIS               DATE NOT NULL,
CCC004*      PMDS_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PMDS_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0123                           *
      ******************************************************************
       01  TBPA0123-GESAMT.
         05  DCL-TBPA0123.
010200     10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PMBE-ID              PIC S9(15)V USAGE COMP-3.
           10 PMBE-VERSION         PIC S9(4) USAGE COMP.
           10 PMDS-PMBE-ID         PIC S9(15)V USAGE COMP-3.
           10 PMDS-PMBE-VERSION    PIC S9(4) USAGE COMP.
060898     10 PMDS-GUELTIG-AB      PIC X(26).
           10 PMDS-GUELTIG-BIS     PIC X(10).
           10 PMDS-LAST-UPDATE     PIC X(26).
           10 PMDS-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
