      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0027.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\ctpa0027.cpv  $
      *
      *   Rev 2.2   Mar 16 2000 16:19:32   T13EX13
      *APA00067 Anpassung an Badenia +
      *Umstellung PGDF -> PGKF
      *
      *   Rev 2.1   Feb 22 2000 08:39:24   T13AE18
      *apa00080
      *
      *   Rev 2.0   Feb 27 1999 14:26:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   04 Jul 1997 08:01:58   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*


      ******************************************************************
      * DCLGEN TABLE(TBPA0027)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0027 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      KTY_KONF_ART                   SMALLINT NOT NULL,
CCC004*      KNR_KONF_ART                   SMALLINT NOT NULL,
CCC004*      KTY_KONF_REGEL                 SMALLINT NOT NULL,
CCC004*      KNR_KONF_REGEL                 SMALLINT NOT NULL,
CCC004*      PGKF_KONF_Z_INFO               SMALLINT NOT NULL,
140300***    KNR_DIA_FELD_NAME              SMALLINT NOT NULL,
140300***    KNR_DIA_FELD_BEARB             SMALLINT NOT NULL,
140300***    PGDF_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PGKF_LAST_UPDATE               TIMESTAMP NOT NULL,
140300***    PGDF_KENNUNG                   CHAR(8) NOT NULL
CCC004*      PGKF_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0027                           *
      ******************************************************************
       01  TBPA0027-GESAMT.
         05  DCL-TBPA0027.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 KTY-KONF-ART         PIC S9(4) USAGE COMP.
           10 KNR-KONF-ART         PIC S9(4) USAGE COMP.
           10 KTY-KONF-REGEL       PIC S9(4) USAGE COMP.
           10 KNR-KONF-REGEL       PIC S9(4) USAGE COMP.
           10 PGKF-KONF-Z-INFO     PIC S9(4) USAGE COMP.
140300***  10 KNR-DIA-FELD-NAME    PIC S9(4) USAGE COMP.
140300***  10 KNR-DIA-FELD-BEARB   PIC S9(4) USAGE COMP.
140300***  10 PGDF-LAST-UPDATE     PIC X(26).
140300***  10 PGDF-KENNUNG         PIC X(8).
           10 PGKF-LAST-UPDATE     PIC X(26).
           10 PGKF-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS  5      *
      ******************************************************************
