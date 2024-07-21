      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0009.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTPA0009.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:37:08   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Mar 27 2003 10:57:18   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.0   Feb 27 1999 14:30:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Mar 06 1997 14:12:02   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:35:22   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:26:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0009                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0009                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0009)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0009))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
      *    EXEC SQL DECLARE T13AE.TBPA0009 TABLE
CCC005*    EXEC SQL DECLARE TBPA0009 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PGTA_TARIF                     CHAR(15) NOT NULL,
CCC004*      PGTA_GUELT_AB                  DATE NOT NULL,
CCC004*      PGTA_GUELT_BIS                 DATE NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PGTA_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PGTA_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      KNR_MANDANT                    SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0009                     *
      ******************************************************************
      *01  DCLTBPA0009.
       01  TBPA0009-GESAMT.
           05  DCL-TBPA0009.
               10 PGEB-GES                  PIC S9(4) USAGE COMP.
               10 PGTA-TARIF                PIC X(15).
               10 PGTA-GUELT-AB             PIC X(10).
               10 PGTA-GUELT-BIS            PIC X(10).
               10 PROD-PROD-SCHL            PIC S9(4) USAGE COMP.
               10 PGTA-LAST-UPDATE          PIC X(26).
               10 PGTA-KENNUNG              PIC X(8).
110308         10 KNR-MANDANT               PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
