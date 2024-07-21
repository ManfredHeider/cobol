      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0014.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTAN0014.CPv  $
      *
      *   Rev 2.4   Jul 30 2020 08:32:42   t13ae34
      *AAN01472: Ablösung Lotus-Notes - Produktmaßnahmendatenbank P
      *MDB durch Mendix
      *
      *   Rev 2.3   Jun 26 2009 10:44:40   i13az24
      *AAN00860: Automatisierung Host-Konfiguration aufgrund DB PM
      *
      *   Rev 2.2   Jun 15 2009 10:08:08   i13az24
      *AAN00860: Automatisierung Host-Konfiguration aufgrund DB PM
      *
      *   Rev 2.1   Jun 05 2009 14:26:54   i13az24
      *
      *   Rev 2.0   May 19 2009 11:09:52   i13az24
      *Archiverstellung
      *
      *---------------------------------------------------------------*
 
      ******************************************************************
      * DCLGEN TABLE(T13VBS.TBAN0014)
      *        LIBRARY(T130005.VB010000.SRCDCL(CTAN0014))
      *        ACTION(REPLACE)
      *        LANGUAGE(COBOL)
      *        QUOTE
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAN0014 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      ANLZ_LFD_NR                    SMALLINT NOT NULL,
CCC004*      ANLZ_GUELTIG_AB                DATE NOT NULL,
CCC004*      ANLZ_GUELTIG_BIS               DATE NOT NULL,
CCC004*      ANLZ_LZ_VON                    SMALLINT NOT NULL,
CCC004*      ANLZ_LZ_BIS                    SMALLINT NOT NULL,
CCC004*      ANLZ_SATZ                      DECIMAL(8, 4) NOT NULL,
CCC004*      ANLZ_ZEIT_INSERT               TIMESTAMP NOT NULL,
CCC004*      ANLZ_KENNUNG_INSERT            CHAR(8) NOT NULL,
CCC004*      ANLZ_ZEIT_UPDATE               TIMESTAMP NOT NULL,
CCC004*      ANLZ_KENNUNG_UPDATE            CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13VBS.TBAN0014
      *
      ******************************************************************
       01  DCLTBAN0014.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 ANLZ-LFD-NR          PIC S9(4) USAGE COMP.
           10 ANLZ-GUELTIG-AB      PIC X(10).
           10 ANLZ-GUELTIG-BIS     PIC X(10).
           10 ANLZ-LZ-VON          PIC S9(4) USAGE COMP.
           10 ANLZ-LZ-BIS          PIC S9(4) USAGE COMP.
           10 ANLZ-SATZ            PIC S9(4)V9(4) USAGE COMP-3.
           10 ANLZ-ZEIT-INSERT     PIC X(26).
           10 ANLZ-KENNUNG-INSERT  PIC X(8).
           10 ANLZ-ZEIT-UPDATE     PIC X(26).
           10 ANLZ-KENNUNG-UPDATE  PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12
      ******************************************************************

