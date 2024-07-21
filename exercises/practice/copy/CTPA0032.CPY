      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.08.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0032.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTPA0032.CPv  $
      *
      *   Rev 2.3   Mar 10 2014 16:52:02   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.2   Dec 11 2013 15:33:18   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *- Kollektivv
      *erträge-
      *
      *   Rev 2.1   Aug 21 2013 14:13:28   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *- Kollektivv
      *erträge-
      *
      *   Rev 2.0   Aug 19 2013 10:01:32   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0032)                                         *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0032 TABLE
CCC004*    ( PRSS_VERM_GES                  SMALLINT NOT NULL,
CCC004*      KNR_RAB_ST                     SMALLINT NOT NULL,
CCC004*      KNR_ZAHLW                      SMALLINT NOT NULL,
CCC004*      PRSS_GUELTIG_VON               DATE NOT NULL,
CCC004*      PRSS_GUELTIG_BIS               DATE NOT NULL,
CCC004*      PRSS_SATZ_AP                   DECIMAL(9, 6) NOT NULL,
CCC004*      PRSS_SATZ_PRO_RATA             DECIMAL(9, 6) NOT NULL,
CCC004*      PRSS_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PRSS_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13DIS.TBPA0032                    *
      ******************************************************************
       01  TBPA0032-GESAMT.
         05  DCL-TBPA0032.
           10 PRSS-VERM-GES        PIC S9(4) USAGE COMP.
           10 KNR-RAB-ST           PIC S9(4) USAGE COMP.
           10 KNR-ZAHLW            PIC S9(4) USAGE COMP.
           10 PRSS-GUELTIG-VON     PIC X(10).
           10 PRSS-GUELTIG-BIS     PIC X(10).
           10 PRSS-SATZ-AP         PIC S9(3)V9(6) USAGE COMP-3.
           10 PRSS-SATZ-PRO-RATA   PIC S9(3)V9(6) USAGE COMP-3.
           10 PRSS-LAST-UPDATE     PIC X(26).
           10 PRSS-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
