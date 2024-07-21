      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0030.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTPA0030.CPv  $
      *
      *   Rev 2.3   Jun 22 2016 12:56:28   T13ex24
      *APA02260: Ergänzung des Produktes 50/91 um die Vorbewegung P
      *S 39
      *
      *   Rev 2.2   Jun 22 2016 12:30:10   T13ex24
      *APA02260: Ergänzung des Produktes 50/91 um die Vorbewegung P
      *S 39
      *
      *   Rev 2.1   Mar 04 2009 15:27:56   t13ex44
      *APA00725: VermögensSparplan - lfd. Verprovisionierung gem. §
      * 125InvG
      *
      *   Rev 2.0   Feb 27 1999 14:26:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   04 Jul 1997 08:02:30   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(T13PAS.TBPA0030)                                  *
      *        LIBRARY(T130005.PA010000.SRCDCL(CTPA0030))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0030 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      ERST_PROD_SCHL_LFDNR           SMALLINT NOT NULL,
CCC004*      PPEZ_GUELTIG_AB                DATE NOT NULL,
CCC004*      PPEZ_GUELTIG_BIS               DATE NOT NULL,
CCC004*      ERST_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PPEZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PPEZ_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE        TBPA0030                    *
      ******************************************************************
       01  DCL-TBPA0030.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 ERST-PROD-SCHL-LFDNR PIC S9(4) USAGE COMP.
           10 PPEZ-GUELTIG-AB      PIC X(10).
           10 PPEZ-GUELTIG-BIS     PIC X(10).
           10 ERST-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 PPEZ-LAST-UPDATE     PIC X(26).
           10 PPEZ-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
 

