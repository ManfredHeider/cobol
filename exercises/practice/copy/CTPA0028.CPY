      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0028.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\ctpa0028.cpv  $
      *
      *   Rev 2.1   Mar 09 2000 17:29:56   T13EX13
      *APA00087 Umstellung auf PPGZ 
      *
      *   Rev 2.0   Feb 27 1999 14:26:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   04 Jul 1997 08:02:06   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0028)                                  *
      *        LIBRARY(T130005.FILETRAN.SRCDCL(TBPA0028))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0028 TABLE
CCC004*    ( KNR_PGV                        SMALLINT NOT NULL,
CCC004*      KNR_PVW_MODUS                  SMALLINT NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PPGZ_GUELTIG_AB                DATE NOT NULL,
CCC004*      PPGZ_GUELTIG_BIS               DATE NOT NULL,
CCC004*      KNR_PVW_GEVO                   SMALLINT NOT NULL,
CCC004*      PPGZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PPGZ_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13PAS.TBPA0028                    *
      ******************************************************************
       01  TBPA0028-GESAMT.
         05  DCL-TBPA0028.
           10 KNR-PGV              PIC S9(4) USAGE COMP.
           10 KNR-PVW-MODUS        PIC S9(4) USAGE COMP.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 PPGZ-GUELTIG-AB      PIC X(10).
           10 PPGZ-GUELTIG-BIS     PIC X(10).
           10 KNR-PVW-GEVO         PIC S9(4) USAGE COMP.
           10 PPGZ-LAST-UPDATE     PIC X(26).
           10 PPGZ-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
