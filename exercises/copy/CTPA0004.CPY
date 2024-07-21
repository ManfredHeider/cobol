      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0004.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTPA0004.CPv  $
      *
      *   Rev 2.4   May 27 2019 12:25:04   t13ex44
      *APA02855: Umstellung Haustarif-Verträge AMV
      *
      *   Rev 2.3   May 17 2019 14:43:06   t13ex44
      *APA02855: Umstellung Haustarif-Verträge AMV
      *
      *   Rev 2.2   Jul 16 2015 15:10:08   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.1   Jun 10 2008 10:36:58   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.0   Feb 27 1999 14:30:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Mar 06 1997 11:09:12   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:36:40   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:25:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0004                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0004                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0004)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0004))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
      *    EXEC SQL DECLARE T13AE.TBPA0004 TABLE
CCC005*    EXEC SQL DECLARE TBPA0004 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PROD_BEGINN                    DATE NOT NULL,
CCC004*      PROD_ENDE                      DATE NOT NULL,
CCC004*      PROD_BEZ                       CHAR(40) NOT NULL,
CCC004*      PROD_BEZ_KURZ                  CHAR(10) NOT NULL,
CCC004*      PROD_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PROD_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      KNR_MANDANT                    SMALLINT NOT NULL,
CCC004*      PROD_UST_SATZ                  DECIMAL (6, 3) NOT NULL,
CCC004*      KNR_KZ_SOND_LST                SMALLINT NOT NULL,
CCC004*      PROD_DAT_ABGRENZ               DATE NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0004                     *
      ******************************************************************
      *01  DCLTBPA0004.
       01  TBPA0004-GESAMT.
           05  DCL-TBPA0004.
               10 PGEB-GES             PIC S9(4) USAGE COMP.
               10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
               10 PROD-BEGINN          PIC X(10).
               10 PROD-ENDE            PIC X(10).
               10 PROD-BEZ             PIC X(40).
               10 PROD-BEZ-KURZ        PIC X(10).
               10 PROD-LAST-UPDATE     PIC X(26).
               10 PROD-KENNUNG         PIC X(8).
110308         10 KNR-MANDANT          PIC S9(4) USAGE COMP.
010415         10 PROD-UST-SATZ        PIC S9(3)V999 COMP-3.
010415         10 KNR-KZ-SOND-LST      PIC S9(4) USAGE COMP.
130519         10 PROD-DAT-ABGRENZ     PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
 


