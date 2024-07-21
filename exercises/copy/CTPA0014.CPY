      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0014.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\ctpa0014.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:30:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Mar 06 1997 14:12:24   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:33:38   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:27:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0014                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0014                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0014)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0014))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
bb    *    EXEC SQL DECLARE T13AE.TBPA0014 TABLE
CCC005*    EXEC SQL DECLARE TBPA0014 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PEWS_PROGN_DAT                 DATE NOT NULL,
CCC004*      PVSG_VSTAT_GRP                 SMALLINT NOT NULL,
CCC004*      PEWS_ANZ_ANTRAG                INTEGER NOT NULL,
CCC004*      PEWS_ANZ_VERTR_6               INTEGER NOT NULL,
CCC004*      PEWS_ANZ_VERTR_4               INTEGER NOT NULL,
CCC004*      PEWS_ANZ_VERTR_2               INTEGER NOT NULL,
CCC004*      PEWS_ANZ_VERTR_1               INTEGER NOT NULL,
CCC004*      PEWS_BETR_ANTR                 INTEGER NOT NULL,
CCC004*      PEWS_BETR_VERTR                INTEGER NOT NULL,
CCC004*      PEWS_EINH_ANTR                 INTEGER NOT NULL,
CCC004*      PEWS_EINH_VERTR                INTEGER NOT NULL,
CCC004*      PEWS_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PEWS_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0014                     *
      ******************************************************************
bb    *01  DCLTBPA0014.
BB     01  TBPA0014-GESAMT.
BB         05  DCL-TBPA0014.
               10 PGEB-GES         PIC S9(4) USAGE COMP.
               10 PROD-PROD-SCHL   PIC S9(4) USAGE COMP.
               10 PEWS-PROGN-DAT   PIC X(10).
               10 PVSG-VSTAT-GRP   PIC S9(4) USAGE COMP.
               10 PEWS-ANZ-ANTRAG  PIC S9(9) USAGE COMP.
               10 PEWS-ANZ-VERTR-6 PIC S9(9) USAGE COMP.
               10 PEWS-ANZ-VERTR-4 PIC S9(9) USAGE COMP.
               10 PEWS-ANZ-VERTR-2 PIC S9(9) USAGE COMP.
               10 PEWS-ANZ-VERTR-1 PIC S9(9) USAGE COMP.
               10 PEWS-BETR-ANTR   PIC S9(9) USAGE COMP.
               10 PEWS-BETR-VERTR  PIC S9(9) USAGE COMP.
               10 PEWS-EINH-ANTR   PIC S9(9) USAGE COMP.
               10 PEWS-EINH-VERTR  PIC S9(9) USAGE COMP.
               10 PEWS-LAST-UPDATE PIC X(26).
               10 PEWS-KENNUNG     PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
