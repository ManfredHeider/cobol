      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0012.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\ctpa0012.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:30:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Mar 06 1997 14:12:14   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:34:20   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:27:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0012                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0012                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0012)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0012))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
bb    *    EXEC SQL DECLARE T13AE.TBPA0012 TABLE
CCC005*    EXEC SQL DECLARE TBPA0012 TABLE
CCC004*    ( PZVP_VERFAHREN                 CHAR(20) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PZVP_GUELT_AB                  DATE NOT NULL,
CCC004*      PZVP_GUELT_BIS                 DATE NOT NULL,
CCC004*      PZVP_PROGRAMM                  CHAR(8) NOT NULL,
CCC004*      PZVP_FUNKTION                  CHAR(8) NOT NULL,
CCC004*      PZVP_IO_DATEN                  CHAR(8) NOT NULL,
CCC004*      PZVP_VERSION                   SMALLINT NOT NULL,
CCC004*      PZVP_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PZVP_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0012                     *
      ******************************************************************
bb    *01  DCLTBPA0012.
BB     01  TBPA0012-GESAMT.
BB         05  DCL-TBPA0012.
               10 PZVP-VERFAHREN   PIC X(20).
               10 PGEB-GES         PIC S9(4) USAGE COMP.
               10 PROD-PROD-SCHL   PIC S9(4) USAGE COMP.
               10 PZVP-GUELT-AB    PIC X(10).
               10 PZVP-GUELT-BIS   PIC X(10).
               10 PZVP-PROGRAMM    PIC X(8).
               10 PZVP-FUNKTION    PIC X(8).
               10 PZVP-IO-DATEN    PIC X(8).
               10 PZVP-VERSION     PIC S9(4) USAGE COMP.
               10 PZVP-LAST-UPDATE PIC X(26).
               10 PZVP-KENNUNG     PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
