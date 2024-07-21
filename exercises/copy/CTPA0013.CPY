      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0013.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\ctpa0013.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:30:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Mar 06 1997 14:12:20   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:34:00   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:27:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0013                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0013                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0013)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0013))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
bb    *    EXEC SQL DECLARE T13AE.TBPA0013 TABLE
CCC005*    EXEC SQL DECLARE TBPA0013 TABLE
CCC004*    ( PGRP_ID                        SMALLINT NOT NULL,
CCC004*      PGKO_GUELT_AB                  DATE NOT NULL,
CCC004*      PGKO_GUELT_BIS                 DATE NOT NULL,
CCC004*      PGKO_SEL_1_VON                 SMALLINT NOT NULL,
CCC004*      PGKO_SEL_1_BIS                 SMALLINT NOT NULL,
CCC004*      PGKO_SEL_2_VON                 SMALLINT NOT NULL,
CCC004*      PGKO_SEL_2_BIS                 SMALLINT NOT NULL,
CCC004*      PGKO_SEL_3_VON                 SMALLINT NOT NULL,
CCC004*      PGKO_SEL_3_BIS                 SMALLINT NOT NULL,
CCC004*      PKON_KOND_NR                   SMALLINT NOT NULL,
CCC004*      PGKO_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PGKO_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0013                     *
      ******************************************************************
bb    *01  DCLTBPA0013.
BB     01  TBPA0013-GESAMT.
BB         05  DCL-TBPA0013.
               10 PGRP-ID          PIC S9(4) USAGE COMP.
               10 PGKO-GUELT-AB    PIC X(10).
               10 PGKO-GUELT-BIS   PIC X(10).
               10 PGKO-SEL-1-VON   PIC S9(4) USAGE COMP.
               10 PGKO-SEL-1-BIS   PIC S9(4) USAGE COMP.
               10 PGKO-SEL-2-VON   PIC S9(4) USAGE COMP.
               10 PGKO-SEL-2-BIS   PIC S9(4) USAGE COMP.
               10 PGKO-SEL-3-VON   PIC S9(4) USAGE COMP.
               10 PGKO-SEL-3-BIS   PIC S9(4) USAGE COMP.
               10 PKON-KOND-NR     PIC S9(4) USAGE COMP.
               10 PGKO-LAST-UPDATE PIC X(26).
               10 PGKO-KENNUNG     PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
