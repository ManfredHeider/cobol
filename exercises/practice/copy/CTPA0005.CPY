      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0005.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\ctpa0005.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:30:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Mar 06 1997 11:09:16   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:36:28   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:25:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0005                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0005                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0005)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0005))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
bb    *    EXEC SQL DECLARE T13AE.TBPA0005 TABLE
CCC005*    EXEC SQL DECLARE TBPA0005 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PGRP_ID                        SMALLINT NOT NULL,
CCC004*      PPGR_GUELT_AB                  DATE NOT NULL,
CCC004*      PPGR_GUELT_BIS                 DATE NOT NULL,
CCC004*      PPGR_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PPGR_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0005                     *
      ******************************************************************
bb    *01  DCLTBPA0005.
BB     01  TBPA0005-GESAMT.
BB         05  DCL-TBPA0005.
               10 PGEB-GES         PIC S9(4) USAGE COMP.
               10 PROD-PROD-SCHL   PIC S9(4) USAGE COMP.
               10 PGRP-ID          PIC S9(4) USAGE COMP.
               10 PPGR-GUELT-AB    PIC X(10).
               10 PPGR-GUELT-BIS   PIC X(10).
               10 PPGR-LAST-UPDATE PIC X(26).
               10 PPGR-KENNUNG     PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
