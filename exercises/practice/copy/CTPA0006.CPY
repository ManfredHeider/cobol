      * T13AE34   Stand: 25/09/97    09:49:52
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0006.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\ctpa0006.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:30:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   07 Aug 1998 11:47:04   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.2   Mar 06 1997 11:20:38   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:36:12   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:25:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0006                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0006                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0006)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0006))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
bb    *    EXEC SQL DECLARE T13AE.TBPA0006 TABLE
CCC005*    EXEC SQL DECLARE TBPA0006 TABLE
CCC004*    ( PGRP_ID                        SMALLINT NOT NULL,
CCC004*      KNR_PGRP_ART                   SMALLINT NOT NULL,
CCC004*      KTY_PGRP_ART                   SMALLINT NOT NULL,
CCC004*      KTY_PGRP_ART_AUSPR             SMALLINT NOT NULL,
CCC004*      KNR_PGRP_ART_AUSPR             SMALLINT NOT NULL,
CCC004*      PGRP_ALPHA                     CHAR(10) NOT NULL,
CCC004*      PGRP_NUMMER                    INTEGER NOT NULL,
CCC004*      PGRP_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PGRP_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0006                     *
      ******************************************************************
bb    *01  DCLTBPA0006.
BB     01  TBPA0006-GESAMT.
BB         05  DCL-TBPA0006.
               10 PGRP-ID             PIC S9(4) USAGE COMP.
               10 KNR-PGRP-ART        PIC S9(4) USAGE COMP.
               10 KTY-PGRP-ART        PIC S9(4) USAGE COMP.
               10 KTY-PGRP-ART-AUSPR  PIC S9(4) USAGE COMP.
               10 KNR-PGRP-ART-AUSPR  PIC S9(4) USAGE COMP.
               10 PGRP-ALPHA          PIC X(10).
               10 PGRP-NUMMER         PIC S9(9) USAGE COMP.
               10 PGRP-LAST-UPDATE    PIC X(26).
               10 PGRP-KENNUNG        PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
