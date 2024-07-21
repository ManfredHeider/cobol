      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0016.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\ctpa0016.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:30:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Mar 06 1997 14:12:32   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:33:00   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:28:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0016                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0016                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0016)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0016))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0016 TABLE
CCC004*    ( PVSG_VSTAT_GRP                 SMALLINT NOT NULL,
CCC004*      PVST_VSTAT                     CHAR(4) NOT NULL,
CCC004*      KTY_VSTAT_GRP                  SMALLINT NOT NULL,
CCC004*      PVSG_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PVSG_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0016                     *
      ******************************************************************
       01  TBPA0016-GESAMT.
           05  DCL-TBPA0016.
               10 PVSG-VSTAT-GRP   PIC S9(4) USAGE COMP.
               10 PVST-VSTAT       PIC X(4).
               10 KTY-VSTAT-GRP    PIC S9(4) USAGE COMP.
               10 PVSG-LAST-UPDATE PIC X(26).
               10 PVSG-KENNUNG     PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
