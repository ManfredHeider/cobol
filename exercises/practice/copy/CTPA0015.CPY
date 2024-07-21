      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0015.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\ctpa0015.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:30:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Mar 06 1997 14:12:28   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:33:18   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:27:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0015                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0015                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0015)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0015))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0015 TABLE
CCC004*    ( PVST_VSTAT                     CHAR(4) NOT NULL,
CCC004*      PVST_KMPTZ                     SMALLINT NOT NULL,
CCC004*      PVST_VSTAT_NEU                 CHAR(4) NOT NULL,
CCC004*      PVST_TEXT                      CHAR(50) NOT NULL,
CCC004*      PVST_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PVST_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0015                     *
      ******************************************************************
       01  TBPA0015-GESAMT.
           05  DCL-TBPA0015.
               10 PVST-VSTAT       PIC X(4).
               10 PVST-KMPTZ       PIC S9(4) USAGE COMP.
               10 PVST-VSTAT-NEU   PIC X(4).
               10 PVST-TEXT        PIC X(50).
               10 PVST-LAST-UPDATE PIC X(26).
               10 PVST-KENNUNG     PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
