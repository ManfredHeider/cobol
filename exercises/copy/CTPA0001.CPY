      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\ctpa0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:30:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Mar 06 1997 11:08:56   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:37:26   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:23:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0001                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0001                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0001)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0001))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
bb    *    EXEC SQL DECLARE T13AE.TBPA0001 TABLE
CCC005*    EXEC SQL DECLARE TBPA0001 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PDSP_DTA_ART                   SMALLINT NOT NULL,
CCC004*      KTY_DTA_ART                    SMALLINT NOT NULL,
CCC004*      PDSP_FREIGABE_JN               CHAR(1) NOT NULL,
CCC004*      PDSP_ANZ_FEHL                  INTEGER NOT NULL,
CCC004*      PDSP_PSATZ_FEHL                DECIMAL(5, 2) NOT NULL,
CCC004*      PDSP_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PDSP_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0001                     *
      ******************************************************************
bb    *01  DCLTBPA0001.
BB     01  TBPA0001-GESAMT.
BB         05  DCL-TBPA0001.
               10 PGEB-GES             PIC S9(4) USAGE COMP.
               10 PDSP-DTA-ART         PIC S9(4) USAGE COMP.
               10 KTY-DTA-ART          PIC S9(4) USAGE COMP.
               10 PDSP-FREIGABE-JN     PIC X(1).
               10 PDSP-ANZ-FEHL        PIC S9(9) USAGE COMP.
               10 PDSP-PSATZ-FEHL      PIC S9(3)V9(2) USAGE COMP-3.
               10 PDSP-LAST-UPDATE     PIC X(26).
               10 PDSP-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
