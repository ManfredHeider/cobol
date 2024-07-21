      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0373.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0373.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:22   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   22 Sep 1997 12:58:20   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  NBSEITENPROZESS  TABLE
CCC004*    ( NBSEITENPROZESSID       SMALLINT NOT NULL,
CCC004*      NBPROZESSKLASSE         CHAR(72) NOT NULL,
CCC004*      NBVIEWKLASSE            CHAR(72) NOT NULL,
CCC004*      NBSEITENTITEL           CHAR(72) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  NBSEITENPROZESS-GESAMT.
        05  NI-NBSEITENPROZESS-TAB.
           10 NI-NBSEITENPROZESS-ELE PIC S9(04) COMP OCCURS 4 TIMES.
        05  NI-NBSEITENPROZESS  REDEFINES  NI-NBSEITENPROZESS-TAB.
           10 NI-NBSEITENPROZESSID        PIC S9(4) COMP.
           10 NI-NBPROZESSKLASSE          PIC S9(4) COMP.
           10 NI-NBVIEWKLASSE             PIC S9(4) COMP.
           10 NI-NBSEITENTITEL            PIC S9(4) COMP.
        05  DCL-NBSEITENPROZESS.
           10 NBSEITENPROZESSID           PIC S9(4) COMP.
           10 NBPROZESSKLASSE             PIC X(72).
           10 NBVIEWKLASSE                PIC X(72).
           10 NBSEITENTITEL               PIC X(72).
      ******************************************************************
