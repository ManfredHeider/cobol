      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0310.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0310.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:38   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:56:52   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  GEFAHR  TABLE
CCC004*    ( GEFGEFAHRCODE           SMALLINT NOT NULL,
CCC004*      GEFGEFAHRKTEXT          CHAR (3),
CCC004*      GEFGEFAHRTEXT           CHAR (30)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  GEFAHR-GESAMT.
        05  NI-GEFAHR-TAB.
           10 NI-GEFAHR-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05  NI-GEFAHR  REDEFINES  NI-GEFAHR-TAB.
           10 NI-GEFGEFAHRCODE            PIC S9(4) COMP.
           10 NI-GEFGEFAHRKTEXT           PIC S9(4) COMP.
           10 NI-GEFGEFAHRTEXT            PIC S9(4) COMP.
        05  DCL-GEFAHR.
           10 GEFGEFAHRCODE               PIC S9(4) COMP.
           10 GEFGEFAHRKTEXT              PIC X(3).
           10 GEFGEFAHRTEXT               PIC X(30).
      ******************************************************************
