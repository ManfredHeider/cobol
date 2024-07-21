      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0312.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0312.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:40   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:57:00   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  GRVLEBEN  TABLE
CCC004*    ( GRVLEBENID              SMALLINT NOT NULL,
CCC004*      GRVPROZENT              CHAR (15) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  GRVLEBEN-GESAMT.
        05  NI-GRVLEBEN-TAB.
           10 NI-GRVLEBEN-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-GRVLEBEN  REDEFINES  NI-GRVLEBEN-TAB.
           10 NI-GRVLEBENID               PIC S9(4) COMP.
           10 NI-GRVPROZENT               PIC S9(4) COMP.
        05  DCL-GRVLEBEN.
           10 GRVLEBENID                  PIC S9(4) COMP.
           10 GRVPROZENT                  PIC X(15).
      ******************************************************************
