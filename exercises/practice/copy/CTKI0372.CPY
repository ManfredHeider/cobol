      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0372.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0372.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:22   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:16:24   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  NBPROZESS  TABLE
CCC004*    ( NBSEITENPROZESS         SMALLINT NOT NULL,
CCC004*      NBPRODUKT               SMALLINT NOT NULL,
CCC004*      FREMDVERTRAGSKZ         CHAR NOT NULL,
CCC004*      POSITION                SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  NBPROZESS-GESAMT.
        05  NI-NBPROZESS-TAB.
           10 NI-NBPROZESS-ELE PIC S9(04) COMP OCCURS 4 TIMES.
        05  NI-NBPROZESS  REDEFINES  NI-NBPROZESS-TAB.
           10 NI-NBSEITENPROZESS          PIC S9(4) COMP.
           10 NI-NBPRODUKT                PIC S9(4) COMP.
           10 NI-FREMDVERTRAGSKZ          PIC S9(4) COMP.
           10 NI-POSITION                 PIC S9(4) COMP.
        05  DCL-NBPROZESS.
           10 NBSEITENPROZESS             PIC S9(4) COMP.
           10 NBPRODUKT                   PIC S9(4) COMP.
           10 FREMDVERTRAGSKZ             PIC X(01).
           10 POSITION                    PIC S9(4) COMP.
      ******************************************************************
