      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0379.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0379.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:30   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   17 Jan 1998 12:15:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  KVGESCHAEFTST  TABLE
CCC004*    ( KVGESCHAEFTSTID         SMALLINT NOT NULL,
CCC004*      KVGESCHAEFTSTTEXT       CHAR (30)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  KVGESCHAEFTST-GESAMT.
        05  NI-KVGESCHAEFTST-TAB.
           10 NI-KVGESCHAEFTST-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-KVGESCHAEFTST  REDEFINES  NI-KVGESCHAEFTST-TAB.
           10 NI-KVGESCHAEFTSTID          PIC S9(4) COMP.
           10 NI-KVGESCHAEFTSTTEXT        PIC S9(4) COMP.
        05  DCL-KVGESCHAEFTST.
           10 KVGESCHAEFTSTID             PIC S9(4) COMP.
           10 KVGESCHAEFTSTTEXT           PIC X(30).
      ******************************************************************
