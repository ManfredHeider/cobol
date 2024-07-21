      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0381.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0381.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Mar 1998 18:21:10   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   17 Jan 1998 12:08:46   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19982502'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERTRAGKZ  TABLE
CCC004*    ( VKZID CHAR NOT NULL,
CCC004*      VKZTEXT VARCHAR (30)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERTRAGKZ-GESAMT.
        05  NI-VERTRAGKZ-TAB.
           10 NI-VERTRAGKZ-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-VERTRAGKZ  REDEFINES  NI-VERTRAGKZ-TAB.
           10 NI-VKZID                    PIC S9(4) COMP.
           10 NI-VKZTEXT                  PIC S9(4) COMP.
        05  DCL-VERTRAGKZ.
           10 VKZID                       PIC X(01).
           10 VKZTEXT.
              49 VKZTEXT-LEN              PIC S9(4) COMP.
              49 VKZTEXT-TEXT             PIC X(30).
      ******************************************************************
