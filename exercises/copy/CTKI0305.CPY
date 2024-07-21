      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0305.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0305.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:28   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:56:30   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  BUCHUNGSTEXT  TABLE
CCC004*    ( BUCHUNGSTEXTID          SMALLINT NOT NULL,
CCC004*      BUCHUNGSTEXT            CHAR (40) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  BUCHUNGSTEXT-GESAMT.
        05  NI-BUCHUNGSTEXT-TAB.
           10 NI-BUCHUNGSTEXT-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-BUCHUNGSTEXT  REDEFINES  NI-BUCHUNGSTEXT-TAB.
           10 NI-BUCHUNGSTEXTID           PIC S9(4) COMP.
           10 NI-BUCHUNGSTEXT             PIC S9(4) COMP.
        05  DCL-BUCHUNGSTEXT.
           10 BUCHUNGSTEXTID              PIC S9(4) COMP.
           10 BUCHUNGSTEXT                PIC X(40).
      ******************************************************************
