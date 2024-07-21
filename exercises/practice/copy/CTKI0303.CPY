      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0303.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0303.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:26   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:56:22   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  BAUSPARKTOSTANDKZ  TABLE
CCC004*    ( BSKSKONTOSTANDID        SMALLINT NOT NULL,
CCC004*      BSKSKONTOSTANDTEXT      CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  BAUSPARKTOSTANDKZ-GESAMT.
        05  NI-BAUSPARKTOSTANDKZ-TAB.
           10 NI-BAUSPARKTOSTANDKZ-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-BAUSPARKTOSTANDKZ  REDEFINES  NI-BAUSPARKTOSTANDKZ-TAB.
           10 NI-BSKSKONTOSTANDID         PIC S9(4) COMP.
           10 NI-BSKSKONTOSTANDTEXT       PIC S9(4) COMP.
        05  DCL-BAUSPARKTOSTANDKZ.
           10 BSKSKONTOSTANDID            PIC S9(4) COMP.
           10 BSKSKONTOSTANDTEXT          PIC X(30).
      ******************************************************************
