      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0030.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0030.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   09 May 1998 13:27:10   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:53:36   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  KOMMUNIKATION  TABLE
CCC004*    ( KOMMPERSONENID CHAR (13) NOT NULL,
CCC004*      KOMMTELARTID SMALLINT NOT NULL,
CCC004*      KOMMLAUFENDENUMMER INTEGER NOT NULL,
CCC004*      KOMMNUMMER VARCHAR (128) NOT NULL,
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  KOMMUNIKATION-GESAMT.
        05  NI-KOMMUNIKATION-TAB.
           10 NI-KOMMUNIKATION-ELE PIC S9(04) COMP OCCURS 5 TIMES.
        05  NI-KOMMUNIKATION  REDEFINES  NI-KOMMUNIKATION-TAB.
           10 NI-KOMMPERSONENID           PIC S9(4) COMP.
           10 NI-KOMMTELARTID             PIC S9(4) COMP.
           10 NI-KOMMLAUFENDENUMMER       PIC S9(4) COMP.
           10 NI-KOMMNUMMER               PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-KOMMUNIKATION.
           10 KOMMPERSONENID              PIC X(13).
           10 KOMMTELARTID                PIC S9(4) COMP.
           10 KOMMLAUFENDENUMMER          PIC S9(9) COMP.
           10 KOMMNUMMER.
              49 KOMMNUMMER-LEN           PIC S9(4) COMP.
              49 KOMMNUMMER-TEXT          PIC X(128).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
