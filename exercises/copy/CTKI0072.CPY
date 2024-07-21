      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0072.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0072.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:27:58   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   22 Oct 1997 12:08:16   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  BUNDESLANDPLZ  TABLE
CCC004*    ( LANDID                  CHAR (3) NOT NULL,
CCC004*      PLZVON                  CHAR (15) NOT NULL,
CCC004*      PLZBIS                  CHAR (15) NOT NULL,
CCC004*      LAUFENDENUMMER          SMALLINT NOT NULL,
CCC004*      BUNDESLANDID            CHAR (3) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  BUNDESLANDPLZ-GESAMT.
        05  NI-BUNDESLANDPLZ-TAB.
           10 NI-BUNDESLANDPLZ-ELE PIC S9(04) COMP OCCURS 5 TIMES.
        05  NI-BUNDESLANDPLZ  REDEFINES  NI-BUNDESLANDPLZ-TAB.
           10 NI-LANDID                   PIC S9(4) COMP.
           10 NI-PLZVON                   PIC S9(4) COMP.
           10 NI-PLZBIS                   PIC S9(4) COMP.
           10 NI-LAUFENDENUMMER           PIC S9(4) COMP.
           10 NI-BUNDESLANDID             PIC S9(4) COMP.
        05  DCL-BUNDESLANDPLZ.
           10 LANDID                      PIC X(3).
           10 PLZVON                      PIC X(15).
           10 PLZBIS                      PIC X(15).
           10 LAUFENDENUMMER              PIC S9(4) COMP.
           10 BUNDESLANDID                PIC X(3).
      ******************************************************************
