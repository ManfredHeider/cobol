      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0311.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0311.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:38   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:56:56   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  GLASBERECHNET  TABLE
CCC004*    ( GLASBERECHNETID         SMALLINT NOT NULL,
CCC004*      GLASBERECHTEXT          CHAR (25) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  GLASBERECHNET-GESAMT.
        05  NI-GLASBERECHNET-TAB.
           10 NI-GLASBERECHNET-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-GLASBERECHNET  REDEFINES  NI-GLASBERECHNET-TAB.
           10 NI-GLASBERECHNETID          PIC S9(4) COMP.
           10 NI-GLASBERECHTEXT           PIC S9(4) COMP.
        05  DCL-GLASBERECHNET.
           10 GLASBERECHNETID             PIC S9(4) COMP.
           10 GLASBERECHTEXT              PIC X(25).
      ******************************************************************
