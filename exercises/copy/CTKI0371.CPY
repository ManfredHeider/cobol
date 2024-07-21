      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0371.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0371.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:20   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:15:50   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  NBPRODUKT  TABLE
CCC004*    ( PRODUKT                 SMALLINT NOT NULL,
CCC004*      PRODUKTMENUETEXT        CHAR(35) NOT NULL,
CCC004*      PRODUKTKLASSE           CHAR(72) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  NBPRODUKT-GESAMT.
        05  NI-NBPRODUKT-TAB.
           10 NI-NBPRODUKT-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05  NI-NBPRODUKT  REDEFINES  NI-NBPRODUKT-TAB.
           10 NI-PRODUKT                  PIC S9(4) COMP.
           10 NI-PRODUKTMENUETEXT         PIC S9(4) COMP.
           10 NI-PRODUKTKLASSE            PIC S9(4) COMP.
        05  DCL-NBPRODUKT.
           10 PRODUKT                     PIC S9(4) COMP.
           10 PRODUKTMENUETEXT            PIC X(35).
           10 PRODUKTKLASSE               PIC X(72).
      ******************************************************************
