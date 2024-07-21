      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0391.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0391.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   29 Jan 1999 11:34:28   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   07 Jul 1998 16:49:18   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.100 19980703'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  RATENZAHLZUSCHLAG  TABLE
CCC004*    ( RZZRATENZAHLZID         SMALLINT NOT NULL,
CCC004*      RZZRATENZAHLZPROZ       DECIMAL (5, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  RATENZAHLZUSCHLAG-GESAMT.
        05  NI-RATENZAHLZUSCHLAG-TAB.
           10 NI-RATENZAHLZUSCHLAG-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-RATENZAHLZUSCHLAG  REDEFINES  NI-RATENZAHLZUSCHLAG-TAB.
           10 NI-RZZRATENZAHLZID          PIC S9(4) COMP.
           10 NI-RZZRATENZAHLZPROZ        PIC S9(4) COMP.
        05  DCL-RATENZAHLZUSCHLAG.
           10 RZZRATENZAHLZID             PIC S9(4) COMP.
           10 RZZRATENZAHLZPROZ           PIC S9(3)V9(2) COMP-3.
      ******************************************************************
