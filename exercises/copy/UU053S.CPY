      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU053S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU053S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:50:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ******************************************************
      *                  COPY-MEMBER                       *
      ******************************************************
      *                                                    *
      *  BEZEICHNUNG       : DIMENSION 7 HISTORIENDATEI    *
      *  DATEINUMMER       : UU0138                        *
      *  COPYMEMBER        : UU053S                        *
      *  SATZLAENGE        : 42 BYTES                      *
      *  SCHLUESSEL        : D7H-VBNR                      *
      *  SCHLUESSELLAENGE  : 7 BYTES                       *
      *                                                    *
      *  GEÄNDERT: 25.06.1991 / SEIPEL                     *
      *            ==> D7H-DIR = NEUES FELD                *
      *            ==> NEUE SATZLÄNGE = 42 BYTE            *
      ******************************************************


       01  UU053S-REC.
           05  D7H-VBNR              PIC 9(7).
           05  D7H-DIR               PIC 999.
           05  D7H-AKT-HOLOG-MM      PIC 99.
           05  D7H-AKT-HOLOG-JJ      PIC 99.
           05  D7H-HOLOG-TABELLE.
               10  D7H-HOLOG         PIC X OCCURS 12.
           05  D7H-AKT-STUFE-MM      PIC 99.
           05  D7H-AKT-STUFE-JJ      PIC 99.
           05  D7H-STUFEN-TABELLE.
               10  D7H-STUFE         PIC 9 OCCURS 12.

