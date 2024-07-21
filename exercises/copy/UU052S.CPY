      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU052S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU052S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:50:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BANZ-DATEI                                 *
      *    ---------------------------                                 *
      *                                                                *
      *    LAENGE = 61 BYTES         ERSTELLT AM 25.05.87 / BIEHL      *
      *                                                                *
      *    INDEX-DATEI MIT INDEX = BANZ-SCHL (= BANZ-PCNR)             *
      *    DATEINAME  = U.UU0227                                       *
      *    COPYMEMBER = UU052S                                         *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU052S.
           05  BANZ-SCHL.
               10  BANZ-PCNR             PIC 9(5).
           05  BANZ-SATZ.
               10 BANZ-TAB OCCURS 12.
                  15 BANZ-SUM        PIC 9(7)  COMP-3.
               10 BANZ-NEU           PIC 9(5)  COMP-3.
               10 BANZ-FREIPL        PIC 9(9)  COMP-3.
           05  BANZ-DUMMY REDEFINES BANZ-SATZ.
               10  BANZ-ERSTELLDAT         PIC 9(7)  COMP-3.
               10  FILLER                  PIC X(52).

