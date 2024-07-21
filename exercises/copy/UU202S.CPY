      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU202S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU202S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:56:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ******************************************************************
      *****                                                        *****
      *****        COPY-MEMBER BETREUER-VERTRAUENSMANN-DATEI       *****
      *****        DATEINUMMER: UU0027                             *****
      *****        COPYMEMBER:  UU202S                             *****
      *****        SATZLAENGE:  247    BYTES                       *****
      *****                                                        *****
      ******************************************************************
       01  UU202S.
           05  BETREUER-VM.
               10  BETRVM              PIC 9(7).
               10  TABVM               OCCURS 24.
                   20  JJMM-VM         PIC 9(4).
                   20  ANZ-VM          PIC 9(5)    COMP-3.
                   20  ANZ-VM-O        PIC 9(5)    COMP-3.
