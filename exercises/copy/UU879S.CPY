      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU879S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU879S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:05:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ******************************************************************
      *****                                                        *****
      *****        COPY-MEMBER  UU879S                             *****
      *****        - VORLAUFKARTE FUER PROGRAMM UU879              *****
      *****        - LAENGE     80 BYTES                           *****
      ******************************************************************
       01  UU879S.
               10  VK-KENNUNG          PIC X(8).
               10  FILLER              PIC X.
               10  VK-DTANR            PIC XXX.
               10  FILLER              PIC X.
               10  VK-DATUM1           PIC 999999.
               10  FILLER              PIC X.
               10  VK-BANDNR.
                   15  VK-BAND1        PIC XX.
                   15  VK-BAND2        PIC 9999.
               10  FILLER              PIC X.
               10  VK-DATUM2           PIC 999999.
               10  FILLER              PIC X(47).
