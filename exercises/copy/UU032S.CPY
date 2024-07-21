      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU032S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU032S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:48:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ******************************************************************
      *****        AKTIONSSCHLUESSELTABELLEN - SATZ                *****
      ******************************************************************
CCC009*    SKIP1
       01  UU032S.
           10  ATSA                    PIC 99.
           10  ATGES                   PIC 99.
           10  ATPRODSCHL               PIC 99.
           10  ATAKTION OCCURS 3.
               20  ATAKTSCHL           PIC 99.
               20  ATAKTVON            PIC 9(6)    COMP-3.
               20  ATAKTBIS            PIC 9(6)    COMP-3.
           10  ATLEER                  PIC X(4).
CCC009*    SKIP2
      *                                *
      *    LAENGE = 40 BYTES           *
      *                                *

