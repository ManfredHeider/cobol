      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU919S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU919S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:07:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ******************************************************
      *                                                    *
      *         COPY-MEMBER ANREDESCHLUESSEL               *
      *                                                    *
      *                    ERSTELLT:  HUEBNER/GEDO         *
      *                               12. AUG. 1981        *
      *                    AENDERUNG: NAUMANN              *
      *                               13.11.1991           *
      *                    VON OCC. 17 AUF OCC. 18 ERWEI-  *
      *                    TERT                            *
      *                                                    *
      ******************************************************
       01  UU919S.
           05  ANREDE.
               10  FILLER          PIC X(10)        VALUE 'HERR     1'.
               10  FILLER          PIC X(10)        VALUE 'H.       1'.
               10  FILLER          PIC X(10)        VALUE 'H        1'.
               10  FILLER          PIC X(10)        VALUE 'FRAU     2'.
               10  FILLER          PIC X(10)        VALUE 'FR.      2'.
               10  FILLER          PIC X(10)        VALUE 'FR       2'.
               10  FILLER          PIC X(10)        VALUE 'FRAEULEIN3'.
               10  FILLER          PIC X(10)        VALUE 'FRL.     3'.
               10  FILLER          PIC X(10)        VALUE 'FRL      3'.
               10  FILLER          PIC X(10)        VALUE 'EHELEUTE 4'.
               10  FILLER          PIC X(10)        VALUE 'EHEL.    4'.
               10  FILLER          PIC X(10)        VALUE 'EHEL     4'.
               10  FILLER          PIC X(10)        VALUE 'FIRMA    5'.
               10  FILLER          PIC X(10)        VALUE 'FA.      5'.
               10  FILLER          PIC X(10)        VALUE 'FA       5'.
               10  FILLER          PIC X(10)        VALUE 'OHNE     9'.
               10  FILLER          PIC X(10)        VALUE '         0'.
               10  FILLER          PIC X(10)        VALUE 'ENDE     0'.
           05  ANREDE1     REDEFINES   ANREDE       OCCURS 18
                                               INDEXED BY WS-I1.
               10  ANR             PIC X(9).
               10  ANRZI           PIC 9.

