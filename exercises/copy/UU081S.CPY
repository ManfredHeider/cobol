      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU081S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU081S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:54:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER LAENDERSCHLUESSEL BFG                      *
      *    --------------------------------------                      *
      *                                                                *
      *    VERSION 01                ERSTELLT AM 04.12.90.             *
      *                                          KUHN                  *
      *                                                                *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU081S.
         05  BFG-LAND.
           10  FILLER                  PIC 9(3)     VALUE 000.
           10  FILLER                  PIC X(5)     VALUE '     '.
           10  FILLER                  PIC 9(3)     VALUE 001.
           10  FILLER                  PIC X(5)     VALUE 'F    '.
           10  FILLER                  PIC 9(3)     VALUE 003.
           10  FILLER                  PIC X(5)     VALUE 'NL   '.
           10  FILLER                  PIC 9(3)     VALUE 004.
           10  FILLER                  PIC X(5)     VALUE '     '.
           10  FILLER                  PIC 9(3)     VALUE 005.
           10  FILLER                  PIC X(5)     VALUE 'I    '.
           10  FILLER                  PIC 9(3)     VALUE 006.
           10  FILLER                  PIC X(5)     VALUE 'GB   '.
           10  FILLER                  PIC 9(3)     VALUE 007.
           10  FILLER                  PIC X(5)     VALUE 'IRL  '.
           10  FILLER                  PIC 9(3)     VALUE 008.
           10  FILLER                  PIC X(5)     VALUE 'DK   '.
           10  FILLER                  PIC 9(3)     VALUE 009.
           10  FILLER                  PIC X(5)     VALUE 'GR   '.
           10  FILLER                  PIC 9(3)     VALUE 010.
           10  FILLER                  PIC X(5)     VALUE 'P    '.
           10  FILLER                  PIC 9(3)     VALUE 011.
           10  FILLER                  PIC X(5)     VALUE 'E    '.
           10  FILLER                  PIC 9(3)     VALUE 024.
           10  FILLER                  PIC X(5)     VALUE 'IS   '.
           10  FILLER                  PIC 9(3)     VALUE 025.
           10  FILLER                  PIC X(5)     VALUE 'FR   '.
           10  FILLER                  PIC 9(3)     VALUE 028.
           10  FILLER                  PIC X(5)     VALUE 'N    '.
           10  FILLER                  PIC 9(3)     VALUE 030.
           10  FILLER                  PIC X(5)     VALUE 'S    '.
           10  FILLER                  PIC 9(3)     VALUE 032.
           10  FILLER                  PIC X(5)     VALUE 'SF   '.
           10  FILLER                  PIC 9(3)     VALUE 036.
           10  FILLER                  PIC X(5)     VALUE 'CH   '.
           10  FILLER                  PIC 9(3)     VALUE 038.
           10  FILLER                  PIC X(5)     VALUE 'A    '.
           10  FILLER                  PIC 9(3)     VALUE 043.
           10  FILLER                  PIC X(5)     VALUE 'AND  '.
           10  FILLER                  PIC 9(3)     VALUE 044.
           10  FILLER                  PIC X(5)     VALUE 'GBZ  '.
           10  FILLER                  PIC 9(3)     VALUE 045.
           10  FILLER                  PIC X(5)     VALUE 'V    '.
           10  FILLER                  PIC 9(3)     VALUE 046.
           10  FILLER                  PIC X(5)     VALUE 'M    '.
           10  FILLER                  PIC 9(3)     VALUE 048.
           10  FILLER                  PIC X(5)     VALUE 'YU   '.
           10  FILLER                  PIC 9(3)     VALUE 052.
           10  FILLER                  PIC X(5)     VALUE 'TR   '.
           10  FILLER                  PIC 9(3)     VALUE 058.
           10  FILLER                  PIC X(5)     VALUE 'O    '.
           10  FILLER                  PIC 9(3)     VALUE 102.
           10  FILLER                  PIC X(5)     VALUE 'B    '.
           10  FILLER                  PIC 9(3)     VALUE 104.
           10  FILLER                  PIC X(5)     VALUE 'L    '.
           10  FILLER                  PIC 9(3)     VALUE 388.
           10  FILLER                  PIC X(5)     VALUE 'ZA   '.
           10  FILLER                  PIC 9(3)     VALUE 400.
           10  FILLER                  PIC X(5)     VALUE 'USA  '.
           10  FILLER                  PIC 9(3)     VALUE 404.
           10  FILLER                  PIC X(5)     VALUE 'CDN  '.
           10  FILLER                  PIC 9(3)     VALUE 732.
           10  FILLER                  PIC X(5)     VALUE 'J    '.
           10  FILLER                  PIC 9(3)     VALUE 800.
           10  FILLER                  PIC X(5)     VALUE 'AUS  '.
           10  FILLER                  PIC 9(3)     VALUE 804.
           10  FILLER                  PIC X(5)     VALUE 'NZ   '.
         05  BFG-LAND-TAB REDEFINES BFG-LAND  OCCURS 33,
             ASCENDING KEY IS BFG-AUSLNUM,
             INDEXED BY IND-BFG.
           10  BFG-AUSLNUM             PIC 9(3).
           10  BFG-AUSLBEZ             PIC X(5).
