      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU070S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU070S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:52:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER ID-DATEI (KUNDEN-NR.)                      *
      *    --------------------------------------                      *
      *                                                                *
      *    LAENGE = 185 BYTES        ERSTELLT AM 06.05.82.             *
      *                              HUEBNER (GEDO)                    *
      *                                                                *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU070S.
           10  ID-SA                   PIC 99.
           10  ID-KEY.
               20  ID-KDNR             PIC 9(7).
               20  ID-SANR             PIC 9999.
           10  ID-PRZ                  PIC 99.
           10  ID-MC                   PIC X(17).
           10  ID-FOLGE                PIC 9(5)           COMP-3.
           10  ID-SATZ.
               20  ID-VERTRTAB         OCCURS 5.
                   30  ID-VNR          PIC 9(15)          COMP-3.
                   30  ID-GES          PIC 99.
                   30  ID-VBE          PIC 9(7)           COMP-3.
                   30  ID-KZE          PIC 99.
                   30  ID-VBF          PIC 9(7)           COMP-3.
                   30  ID-KZF          PIC 99.
                   30  FILLER          PIC X(8).
           10  ID-DUMMYSATZ     REDEFINES     ID-SATZ.
               20  ID-ZNR              PIC 9(7)           COMP-3.
               20  ID-TAB              OCCURS 36.
                   30  ID-FRNR         PIC 9(7)           COMP-3.

