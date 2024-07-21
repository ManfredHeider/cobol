      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU036S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU036S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:49:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *****************************************************************
      **   18.12.1990 / DUONG                                         *
      **                                                              *
      **   SATZAUFBAU DER DIREKTIONSDATEI FUER FD-EINTRAGUNG          *
      **                                                              *
      *****************************************************************
      *
       01  UU036S.
           02 ANSCHRIFTEN.
              10 DDIR              PIC 9(03).
              10 DMANR             PIC 9(07).
              10 DZUNAM            PIC X(20).
              10 DVNAME            PIC X(20).
              10 FILLER            PIC X(30).
