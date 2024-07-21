      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU033S.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\UU033S.CPv  $
      *
      *   Rev 2.1   Nov 22 1999 13:59:06   T13AE35
      *AVB00441 - Dir-Nr-Erweiterung 1999 um
      *4-er Nummern
      *
      *   Rev 2.0   Feb 27 1999 15:15:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   05 Dec 1997 11:32:16   T13AE35
      *AVB00393 - Aenderung, Occurs auf 401
      *
      *   Rev 1.0   Nov 04 1995 09:48:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *****************************************************************
      **   18.12.1990 / DUONG                                         *
      **                                                              *
      **   INTERNE DIREKTIONSTABELLE FUER WS-EINTRAGUNG               *
      **                                                              *
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00393!21.11.97! 02 ! OCCURS AUF 401 ERHOEHT    ! J.HERING *
      * AVB00441!15.11.99! 03 ! OCCURS AUF 501 ERHOEHT    ! S.WALB   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      **                                                              *
      *****************************************************************
      *
       01  UU033S.
           02 DIRANSCHR  OCCURS 75 TO 501  DEPENDING  ON  I-33.
              05 TDIR                   PIC 9(03).
              05 TDIRVB                 PIC 9(07).
              05 TDIRZNA                PIC X(20).
              05 TDIRVNA                PIC X(20).
