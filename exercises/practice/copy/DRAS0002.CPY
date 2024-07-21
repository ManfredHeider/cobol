      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0002.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAS0002.CPv  $
      *
      *   Rev 2.0   Mar 01 1999 11:06:50   T13AE26
      *Wettbewerbe 99
      *
      *   Rev 1.0   12 Jan 1999 15:55:28   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *      VERSION
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00088!12.01.99!01.0! Zielwettbewerb 1999       !T. Moll   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                               *
      *            COPY-MEMBER Zwischendatei Zielwettbewerb 1999      *
      *            DATEINUMMER: ?        (temp. Datei)                *
      *            COPYMEMBER:  DRAS0002                              *
      *            SATZLAENGE:  32 BYTES                              *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRAS0002-SATZ.
           05  SATZART                 PIC  X(01).
           05  BETREUER                PIC  9(07).
           05  UMS-WBW-KUM             PIC S9(10)V9(01)  COMP-3.
           05  ZIEL-WBW-MTL            PIC S9(10)V9(01)  COMP-3.
           05  BASIS-KUM               PIC S9(10)V9(01)  COMP-3.
           05  BASIS-MTL               PIC S9(10)V9(01)  COMP-3.
      *---------------------------------------------------------------*
