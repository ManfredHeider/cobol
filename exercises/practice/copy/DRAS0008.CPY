      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0008.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAS0008.CPv  $
      *
      *   Rev 2.1   Mar 01 2000 14:18:26   T13AE26
      *AAS00201 Jahreswettbewerbe 2000
      *
      *   Rev 2.0   Feb 27 1999 11:26:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:56:02   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00201!08.02.00! 2.1! Neu                       ! T. Moll  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                               *
      *            COPY-MEMBER  DATEI für Gruppenumsatz-Wettbewerb    *
      *            DATEINUMMER: ?                                     *
      *            COPYMEMBER:  DRAS0008                              *
      *            SATZLAENGE:  28 BYTES                              *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRAS0008-SATZ.
           05  VBNR                        PIC  9(07).
           05  STUFE-STICHTAG              PIC  X(04).
           05  UMS-WBW-AUFGELAUFEN-A       PIC S9(07)V9(01)  COMP-3.
           05  UMS-WBW-AUFGELAUFEN-B       PIC S9(07)V9(01)  COMP-3.
           05  UMS-WBW-AUFGELAUFEN-WERTUNG PIC S9(07)V9(01)  COMP-3.
           05  ANZ-MON-WBW                 PIC  9(02).
      *---------------------------------------------------------------*
