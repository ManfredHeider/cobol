      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0271.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drat0271.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Nov 1998 12:34:08   T13EX26
      *apa00033 - Neuerstellung
      *
      *   Rev 1.0   06 Jul 1998 10:41:52   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0062  FUNKTION DRUCKEN FEHLERPROTOKOLL   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Parent                                          *
      *    DATUM:    03.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY   !03.07.98! 01 ! NEUERSTELLUNG             ! Parent   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRAT0271.
          05 PI-DRAT0271-D-ERR-SCHLUESSEL.
             10 PI-DRAT0271-D-ERR-ZEILE-26  PIC  X(080).
             10 PI-DRAT0271-D-ERR-ZEILE-27  PIC  X(080).
             10 PI-DRAT0271-D-ERR-ZEILE-28  PIC  X(080).
             10 PI-DRAT0271-D-ERR-ZEILE-29  PIC  X(080).
      *
       01 PO-DRAT0271.
          05 PO-DRAT0271-RC                 PIC  X(003).
             88 PO-DRAT0271-RC-OK           VALUE 'OK '.
             88 PO-DRAT0271-RC-NOK          VALUE 'NOK'.
      *
      ***** ENDE-COPY-MEMBER *****************************************
