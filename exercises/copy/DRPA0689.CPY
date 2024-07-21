      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0689.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0689.CPv  $
      *
      *   Rev 2.1   Oct 12 2004 08:10:52   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Feb 27 1999 14:29:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 11:10:06   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 17:00:22   P13SP99
      *Initial revision.
      *
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0689  FUNKTIONSAUFRUFE FÜR ZXPA0115      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    09.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.12.97! 01  ! NEUERSTELLUNG            ! PARENT   *
      * AFPA0012!08.07.98!01.02! neue funktion drpa1006   ! bohlinger*
      * APA00218!15.07.04! 2.01! NEUE FUNKTION DRPA1619   ! SCHEIDEL *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  PI-ZXPA0115.
           05  PI-ZXPA0115-FUNKTION            PIC  X(008).
               88 PI-ZXPA0115-DRPA0691              VALUE 'DRPA0691'.
               88 PI-ZXPA0115-DRPA1006              VALUE 'DRPA1006'.
150704         88 PI-ZXPA0115-DRPA1619              VALUE 'DRPA1619'.
      *
