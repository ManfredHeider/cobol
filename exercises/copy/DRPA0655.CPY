      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0655.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0655.CPv  $
      *
      *   Rev 2.3   Oct 12 2004 08:10:42   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.2   Sep 07 1999 13:26:24   T13EX25
      *APA00051 Neue Funktion DRPA1300
      *
      *   Rev 2.1   May 27 1999 16:41:44   T13EX26
      *MFE die 2.
      *
      *   Rev 2.0   May 26 1999 16:57:50   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:58:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0655  FUNKTIONSAUFRUFE FÜR ZXPA0113      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    03.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.12.97! 01 ! NEUERSTELLUNG             ! PARENT   *
      * AFPA0012!15.01.98! 02 ! NEUE FUNKTION DRPA0933    ! BOHLINGER*
      * FPA00063!26.08.99! 03 ! NEUE FUNKTION DRPA1300    ! Jäger    *
      * APA00218!15.07.04! 2.4! NEUE FUNKTION DRPA1618    ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZXPA0113.
           05  PI-ZXPA0113-FUNKTION            PIC  X(008).
               88 PI-ZXPA0113-DRPA0660              VALUE 'DRPA0660'.
               88 PI-ZXPA0113-DRPA0661              VALUE 'DRPA0661'.
               88 PI-ZXPA0113-DRPA0933              VALUE 'DRPA0933'.
260899         88 PI-ZXPA0113-DRPA1300              VALUE 'DRPA1300'.
150704         88 PI-ZXPA0113-DRPA1618              VALUE 'DRPA1618'.
