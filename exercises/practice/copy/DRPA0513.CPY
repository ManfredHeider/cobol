      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0513.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA0513.CPv  $
      *
      *   Rev 2.7   Jul 15 2005 11:45:30   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.6   Jul 15 2005 10:18:32   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.5   Jul 01 2005 17:14:30   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.4   Jul 01 2005 17:05:36   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.3   Jul 01 2005 16:56:54   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.2   Jul 01 2005 16:36:00   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.1   May 28 1999 17:51:24   T13EX21
      *APA00012 - formale Korrektur
      *
      *   Rev 2.0   May 25 1999 13:25:44   T13AE18
      *apa00012
      *
      *   Rev 1.2   May 25 1999 13:17:42   T13AE18
      *APA00012
      *
      *   Rev 1.1   07 Aug 1998 11:39:06   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   12 Sep 1997 16:59:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    Copy-Member: DRPA0513  Funktionen List-OZP                *
      *                           Prko TBPA0023                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    19.09.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!19.09.97! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      * AFPA0020!05.11.98! 01 ! NEUE FUNKTION DRPA1119    ! BOHLINGER*
      * AAN00582!01.07.05!2.2 ! DRPA1652                  ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZXPA0023.
           05 PI-ZXPA0023-FUNKTION            PIC X(008).
              88 PI-ZXPA0023-DRPA0514             VALUE 'DRPA0514'.
051198        88 PI-ZXPA0023-DRPA1119             VALUE 'DRPA1119'.
010705        88 PI-ZXPA0023-DRPA1652             VALUE 'DRPA1652'.
