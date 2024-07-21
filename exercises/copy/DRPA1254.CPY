      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1254.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRPA1254.CPv  $
      *
      *   Rev 2.3   12 Jul 2000 12:03:12   T13EX21
      *APA00096 - Neue Funktion DRPA1351
      *
      *   Rev 2.2   Jul 27 1999 10:20:36   T13AE18
      *apa00012
      *
      *   Rev 2.1   Jun 22 1999 23:16:58   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 30 1999 08:44:14   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      ****************************************************************
      *    COPY-MEMBER: DRPA1254  FUNKTIONSAUFRUFE FÜR UPPA0117      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    08.06.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.06.99!2.01! NEUERSTELLUNG             ! BOHLINGER*
      * APA00012!21.07.99!2.02! NEUE FUNKTION DRPA1282    ! BOHLINGER*
      * APA00096!16.03.00!2.03! NEUE FUNKTION DRPA1351    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UPPA0117.
           05  PI-UPPA0117-FUNKTION            PIC  X(008).
               88 PI-UPPA0117-DRPA1255              VALUE 'DRPA1255'.
210799         88 PI-UPPA0117-DRPA1282              VALUE 'DRPA1282'.
160300         88 PI-UPPA0117-DRPA1351              VALUE 'DRPA1351'.
      *---------------------------------------------------------------*
