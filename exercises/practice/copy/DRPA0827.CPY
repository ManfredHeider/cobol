      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0827.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0827.CPv  $
      *
      *   Rev 2.2   19 Mar 2001 09:15:48   T13AE23
      *AAN00292 - PEW Banken -
      *
      *   Rev 2.1   Mar 09 2000 15:57:04   T13EX22
      *APA00085: Verarbeitung von Fälligkeiten älter 2 Jahre
      *
      *   Rev 2.0   Feb 27 1999 14:29:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Aug 1998 11:33:18   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   14 Jan 1998 12:44:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0827  FUNKTIONEN ZU UPPA0090             *
      *                "PRODUKTGEBERGESELLSCHAFTS-INFORMATIONEN"     *
      *                                                              *
      *    DIE EINZELNEN FUNKTIONEN :                                *
      *    DRPA0834 - ERMITTELN RATENZAHLUNGSZUSCHLAG                *
      *                                                              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    06.02.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00020!06.02.98!01.01!NEUERSTELLUNG             ! BOHLINGER*
      */AAN00167!        !     !                          !          *
      * APA00085!08.03.00!01.02!DRPA1324 MIGRATIONSDATUM  ! MUND     *
      *         !        !     !ERMITTLEN NEU             !          *
      * AAN00292!08.03.01!2.2  !DRPA1480 MIGRATIONSDATUM  ! RAHNER   *
      *         !        !     !PEW ERMITTELN NEU         !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UPPA0090.
           05 PI-UPPA0090-FUNKTION            PIC X(008).
              88 PI-UPPA0090-DRPA0834             VALUE 'DRPA0834'.
080300        88 PI-UPPA0090-DRPA1324             VALUE 'DRPA1324'.
080301        88 PI-UPPA0090-DRPA1480             VALUE 'DRPA1480'.
