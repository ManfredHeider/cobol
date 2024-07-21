      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0823.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0823.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Aug 1998 11:32:56   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   14 Jan 1998 12:44:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0823 FUNKTION ERMITTELN VERSICHERUNGS-   *
      *                           STEUER                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    02.02.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00020!02.02.98! 01 !NEU                        !BOHLINGER *
      */AAN00167!        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0823                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0823.
          05 PI-DRPA0823-DATEN.
             10 PI-DRPA0823-PGEB-GES             PIC S9(004)  BINARY.
             10 PI-DRPA0823-PROD-PROD-SCHL       PIC S9(004)  BINARY.
             10 PI-DRPA0823-STICHTAG             PIC  X(010).
      *
       01 PO-DRPA0823.
          05 PO-DRPA0823-RC                      PIC  X(003).
             88 PO-DRPA0823-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0823-RC-NOT-FOUND             VALUE 'NFD'.
          05 PO-DRPA0823-DATEN.
             10 PO-DRPA0823-VERS-STEUER-STZ      PIC S9(003)V999
                                                     PACKED-DECIMAL.
             10 PO-DRPA0823-KNR-VERS-STEUER      PIC S9(004) BINARY.
      *
      ******** Ende Copy-Member **************************************
