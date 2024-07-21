      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1281.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1281.CPv  $
      *
      *   Rev 2.1   Jul 13 1999 10:27:16   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 30 1999 08:44:22   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1281                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. JNP für Vorvertrag <<               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                        *
      *    DATUM:    02.07.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!02.07.99!2.1 ! NEU                       ! Bohlinger*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1281                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1281.
          05 PI-DRPA1281-DATEN.
             10 PI-DRPA1281-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1281-PROD-PROD-SCHL          PIC S9(004) BINARY.
             10 PI-DRPA1281-PMBE-V-GUELTIG-AB       PIC X(026).
      *
       01 PO-DRPA1281.
          05 PO-DRPA1281-RC                         PIC  X(003).
             88 PO-DRPA1281-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1281-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1281-DATEN.
             10 PO-DRPA1281-PMPR-PG-PROD-DATEN      PIC  X(030).
      *
      ******* ENDE COPY-MEMBER **************************************
