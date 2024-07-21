      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1261.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1261.CPv  $
      *
      *   Rev 2.2   Jan 18 2000 15:01:20   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.1   Jan 18 2000 14:42:30   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Mar 30 1999 08:44:16   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:48   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1261                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1261 IM ZWPA0026        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    22.11.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00071!22.11.99!    ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1261: AKTUELLSTE PGV ZURODNUNG ERMITTELN
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1261.
           05  PI-DRPA1261-DATEN.
               10 PI-DRPA1261-PGEB-GES            PIC  S9(004) COMP.
               10 PI-DRPA1261-PGVZ-PGV            PIC   X(008).
               10 PI-DRPA1261-PGVZ-VZ-POSITIV     PIC   X(001).
      *
       01  PO-DRPA1261.
           05  PO-DRPA1261-RC                     PIC  X(003).
               88 PO-DRPA1261-RC-OK                    VALUE 'OK '.
               88 PO-DRPA1261-RC-NOT-FOUND             VALUE 'NFD'.
           05  PO-DRPA1261-DATEN.
               10 PO-DRPA1261-GUELTIG-AB          PIC  X(010).
               10 PO-DRPA1261-GUELTIG-BIS         PIC  X(010).
      *
