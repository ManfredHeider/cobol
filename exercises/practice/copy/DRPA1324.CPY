      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1324.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1324.CPv  $
      *
      *   Rev 2.1   Mar 09 2000 15:57:06   T13EX22
      *APA00085: Verarbeitung von Fälligkeiten älter 2 Jahre
      *
      *   Rev 2.0   Dec 14 1999 16:42:12   P13SP20
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRPA1324                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1324 IM UPPA0090        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    22.11.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00085!08.03.00!    ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1324: MIGRATIONSDATUM ZU PGEB-GES
      *                               ERMITTLEN
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1324.
           05  PI-DRPA1324-DATEN.
               10 PI-DRPA1324-PGEB-GES            PIC  S9(004) COMP.
      *
       01  PO-DRPA1324.
           05  PO-DRPA1324-RC                     PIC  X(003).
               88 PO-DRPA1324-RC-OK                    VALUE 'OK '.
               88 PO-DRPA1324-RC-NOT-FOUND             VALUE 'NFD'.
           05  PO-DRPA1324-DATEN.
               10 PO-DRPA1324-MIGRAT-DAT          PIC X(010).
      *---------------------------------------------------------------*
