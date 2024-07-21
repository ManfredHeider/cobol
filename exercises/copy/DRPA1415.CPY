      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1415.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1415.CPv  $
      *
      *   Rev 2.1   09 Aug 2000 08:23:26   T13EX22
      *APA00098: PIN-Prüfung für iii
      *
      *   Rev 2.0   12 Jul 2000 12:32:42   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1415 FUNKTION PIN FÜR III PRÜFEN         *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    07.08.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00098!07.08.00!2.0 !NEU                        ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1415                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1415.
             10 PI-DRPA1415-ZVP-VERSION     PIC S9(004) BINARY.
             10 PI-DRPA1415-PGEB-GES        PIC S9(004) BINARY.
             10 PI-DRPA1415-PIN             PIC S9(009) BINARY.
             10 PI-DRPA1415-VERTR-BEGINN    PIC  X(010).

       01 PO-DRPA1415.
          05 PO-DRPA1415-RC                 PIC  X(003).
             88 PO-DRPA1415-RC-OK                VALUE 'OK '.
             88 PO-DRPA1415-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
