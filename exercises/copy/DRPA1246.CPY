      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1246.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1246.cpv  $
      *
      *   Rev 2.1   Jun 25 1999 11:20:38   T13EX22
      * 
      *
      *   Rev 2.0   Mar 30 1999 08:44:12   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:32   P13SP20
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRPA1246                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<DAISY-PEW markieren>>                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    26.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!26.05.99! 01 ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1246                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1246.
          05 PI-DRPA1246-PIN                   PIC S9(009) BINARY.
          05 PI-DRPA1246-PGEB-GES              PIC S9(4) USAGE COMP.
          05 PI-DRPA1246-KNR-PEWA-ZUSTAND      PIC S9(004) BINARY.

      *
       01 PO-DRPA1246.
          05 PO-DRPA1246-RC                    PIC   X(003).
             88 PO-DRPA1246-RC-OK              VALUE  'OK '.
             88 PO-DRPA1246-RC-NOT-FOUND       VALUE  'NFD'.
             88 PO-DRPA1246-RC-ABGER           VALUE  'ABG'.
      ********** ENDE COPY-MEMBER **************************************
