      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1257.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1257.cpv  $
      *
      *   Rev 2.1   Jun 09 1999 13:32:26   T01JM2S
      *APA00012 - UMSETZUNG FACHL. ANFORDERUNG FKPAG026
      *
      *   Rev 2.0   Mar 30 1999 08:44:16   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1257                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PEG-ÄNDERUNG E902   >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    07.06.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!07.06.99!2.1 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1257                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1257.
          05 PI-DRPA1257-PMPG-ALT.
             COPY DRPA1258.
          05 PI-DRPA1257-PMPG-NEU.
             COPY DRPA1258.
      *
       01 PO-DRPA1257.
          05 PO-DRPA1257-RC                         PIC  X(003).
             88 PO-DRPA1257-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1257-RC-CHANGED                    VALUE 'CHA'.
      *
      ******* ENDE COPY-MEMBER **************************************
