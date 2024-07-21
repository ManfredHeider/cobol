      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1529.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1529.CPv  $
      *
      *   Rev 2.1   Mar 20 2002 10:22:46   t13ex21
      *FPA00368 - Neuerstellung
      *
      *   Rev 2.0   04 Jul 2001 11:11:18   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1529                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ZURÜCKSETZEN CHECKPOINT >>               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    19.03.2002                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00368!19.03.02!2.01! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1529                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1529.
          05 PI-DRPA1529-DATEN.
             10 PI-DRPA1529-CHECKPOINT              PIC S9(004) BINARY.
      *
       01 PO-DRPA1529.
          05 PO-DRPA1529-RC                         PIC  X(003).
             88 PO-DRPA1529-RC-OK                         VALUE 'OK '.
      *
      ******* ENDE COPY-MEMBER **************************************
