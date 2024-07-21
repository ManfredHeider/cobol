      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0735.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0735.CPv  $
      *
      *   Rev 2.1   Jan 19 2000 08:48:28   T13EX21
      *APA00063 - DTA-Sachbearbeiter
      *
      *   Rev 2.0   Feb 27 1999 14:29:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:44:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0735                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PEG ZUM LÖSCHEN KENNZEICHNEN >>          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    26.10.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00063!26.10.99!2.0 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0735                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0735.
          05 PI-DRPA0735-PMPG-ID            PIC S9(15) PACKED-DECIMAL.
      *
       01 PO-DRPA0735.
          05 PO-DRPA0735-RC                 PIC  X(003).
             88 PO-DRPA0735-RC-OK                       VALUE 'OK '.
             88 PO-DRPA0735-RC-NFD                      VALUE 'NFD'.
             88 PO-DRPA0735-RC-CHANGED                  VALUE 'CHA'.
      *
      ******* ENDE COPY-MEMBER **************************************
