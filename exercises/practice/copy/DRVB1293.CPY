      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.10.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1293.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1293.CPv  $
      *
      *   Rev 2.1   Dec 21 2020 17:34:40   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.0   Oct 20 2020 17:11:56   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * VLA-988 !12.10.20!2.01! NEU                       ! T13AE30  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1293                                 *
      *    ERMITTELN MA-BEFBED-NEU                                   *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1293.
          05 PI-DRVB1293-DATEN.
             10 PI-DRVB1293-MA-ST-NR         PIC S9(009) COMP.

      *
       01 PO-DRVB1293.
          05 PO-DRVB1293-RC                  PIC  X(003).
             88 PO-DRVB1293-RC-OK            VALUE 'OK '.
             88 PO-DRVB1293-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB1293-DATEN.
      *      DATUM IM ISO FORMAT
             10 PO-DRVB1293-MA-BEFBED-2020   PIC X(10).
      ******** ENDE COPY-MEMBER **************************************

