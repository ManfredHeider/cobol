      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.08.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0789.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0789.CPv  $
      *
      *   Rev 2.1   Sep 19 2005 12:14:50   t13ae30
      *AVB00811: RFT - Rangfrei Tod
      *
      *   Rev 2.0   Aug 05 2005 09:54:08   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0789  FUNKTION FüR ZWVB0005 RFT          *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    05.08.2005                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00811!05.08.05! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0789                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0789.
          05 PI-DRVB0789-DATEN.
             10 PI-DRVB0789-MA-ST-NR         PIC S9(009)  COMP.
             10 PI-DRVB0789-ZEITPUNKT-ISO    PIC  X(10).
      *
       01 PO-DRVB0789.
          05 PO-DRVB0789-RC                  PIC  X(003).
             88 PO-DRVB0789-RC-OK            VALUE 'OK '.
             88 PO-DRVB0789-RC-NOT-FOUND     VALUE 'NFD'.
      *
      ******** Ende Copy-Member **************************************
