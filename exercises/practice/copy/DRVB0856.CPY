      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.06.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0856.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0856.CPv  $
      *
      *   Rev 2.1   30 Oct 2007 11:07:26   t13ae30
      *AVB00905: Allfinanz - Tabellenerweiterungen TWI
      *
      *   Rev 2.0   21 Jun 2007 10:38:18   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0856  FUNKTION                           *
      *                           ERMITTELN DES KL-TYPS              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    21.06.2007                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00905!21.06.07! 05 ! EINFÜHRUNG ALLFINANZ      ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0856                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0856.
          05 PI-DRVB0856-DATEN.
             10 PI-DRVB0856-MA-ST-NR         PIC S9(009) BINARY.
      *
       01 PO-DRVB0856.
          05 PO-DRVB0856-RC                  PIC  X(003).
             88 PO-DRVB0856-RC-OK            VALUE 'OK '.
             88 PO-DRVB0856-RC-NFD           VALUE 'NFD'.

          05 PO-DRVB0856-DATEN.
             10 PO-DRVB0856-KNR-KL-TYP       PIC S9(004) COMP.
      *
      ******** Ende Copy-Member **************************************
