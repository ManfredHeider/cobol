      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0861.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0861.CPv  $
      *
      *   Rev 2.1   30 Oct 2007 11:07:46   t13ae30
      *AVB00905: Allfinanz - Tabellenerweiterungen TWI
      *
      *   Rev 2.0   05 Jul 2007 13:20:12   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0861  FUNKTION                           *
      *                           ERMITTELN DES MAX-DAT-VON          *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    05.07.2007                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00905!05.07.07! 01 ! EINFÜHRUNG ALLFINANZ      ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0861                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0861.
          05 PI-DRVB0861-DATEN.
             10 PI-DRVB0861-KL-ST-NR         PIC S9(009) BINARY.
      *
       01 PO-DRVB0861.
          05 PO-DRVB0861-RC                  PIC  X(003).
             88 PO-DRVB0861-RC-OK            VALUE 'OK '.
             88 PO-DRVB0861-RC-NFD           VALUE 'NFD'.

          05 PO-DRVB0861-DATEN.
             10 PO-DRVB0861-KL-DAT-VON       PIC  X(010).
      *
      ******** Ende Copy-Member **************************************
