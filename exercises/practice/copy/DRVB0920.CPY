      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0920.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0920.CPv  $
      *
      *   Rev 2.1   Jan 09 2009 17:04:52   t13ae30
      *AAS00779: Allfinanz "Champions-Club" 
      *
      *   Rev 2.0   Sep 03 2008 15:54:24   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0920                                     *
      *    FUNKTION CHAMPIONS-CLUB, KANDIDATEN ERMITTELN, GRUPPENWEG *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    01.09.2008                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00779!01.09.08! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0920                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0920.
          05 PI-DRVB0920-PROGRAMM           PIC  X(008).
          05 PI-DRVB0920-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0920.
          05 PO-DRVB0920-RC                 PIC  X(003).
             88 PO-DRVB0920-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0920-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0920-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0920-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0920-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0920-DATEN.
             10 PO-DRVB0920-MA-ST-NR        PIC  9(007)  COMP-3.
      *
      ******** Ende Copy-Member DRVB0920 *****************************
