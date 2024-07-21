      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0919.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0919.CPv  $
      *
      *   Rev 2.1   Jan 09 2009 17:04:42   t13ae30
      *AAS00779: Allfinanz "Champions-Club" 
      *
      *   Rev 2.0   Sep 01 2008 15:50:20   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0919                                     *
      *    FUNKTION CHAMPIONS-CLUB, KANDIDATEN ERMITTELN, PRAXISWEG  *
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
      *    FÜR DIE FUNKTION DRVB0919                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0919.
          05 PI-DRVB0919-PROGRAMM           PIC  X(008).
          05 PI-DRVB0919-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0919.
          05 PO-DRVB0919-RC                 PIC  X(003).
             88 PO-DRVB0919-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0919-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0919-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0919-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0919-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0919-DATEN.
             10 PO-DRVB0919-MA-ST-NR        PIC  9(007)  COMP-3.
      *
      ******** Ende Copy-Member DRVB0919 *****************************
