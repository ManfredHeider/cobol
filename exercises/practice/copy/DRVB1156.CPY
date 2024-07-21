      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.11.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1156.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1156.CPv  $
      *
      *   Rev 2.2   Feb 10 2017 15:01:32   T13AE30
      *AAS01186: DVAG-Wien: Admirals-Club
      *
      *   Rev 2.1   Jan 16 2017 11:02:36   t13ae30
      *AAS01186: DVAG-Wien: Admirals-Club
      *
      *   Rev 2.0   Nov 03 2016 17:13:30   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1156                                     *
      *    FUNKTION ADMIRALS-CLUB, DVAG, WIEN                        *
      *    AKTUELLE MITGLIEDER ERMITTELN                             *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    15.09.2016                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS01186!15.09.16! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1156                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1156.
          05 PI-DRVB1156-PROGRAMM           PIC  X(008).
          05 PI-DRVB1156-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB1156.
          05 PO-DRVB1156-RC                 PIC  X(003).
             88 PO-DRVB1156-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1156-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB1156-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1156-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1156-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1156-DATEN.
             10 PO-DRVB1156-MA-ST-NR        PIC  9(007)  COMP-3.
      *
      ******** Ende Copy-Member DRVB1156 *****************************


