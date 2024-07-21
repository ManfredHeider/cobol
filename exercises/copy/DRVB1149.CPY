      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1149.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1149.CPv  $
      *
      *   Rev 2.2   Feb 10 2017 15:00:54   T13AE30
      *AAS01186: DVAG-Wien: Admirals-Club
      *
      *   Rev 2.1   Jan 16 2017 11:01:50   t13ae30
      *AAS01186: DVAG-Wien: Admirals-Club
      *
      *   Rev 2.0   Sep 15 2016 12:46:08   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1149                                     *
      *    FUNKTION ADMIRALS-CLUB, WIEN KRITERIEN PRÜFEN             *
      *                                                              *
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
      *    FÜR DIE FUNKTION DRVB1149                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1149.
          05 PI-DRVB1149-PROGRAMM           PIC  X(008).
          05 PI-DRVB1149-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1149-MA-ST-NR           PIC  9(007).
          05 PI-DRVB1149-AST-MONAT          PIC  X(010).
      *
       01 PO-DRVB1149.
          05 PO-DRVB1149-RC                 PIC  X(003).
             88 PO-DRVB1149-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1149-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1149-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1149-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB1149-RC-ERR                       VALUE 'ERR'.

      *
      ******** Ende Copy-Member DRVB1149 *****************************


