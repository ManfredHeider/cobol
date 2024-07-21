      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  30.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVA0004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVA0004.CPv  $
      *
      *   Rev 2.1   Jan 21 2020 10:15:30   t13ex57
      *AVA00004: Einführung AS-Club Allfinanz 2(VB-System) - Pgms
      *
      *   Rev 2.0   Oct 30 2019 14:27:42   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
     ****************************************************************
      *    COPY-MEMBER: DRVA0004                                    *
      *    FUNKTION AS-CLUB KRITERIEN PRÜFEN                         *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: BOLL                                            *
      *    DATUM:    30.10.2019                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVA00004!30.10.19! 01 ! NEUERSTELLUNG             ! BOLL     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVA0004                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVA0004.
          05 PI-DRVA0004-PROGRAMM           PIC  X(008).
          05 PI-DRVA0004-UFKT-CODE          PIC  9(002).
          05 PI-DRVA0004-MA-ST-NR           PIC  9(007).
          05 PI-DRVA0004-AST-MONAT          PIC  X(010).
      *
       01 PO-DRVA0004.
          05 PO-DRVA0004-RC                 PIC  X(003).
             88 PO-DRVA0004-RC-OK                        VALUE 'OK '.
             88 PO-DRVA0004-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVA0004-RC-LEER                      VALUE 'LER'.
             88 PO-DRVA0004-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVA0004-RC-ERR                       VALUE 'ERR'.

      *
      ******** Ende Copy-Member DRVA0004 *****************************
      *---------------------------------------------------------------*

