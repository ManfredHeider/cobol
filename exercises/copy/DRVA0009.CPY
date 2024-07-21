      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  14.11.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVA0009.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVA0009.CPv  $
      *
      *   Rev 2.2   Jan 21 2020 11:37:24   t13ex57
      *AVA00004: Einführung AS-Club Allfinanz 2(VB-System) - Pgms
      *
      *   Rev 2.1   Jan 21 2020 10:44:16   t13ex57
      *AVA00004: Einführung AS-Club Allfinanz 2(VB-System) (Kapt. Cl)
      *
      *   Rev 2.0   Nov 14 2019 14:47:28   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
****************************************************************
      *    COPY-MEMBER: DRVA0009                                     *
      *    FUNKTION KAPITÄNS-CLUB KRITERIEN PRÜFEN                   *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: BOLL                                          *
      *    DATUM:    14.11.2019                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVA00004!14.11.19! 01 ! NEUERSTELLUNG             ! BOLL     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVA0009                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVA0009.
          05 PI-DRVA0009-PROGRAMM           PIC  X(008).
          05 PI-DRVA0009-UFKT-CODE          PIC  9(002).
          05 PI-DRVA0009-MA-ST-NR           PIC  9(007).
          05 PI-DRVA0009-AST-MONAT          PIC  X(010).
      *
       01 PO-DRVA0009.
          05 PO-DRVA0009-RC                 PIC  X(003).
             88 PO-DRVA0009-RC-OK                        VALUE 'OK '.
             88 PO-DRVA0009-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVA0009-RC-LEER                      VALUE 'LER'.
             88 PO-DRVA0009-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVA0009-RC-ERR                       VALUE 'ERR'.
      *
      ******** Ende Copy-Member DRVA0009 *****************************


