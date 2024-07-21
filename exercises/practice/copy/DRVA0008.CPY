      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.11.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVA0008.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVA0008.CPv  $
      *
      *   Rev 2.1   Jan 21 2020 10:16:30   t13ex57
      *AVA00004: Einführung AS-Club Allfinanz 2(VB-System) - Pgms
      *
      *   Rev 2.0   Nov 06 2019 17:28:44   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
****************************************************************
      *    COPY-MEMBER: DRVB0681                                     *
      *    FUNKTION BES.LEIST. IM AS-CLUB KRITERIEN PRÜFEN           *
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
      *    FÜR DIE FUNKTION DRVA0008                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVA0008.
          05 PI-DRVA0008-PROGRAMM           PIC  X(008).
          05 PI-DRVA0008-UFKT-CODE          PIC  9(002).
          05 PI-DRVA0008-MA-ST-NR           PIC  9(007).
          05 PI-DRVA0008-AST-MONAT          PIC  X(010).
      *
       01 PO-DRVA0008.
          05 PO-DRVA0008-RC                 PIC  X(003).
             88 PO-DRVA0008-RC-OK                        VALUE 'OK '.
             88 PO-DRVA0008-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVA0008-RC-LEER                      VALUE 'LER'.
             88 PO-DRVA0008-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVA0008-RC-ERR                       VALUE 'ERR'.
      *
      ******** Ende Copy-Member DRVA0008 *****************************

