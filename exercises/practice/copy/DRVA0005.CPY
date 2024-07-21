      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  31.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVA0005.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVA0005.CPv  $
      *
      *   Rev 2.1   Jan 20 2020 12:05:46   t13ae30
      *AVA00003: Einführung Clubs Allfinanz 1(VB-System)
      *
      *   Rev 2.0   Oct 31 2019 16:44:00   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVA0005                                     *
      *    FUNKTION KAPITÄNS-CLUB KRITERIEN PRÜFEN                   *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: B.Pretsch                                       *
      *    DATUM:    30.10.2019                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * VLA-84  !30.10.19! 01 ! Neuerstellung             !B.Pretsch *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVA0005                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVA0005.
          05 PI-DRVA0005-PROGRAMM           PIC  X(008).
          05 PI-DRVA0005-UFKT-CODE          PIC  9(002).
          05 PI-DRVA0005-MA-ST-NR           PIC  9(007).
      *
       01 PO-DRVA0005.
          05 PO-DRVA0005-RC                 PIC  X(003).
             88 PO-DRVA0005-RC-OK                        VALUE 'OK '.
             88 PO-DRVA0005-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVA0005-RC-LEER                      VALUE 'LER'.
             88 PO-DRVA0005-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVA0005-RC-ERR                       VALUE 'ERR'.
      *
      ******** Ende Copy-Member DRVB0607 *****************************

