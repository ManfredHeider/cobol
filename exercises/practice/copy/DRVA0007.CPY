      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  31.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVA0007.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVA0007.CPv  $
      *
      *   Rev 2.2   Jan 20 2020 12:06:32   t13ae30
      *AVA00003: Einführung Clubs Allfinanz 1(VB-System)
      *
      *   Rev 2.1   Nov 12 2019 16:04:28   t13ex71
      *AVA00003: Einführung AS-Club Allfinanz 1(VB-System) - Copybo
      *oks (BPR)
      *
      *   Rev 2.0   Oct 31 2019 16:44:24   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVA0007                                     *
      *    FUNKTION ADMIRALS-CLUB KRITERIEN PRÜFEN                   *
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
      *    FÜR DIE FUNKTION DRVA0007                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVA0007.
          05 PI-DRVA0007-PROGRAMM           PIC  X(008).
          05 PI-DRVA0007-UFKT-CODE          PIC  9(002).
          05 PI-DRVA0007-MA-ST-NR           PIC  9(007).
          05 PI-DRVA0007-AST-MONAT          PIC  X(010).
      *
       01 PO-DRVA0007.
          05 PO-DRVA0007-RC                 PIC  X(003).
             88 PO-DRVA0007-RC-OK                        VALUE 'OK '.
             88 PO-DRVA0007-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVA0007-RC-LEER                      VALUE 'LER'.
             88 PO-DRVA0007-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVA0007-RC-ERR                       VALUE 'ERR'.
 
      *
      ******** Ende Copy-Member DRVA0007 *****************************

