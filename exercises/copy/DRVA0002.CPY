      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVA0002.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVA0002.CPv  $
      *
      *   Rev 2.2   Jan 20 2020 12:05:22   t13ae30
      *AVA00003: Einführung Clubs Allfinanz 1(VB-System)
      *
      *   Rev 2.1   Jan 09 2009 17:03:30   t13ae30
      *AAS00779: Allfinanz "Champions-Club"
      *
      *   Rev 2.0   Sep 01 2008 13:13:12   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVA0002                                     *
      *    FUNKTION CHAMPIONS-CLUB KRITERIEN PRÜFEN                  *
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
      *    FÜR DIE FUNKTION DRVA0002                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVA0002.
          05 PI-DRVA0002-PROGRAMM           PIC  X(008).
          05 PI-DRVA0002-UFKT-CODE          PIC  9(002).
          05 PI-DRVA0002-MA-ST-NR           PIC  9(007).
          05 PI-DRVA0002-AST-MONAT          PIC  X(010).
      *
       01 PO-DRVA0002.
          05 PO-DRVA0002-RC                 PIC  X(003).
             88 PO-DRVA0002-RC-OK                        VALUE 'OK '.
             88 PO-DRVA0002-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVA0002-RC-LEER                      VALUE 'LER'.
             88 PO-DRVA0002-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVA0002-RC-ERR                       VALUE 'ERR'.
      *
      ******** Ende Copy-Member DRVA0002 *****************************

