      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  30.03.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1280.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1280.CPv  $
      *
      *   Rev 2.1   Apr 03 2020 11:23:34   t13ae56
      *AVB01574: PGM - Meldung im VB23, wenn VB einer NFR gekündigt
      * hat
      *
      *   Rev 2.0   Mar 30 2020 16:47:32   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1280                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0126                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    30.03.2020                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VLA-430 !30.03.20!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1280 - ERMITTELN ABGEBENDER VB LFD. NACHFOLGER*
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1280.
           05  PI-DRVB1280-DATEN.
               10 PI-DRVB1280-NRB-AUFNEHM-NR   PIC S9(009)      BINARY.
      *
       01  PO-DRVB1280.
           05  PO-DRVB1280-RC                  PIC  X(003).
               88 PO-DRVB1280-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1280-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB1280-DATEN.
               10 PO-DRVB1280-NRB-ABGEB-NR-1   PIC  9(007).
               10 PO-DRVB1280-NRB-ABGEB-NR-2   PIC  9(007).
               10 PO-DRVB1280-NRB-ABGEB-NR-3   PIC  9(007).
               10 PO-DRVB1280-NRB-ABGEB-NR-4   PIC  9(007).
      ****
      **** ENDE COPY DRVB1280 ****************************************
      ****
