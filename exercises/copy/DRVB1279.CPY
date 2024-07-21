      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  04.02.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1279.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1279.CPv  $
      *
      *   Rev 2.1   31 Mar 2020 11:46:04   t13ae56
      *AVB01574: PGM - Nachfolgeregelung Praxis (VB61)
      *
      *   Rev 2.0   Feb 04 2020 13:49:42   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1279                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LIST-OZP ZXVB0127                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    04.02.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-670 !04.02.20!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1279 - ERMITTELN NRB-SÄTZE FÜR VBNR-PÄRCHEN   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1279.
          05 PI-DRVB1279-UFKT-CODE             PIC  9(002).
          05 PI-DRVB1279-NRP-ABGEB-NR          PIC S9(009)      BINARY.
          05 PI-DRVB1279-NRP-AUFNEHM-NR        PIC S9(009)      BINARY.
          05 PI-DRVB1279-KNR-NR-TYP            PIC S9(004)      BINARY.
      *
       01 PO-DRVB1279.
          05 PO-DRVB1279-RC                    PIC  X(003).
             88 PO-DRVB1279-RC-OK                         VALUE 'OK '.
             88 PO-DRVB1279-RC-EOF                        VALUE 'EOF'.
             88 PO-DRVB1279-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRVB1279-DATEN.
             10 PO-DRVB1279-NRP-SATZ           PIC  X(196).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1279   --------------------------*
      *---------------------------------------------------------------*
