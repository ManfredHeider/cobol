      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  23.09.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1236.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1236.CPv  $
      *
      *   Rev 2.1   Jan 17 2020 10:10:46   t13ae56
      *AVB01581: PGM - Paket 2 - Dialoge VB31 + VB49
      *
      *   Rev 2.0   Sep 23 2019 15:24:24   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1236                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LIST-OZP ZXVB0027                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    23.09.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-819 !23.09.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1236 - ERMITTELN FAPZ-SÄTZE FÜR VB-NUMMER     *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1236.
          05 PI-DRVB1236-UFKT-CODE             PIC  9(002).
          05 PI-DRVB1236-MA-ST-NR              PIC S9(009)      BINARY.
      *
       01 PO-DRVB1236.
          05 PO-DRVB1236-RC                    PIC  X(003).
             88 PO-DRVB1236-RC-OK                         VALUE 'OK '.
             88 PO-DRVB1236-RC-EOF                        VALUE 'EOF'.
             88 PO-DRVB1236-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRVB1236-DATEN.
             10 PO-DRVB1236-FAPZ-SATZ          PIC  X(135).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1236   --------------------------*
      *---------------------------------------------------------------*
