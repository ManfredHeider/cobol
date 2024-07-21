      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  21.08.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1230.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1230.CPv  $
      *
      *   Rev 2.1   Aug 27 2019 11:59:02   t13ae56
      *AVB01581: PGM - Paket 1 - Zentrale Ermittlungsroutine
      *
      *   Rev 2.0   Aug 21 2019 12:39:04   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1230                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0234                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    21.08.2019                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VBL-819 !21.08.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1230 - ERMITTELN LAUFENDE NUMMER (FAPP) ART   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1230.
           05  PI-DRVB1230-DATEN.
               10 PI-DRVB1230-FAPP-ART         PIC  X(001).
               10 PI-DRVB1230-KNR-GESELL-TYP   PIC S9(004)      BINARY.
      *
       01  PO-DRVB1230.
           05  PO-DRVB1230-RC                  PIC  X(003).
               88 PO-DRVB1230-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1230-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB1230-DATEN.
               10 PO-DRVB1230-FAPP-LFDNR       PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB1230 ****************************************
      ****
