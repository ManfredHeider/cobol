      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.08.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1229.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1229.CPv  $
      *
      *   Rev 2.1   Aug 27 2019 11:58:26   t13ae56
      *AVB01581: PGM - Paket 1 - Zentrale Ermittlungsroutine
      *
      *   Rev 2.0   Aug 07 2019 11:30:48   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1229                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0234                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    07.08.2919                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VBL-819 !07.08.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1229 - ERMITTELN MIN. STATUS (FAPP)           *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1229.
           05  PI-DRVB1229-DATEN.
               10 PI-DRVB1229-DUMMY            PIC  X(001).
      *
       01  PO-DRVB1229.
           05  PO-DRVB1229-RC                  PIC  X(003).
               88 PO-DRVB1229-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1229-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB1229-DATEN.
               10 PO-DRVB1229-FAPP-STATUS      PIC  X(001).
      ****
      **** ENDE COPY DRVB1229 ****************************************
      ****
