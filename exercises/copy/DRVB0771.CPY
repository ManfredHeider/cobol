      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.04.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0771.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0771.CPv  $
      *
      *   Rev 2.1   Apr 06 2005 11:45:44   t13ex43
      *ACV00033: PGM - Neuer CTV-Online-Brief BUU1073
      *
      *   Rev 2.0   Apr 05 2005 15:21:34   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0771                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0024                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    05.04.2005                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * ACV00033!05.04.05!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0771 - ERMITTELN ERSTES ZV-DATUM AUS MA/ZVZ   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0771.
           05  PI-DRVB0771-DATEN.
               10  PI-DRVB0771-MA-ST-NR          PIC S9(09) COMP.
      *
       01  PO-DRVB0771.
           05  PO-DRVB0771-RC                    PIC  X(03).
               88  PO-DRVB0771-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0771-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0771-DATEN.
               10  PO-DRVB0771-ZV-DAT-VON        PIC  X(10).
      ****
      **** ENDE COPY DRVB0771 ****************************************
      ****
