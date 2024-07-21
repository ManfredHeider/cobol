      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.08.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0916.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0916.CPv  $
      *
      *   Rev 2.2   Dec 10 2009 12:47:16   t13ex43
      *ASL00335: PGM - Anpassung Ermittlung ZVZ
      *
      *   Rev 2.1   Nov 20 2008 14:05:04   t13ex43
      *ASL00309: PGM - Stichtagsprüfung ZVZ
      *
      *   Rev 2.0   Aug 28 2008 16:03:26   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0916                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0030                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    19.11.2008                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00309!19.11.08!2.1 ! NEU                       ! KLEEMEIER *
101209* ASL00335!10.12.09!2.2 ! ANPASSUNG AN NEUE ZVZ     ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0916 - ERMITTELN AKTUELLSTE ZVZ-DATEN         *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0916.
           05  PI-DRVB0916-DATEN.
               10  PI-DRVB0916-MA-ST-NR         PIC S9(09)       BINARY.
101209****     10  PI-DRVB0916-ZV-NR            PIC  X(01).
      *
       01  PO-DRVB0916.
         03  PO-DRVB0916-RC                     PIC  X(03).
               88  PO-DRVB0916-RC-OK                 VALUE 'OK '.
               88  PO-DRVB0916-RC-NOT-FOUND          VALUE 'NFD'.
         03  PO-DRVB0916-DATEN.
101209         10  PO-DRVB0916-ZV-NR            PIC  X(01).
               10  PO-DRVB0916-ZVZ-DAT-VON      PIC  X(10).
               10  PO-DRVB0916-ZVZ-DAT-BIS      PIC  X(10).
101209****     10  PO-DRVB0916-ZVZ-KUE-DAT      PIC  X(10).
      ****
      **** ENDE COPY DRVB0916 ****************************************
      ****
