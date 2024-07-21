      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.07.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0946.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0946.CPv  $
      *
      *   Rev 2.1   Nov 25 2009 13:42:44   t13ae30
      *AAS00841: Kapitäns-Club 2010
      *
      *   Rev 2.0   Jul 06 2009 14:54:56   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0946                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    06.07.2009                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00841!06.07.09!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0946 - ERM. ANZ. STERNE GEM. D-CHANCEN-PROFIL *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0946.
           05  PI-DRVB0946-DATEN.
               10  PI-DRVB0946-MA-ST-NR         PIC S9(09)       BINARY.
      *
       01  PO-DRVB0946.
         03  PO-DRVB0946-RC                     PIC  X(03).
               88  PO-DRVB0946-RC-OK                 VALUE 'OK '.
               88  PO-DRVB0946-RC-NOT-FOUND          VALUE 'NFD'.
         03  PO-DRVB0946-DATEN.
           05  PO-DRVB0946-NI-VIVB0300.
               10  NI-DRVB0946-MA-ST-NR         PIC S9(04)       BINARY.
               10  NI-DRVB0946-CPROF-STERN      PIC S9(04)       BINARY.
           05  PO-DRVB0946-DCL-VIVB0300.
               10  PO-DRVB0946-MA-ST-NR         PIC S9(09)       BINARY.
               10  PO-DRVB0946-CPROF-STERN      PIC S9(04)       BINARY.
      ****
      ******************** ENDE COPY DRVB0946 ************************
