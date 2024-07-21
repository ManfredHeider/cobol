      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.08.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0915.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0915.CPv  $
      *
      *   Rev 2.1   Sep 10 2008 11:51:40   t13ex43
      *ASL00309: PGM - Redesign Versorgungswerk (Batch)
      *
      *   Rev 2.0   Aug 12 2008 08:15:10   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0915                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0115                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    12.08.2008                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00309!12.08.08!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0915 - ERMITTELN VSWVERS-SATZ                 *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0915.
           05  PI-DRVB0915-DATEN.
               10  PI-DRVB0915-MA-ST-NR         PIC S9(09)       BINARY.
               10  PI-DRVB0915-VERS-NR          PIC S9(15)       COMP-3.
      *
       01  PO-DRVB0915.
         03  PO-DRVB0915-RC                     PIC  X(03).
               88  PO-DRVB0915-RC-OK                 VALUE 'OK '.
               88  PO-DRVB0915-RC-NOT-FOUND          VALUE 'NFD'.
         03  PO-DRVB0915-DATEN.
           05  PO-DRVB0915-NI-VIVB0115.
               10  NI-DRVB0915-MA-ST-NR         PIC S9(04)       BINARY.
               10  NI-DRVB0915-VERS-NR          PIC S9(04)       BINARY.
               10  NI-DRVB0915-STATUS-KZ        PIC S9(04)       BINARY.
               10  NI-DRVB0915-VSWV-LAST-UPDATE PIC S9(04)       BINARY.
           05  PO-DRVB0915-DCL-VIVB0115.
               10  PO-DRVB0915-MA-ST-NR         PIC S9(09)       BINARY.
               10  PO-DRVB0915-VERS-NR          PIC S9(15)       COMP-3.
               10  PO-DRVB0915-STATUS-KZ        PIC  X(02).
               10  PO-DRVB0915-VSWV-LAST-UPDATE PIC  X(26).
      ****
      **** ENDE COPY DRVB0915 ****************************************
      ****
