      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0905.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0905.CPv  $
      *
      *   Rev 2.1   Sep 09 2008 12:05:20   t13ex43
      *ASL00309: PGM - Redesign Versorgungswerk (Dialog 1)
      *
      *   Rev 2.0   Jun 26 2008 14:05:06   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0905                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0113                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    27.06.2008                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00309!27.06.08!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0905 - ERMITTELN MIN. STATUS (VSWP)           *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0905.
           05  PI-DRVB0905-DATEN.
               10 PI-DRVB0905-VSWP-LEISTUNG    PIC  X(001).
      *
       01  PO-DRVB0905.
           05  PO-DRVB0905-RC                  PIC  X(003).
               88 PO-DRVB0905-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0905-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0905-DATEN.
               10 PO-DRVB0905-VSWP-STATUS      PIC  X(001).
      ****
      **** ENDE COPY DRVB0905 ****************************************
      ****
