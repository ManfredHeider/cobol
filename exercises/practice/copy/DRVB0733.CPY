      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0733.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0733.CPv  $
      *
      *   Rev 2.1   Sep 20 2004 10:02:44   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.0   Aug 26 2004 14:13:10   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0733                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LIST-OZP ZXVB0012                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    26.08.2004                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00001!26.08.04!2.1 ! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE *
      *    FUNKTION DRVB0733 - ERMITTELN MSBZ-SÄTZE PER STICHTAG     *
      *                                                              *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0733.
           05  PI-DRVB0733-UFKT-CODE           PIC  9(002).
           05  PI-DRVB0733-MA-ST-NR            PIC S9(009) COMP.
           05  PI-DRVB0733-STICHTAG-ISO        PIC  X(010).
      *
       01  PO-DRVB0733.
           05  PO-DRVB0733-RC                  PIC  X(003).
               88 PO-DRVB0733-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0733-RC-EOF                       VALUE 'EOF'.
               88 PO-DRVB0733-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0733-MSBZ-SATZ.
               10  PO-DRVB0733-MA-ST-NR            PIC S9(009) COMP.
               10  PO-DRVB0733-MS-NR               PIC S9(004) COMP.
               10  PO-DRVB0733-BS-NR               PIC S9(004) COMP.
               10  PO-DRVB0733-MSBZ-DAT-VON        PIC  X(026).
               10  PO-DRVB0733-MSBZ-LAST-UPDATE    PIC  X(026).
               10  PO-DRVB0733-MSBZ-DAT-BIS        PIC  X(010).
      ****
      **** ENDE COPY DRVB0733 ****************************************
      ****
