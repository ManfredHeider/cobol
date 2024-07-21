      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0735.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0735.CPv  $
      *
      *   Rev 2.1   Sep 20 2004 10:03:18   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.0   Aug 31 2004 14:16:44   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0735                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LIST-OZP ZXVB0016                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    31.08.2004                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00001!31.08.04!2.1 ! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE *
      *    FUNKTION DRVB0735 - ERMITTELN IA-SÄTZE FÜR MSBZ-SATZ      *
      *                                                              *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0735.
           05  PI-DRVB0735-UFKT-CODE           PIC  9(002).
           05  PI-DRVB0735-MA-ST-NR            PIC S9(009) COMP.
           05  PI-DRVB0735-MSBZ-DAT-VON        PIC  X(026).
           05  PI-DRVB0735-MSBZ-DAT-BIS        PIC  X(010).
      *
       01  PO-DRVB0735.
           05  PO-DRVB0735-RC                  PIC  X(003).
               88 PO-DRVB0735-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0735-RC-EOF                       VALUE 'EOF'.
               88 PO-DRVB0735-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0735-IA-SATZ.
               10  PO-DRVB0735-IA-DAT-VON          PIC  X(026).
               10  PO-DRVB0735-IA-LAST-UPDATE      PIC  X(026).
               10  PO-DRVB0735-IA-DAT-BIS          PIC  X(010).
      ****
      **** ENDE COPY DRVB0735 ****************************************
      ****
