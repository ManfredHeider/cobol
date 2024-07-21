      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0737.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0737.CPv  $
      *
      *   Rev 2.1   Sep 20 2004 10:03:54   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.0   Aug 31 2004 15:29:14   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0737                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LIST-OZP ZXVB0055                    *
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
      *    FUNKTION DRVB0737 - ERMITTELN MAST-SÄTZE FÜR MSBZ-SATZ    *
      *                                                              *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0737.
           05  PI-DRVB0737-UFKT-CODE           PIC  9(002).
           05  PI-DRVB0737-MA-ST-NR            PIC S9(009) COMP.
           05  PI-DRVB0737-MSBZ-DAT-VON        PIC  X(026).
           05  PI-DRVB0737-MSBZ-DAT-BIS        PIC  X(010).
      *
       01  PO-DRVB0737.
           05  PO-DRVB0737-RC                  PIC  X(003).
               88 PO-DRVB0737-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0737-RC-EOF                       VALUE 'EOF'.
               88 PO-DRVB0737-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0737-MAST-SATZ.
               10  PO-DRVB0737-MAST-DAT-VON        PIC  X(026).
               10  PO-DRVB0737-MAST-LAST-UPDATE    PIC  X(026).
               10  PO-DRVB0737-MAST-DAT-BIS        PIC  X(010).
               10  PO-DRVB0737-KNR-MAST-TYP        PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB0737 ****************************************
      ****
