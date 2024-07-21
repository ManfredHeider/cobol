      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0692.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0692.CPv  $
      *
      *   Rev 2.1   Dec 18 2003 15:13:26   t13ae27
      *NEU: Jahres-WBW-2004
      *
      *   Rev 2.0   Feb 17 2003 11:01:14   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0692                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0009                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    09.02.00                                         *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00443!02.12.03! 01 ! NEUERSTELLUNG             ! P.Duerr  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0692                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0692.
          05 PI-DRVB0692-PROGRAMM           PIC  X(008).
          05 PI-DRVB0692-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0692-BETR-NR            PIC  S9(009).
          05 PI-DRVB0692-STUFE-AB           PIC  S9(004).
          05 PI-DRVB0692-ZEITPUNKT-VON      PIC  X(010).
          05 PI-DRVB0692-ZEITPUNKT-BIS      PIC  X(010).
      *
       01 PO-DRVB0692.

          05 PO-DRVB0692-RC                 PIC  X(003).
             88 PO-DRVB0692-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0692-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0692-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0692-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0692-DATEN.
             10 PO-DRVB0692-BVH-UNTST-NR     PIC  S9(009) COMP.
             10 PO-DRVB0692-MS-NR            PIC  S9(004) COMP.
             10 PO-DRVB0692-BVH-DAT-VON      PIC  X(010).
             10 PO-DRVB0692-BVH-DAT-BIS      PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0692 *****************************
