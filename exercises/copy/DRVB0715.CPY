      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0715.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0715.CPv  $
      *
      *   Rev 2.3   Sep 30 2008 17:58:02   t13ae30
      *AVB00981: Erweiterung der Host-Stammdaten für österr. VB
      *
      *   Rev 2.2   12 Apr 2007 16:18:24   t13ae30
      *AAS00689: Gewerbeberechtigungen österreichische VB
      *
      *   Rev 2.1   Jul 01 2004 17:33:12   t13ae30
      *AAS00472: Erweiterung F&A Schnittstelle
      *
      *   Rev 2.0   Jun 15 2004 13:25:12   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00715!15.06.04! 01 ! NEU                       ! WILLIG   *
      * AAS00689!12.04.07! 02 ! ERW. FKT.DRVB0715         ! WILLIG   *
      * AAS00981!10.09.08! 08 ! ERW. FKT.DRVB0715         ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0715                                 *
      *    ERMITTELN MA-BEFBED-NEU                                   *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0715.
          05 PI-DRVB0715-DATEN.
             10 PI-DRVB0715-MA-ST-NR         PIC S9(009) COMP.
      *
       01 PO-DRVB0715.
          05 PO-DRVB0715-RC                  PIC  X(003).
             88 PO-DRVB0715-RC-OK            VALUE 'OK '.
             88 PO-DRVB0715-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0715-DATEN.
             10 PO-DRVB0715-MA-FG-DAT        PIC  X(010).
             10 PO-DRVB0715-MA-127-DAT       PIC  X(010).
             10 PO-DRVB0715-MA-VA-DAT        PIC  X(010).
             10 PO-DRVB0715-MA-127-UNBES-DAT PIC  X(010).
100908       10 PO-DRVB0715-MA-FG-VOM        PIC  X(010).
100908       10 PO-DRVB0715-MA-127-VOM       PIC  X(010).
100908       10 PO-DRVB0715-MA-VA-VOM        PIC  X(010).
100908       10 PO-DRVB0715-MA-SCHUFA-VOM    PIC  X(010).
100908       10 PO-DRVB0715-MA-FUEH-ZEUG-VOM PIC  X(010).
100908       10 PO-DRVB0715-MA-GEW-Z-REG-VOM PIC  X(010).

      ******** ENDE COPY-MEMBER **************************************
