      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.10.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0868.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0868.CPv  $
      *
      *   Rev 2.2   Sep 30 2008 17:58:06   t13ae30
      *AVB00981: Erweiterung der Host-Stammdaten für österr. VB
      *
      *   Rev 2.1   24 Oct 2007 15:12:18   t13ae30
      *AAS00716: Erweiterung Access-DB um Gew.Reg.Nummern
      *
      *   Rev 2.0   23 Oct 2007 12:14:10   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00726!23.10.07! 01 ! NEU                       !WILLIG    *
      * AVB00981!11.06.08! 02 ! GEWERBESCHEIN-DATUM       !WILLIG    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0868                                 *
      *    ERMITTELN MA-BEFBED-NEU                                   *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0868.
          05 PI-DRVB0868-DATEN.
             10 PI-DRVB0868-MA-ST-NR             PIC S9(009) COMP.

      *
       01 PO-DRVB0868.
          05 PO-DRVB0868-RC                      PIC  X(003).
             88 PO-DRVB0868-RC-OK                VALUE 'OK '.
             88 PO-DRVB0868-RC-NFD               VALUE 'NFD'.
          05 PO-DRVB0868-DATEN.
             10 PO-DRVB0868-MA-HROE-EINTR-VA     PIC X(25).
             10 PO-DRVB0868-MA-HROE-EINTR-VB     PIC X(25).
100608       10 PO-DRVB0868-MA-HROE-EINTR-FG     PIC X(25).

