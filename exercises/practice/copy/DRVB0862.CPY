      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0862.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0862.CPv  $
      *
      *   Rev 2.3   24 Oct 2007 16:13:04   t13ae30
      *AAS00716: Erweiterung Access-DB um Gew.Reg.Nummern
      *
      *   Rev 2.2   23 Oct 2007 10:32:56   t13ex22
      *AVD00047: VBI Release 4.14 - Cobol Programme
      *
      *   Rev 2.1   16 Aug 2007 09:50:00   t13ex22
      *AVD00044: VBI Release 4.10 Beförderungsvorausschau
      *
      *   Rev 2.0   15 Aug 2007 14:22:20   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00044!15.08.07! 01 ! NEU                       !MUND      *
      * AAS00716!24.10.07! 02 ! KORREKTUR                 !MUND/WILLIG
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0862                                 *
      *    ERMITTELN MA-BEFBED-NEU                                   *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0862.
          05 PI-DRVB0862-DATEN.
             10 PI-DRVB0862-MA-ST-NR         PIC S9(009) COMP.

      *
       01 PO-DRVB0862.
          05 PO-DRVB0862-RC                  PIC  X(003).
             88 PO-DRVB0862-RC-OK            VALUE 'OK '.
             88 PO-DRVB0862-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0862-DATEN.
      *      DATUM IM ISO FORMAT
             10 PO-DRVB0862-MA-BEFBED-07-DAT   PIC X(10).

