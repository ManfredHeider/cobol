      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0640.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0640.CPv  $
      *
      *   Rev 2.1   Feb 20 2003 15:33:16   t13ex22
      *AVB00648: Unterscheidung VBA/ VBA-Karriere
      *
      *   Rev 2.0   Feb 17 2003 10:56:08   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00648!17.02.03! 01 ! NEU                       !MUND      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0640                                 *
      *    ERMITTELN MA-BEFBED-NEU                                   *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0640.
          05 PI-DRVB0640-DATEN.
             10 PI-DRVB0640-MA-ST-NR         PIC S9(009) COMP.

      *
       01 PO-DRVB0640.
          05 PO-DRVB0640-RC                  PIC  X(003).
             88 PO-DRVB0640-RC-OK            VALUE 'OK '.
             88 PO-DRVB0640-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0640-DATEN.
      *      DATUM IM ISO FORMAT
             10 PO-DRVB0640-MA-BEFBED-NEU   PIC X(10).
      ******** ENDE COPY-MEMBER **************************************
