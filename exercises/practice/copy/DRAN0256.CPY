      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0256.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0256.CPv  $
      *
      *   Rev 2.1   Mar 10 2010 09:28:02   t13ae34
      *AAN00944: OPC-Jobs für Anträge in den Testumgebungen
      *
      *   Rev 2.0   Feb 16 2010 13:36:26   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0256  FUNKTION SELECT MAX-PIN            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00944!16.02.10!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0256                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  MAX-PIN  <<<       TABELLE ANTA TBAN0008     *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0256.
             10 PI-DRAN0256-PGEB-GES          PIC S9(4) COMP.
             10 PI-DRAN0256-PIN-VON           PIC S9(9) COMP.
             10 PI-DRAN0256-PIN-BIS           PIC S9(9) COMP.
      *
       01 PO-DRAN0256.
          05 PO-DRAN0256-RC                   PIC X(003).
             88 PO-DRAN0256-RC-OK                 VALUE 'OK '.
             88 PO-DRAN0256-RC-NOT-OK             VALUE 'NOK'.
      *
          05 PO-DRAN0256-ANTS-DATEN.
             10 PO-DRAN0256-MAX-PIN           PIC S9(9) COMP.
      ****** ENDE COPY-MEMBER ****************************************
