      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0405.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0405.CPv  $
      *
      *   Rev 2.1   Dec 02 2004 11:38:50   t13ex22
      *FVB00590: Meldung bei mehreren Orten zu einer PLZ
      *
      *   Rev 2.0   Nov 29 2004 15:09:06   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0405                                     *
      *    FUNKTION   : POSTLEITZAHLEN - ERMITTELN ORTSNAMEN ZU      *
      *                 GEGEBENER POSTLEITZAHL                       *
      *                 IM GEGENSATZ ZU DRAT0304 ERWEITERT UM LÄNDER-
      *                 KENNUNG UND PO-DIFFERENZIERUNG ZWISCHEN NICHT
      *                 GEFUNDENER PLZ UND NICHT GEFUNDENER KOMBINATION
      *                 AUS PLZ UND ORT
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    29.11.2004                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FVB00590!29.11.04!2.1 !NEU                        !MUND      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0304                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0405.
          05 PI-DRAT0405-DATEN.
      *         MUSS-FELD
             10 PI-DRAT0405-PLZ                  PIC  X(005).
      *         OPTIONAL
             10 PI-DRAT0405-ORT-ANFANG           PIC  X(035).
      *
       01 PO-DRAT0405.
          05 PO-DRAT0405-RC                      PIC  X(003).
      *         GENAU EINEN ORT GEFUNDEN
             88 PO-DRAT0405-RC-OK                     VALUE 'OK '.
      *         PLZ NICHT GEFUNDEN
             88 PO-DRAT0405-RC-NOT-FOUND              VALUE 'NFD'.
      *         VORGEGEBENEN ORT NICHT GEFUNDEN
             88 PO-DRAT0405-RC-ORT-NOT-FOUND          VALUE 'ONF'.
      *         MEHRERE ORTE GEFUNDEN
             88 PO-DRAT0405-RC-DUPLICATE-VALUE        VALUE 'DVA'.
          05 PO-DRAT0405-DATEN.
             10 PO-DRAT0405-ORT                  PIC  X(035).
      *
      ******** ENDE COPY-MEMBER **************************************

