      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.03.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1743.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1743.CPv  $
      *
      *   Rev 2.1   Dec 18 2013 14:34:48   t13ae46
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO-Dialoge (PO11-PO13)
      *
      *   Rev 2.0   Mar 28 2013 13:26:58   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1743  FUNKTION                           *
      *                      GET  UNTERNR UND SCHICHTNR ( UGPA0002)  *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    28.03.2013                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!28.03.13! 01  !NEUERSTELLUNG             ! WANG     *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1743                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1743.
          05 PI-DRPA1743-DATEN.
             10 PI-DRPA1743-PRINTE           PIC S9(015)V USAGE COMP-3.
      *
       01 PO-DRPA1743.
          05 PO-DRPA1743-RC                  PIC  X(003).
             88 PO-DRPA1743-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1743-RC-NOT-FOUND          VALUE 'NFD'.

          05 PO-DRPA1743-MELDUNG.
             10 PO-DRPA1743-MELDUNG-TYP      PIC  X(001).
             10 PO-DRPA1743-MELDUNG-NR       PIC S9(004) BINARY.

          05 PO-DRPA1743-DATEN.
             10 PO-DRPA1743-UNTERNUMMER      PIC S9(04) COMP.
             10 PO-DRPA1743-SCHICHTNUMMER    PIC S9(04) COMP.
