      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.09.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0597.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0597.CPv  $
      *
      *   Rev 2.1   Mar 28 2017 13:38:50   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - AT Teile
      *
      *   Rev 2.0   Sep 03 2015 14:48:54   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0597 LIST ZXAT0008                       *
      *                          MIT KTY-NR, KNR-BEZ                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    28.11.0599                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02353!22.02.17!2.1 !NEU                        !BUCH      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0597                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ERMITTELN EINER NOTIZ ZU EINER ART VON OBJEKT   *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0597.
          05 PI-DRAT0597-AUSWAHL-KRITERIEN.
             10 PI-DRAT0597-KTY-NR             PIC S9(4) USAGE COMP.
             10 PI-DRAT0597-KNR-BEZ            PIC  X(80).
 
      *
       01 PO-DRAT0597.
          05 PO-DRAT0597-RC                    PIC  X(003).
             88 PO-DRAT0597-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0597-RC-NFD                       VALUE 'NFD'.
          05 PO-DRAT0597-PGES-DATEN.
             10 PO-DRAT0597-KNR-NR             PIC S9(4) USAGE COMP.
      *
      ******** ENDE COPY-MEMBER **************************************

