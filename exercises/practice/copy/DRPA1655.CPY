      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.10.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1655.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1655.CPv  $
      *
      *   Rev 2.1   Oct 19 2005 12:02:56   t13ex44
      *APA00388: Erweiterung SA815 (Satzlänge/3-stellige Prod-Schlü
      *ssel)
      *
      *   Rev 2.0   Oct 18 2005 13:11:46   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1655  LESEN PDPT-FIRST-INSERT            *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    04.10.2005                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00388!04.10.05! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1655                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN PDPT-FIRST-INSERT  <<<                         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1655.
          05 PI-DRPA1655-DATEN.
             10 PI-DRPA1655-PGEB-GES        PIC S9(004)  BINARY.
             10 PI-DRPA1655-PDPT-ID         PIC S9(015)  PACKED-DECIMAL.
      *
       01 PO-DRPA1655.
          05 PO-DRPA1655-RC                 PIC  X(003).
             88 PO-DRPA1655-RC-OK           VALUE 'OK '.
             88 PO-DRPA1655-RC-NOT-FOUND    VALUE 'NFD'.
          05 PO-DRPA1655-PDPT-FIRST-INSERT  PIC  X(026).
      *
      ***** ENDE-COPY-MEMBER *****************************************
