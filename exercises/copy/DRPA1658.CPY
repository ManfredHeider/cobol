      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1658.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1658.CPv  $
      *
      *   Rev 2.1   18 Jul 2006 09:36:44   t13ae46
      *APA00458: SF44 Textanzeige für PGV
      *
      *   Rev 2.0   May 24 2006 10:30:32   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1658                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1658 IM ZWPA0026        *
      *                  ERMITTELT AKTUELLE BEZEICHNUNG ZUR           *
      *                  GESELLSCHAFT UND DEM PG-PGV                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WANG                                             *
      *    DATUM:    24.05.2006                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00458!24.05.06!2.01! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1658: PG-PGV-BEZ.  ERMITTELN
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1658.
           05  PI-DRPA1658-DATEN.
               10 PI-DRPA1658-PGEB-GES         PIC S9(004) COMP.
               10 PI-DRPA1658-PG-PGV           PIC  X(008).
      *
       01  PO-DRPA1658.
           05  PO-DRPA1658-RC                  PIC  X(003).
               88 PO-DRPA1658-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1658-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRPA1658-DATEN.
               10 PO-DRPA1658-PG-PGV-TEXT      PIC  X(042).
      *
