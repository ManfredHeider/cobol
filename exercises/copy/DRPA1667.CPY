      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1667.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1667.CPv  $
      *
      *   Rev 2.3   Jun 19 2012 09:31:42   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:56:38   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 13:50:14   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Aug 07 2007 08:23:04   t13ex21
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1667                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF < KNR-VGES DES DTA'S ERMITTELN >           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    07.08.2007                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00622!07.08.07! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA01101!23.02.12!2.02! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1667                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1667.
          05 PI-DRPA1667-DATEN.
             10 PI-DRPA1667-PGEB-GES       PIC S9(004) BINARY.
             10 PI-DRPA1667-PDTA-ID        PIC S9(015) COMP-3.
      *
       01 PO-DRPA1667.
          05 PO-DRPA1667-RC                PIC  X(003).
             88 PO-DRPA1667-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1667-RC-NOT-FOUND                VALUE 'NFD'.
      *
          05 PO-DRPA1667-DATEN.
             10 PO-DRPA1667-KNR-MANDANT    PIC S9(4) USAGE COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
