      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  26.07.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1901.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1901.CPv  $
      *
      *   Rev 2.1   Aug 03 2018 15:10:32   T13AE37
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - prüf
      *e Bew-Schl
      *
      *   Rev 2.0   26 Jul 2018 09:09:10   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1901                                     *
      *    --------------------------------------------------------  *
      *    FUNKTIONSAUFRUF << Ges 21.BEWEGUNGSSCHLÜSSEL PRÜFEN >>    *
      *    --------------------------------------------------------  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02579!26.01.18!2.1 ! NEU                       ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1901                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1901.
          05 PI-DRPA1901-DATEN.
             10 PI-DRPA1901-GEL-GEVO                PIC  X(007).
             10 PI-DRPA1901-HERKUNFT                PIC S9(004) BINARY.
      *
       01 PO-DRPA1901.
          05 PO-DRPA1901-RC                         PIC  X(003).
             88 PO-DRPA1901-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1901-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1901-DATEN.
             10 PO-DRPA1901-GEL-BEW-SCHL            PIC S9(009) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
 

