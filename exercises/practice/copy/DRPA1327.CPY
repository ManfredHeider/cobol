      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1327.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1327.CPv  $
      *
      *   Rev 2.3   Jun 19 2012 09:29:38   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:54:22   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Jan 18 2000 10:03:42   T13AE18
      *APA00079
      *
      *   Rev 2.0   Dec 14 1999 16:42:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1327  COVERKONTEN ERMITTELN              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    06.01.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00071!06.01.00! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      * APA01101!29.02.12!2.02! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1327                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM WORKFLOW                                      *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1327.
          05 PI-DRPA1327-PGEB-GES             PIC S9(004) BINARY.

       01 PO-DRPA1327.
          05 PO-DRPA1327-RC                   PIC  X(003).
             88 PO-DRPA1327-RC-OK                  VALUE 'OK '.
          05 PO-DRPA1327-DATEN.
             10 PO-DRPA1327-MANDANT-VWF       PIC S9(009) BINARY.
             10 PO-DRPA1327-MANDANT-VTF       PIC S9(009) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************
