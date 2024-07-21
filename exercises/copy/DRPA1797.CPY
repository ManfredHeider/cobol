      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.07.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1797.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1797.CPv  $
      *
      *   Rev 2.2   Feb 03 2016 14:59:48   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.1   Dec 08 2014 09:23:54   t13ex67
      *APA01809: Projekt Integration FVD    //       Migrationsprog
      *ramm
      *
      *   Rev 2.0   Jul 23 2014 13:21:32   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1797  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: M.STORCH                                         *
      *    DATUM:    24.07.2014                                      *
      ****************************************************************
      *    VERSION   2.01                                            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01809!24.07.14!2.01! NEUERSTELLUNG             ! STORCH   *
      * APA02208!01.02.16!2.2 ! STEUERJAHR ERGÄNZT        ! BUCH     *
      * APA02208!01.02.16!2.2 ! PDPE-DATEN VERLÄNGERT     ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1797                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PDPTH TBPA0322             *
      ****************************************************************
       01 PI-DRPA1797.
           05 PI-DRPA1797-PDPT-DATEN.
              10 PI-DRPA1797-PGEB-GES              PIC S9(004) BINARY.
              10 PI-DRPA1797-PDPT-ID               PIC S9(015) COMP-3.
              10 PI-DRPA1797-PMPT-ID               PIC S9(015) COMP-3.
              10 PI-DRPA1797-PMPT-VERSION          PIC S9(004) BINARY.
              10 PI-DRPA1797-PEST-ID               PIC S9(015) COMP-3.
              10 PI-DRPA1797-PDPT-BUCH-RFOLGE      PIC S9(004) BINARY.
              10 PI-DRPA1797-PDPT-MA-ABR-DAT       PIC  X(010).
              10 PI-DRPA1797-KNR-BUCHUNGS-STAND    PIC S9(004) BINARY.
              10 PI-DRPA1797-KNR-BUCHUNGSART       PIC S9(004) BINARY.
              10 PI-DRPA1797-KNR-BUCHUNGS-GRUND    PIC S9(004) BINARY.
              10 PI-DRPA1797-KNR-KZ-EH-MAX         PIC S9(004) BINARY.
              10 PI-DRPA1797-KNR-BERCH-WAEHRUNG    PIC S9(004) BINARY.
              10 PI-DRPA1797-PDPT-DGV              PIC S9(004) BINARY.
              10 PI-DRPA1797-PDPT-PRODART          PIC S9(004) BINARY.
              10 PI-DRPA1797-PDPT-FIRST-INSERT     PIC  X(026).
              10 PI-DRPA1797-PDPT-FIRST-KENNUNG    PIC  X(008).
              10 PI-DRPA1797-PDPT-LAST-UPDATE      PIC  X(026).
              10 PI-DRPA1797-PDPT-KENNUNG          PIC  X(008).
              10 PI-DRPA1797-PDPT-PDPE-DATEN.
                 15 PI-DRPA1797-PDPT-PDPE-DATEN-L  PIC S9(004) BINARY.
010216*          15 PI-DRPA1797-PDPT-PDPE-DATEN-T  PIC  X(720).
010216           15 PI-DRPA1797-PDPT-PDPE-DATEN-T  PIC  X(1220).
010216        10 PI-DRPA1797-PDPT-UST-JAHR         PIC S9(004) BINARY.
      *
       01 PO-DRPA1797.
          05 PO-DRPA1797-RC                   PIC  X(003).
             88 PO-DRPA1797-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1797-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1797-RC-NOT-FOUND                  VALUE 'NFD'.
 

