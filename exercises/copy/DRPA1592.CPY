      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1592.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1592.CPv  $
      *
      *   Rev 2.3   Feb 03 2016 14:59:10   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.2   Jul 16 2015 15:09:26   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.1   Oct 12 2004 08:13:16   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 16 2004 14:44:12   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1592  FUNKTION INSERT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA01932!21.04.15! 2.1! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      * APA02208!01.02.16! 2.3! EINFÜHRUNG STEUERJAHR     ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1592                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PMPRH TBPA0322             *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1592.
           05 PI-DRPA1592-UFKT         PIC  9(002).
           05 PI-DRPA1592-PDPT-DATEN.
              10 PI-DRPA1592-PGEB-GES  PIC S9(004)       BINARY.
              10 PI-DRPA1592-PDPT-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PI-DRPA1592-PMPT-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PI-DRPA1592-PMPT-VERSION
                                       PIC S9(004)       BINARY.
              10 PI-DRPA1592-PEST-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PI-DRPA1592-PDPT-BUCH-RFOLGE
                                       PIC S9(004)       BINARY.
              10 PI-DRPA1592-PDPT-MA-ABR-DAT
                                       PIC  X(010).
              10 PI-DRPA1592-KNR-BUCHUNGS-STAND
                                       PIC S9(004)       BINARY.
              10 PI-DRPA1592-KNR-BUCHUNGSART
                                       PIC S9(004)       BINARY.
              10 PI-DRPA1592-KNR-BUCHUNGS-GRUND
                                       PIC S9(004)       BINARY.
              10 PI-DRPA1592-KNR-KZ-EH-MAX
                                       PIC S9(004)       BINARY.
              10 PI-DRPA1592-KNR-BERCH-WAEHRUNG
                                       PIC S9(004)       BINARY.
              10 PI-DRPA1592-PDPT-DGV  PIC S9(004)       BINARY.
              10 PI-DRPA1592-PDPT-PRODART
                                       PIC S9(004)       BINARY.
              10 PI-DRPA1592-PDPT-FIRST-INSERT
                                       PIC  X(026).
              10 PI-DRPA1592-PDPT-FIRST-KENNUNG
                                       PIC  X(008).
              10 PI-DRPA1592-PDPT-LAST-UPDATE
                                       PIC  X(026).
              10 PI-DRPA1592-PDPT-KENNUNG
                                       PIC  X(008).
              10 PI-DRPA1592-PDPT-PDPE-DATEN.
                 15 PI-DRPA1592-PDPT-PDPE-DATEN-L
                                       PIC S9(004)       BINARY.
                 15 PI-DRPA1592-PDPT-PDPE-DATEN-T
210415*                                PIC  X(720).
210415                                 PIC  X(1220).
010216        10 PI-DRPA1592-PDPT-UST-JAHR
010216                                 PIC S9(004)       BINARY.
      *
        01 PO-DRPA1592.
           05 PO-DRPA1592-RC           PIC  X(003).
              88 PO-DRPA1592-RC-OK          VALUE 'OK '.
              88 PO-DRPA1592-RC-DUPLICATE-VALUE
                                            VALUE 'DVA'.
 

