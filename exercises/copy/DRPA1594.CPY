      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1594.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1594.CPv  $
      *
      *   Rev 2.3   Feb 03 2016 14:59:18   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.2   Nov 24 2015 09:30:56   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.1   Oct 12 2004 08:13:40   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 16 2004 14:44:40   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1594  FUNKTION SELECT-ALL STANDARD-OZP   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA01932!24.11.15! 2.1! ERWEITERUNG LÄNGE PDPE    ! KÖPRÜCÜ  *
      * APA02208!01.02.16! 2.3! EINFÜHRUNG STEUERJAHR     ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1594                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>SELECT ALL<<<       TABELLE PDPTH TBPA0322             *
      *                                                              *
      ****************************************************************
      *
        01 PI-DRPA1594.
           05 PI-DRPA1594-PDPT-KEY.
              10 PI-DRPA1594-PGEB-GES  PIC S9(004)       BINARY.
              10 PI-DRPA1594-PDPT-ID   PIC S9(015)       PACKED-DECIMAL.
      *
        01 PO-DRPA1594.
           05 PO-DRPA1594-RC           PIC  X(003).
              88 PO-DRPA1594-RC-OK          VALUE 'OK '.
              88 PO-DRPA1594-RC-NOT-FOUND   VALUE 'NFD'.
           05 PO-DRPA1594-PDPT-DATEN.
              10 PO-DRPA1594-PGEB-GES  PIC S9(004)       BINARY.
              10 PO-DRPA1594-PDPT-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PO-DRPA1594-PMPT-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PO-DRPA1594-PMPT-VERSION
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1594-PEST-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PO-DRPA1594-PDPT-BUCH-RFOLGE
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1594-PDPT-MA-ABR-DAT
                                       PIC  X(010).
              10 PO-DRPA1594-KNR-BUCHUNGS-STAND
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1594-KNR-BUCHUNGSART
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1594-KNR-BUCHUNGS-GRUND
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1594-KNR-KZ-EH-MAX
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1594-KNR-BERCH-WAEHRUNG
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1594-PDPT-DGV  PIC S9(004)       BINARY.
              10 PO-DRPA1594-PDPT-PRODART
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1594-PDPT-FIRST-INSERT
                                       PIC  X(026).
              10 PO-DRPA1594-PDPT-FIRST-KENNUNG
                                       PIC  X(008).
              10 PO-DRPA1594-PDPT-LAST-UPDATE
                                       PIC  X(026).
              10 PO-DRPA1594-PDPT-KENNUNG
                                       PIC  X(008).
              10 PO-DRPA1594-PDPT-PDPE-DATEN.
                 15 PO-DRPA1594-PDPT-PDPE-DATEN-L
                                       PIC S9(004)       BINARY.
241115*          15 PO-DRPA1594-PDPT-PDPE-DATEN-T
241115*                                PIC  X(720).
241115           15 PO-DRPA1594-PDPT-PDPE-DATEN-T
241115                                 PIC  X(1220).
010216        10 PO-DRPA1594-PDPT-UST-JAHR
                                       PIC S9(004)       BINARY.
 

