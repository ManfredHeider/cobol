      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1582.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRPA1582.CPv  $
      *
      *   Rev 2.11   Mar 24 2020 06:50:36   t13ex67
      *APA02884: Einführung "Bezugsjahr" bei allen USt.pfl. Produkt
      *en in DAISY - OZP's
      *
      *   Rev 2.10   May 14 2018 11:18:06   t13ae53
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - Copy
      * TTH
      *
      *   Rev 2.9   Mar 28 2017 08:14:18   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - PVW
      *
      *   Rev 2.8   Oct 02 2015 09:15:04   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.7   May 04 2015 11:31:38   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.6   Mar 10 2014 16:49:16   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.5   Feb 28 2014 15:52:48   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.4   Jun 19 2012 09:31:20   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:56:16   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 13:26:40   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   Oct 12 2004 08:11:44   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 16 2004 14:41:56   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1582  FUNKTION SELECT-ALL STANDARD-OZP   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA00622!04.06.07! 2.2! NEUES FELD KNR-VGES       ! STORCH   *
      * APA01101!23.02.12! 2.3! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01539!28.02.14! 2.5! ERWEITERUNG  FÜR KALYPSO  ! RAHNER   *
      * APA01932!31.03.15! 2.6! ERWEITERUNG  FÜR LUCY     ! KÖPRÜCÜ  *
      *         !02.10.15! 2.7! NEUES FELD VP-NR (CKV)    ! KÖPRÜCÜ  *
      * APA02353!23.02.17! 2.8! TELESON RECH-NR           ! BUCH     *
      * APA02884!19.11.19! 2.9! NEUES ATTRIBUT BEZUGSJAHR ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1582                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>SELECT ALL<<<       TABELLE PMBEH TBPA0313             *
      *                                                              *
      ****************************************************************
      *
        01 PI-DRPA1582.
           05 PI-DRPA1582-PMBE-KEY.
              10 PI-DRPA1582-PGEB-GES  PIC S9(004)       BINARY.
              10 PI-DRPA1582-PMBE-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PI-DRPA1582-PMBE-VERSION
                                       PIC S9(004)       BINARY.
      *
        01 PO-DRPA1582.
           05 PO-DRPA1582-RC           PIC  X(003).
              88 PO-DRPA1582-RC-OK          VALUE 'OK '.
              88 PO-DRPA1582-RC-NOT-FOUND   VALUE 'NFD'.
           05 PO-DRPA1582-PMBE-DATEN.
              10 PO-DRPA1582-PGEB-GES              PIC S9(004) BINARY.
              10 PO-DRPA1582-PMBE-ID        PIC S9(015) PACKED-DECIMAL.
              10 PO-DRPA1582-PMBE-VERSION          PIC S9(004) BINARY.
              10 PO-DRPA1582-PMBE-V-GUELTIG-AB     PIC  X(026).
              10 PO-DRPA1582-PMBE-V-GUELTIG-BIS    PIC  X(010).
              10 PO-DRPA1582-PMBE-V-KENNUNG        PIC  X(008).
              10 PO-DRPA1582-KNR-PMBE-ART          PIC S9(004) BINARY.
              10 PO-DRPA1582-PDTA-ID        PIC S9(015) PACKED-DECIMAL.
              10 PO-DRPA1582-PRINTE         PIC S9(015) PACKED-DECIMAL.
              10 PO-DRPA1582-PVER-VERSION          PIC S9(004) BINARY.
              10 PO-DRPA1582-PMBE-RFOLGE-NR        PIC S9(004) BINARY.
              10 PO-DRPA1582-PMBE-PGFAELLIG-VON    PIC  X(010).
              10 PO-DRPA1582-PMBE-PGFAELLIG-BIS    PIC  X(010).
              10 PO-DRPA1582-PMBE-PG-ABR-DAT       PIC  X(010).
              10 PO-DRPA1582-PMBE-PG-EINL-DAT      PIC  X(010).
              10 PO-DRPA1582-PMBE-GSMT-PROV-DAT    PIC  X(010).
              10 PO-DRPA1582-PMBE-KD-BETR          PIC S9(009) BINARY.
              10 PO-DRPA1582-KNR-ZAHLWEISE         PIC S9(004) BINARY.
              10 PO-DRPA1582-PMBE-VSTAT            PIC  X(004).
              10 PO-DRPA1582-PMBE-VSTAT-UNR        PIC S9(004) BINARY.
              10 PO-DRPA1582-PMBE-PG-DATEN.
                 15 PO-DRPA1582-PMBE-PG-DATEN-LEN  PIC S9(004) BINARY.
                 15 PO-DRPA1582-PMBE-PG-DATEN-TEXT PIC  X(300).
              10 PO-DRPA1582-PMBE-LAST-UPDATE      PIC  X(026).
              10 PO-DRPA1582-PMBE-KENNUNG          PIC  X(008).
              10 PO-DRPA1582-KNR-MANDANT           PIC S9(004) BINARY.
280214        10 PO-DRPA1582-PMBE-UNR              PIC S9(4) BINARY.
280214        10 PO-DRPA1582-PMBE-SCHICHT-NR       PIC S9(4) BINARY.
280214        10 PO-DRPA1582-PMBE-INKST-DAT        PIC X(10).
280214        10 PO-DRPA1582-KNR-INK-ART           PIC S9(4) BINARY.
280214        10 PO-DRPA1582-PMBE-SAM-NR           PIC S9(9) BINARY.
280214        10 PO-DRPA1582-KNR-RAB-ST            PIC S9(4) BINARY.
280214        10 PO-DRPA1582-PMBE-VVG-KZ           PIC X(1).
280214        10 PO-DRPA1582-KNR-DEPOT-KZ          PIC S9(4) BINARY.
280214        10 PO-DRPA1582-PMBE-PROV-SCHL        PIC X(5).
280214        10 PO-DRPA1582-PMBE-BEW-DAT          PIC X(10).
280214        10 PO-DRPA1582-PMBE-POL-DAT          PIC X(10).
280214        10 PO-DRPA1582-PMBE-BESVV      PIC S9(1)V PACKED-DECIMAL.
280214        10 PO-DRPA1582-PMBE-PROV-BEGINN      PIC X(10).
280214        10 PO-DRPA1582-PMBE-PROV-ABLAUF      PIC X(10).
280214        10 PO-DRPA1582-PMBE-KZ-EIGENV        PIC X(1).
280214        10 PO-DRPA1582-PMBE-RATENBEITRAG
280214                                   PIC S9(9)V9(2) PACKED-DECIMAL.
230217*       10 PO-DRPA1582-PMBE-PG-RECH-NR       PIC X(12).
230217        10 PO-DRPA1582-PMBE-PG-RECH-NR       PIC X(30).
310315        10 PO-DRPA1582-PMBE-HIST-UNR         PIC S9(4) BINARY.
021015        10 PO-DRPA1582-PMBE-VP-NR            PIC S9(4) BINARY.
260418        10 PO-DRPA1582-PMBE-GEL-KENNUNG      PIC X(6).
191119        10 PO-DRPA1582-PMBE-BEZUGSJAHR       PIC S9(4) BINARY.

