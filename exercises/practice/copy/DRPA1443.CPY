      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1443.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA1443.CPv  $
      *
      *   Rev 2.7   May 14 2018 11:17:34   t13ae53
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - Copy
      * TTH
      *
      *   Rev 2.6   Mar 28 2017 08:20:26   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs
      *
      *   Rev 2.5   Jul 16 2015 15:02:12   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.4   Mar 10 2014 16:48:34   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.3   Jul 26 2013 11:43:12   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.2   Mar 19 2013 16:10:48   i13az40
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.1   05 Feb 2001 10:19:54   T13AE29
      *APA0118: Einführung der Gesellschaften 46 (BfG Invest) und 47 (Bf
      *
      *   Rev 2.0   12 Jul 2000 12:33:14   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1443  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Brenner                                         *
      *    DATUM:    05.01.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG               ! NAME       *
      * -------------------------------------------------------------*
      * APA00118!05.01.01! 01 ! NEUERSTELLUNG           ! BRENNER    *
      * APA01539!19.03.13!2.02! KALYPSO ERGAENZUNGEN    ! HARALANOVA *
      * APA01932!27.03.15!2.03! NEUE FELDER HINZU       ! STORCH     *
      * APA02353!07.03.17!2.04! NEUES FELD TELESON      ! KÖPRÜCÜ    *
      * APA02579!26.04.18!2.05! Frischer Wind           ! Thurmann   *
      *         !        !    !                         !            *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1443                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>> SELECT ALL<<       TABELLE      TBPA0132 PMPEH        *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1443.
          05 PI-DRPA1443-PMPG-KEY.
             10 PI-DRPA1443-PMPG-ID           PIC S9(015) COMP-3.
             10 PI-DRPA1443-PMPG-VERSION      PIC S9(004) COMP.
      *
       01 PO-DRPA1443.
          05 PO-DRPA1443-RC                   PIC  X(003).
             88 PO-DRPA1443-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1443-RC-NOT-FOUND                  VALUE 'NFD'.
      *
          05 PO-DRPA1443-PMPG-VERSION         PIC S9(004) COMP.
          05 PO-DRPA1443-PMPG-LFD-NR-VNR      PIC S9(009) COMP.
          05 PO-DRPA1443-PDTA-ID              PIC S9(015) COMP-3.
          05 PO-DRPA1443-PGEB-GES             PIC S9(004) COMP.
          05 PO-DRPA1443-PMPE-VNR             PIC  X(015).
          05 PO-DRPA1443-PMPE-VERTR-BEGINN    PIC  X(010).
          05 PO-DRPA1443-PMPE-VERTR-ABLAUF    PIC  X(010).
          05 PO-DRPA1443-PMPE-FAELLIG-VON     PIC  X(010).
          05 PO-DRPA1443-PMPE-FAELLIG-BIS     PIC  X(010).
          05 PO-DRPA1443-PMPE-PG-VBNR         PIC S9(009) COMP.
          05 PO-DRPA1443-PMPE-KUNDE-NAME      PIC  X(025).
          05 PO-DRPA1443-PMPE-KUNDE-VNAME     PIC  X(020).
          05 PO-DRPA1443-PMPE-GSMT-PROV-DAT   PIC  X(010).
          05 PO-DRPA1443-PMPE-PG-ABR-DAT      PIC  X(010).
          05 PO-DRPA1443-PMPE-PG-EINL-DAT     PIC  X(010).
          05 PO-DRPA1443-PMPE-PIN             PIC S9(009) COMP.
          05 PO-DRPA1443-KNR-ZAHLWEISE        PIC S9(004) COMP.
          05 PO-DRPA1443-KNR-WAEHRUNG         PIC S9(004) COMP.
          05 PO-DRPA1443-PMPE-VSTAT           PIC  X(004).
          05 PO-DRPA1443-PMPE-LAST-UPDATE     PIC  X(026).
          05 PO-DRPA1443-PMPE-KENNUNG         PIC  X(008).
          05 PO-DRPA1443-PMPEH-LAST-UPDATE    PIC  X(026).
          05 PO-DRPA1443-PMPEH-KENNUNG        PIC  X(008).
          05 PO-DRPA1443-PMPE-PG-PM-DATEN.
             10 PO-DRPA1443-PMPE-PG-DATEN-LEN PIC S9(004) COMP.
             10 PO-DRPA1443-PMPE-PG-DATEN-TEXT PIC X(300).
190313    05 PO-DRPA1443-PMPE-BZ-NAME         PIC X(025).
190313    05 PO-DRPA1443-PMPE-BZ-VNAME        PIC X(020).
190313    05 PO-DRPA1443-PMPE-VP-NAME         PIC X(025).
190313    05 PO-DRPA1443-PMPE-VP-VNAME        PIC X(020).
190313    05 PO-DRPA1443-PMPE-KZ-EIGENV       PIC X(1).
190313    05 PO-DRPA1443-KNR-FEIV-KZ          PIC S9(004) COMP.
190313    05 PO-DRPA1443-KNR-VERM-GES         PIC S9(004) COMP.
190313    05 PO-DRPA1443-PMPE-UMW-NR          PIC S9(015)V COMP-3.
190313    05 PO-DRPA1443-PMPE-UNR             PIC S9(004) COMP.
190313    05 PO-DRPA1443-PMPE-SCHICHT-NR      PIC S9(004) COMP.
190313    05 PO-DRPA1443-PMPE-INKST-DAT       PIC X(010).
190313    05 PO-DRPA1443-KNR-INK-ART          PIC S9(004) COMP.
190313    05 PO-DRPA1443-PMPE-SAM-NR          PIC S9(009) COMP.
190313    05 PO-DRPA1443-KNR-RAB-ST           PIC S9(004) COMP.
190313    05 PO-DRPA1443-PMPE-VVG-KZ          PIC X(001).
190313    05 PO-DRPA1443-KNR-DEPOT-KZ         PIC S9(004) COMP.
190313    05 PO-DRPA1443-PMPE-PROV-SCHL       PIC X(005).
190313    05 PO-DRPA1443-PMPE-BEW-DAT         PIC X(010).
190313    05 PO-DRPA1443-PMPE-URSPR-POL-DAT   PIC X(010).
190313    05 PO-DRPA1443-PMPE-POL-DAT         PIC X(010).
190313    05 PO-DRPA1443-PMPE-BESVV           PIC S9(001)V COMP-3.
190313    05 PO-DRPA1443-PMPE-VN-STRASSE      PIC X(035).
190313    05 PO-DRPA1443-PMPE-VN-PLZ          PIC X(005).
190313    05 PO-DRPA1443-PMPE-VN-ORT          PIC X(035).
190313    05 PO-DRPA1443-PMPE-RATENBEITRAG    PIC S9(009)V9(2) COMP-3.
270315    05 PO-DRPA1443-PMPE-PG-RECH-NR      PIC X(012).
270315    05 PO-DRPA1443-PMPE-HIST-UNR        PIC S9(04).
070317    05 PO-DRPA1443-KNR-TLSN-PARTNER     PIC S9(04) COMP.
260418    05 PO-DRPA1443-PMPE-GEL-KENNUNG     PIC X(006).
      *
      ****** Ende Copy-Member ****************************************
 


