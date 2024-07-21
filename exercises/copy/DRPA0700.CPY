      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0700.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRPA0700.CPv  $
      *
      *   Rev 2.10   Mar 24 2020 06:47:04   t13ex67
      *APA02884: Einführung "Bezugsjahr" bei allen USt.pfl. Produkt
      *en in DAISY - OZP's
      *
      *   Rev 2.9   May 14 2018 11:18:42   t13ae54
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - Copy
      * GHO
      *
      *   Rev 2.8   Mar 30 2017 10:44:18   t13ae37
      *APA02353: Neuer Produktpartner TeleSon
      *
      *   Rev 2.7   Oct 02 2015 09:14:42   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.6   May 04 2015 11:07:52   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.5   Mar 10 2014 16:46:12   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.4   Jul 26 2013 11:41:22   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.3   Jun 19 2012 09:25:26   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:50:22   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 10:33:30   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   May 26 1999 15:10:58   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   11 Dec 1997 11:45:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0700  FUNKTION SELECT-ALL STANDARD-OZP   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    11.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.12.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA00622!01.06.07!2.01! NEUES FELD KNR-VGES       ! STORCH   *
      * APA01101!23.02.12!2.02! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01539!26.07.13!2.4 ! ERWERTERUNG FÜR KALYPSO   ! RAHNER   *
      * APA01932!31.03.15!2.5 ! ERWEITERUNG FÜR LUCY      ! KÖPRÜCÜ  *
      *         !02.10.15!2.5 ! NEUES FELD VP-NR (CKV)    ! KÖPRÜCÜ  *
      * APA02353!30.03.17!2.8 ! TELESON                   ! BUCH     *
      * APA02579!26.04.18!2.9 ! FriWi NEUE DB2-ATTRIBUTE  ! HOFMANN  *
      * APA02884!19.11.19!2.10! NEUES ATTRIBUT BEZUGSJAHR ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0700                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>SELECT ALL<<<       TABELLE PMBE TBPA0113              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0700.
          05 PI-DRPA0700-PMBE-KEY.
             10 PI-DRPA0700-PMBE-ID          PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0700-PMBE-VERSION     PIC S9(4) USAGE COMP.
      *
       01 PO-DRPA0700.
          05 PO-DRPA0700-RC                  PIC  X(003).
             88 PO-DRPA0700-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0700-RC-CHANGED            VALUE 'CHA'.
             88 PO-DRPA0700-RC-NOT-FOUND          VALUE 'NFD'.
          05 PO-DRPA0700-PMBE-DATEN.
             10 PO-DRPA0700-PMBE-LAST-UPDATE    PIC X(26).
             10 PO-DRPA0700-PMBE-VERSION        PIC S9(4) USAGE COMP.
             10 PO-DRPA0700-PMBE-KENNUNG        PIC X(08).
             10 PO-DRPA0700-PMBE-V-GUELTIG-AB   PIC X(26).
             10 PO-DRPA0700-PMBE-V-GUELTIG-BIS  PIC X(10).
             10 PO-DRPA0700-PMBE-V-KENNUNG      PIC X(08).
             10 PO-DRPA0700-KNR-PMBE-ART        PIC S9(4) USAGE COMP.
             10 PO-DRPA0700-PDTA-ID          PIC S9(15)V USAGE COMP-3.
             10 PO-DRPA0700-PRINTE           PIC S9(15)V USAGE COMP-3.
             10 PO-DRPA0700-PVER-VERSION        PIC S9(4) USAGE COMP.
             10 PO-DRPA0700-PMBE-RFOLGE-NR      PIC S9(4) USAGE COMP.
             10 PO-DRPA0700-PMBE-PGFAELLIG-VON  PIC X(10).
             10 PO-DRPA0700-PMBE-PGFAELLIG-BIS  PIC X(10).
             10 PO-DRPA0700-PMBE-PG-ABR-DAT     PIC X(10).
             10 PO-DRPA0700-PMBE-PG-EINL-DAT    PIC X(10).
             10 PO-DRPA0700-PMBE-GSMT-PROV-DAT  PIC X(10).
             10 PO-DRPA0700-PMBE-KD-BETR        PIC S9(9) COMP.
             10 PO-DRPA0700-KNR-ZAHLWEISE       PIC S9(4) COMP.
             10 PO-DRPA0700-PMBE-VSTAT          PIC X(04).
             10 PO-DRPA0700-PMBE-VSTAT-UNR      PIC S9(4) COMP.
             10 PO-DRPA0700-PMBE-PG-DATEN.
                15 PO-DRPA0700-PMBE-PG-DATEN-LEN  PIC S9(4) USAGE COMP.
                15 PO-DRPA0700-PMBE-PG-DATEN-TEXT PIC X(300).
             10 PO-DRPA0700-KNR-MANDANT         PIC S9(4) COMP.
260713       10 PO-DRPA0700-PMBE-UNR            PIC S9(4) USAGE COMP.
260713       10 PO-DRPA0700-PMBE-SCHICHT-NR     PIC S9(4) USAGE COMP.
260713       10 PO-DRPA0700-PMBE-INKST-DAT      PIC X(10).
260713       10 PO-DRPA0700-KNR-INK-ART         PIC S9(4) USAGE COMP.
260713       10 PO-DRPA0700-PMBE-SAM-NR         PIC S9(9) USAGE COMP.
260713       10 PO-DRPA0700-KNR-RAB-ST          PIC S9(4) USAGE COMP.
260713       10 PO-DRPA0700-PMBE-VVG-KZ         PIC X(1).
260713       10 PO-DRPA0700-KNR-DEPOT-KZ        PIC S9(4) USAGE COMP.
260713       10 PO-DRPA0700-PMBE-PROV-SCHL      PIC X(5).
260713       10 PO-DRPA0700-PMBE-BEW-DAT        PIC X(10).
260713       10 PO-DRPA0700-PMBE-POL-DAT        PIC X(10).
260713       10 PO-DRPA0700-PMBE-BESVV          PIC S9(1)V USAGE COMP-3.
260713       10 PO-DRPA0700-PMBE-PROV-BEGINN    PIC X(10).
260713       10 PO-DRPA0700-PMBE-PROV-ABLAUF    PIC X(10).
260713       10 PO-DRPA0700-PMBE-KZ-EIGENV      PIC X(1).
260713       10 PO-DRPA0700-PMBE-RATENBEITRAG
260713                                      PIC S9(9)V9(2) USAGE COMP-3.
300317*      10 PO-DRPA0700-PMBE-PG-RECH-NR     PIC X(12).
300317       10 PO-DRPA0700-PMBE-PG-RECH-NR     PIC X(30).
310315       10 PO-DRPA0700-PMBE-HIST-UNR       PIC S9(4) USAGE COMP.
021015       10 PO-DRPA0700-PMBE-VP-NR          PIC S9(4) USAGE COMP.
240418       10 PO-DRPA0700-PMBE-GEL-KENNUNG    PIC X(6).
191119       10 PO-DRPA0700-PMBE-BEZUGSJAHR     PIC S9(4) USAGE COMP.
      *
      ****** Ende Copy-Member ****************************************
 


