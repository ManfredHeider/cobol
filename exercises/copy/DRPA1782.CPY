      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1782.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA1782.CPv  $
      *
      *   Rev 2.7   Mar 24 2020 06:53:04   t13ex67
      *APA02884: Einführung "Bezugsjahr" bei allen USt.pfl. Produkt
      *en in DAISY - OZP's
      *
      *   Rev 2.6   Mar 30 2017 10:44:38   t13ae37
      *APA02353: Neuer Produktpartner TeleSon
      *
      *   Rev 2.5   Oct 02 2015 09:15:18   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.4   May 04 2015 11:31:32   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.3   Oct 08 2014 15:19:06   t13ex67
      *APA01702: Erweiter. WEPA0008 um Historien-Tab. (TBPA0313/315
      */317/322)
      *
      *   Rev 2.2   Feb 28 2014 14:42:02   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.1   Feb 28 2014 14:37:16   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.0   Dec 13 2013 11:28:32   t13ae39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1782  FUNKTION ALLE MELD ZUR PRINTE      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STRAUSS                                         *
      *    DATUM:    13.12.2013                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01702!13.12.13! 2.0! NEUERSTELLUNG             ! STRAUSS  *
      * APA01539!28.02.14! 2.1! ERWEITERUNGEN FÜR KALYPSO ! RAHNER   *
      * APA01932!31.03.15! 2.2! ERWEITERUNGEN FÜR LUCY    ! KÖPRÜCÜ  *
      *         !02.10.15! 2.3! NEUES FELD VP-NR (CKV)    ! KÖPRÜCÜ  *
      * APA02353!30.03.17! 2.6! TELESON                   ! BUCH     *
      * APA02884!19.11.19! 2.7! NEUES FELD BEZUGSJAHR     ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1782.
          05 PI-DRPA1782-PROGRAMM         PIC  X(008).
          05 PI-DRPA1782-UFKT             PIC  9(002).
          05 PI-DRPA1782-DATEN.
             10 PI-DRPA1782-PGEB-GES      PIC S9(004)  BINARY.
             10 PI-DRPA1782-PRINTE        PIC S9(015)  PACKED-DECIMAL.
      *
       01 PO-DRPA1782.
          05 PO-DRPA1782-RC               PIC  X(003).
             88 PO-DRPA1782-RC-OK                      VALUE 'OK '.
             88 PO-DRPA1782-RC-LEER                    VALUE 'LER'.
             88 PO-DRPA1782-RC-EOF                     VALUE 'EOF'.
          05 PO-DRPA1782-DATEN.
301213*      10 PO-DRPA1782-MELD.
301213*         COPY DRPA0411.
301213         30 PMBEH-DATEN.
301213          35  PGEB-GES                 PIC S9(004) BINARY.
301213          35  PMBE-ID                  PIC S9(015) PACKED-DECIMAL.
301213          35  PMBE-VERSION             PIC S9(004) BINARY.
301213          35  PMBE-V-GUELTIG-AB        PIC  X(026).
301213          35  PMBE-V-GUELTIG-BIS       PIC  X(010).
301213          35  PMBE-V-KENNUNG           PIC  X(008).
301213          35  KNR-PMBE-ART             PIC S9(004) BINARY.
301213          35  PDTA-ID                  PIC S9(015) PACKED-DECIMAL.
301213          35  PRINTE                   PIC S9(015) PACKED-DECIMAL.
301213          35  PVER-VERSION             PIC S9(004) BINARY.
301213          35  PMBE-RFOLGE-NR           PIC S9(004) BINARY.
301213          35  PMBE-PGFAELLIG-VON        PIC  X(010).
301213          35  PMBE-PGFAELLIG-BIS        PIC  X(010).
301213          35  PMBE-PG-ABR-DAT           PIC  X(010).
301213          35  PMBE-PG-EINL-DAT          PIC  X(010).
301213          35  PMBE-GSMT-PROV-DAT        PIC  X(010).
301213          35  PMBE-KD-BETR              PIC S9(009) BINARY.
301213          35  KNR-ZAHLWEISE             PIC S9(004) BINARY.
301213          35  PMBE-VSTAT                PIC  X(004).
301213          35  PMBE-VSTAT-UNR            PIC S9(004) BINARY.
131213          35  PMBE-LAST-UPDATE          PIC X(026).
131213          35  PMBE-KENNUNG              PIC X(008).
301213          35  PMBE-PG-PM-DATEN.
301213              40 PMBE-PG-PM-DATEN-LEN   PIC S9(004) BINARY.
301213              40 PMBE-PG-PM-DATEN-TEXT  PIC  X(300).
301213          35  KNR-MANDANT               PIC S9(004) BINARY.
280214          35  PMBE-UNR             PIC S9(4) BINARY.
280214          35  PMBE-SCHICHT-NR      PIC S9(4) BINARY.
280214          35  PMBE-INKST-DAT       PIC X(10).
280214          35  KNR-INK-ART          PIC S9(4) BINARY.
280214          35  PMBE-SAM-NR          PIC S9(9) BINARY.
280214          35  KNR-RAB-ST           PIC S9(4) BINARY.
280214          35  PMBE-VVG-KZ          PIC X(1).
280214          35  KNR-DEPOT-KZ         PIC S9(4) BINARY.
280214          35  PMBE-PROV-SCHL       PIC X(5).
280214          35  PMBE-BEW-DAT         PIC X(10).
280214          35  PMBE-POL-DAT         PIC X(10).
280214          35  PMBE-BESVV           PIC S9(1)V USAGE COMP-3.
280214          35  PMBE-PROV-BEGINN     PIC X(10).
280214          35  PMBE-PROV-ABLAUF     PIC X(10).
280214          35  PMBE-KZ-EIGENV       PIC X(1).
280214          35  PMBE-RATENBEITRAG    PIC S9(9)V9(2) USAGE COMP-3.
300317*         35  PMBE-PG-RECH-NR      PIC X(12).
300317          35  PMBE-PG-RECH-NR      PIC X(30).
310315          35  PMBE-HIST-UNR        PIC S9(4) BINARY.
021015          35  PMBE-VP-NR           PIC S9(4) BINARY.
191119          35  PMBE-BEZUGSJAHR      PIC S9(4) BINARY.
      *

