      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0871.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA0871.CPv  $
      *
      *   Rev 2.7   Mar 28 2017 09:03:40   t13ex67
      *APA02353: Neuer Produktpartner TeleSon - Dialoge
      *
      *   Rev 2.6   Dec 18 2013 14:34:26   t13ae46
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO-Dialoge (PO11-PO13)
      *
      *   Rev 2.5   Jun 19 2012 09:25:52   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.4   May 21 2012 08:50:46   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   Dec 10 2007 10:57:00   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.2   23 Aug 2007 09:53:06   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.1   03 Nov 2006 09:19:44   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 13:21:38   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:47:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0871  FUNKTION VERTRAG LESEN (UGPA0001)  *
      *    --------------------------------------------------------- *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    26.02.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!26.02.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !12.10.98!    ! KNR-PVER-ART UND          ! STORCH   *
      *         !        !    ! PVER-PG-VBNR HINZUGEFÜGT  !          *
      *         !09.11.98!    ! SCHALTER PROVDATEN-GELESEN! STORCH   *
      *         !        !    ! HINZUGEFüGT               !          *
      * APA00453!02.11.06!2.1 ! ERW. WG. 3-STELL. PRODUKTE! WANG     *
      * APA00639!26.06.07!2.2 ! ERW. DER PIN AUF 9 STELLEN! WANG     *
      * APA00622!14.06.07!2.3 ! NEU PO-KNR-VGES           ! STORCH   *
      * APA01101!14.03.12!2.4 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    ! *-ZEILEN ENTFERNT         !          *
      * APA01526!03.04.13!2.6 !PO-BEREICH ERWEITERT       ! WANG     *
      * APA02353!28.02.17!2.7 !PO-BEREICH ERWEITERT       ! STORCH   *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0871                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0871.
          05  PI-DRPA0871-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=K-VARIANTE-1, 2=K-VARIANTE-2,
      *                          3=K-VARIANTE-3
          05 PI-DRPA0871-DATEN.
             10 PI-DRPA0871-PGEB-GES         PIC  X(002).
             10 PI-DRPA0871-VNR              PIC  X(015).
             10 PI-DRPA0871-S-VNR            PIC  X(001).
             10 PI-DRPA0871-PIN              PIC  X(009).
             10 PI-DRPA0871-S-PIN            PIC  X(001).
             10 PI-DRPA0871-HISTORIE         PIC  X(002).
             10 PI-DRPA0871-S-HISTORIE       PIC  X(001).
             10 PI-DRPA0871-PROD-PROD-SCHL   PIC  X(003).
             10 PI-DRPA0871-S-PROD-SCHL      PIC  X(001).
      *
       01 PO-DRPA0871.
          05 PO-DRPA0871-RC                  PIC  X(003).
             88 PO-DRPA0871-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0871-RC-NOT-OK             VALUE 'NOK'.

          05 PO-DRPA0871-MELDUNG.
             10 PO-DRPA0871-MELDUNG-TYP       PIC  X(001).
             10 PO-DRPA0871-MELDUNG-NR        PIC S9(004) BINARY.

          05 PO-DRPA0871-MAP-STEUERUNG.
             10 PO-DRPA0871-MAP-ST-GES.
                20 PO-DRPA0871-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0871-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0871-FELD-CURSOR-KZ PIC X(001).

             10 PO-DRPA0871-MAP-ST-VNR.
                20 PO-DRPA0871-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0871-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0871-FELD-CURSOR-KZ PIC X(001).

             10 PO-DRPA0871-MAP-ST-PIN.
                20 PO-DRPA0871-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0871-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0871-FELD-CURSOR-KZ PIC X(001).

             10 PO-DRPA0871-MAP-ST-HIST.
                20 PO-DRPA0871-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0871-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0871-FELD-CURSOR-KZ PIC X(001).

             10 PO-DRPA0871-MAP-ST-PROD-SCHL.
                20 PO-DRPA0871-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0871-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0871-FELD-CURSOR-KZ PIC X(001).

          05 PO-DRPA0871-DATEN.
             10 PO-DRPA0871-PRINTE            PIC S9(015)V USAGE COMP-3.
             10 PO-DRPA0871-PGEB-GES          PIC S9(004) BINARY.
             10 PO-DRPA0871-PGEB-GES-BEZ10    PIC X(010).
             10 PO-DRPA0871-VNR               PIC X(015).
             10 PO-DRPA0871-PIN               PIC S9(009) BINARY.
             10 PO-DRPA0871-HISTORIE          PIC S9(004) BINARY.
             10 PO-DRPA0871-PVER-KUNDE-NAME   PIC X(025).
             10 PO-DRPA0871-ANZAHL-PROD       PIC 9(002).
             10 PO-DRPA0871-PROD-PROD-SCHL    PIC S9(004) BINARY.
             10 PO-DRPA0871-PROD-SCHL-BM      PIC  X(010).
             10 PO-DRPA0871-KNR-PVER-ART      PIC S9(004) BINARY.
             10 PO-DRPA0871-PVER-PG-VBNR      PIC S9(009) BINARY.
             10 PO-DRPA0871-PROVDAT-GELESEN   PIC  X(001).
             10 PO-DRPA0871-KNR-MANDANT       PIC S9(004) BINARY.
030413       10 PO-DRPA0871-PVER-URSPR-POL-DAT PIC  X(010).
030413       10 PO-DRPA0871-KNR-VERM-GES      PIC S9(004) BINARY.
280217       10 PO-DRPA0871-KNR-TLSN-PARTNER  PIC S9(004) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************

