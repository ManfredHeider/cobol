      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1469.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1469.CPv  $
      *
      *   Rev 2.5   14 May 2018 13:09:28   t13ae37
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - Copy
      * ABU
      *
      *   Rev 2.4   Mar 28 2017 08:21:04   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs
      *
      *   Rev 2.3   Jul 16 2015 15:02:22   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.2   Dec 17 2013 13:15:54   t13ae39
      *APA01539: Kalypso TP5: Anpassungen Datenmodell WEPA0008
      *
      *   Rev 2.1   26 Feb 2001 15:34:44   T13EX21
      *APA00118 - Neuerstellung
      *
      *   Rev 2.0   09 Jan 2001 10:12:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1469                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPE ÜBER PGEB UND VNR>>         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                        *
      *    DATUM:    14.02.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!14.02.01!02.01! NEUERSTELLUNG            ! STORCH   *
      * APA01539!30.07.13!02.02! ERGÄNZUNGEN FÜR KALYPSO  ! STRAUSS  *
      * APA01932!27.03.15!02.03! NEUE FELDER HINZU        ! STORCH   *
      * APA02353!08.03.17!02.04! NEUES FELD HINZU         ! STORCH   *
      * APA02579!14.05.18!02.05! EINFÜHRUNG ALAG          ! BUCH     *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1469                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1469.
          05 PI-DRPA1469-PROGRAMM            PIC  X(008).
          05 PI-DRPA1469-UFKT                PIC  9(002).
          05 PI-DRPA1469-DATEN.
             10 PI-DRPA1469-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA1469-VNR              PIC  X(015).
      *
       01 PO-DRPA1469.
          05 PO-DRPA1469-RC                PIC  X(03).
             88 PO-DRPA1469-RC-OK              VALUE 'OK '.
             88 PO-DRPA1469-RC-EOF             VALUE 'EOF'.
             88 PO-DRPA1469-RC-LEER            VALUE 'LER'.
          05 PO-DRPA1469-DATEN.
             10 PMPG-ID                    PIC S9(15)
                                           PACKED-DECIMAL.
             10 PMPG-VERSION               PIC S9(04) BINARY.
             10 PMPG-LFD-NR-VNR            PIC S9(09) BINARY.
             10 PDTA-ID                    PIC S9(15) PACKED-DECIMAL.
             10 PGEB-GES                   PIC S9(4) USAGE COMP.
             10 PMPE-VNR                   PIC X(15).
             10 PMPE-VERTR-BEGINN          PIC X(10).
             10 PMPE-VERTR-ABLAUF          PIC X(10).
             10 PMPE-FAELLIG-VON           PIC X(10).
             10 PMPE-FAELLIG-BIS           PIC X(10).
             10 PMPE-PG-VBNR               PIC S9(9) USAGE COMP.
             10 PMPE-KUNDE-NAME            PIC X(25).
             10 PMPE-KUNDE-VNAME           PIC X(20).
             10 PMPE-GSMT-PROV-DAT         PIC X(10).
             10 PMPE-PG-ABR-DAT            PIC X(10).
             10 PMPE-PG-EINL-DAT           PIC X(10).
             10 PMPE-PIN                   PIC S9(09) USAGE COMP.
             10 KNR-ZAHLWEISE              PIC S9(04) USAGE COMP.
             10 KNR-WAEHRUNG               PIC S9(04) BINARY.
             10 PMPE-VSTAT                 PIC X(04).
             10 PMPE-LAST-UPDATE           PIC X(26).
             10 PMPE-KENNUNG               PIC X(08).
             10 PMPEH-LAST-UPDATE          PIC X(26).
             10 PMPEH-KENNUNG              PIC X(08).
             10 PMPE-PG-PM-DATEN.
                49 PMPE-PG-PM-DATEN-LEN    PIC S9(04) USAGE COMP.
                49 PMPE-PG-PM-DATEN-TEXT   PIC X(300).
290713       10 PMPE-BZ-NAME               PIC X(25).
290713       10 PMPE-BZ-VNAME              PIC X(20).
290713       10 PMPE-VP-NAME               PIC X(25).
290713       10 PMPE-VP-VNAME              PIC X(20).
290713       10 PMPE-KZ-EIGENV             PIC X(1).
290713       10 KNR-FEIV-KZ                PIC S9(4)  USAGE COMP.
290713       10 KNR-VERM-GES               PIC S9(4)  USAGE COMP.
290713       10 PMPE-UMW-NR                PIC S9(15)V PACKED-DECIMAL.
290713       10 PMPE-UNR                   PIC S9(4)  USAGE COMP.
290713       10 PMPE-SCHICHT-NR            PIC S9(4)  USAGE COMP.
290713       10 PMPE-INKST-DAT             PIC X(10).
290713       10 KNR-INK-ART                PIC S9(4)  USAGE COMP.
290713       10 PMPE-SAM-NR                PIC S9(9)  USAGE COMP.
290713       10 KNR-RAB-ST                 PIC S9(4)  USAGE COMP.
290713       10 PMPE-VVG-KZ                PIC X(1).
290713       10 KNR-DEPOT-KZ               PIC S9(4)  USAGE COMP.
290713       10 PMPE-PROV-SCHL             PIC X(5).
290713       10 PMPE-BEW-DAT               PIC X(10).
290713       10 PMPE-URSPR-POL-DAT         PIC X(10).
290713       10 PMPE-POL-DAT               PIC X(10).
290713       10 PMPE-BESVV                 PIC S9(1)V PACKED-DECIMAL.
290713       10 PMPE-VN-STRASSE            PIC X(35).
290713       10 PMPE-VN-PLZ                PIC X(5).
290713       10 PMPE-VN-ORT                PIC X(35).
290713       10 PMPE-RATENBEITRAG          PIC S9(9)V9(2)
290713                                     PACKED-DECIMAL.
270315       10 PMPE-PG-RECH-NR            PIC X(12).
270315       10 PMPE-HIST-UNR              PIC S9(4)  USAGE COMP.
080317       10 KNR-TLSN-PARTNER           PIC S9(4)  USAGE COMP.
140518       10 PMPE-GEL-KENNUNG           PIC  X(6).
 
 

