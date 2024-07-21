      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0320.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0320.CPv  $
      *
      *   Rev 2.5   14 May 2018 13:08:52   t13ae37
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - Copy
      * ABU
      *
      *   Rev 2.4   Mar 28 2017 08:18:54   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs
      *
      *   Rev 2.3   Jul 16 2015 15:00:16   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.2   Mar 10 2014 16:44:46   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.1   Jul 26 2013 11:40:16   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.0   Feb 27 1999 14:28:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:48:48   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:35:50   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0320  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: OPRITZ                                          *
      *    DATUM:    09.06.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.06.97! 01 ! NEUERSTELLUNG             ! OPRITZ   *
      *         !31.07.97!1.1 ! PMPE-GES-PROV-DAT EINGEFÜGT OPRITZ   *
      * AFPA0022!28.08.97!    ! UMBENENNUNGEN             !JAN OPRITZ*
      * AFPA00??!09.10.97!    ! ANPASSUNGEN AUFGRUND VON  ! STORCH   *
      *         !        !    ! TABELLENÄNDERUNGEN        !          *
      * APA01539!26.07.13! 2.1! ERGÄNZUNGEN FÜR KALYPSO   ! RAHNER   *
      * APA01932!27.03.15! 2.2! NEUE FELDER HINZU         ! STORCH   *
      * APA02353017.02.17! 2.4! Teleson                   ! Buch     *
      * APA02579025.04.18! 2.5! Einführung ALAG           ! Buch     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0320                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE      TBPA0102 PMPE         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0320.
          05 PI-DRPA0320-PMPE-LAST-UPDATE     PIC  X(026).
          05 PI-DRPA0320-PMPG-KEY.
             10 PI-DRPA0320-PMPG-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0320-PMPG-VERSION      PIC S9(004) COMP.
      *
          05 PI-DRPA0320-PMPG-LFD-NR-VNR      PIC S9(009) COMP.
          05 PI-DRPA0320-PDTA-ID              PIC S9(015) COMP-3.
          05 PI-DRAP0320-PGEB-GES             PIC S9(004) COMP.
          05 PI-DRAP0320-PMPE-VNR             PIC  X(015).
          05 PI-DRPA0320-PMPE-VERTR-BEGINN    PIC  X(010).
          05 PI-DRPA0320-PMPE-VERTR-ABLAUF    PIC  X(010).
          05 PI-DRPA0320-PMPE-FAELLIG-VON     PIC  X(010).
          05 PI-DRPA0320-PMPE-FAELLIG-BIS     PIC  X(010).
          05 PI-DRPA0320-PMPE-PG-VBNR         PIC S9(009) COMP.
          05 PI-DRPA0320-PMPE-KUNDE-NAME      PIC  X(025).
          05 PI-DRPA0320-PMPE-KUNDE-VNAME     PIC  X(020).
          05 PI-DRPA0320-PMPE-PG-ABR-DAT      PIC  X(010).
          05 PI-DRPA0320-PMPE-GSMT-PROV-DAT   PIC  X(010).
          05 PI-DRPA0320-PMPE-PG-EINL-DAT     PIC  X(010).
          05 PI-DRPA0320-PMPE-PIN             PIC S9(009) COMP.
          05 PI-DRPA0320-KNR-ZAHLWEISE        PIC S9(004) COMP.
          05 PI-DRPA0320-KNR-WAEHRUNG         PIC S9(004) COMP.
          05 PI-DRPA0320-PMPE-VSTAT           PIC  X(004).
          05 PI-DRPA0320-PMPE-PG-PM-DATEN.
             10 PI-DRPA0320-PMPE-PG-DATEN-LEN  PIC S9(004) COMP.
             10 PI-DRPA0320-PMPE-PG-DATEN-TEXT PIC X(300).
260713    10 PI-DRPA0320-PMPE-BZ-NAME         PIC X(25).
260713    10 PI-DRPA0320-PMPE-BZ-VNAME        PIC X(20).
260713    10 PI-DRPA0320-PMPE-VP-NAME         PIC X(25).
260713    10 PI-DRPA0320-PMPE-VP-VNAME        PIC X(20).
260713    10 PI-DRPA0320-PMPE-KZ-EIGENV       PIC X(1).
260713    10 PI-DRPA0320-KNR-FEIV-KZ          PIC S9(4)    COMP.
260713    10 PI-DRPA0320-KNR-VERM-GES         PIC S9(4)    COMP.
260713    10 PI-DRPA0320-PMPE-UMW-NR          PIC S9(15)V  COMP-3.
260713    10 PI-DRPA0320-PMPE-UNR             PIC S9(4)    COMP.
260713    10 PI-DRPA0320-PMPE-SCHICHT-NR      PIC S9(4)    COMP.
260713    10 PI-DRPA0320-PMPE-INKST-DAT       PIC X(10).
260713    10 PI-DRPA0320-KNR-INK-ART          PIC S9(4)    COMP.
260713    10 PI-DRPA0320-PMPE-SAM-NR          PIC S9(9)    COMP.
260713    10 PI-DRPA0320-KNR-RAB-ST           PIC S9(4)    COMP.
260713    10 PI-DRPA0320-PMPE-VVG-KZ          PIC X(1).
260713    10 PI-DRPA0320-KNR-DEPOT-KZ         PIC S9(4)    COMP.
260713    10 PI-DRPA0320-PMPE-PROV-SCHL       PIC X(5).
260713    10 PI-DRPA0320-PMPE-BEW-DAT         PIC X(10).
260713    10 PI-DRPA0320-PMPE-URSPR-POL-DAT   PIC X(10).
260713    10 PI-DRPA0320-PMPE-POL-DAT         PIC X(10).
260713    10 PI-DRPA0320-PMPE-BESVV           PIC S9(1)V   COMP-3.
260713    10 PI-DRPA0320-PMPE-VN-STRASSE      PIC X(35).
260713    10 PI-DRPA0320-PMPE-VN-PLZ          PIC X(5).
260713    10 PI-DRPA0320-PMPE-VN-ORT          PIC X(35).
260713    10 PI-DRPA0320-PMPE-RATENBEITRAG    PIC S9(9)V9(2) COMP-3.
270315*   10 PI-DRPA0320-PMPE-PG-RECH-NR      PIC X(12).
170217    10 PI-DRPA0320-PMPE-PG-RECH-NR      PIC X(50).
270315    10 PI-DRPA0320-PMPE-HIST-UNR        PIC S9(04).
170217    10 PI-DRPA0320-KNR-TLSN-PARTNER     PIC S9(04) COMP.
250418    10 PI-DRPA0320-PMPE-GEL-KENNUNG     PIC  X(06).
      *
       01 PO-DRPA0320.
          05 PO-DRPA0320-RC                   PIC  X(003).
             88 PO-DRPA0320-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0320-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0320-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
 
 

