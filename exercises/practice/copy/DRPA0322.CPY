      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0322.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA0322.CPv  $
      *
      *   Rev 2.6   14 May 2018 13:09:00   t13ae37
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - Copy
      * ABU
      *
      *   Rev 2.5   Mar 28 2017 10:42:38   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs
      *
      *   Rev 2.4   Mar 28 2017 08:19:00   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs
      *
      *   Rev 2.3   Jul 16 2015 15:00:26   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.2   Mar 10 2014 16:44:52   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.1   Jul 26 2013 11:40:22   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.0   Feb 27 1999 14:28:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:50:00   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:36:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0322  FUNKTION SELECT STANDARD-OZP       *
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
      *         !31.07.07!1.1 !PMPE-GES-PROV-DAT EINGEFÜGT!OPRITZ    *
      * AFPA0022!28.08.97!    ! UMBENENNUNGEN             !JAN OPRITZ*
      * AFPA00??!09.10.97!    ! ANPASSUNGEN AUFGRUND VON  ! STORCH   *
      *         !        !    ! TABELLENÄNDERUNGEN        !          *
      * APA00012!05.03.98!1.04! PMPG-VERSION ENTFERNT     ! JÄGER    *
      * APA01539!26.07.13! 2.1! ERGÄNZUNGEN FÜR KALYPSO   ! RAHNER   *
      * APA01932!27.03.15!2.3 ! NEUE ATTRIBUTE            ! STORCH   *
      * APA02353!17.02.17!2.4 ! Teleson                   ! Buch     *
      * APA02579!25.04.18!2.7 ! Frischer Wind             ! Buch     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0322                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>> SELECT ALL<<       TABELLE      TBPA0102 PMPE         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0322.
          05 PI-DRPA0322-PMPG-KEY.
             10 PI-DRPA0322-PMPG-ID           PIC S9(015) COMP-3.
      *
       01 PO-DRPA0322.
          05 PO-DRPA0322-RC                   PIC  X(003).
             88 PO-DRPA0322-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0322-RC-NOT-FOUND                  VALUE 'NFD'.
      *
          05 PO-DRPA0322-PMPG-VERSION         PIC S9(004) COMP.
          05 PO-DRPA0322-PMPG-LFD-NR-VNR      PIC S9(009) COMP.
          05 PO-DRPA0322-PDTA-ID              PIC S9(015) COMP-3.
          05 PO-DRPA0322-PGEB-GES             PIC S9(004) COMP.
          05 PO-DRPA0322-PMPE-VNR             PIC  X(015).
          05 PO-DRPA0322-PMPE-VERTR-BEGINN    PIC  X(010).
          05 PO-DRPA0322-PMPE-VERTR-ABLAUF    PIC  X(010).
          05 PO-DRPA0322-PMPE-FAELLIG-VON     PIC  X(010).
          05 PO-DRPA0322-PMPE-FAELLIG-BIS     PIC  X(010).
          05 PO-DRPA0322-PMPE-PG-VBNR         PIC S9(009) COMP.
          05 PO-DRPA0322-PMPE-KUNDE-NAME      PIC  X(025).
          05 PO-DRPA0322-PMPE-KUNDE-VNAME     PIC  X(020).
          05 PO-DRPA0322-PMPE-GSMT-PROV-DAT   PIC  X(010).
          05 PO-DRPA0322-PMPE-PG-ABR-DAT      PIC  X(010).
          05 PO-DRPA0322-PMPE-PG-EINL-DAT     PIC  X(010).
          05 PO-DRPA0322-PMPE-PIN             PIC S9(009) COMP.
          05 PO-DRPA0322-KNR-ZAHLWEISE        PIC S9(004) COMP.
          05 PO-DRPA0322-KNR-WAEHRUNG         PIC S9(004) COMP.
          05 PO-DRPA0322-PMPE-VSTAT           PIC  X(004).
          05 PO-DRPA0322-PMPE-LAST-UPDATE     PIC  X(026).
          05 PO-DRPA0322-PMPE-KENNUNG         PIC  X(008).
          05 PO-DRPA0322-PMPE-PG-PM-DATEN.
             10 PO-DRPA0322-PMPE-PG-DATEN-LEN PIC S9(004) COMP.
             10 PO-DRPA0322-PMPE-PG-DATEN-TEXT PIC X(300).
260713    05 PO-DRPA0322-PMPE-BZ-NAME         PIC X(25).
260713    05 PO-DRPA0322-PMPE-BZ-VNAME        PIC X(20).
260713    05 PO-DRPA0322-PMPE-VP-NAME         PIC X(25).
260713    05 PO-DRPA0322-PMPE-VP-VNAME        PIC X(20).
260713    05 PO-DRPA0322-PMPE-KZ-EIGENV       PIC X(1).
260713    05 PO-DRPA0322-KNR-FEIV-KZ          PIC S9(4)    COMP.
260713    05 PO-DRPA0322-KNR-VERM-GES         PIC S9(4)    COMP.
260713    05 PO-DRPA0322-PMPE-UMW-NR          PIC S9(15)V  COMP-3.
260713    05 PO-DRPA0322-PMPE-UNR             PIC S9(4)    COMP.
260713    05 PO-DRPA0322-PMPE-SCHICHT-NR      PIC S9(4)    COMP.
260713    05 PO-DRPA0322-PMPE-INKST-DAT       PIC X(10).
260713    05 PO-DRPA0322-KNR-INK-ART          PIC S9(4)    COMP.
260713    05 PO-DRPA0322-PMPE-SAM-NR          PIC S9(9)    COMP.
260713    05 PO-DRPA0322-KNR-RAB-ST           PIC S9(4)    COMP.
260713    05 PO-DRPA0322-PMPE-VVG-KZ          PIC X(1).
260713    05 PO-DRPA0322-KNR-DEPOT-KZ         PIC S9(4)    COMP.
260713    05 PO-DRPA0322-PMPE-PROV-SCHL       PIC X(5).
260713    05 PO-DRPA0322-PMPE-BEW-DAT         PIC X(10).
260713    05 PO-DRPA0322-PMPE-URSPR-POL-DAT   PIC X(10).
260713    05 PO-DRPA0322-PMPE-POL-DAT         PIC X(10).
260713    05 PO-DRPA0322-PMPE-BESVV           PIC S9(1)V   COMP-3.
260713    05 PO-DRPA0322-PMPE-VN-STRASSE      PIC X(35).
260713    05 PO-DRPA0322-PMPE-VN-PLZ          PIC X(5).
260713    05 PO-DRPA0322-PMPE-VN-ORT          PIC X(35).
260713    05 PO-DRPA0322-PMPE-RATENBEITRAG    PIC S9(9)V9(2) COMP-3.
270315*   05 PO-DRPA0322-PMPE-PG-RECH-NR      PIC X(12).
170217    05 PO-DRPA0322-PMPE-PG-RECH-NR      PIC X(50).
270315    05 PO-DRPA0322-PMPE-HIST-UNR        PIC S9(04).
170217    05 PO-DRPA0322-KNR-TLSN-PARTNER     PIC S9(04) COMP.
250418    05 PO-DRPA0322-PMPE-GEL-KENNUNG     PIC  X(06).
      *
      ****** Ende Copy-Member ****************************************
 
 
 

