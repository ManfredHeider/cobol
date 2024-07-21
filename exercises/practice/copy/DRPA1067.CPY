      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1067.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA1067.CPv  $
      *
      *   Rev 2.7   Aug 17 2015 14:33:26   t13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: PF15
      *
      *   Rev 2.6   Aug 03 2015 14:55:26   T13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: PF15
      *
      *   Rev 2.5   Jan 13 2014 09:47:16   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.4   Jun 19 2012 09:27:56   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:52:46   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 12:16:28   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   07 Nov 2006 15:20:14   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Apr 30 1999 15:35:38   T13EX21
      *APA00012 - Erweiterung
      *
      *   Rev 1.0   20 Jul 1998 10:19:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1067                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1067                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                        *
      *    DATUM:    13.09.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!13.09.98! 01 ! NEU                       ! SCHRIEBER*
      * APA00012!24.04.99!    ! ERWEITERUNG KZ-HERKUNFT   ! STORCH   *
      * APA00452!07.11.06! 2.1! ERW. WG. 3-STELL. PRODUKTE! WANG     *
      * APA00622!08.08.07! 2.2! ERWEITERUNG PO-BEREICH UM ! STORCH   *
      *         !        !    ! KNR-VGES                  !          *
      * APA01101!05.03.12! 2.3! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    ! *-ZEILEN ENTFERNT         !          *
      * APA01526!16.10.13! 2.4! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA01932!07.07.15! 2.5! LUCY: UST FÜR GES. 90 DV  ! STRAUSS  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1067
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1067 DES BUFFER-OZP ZYPA0023 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PF15, ANTRÄGE NACH FEHLERCODE.
      *
      ****************************************************************
      *
       01  PI-DRPA1067.
           05 PI-DRPA1067-UFKT                PIC  9(02).
           05 PI-DRPA1067-NAME-ZW-SPEICHER    PIC  X(08).
           05 PI-DRPA1067-LOG-SATZ-NR         PIC  9(05).
           05 PI-DRPA1067-AUSWAHL-KRITERIEN.
             10 PI-DRPA1067-PGEB-GES         PIC  9(02).
             10 PI-DRPA1067-PRINTE           PIC S9(15)V PACKED-DECIMAL.
             10 PI-DRPA1067-VNR              PIC  X(15).
      *
       01  PO-DRPA1067.
           05  PO-DRPA1067-RC                  PIC  X(03).
               88 PO-DRPA1067-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1067-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1067-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1067-ANZAHL-ERG-ZEILEN   PIC  9(05).
           05  PO-DRPA1067-ERGEBNISZEILE.
               10 PO-DRPA1067-PNAP-ID          PIC  9(15)
                                                      PACKED-DECIMAL.
               10 PO-DRPA1067-PNAP-VERSION     PIC S9(04)
                                                      USAGE IS BINARY.
               10 PO-DRPA1067-KNR-OBJEKT-HKF-TAB  PIC S9(04)
                                                      USAGE IS BINARY.
               10 PO-DRPA1067-PGEB-GES         PIC S9(04)
                                                      USAGE IS BINARY.
               10 PO-DRPA1067-VNR              PIC  X(15).
               10 PO-DRPA1067-KUNDE-NAME       PIC  X(25).
               10 PO-DRPA1067-VSTAT            PIC  X(04).
               10 PO-DRPA1067-PROD-PROD-SCHL1  PIC  S9(04) BINARY.
               10 PO-DRPA1067-PROD-PROD-SCHL2  PIC  S9(04) BINARY.
               10 PO-DRPA1067-PROD-PROD-SCHL3  PIC  S9(04) BINARY.
               10 PO-DRPA1067-PROD-SCHL-KZ     PIC  X(01).
               10 PO-DRPA1067-GSMT-PROV        PIC S9(09)V99
                                                    PACKED-DECIMAL.
070715         10 PO-DRPA1067-UST-BETRAG       PIC S9(09)V99
070715                                              PACKED-DECIMAL.
               10 PO-DRPA1067-FAELLIG-VON      PIC  X(10).
               10 PO-DRPA1067-PG-ABRE-DAT      PIC  X(10).
               10 PO-DRPA1067-KNR-MANDANT      PIC S9(04) BINARY.
               10 PO-DRPA1067-KZ-HERKUNFT      PIC  X(01).
161013         10 PO-DRPA1067-PMPE-UNR         PIC S9(004) BINARY.
161013         10 PO-DRPA1067-PMPE-SCHICHT-NR  PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************


