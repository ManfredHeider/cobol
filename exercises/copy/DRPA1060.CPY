      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1060.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRPA1060.CPv  $
      *
      *   Rev 2.8   Sep 26 2016 11:18:24   t13ex67
      *APA02280: Dialoganpassungen PF11-PF14
      *
      *   Rev 2.7   Jul 16 2015 15:36:58   t13ex44
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.6   Jan 13 2014 09:47:08   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.5   Jun 19 2012 09:27:24   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.4   May 21 2012 08:52:16   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   Dec 10 2007 15:46:04   t13ex21
      *APA00622: Einführung Allfinanz (MST)
      *
      *   Rev 2.2   23 Aug 2007 09:53:44   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.1   07 Nov 2006 15:19:28   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 15:48:42   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   20 Jul 1998 10:19:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1060                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1060                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                        *
      *    DATUM:    10.09.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !10.09.98! 01 ! NEU                       ! SCHRIEBER*
      * APA00452!03.11.06!2.01! ERW. WG. 3-STELL. PRODUKTE! WANG     *
      * APA00639!27.06.07!2.02! ERW. DER PIN AUF 9 STELLEN! WANG     *
      * APA00622!30.07.07!2.03! NEUES AUSWAHLKRIT.KNR-VGES! STORCH   *
      * APA01101!27.02.12!2.04! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01526!21.10.13!2.05! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA01932!07.07.15!2.07! UST-BETRAG HINZU          ! SCHEIDEL *
      * APA02280!20.06.16!2.08! PI-BEREICH ERWEITERT      ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0307
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1060 DES BUFFER-OZP ZYPA0022 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PF14, ANTRäGE NACH FEHLERCODE.
      *
      ****************************************************************
      *
       01  PI-DRPA1060.
           05  PI-DRPA1060-VERZWEIGEN-FKT      PIC  X(03).
           05  PI-DRPA1060-UFKT                PIC  9(02).
           05  PI-DRPA1060-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA1060-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA1060-AUSWAHL-KRITERIEN.
               10 PI-DRPA1060-PGEB-GES         PIC S9(04)
                                                     USAGE IS BINARY.
               10 PI-DRPA1060-VSTAT            PIC  X(04).
               10 PI-DRPA1060-ABRECH-DATUM-VON PIC  X(10).
               10 PI-DRPA1060-ABRECH-DATUM-BIS PIC  X(10).
               10 PI-DRPA1060-MANDANT          PIC  9(02).
               10 PI-DRPA1060-S-MANDANT        PIC  X(01).
200616         10 PI-DRPA1060-PRODUKT          PIC  9(03).
200616         10 PI-DRPA1060-S-PRODUKT        PIC  X(01).
      *
       01  PO-DRPA1060.
           05  PO-DRPA1060-RC                  PIC  X(03).
               88 PO-DRPA1060-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1060-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1060-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1060-ANZAHL-ERG-ZEILEN   PIC  9(05).
           05  PO-DRPA1060-ERGEBNISZEILE.
               10 PO-DRPA1060-PNAP-ID          PIC S9(15)V
                                                      PACKED-DECIMAL.
               10 PO-DRPA1060-PNAP-VERSION     PIC S9(04)
                                                      USAGE IS BINARY.
               10 PO-DRPA1060-KNR-OBJEKT-HKF-TAB  PIC S9(04)
                                                      USAGE IS BINARY.
               10 PO-DRPA1060-PRINTE           PIC S9(15)V
                                                      PACKED-DECIMAL.
               10 PO-DRPA1060-PGEB-GES         PIC S9(04)
                                                      USAGE IS BINARY.
               10 PO-DRPA1060-VNR              PIC  X(15).
               10 PO-DRPA1060-PIN              PIC S9(09)
                                                      PACKED-DECIMAL.
               10 PO-DRPA1060-VSTAT            PIC  X(04).
               10 PO-DRPA1060-PROD-PROD-SCHL1  PIC  S9(04) BINARY.
               10 PO-DRPA1060-PROD-PROD-SCHL2  PIC  S9(04) BINARY.
               10 PO-DRPA1060-PROD-PROD-SCHL3  PIC  S9(04) BINARY.
               10 PO-DRPA1060-PROD-SCHL-KZ     PIC  X(01).
               10 PO-DRPA1060-GSMT-PROV        PIC S9(09)V99
                                                    PACKED-DECIMAL.
               10 PO-DRPA1060-FAELLIG-VON      PIC  X(10).
               10 PO-DRPA1060-PG-ABRE-DAT      PIC  X(10).
               10 PO-DRPA1060-KNR-MANDANT      PIC S9(04) BINARY.
211013         10 PO-DRPA1060-UNR              PIC S9(004) BINARY.
211013         10 PO-DRPA1060-SCHICHT-NR       PIC S9(004) BINARY.
070715         10 PO-DRPA1060-UST-BETRAG       PIC S9(009)V9(02)
070715                                              PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************


