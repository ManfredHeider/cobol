      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1063.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA1063.CPv  $
      *
      *   Rev 2.7   Sep 26 2016 11:18:50   t13ex67
      *APA02280: Dialoganpassungen PF11-PF14
      *
      *   Rev 2.6   Jul 16 2015 15:09:40   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.5   Jun 19 2012 09:27:48   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.4   May 21 2012 08:52:38   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   Dec 10 2007 12:11:40   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.2   23 Aug 2007 09:54:24   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.1   07 Nov 2006 15:20:02   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 11 1999 08:15:22   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   20 Jul 1998 10:19:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1063  FUNKTION SUCHE ALLE EINTRÄGE ZU    *
      *                                    EINEM VSTATUS             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                       *
      *    DATUM:    08.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AFPA0012!08.09.98!01.01! NEUERSTELLUNG             !SCHRIEBER*
      * APA00452!05.11.06! 2.01! ERW. WG. 3-STELL. PRODUKTE!WANG     *
      * APA00639!29.06.07! 2.02! ERW. WG. 9-STELL. PIN     !WANG     *
      * APA00622!30.07.07! 2.03! NEUES AUSWAHLKRIT.KNR-VGES! STORCH  *
      * APA01101!27.02.12! 2.04! VGES DURCH MANDANT ERSETZT! STORCH  *
      * APA01932!06.05.15! 2.6 ! PO-DRPA1063-PG-UST-BETRAG ! WANG    *
      *         !        !     ! NEU                       !         *
      * APA02280!20.06.16! 2.7 ! PI-BEREICH UM PRODUKT ERW.! STORCH  *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1063                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1063.
          05 PI-DRPA1063-PROGRAMM            PIC  X(008).
          05 PI-DRPA1063-UFKT                PIC  9(002).
          05 PI-DRPA1063-DATEN.
             10 PI-DRPA1063-PGEB-GES         PIC S9(04) USAGE IS BINARY.
             10 PI-DRPA1063-VSTAT            PIC  X(04).
             10 PI-DRPA1063-ABRECH-DATUM-VON PIC  X(10).
             10 PI-DRPA1063-ABRECH-DATUM-BIS PIC  X(10).
             10 PI-DRPA1063-MANDANT          PIC  9(02).
             10 PI-DRPA1063-S-MANDANT        PIC  X(01).
200616       10 PI-DRPA1063-PRODUKT          PIC  9(03).
200616       10 PI-DRPA1063-S-PRODUKT        PIC  X(01).
      *
       01 PO-DRPA1063.
          05 PO-DRPA1063-RC                  PIC  X(003).
             88 PO-DRPA1063-RC-OK               VALUE 'OK '.
             88 PO-DRPA1063-RC-LEER             VALUE 'LER'.
             88 PO-DRPA1063-RC-EOF              VALUE 'EOF'.
      *
          05 PO-DRPA1063-DATEN.
             10 PO-DRPA1063-PNAP-ID          PIC S9(15)V PACKED-DECIMAL.
             10 PO-DRPA1063-PNAP-VERSION     PIC S9(04) BINARY.
             10 PO-DRPA1063-OBJEKT-HKF-TAB   PIC S9(04) BINARY.
             10 PO-DRPA1063-PRINTE           PIC S9(15)V PACKED-DECIMAL.
             10 PO-DRPA1063-PGEB-GES         PIC S9(04) BINARY.
             10 PO-DRPA1063-VNR              PIC  X(15).
             10 PO-DRPA1063-PIN              PIC S9(09) PACKED-DECIMAL.
             10 PO-DRPA1063-VSTAT            PIC  X(04).
             10 PO-DRPA1063-PROD-PROD-SCHL1  PIC  S9(04) BINARY.
             10 PO-DRPA1063-PROD-PROD-SCHL2  PIC  S9(04) BINARY.
             10 PO-DRPA1063-PROD-PROD-SCHL3  PIC  S9(04) BINARY.
             10 PO-DRPA1063-PROD-SCHL-KZ     PIC  X(01).
             10 PO-DRPA1063-GSMT-PROV        PIC  S9(09)V9(02)
                                                       PACKED-DECIMAL.
             10 PO-DRPA1063-FAELLIG-VON      PIC  X(10).
             10 PO-DRPA1063-ABRECH-DATUM     PIC  X(10).
             10 PO-DRPA1063-KNR-MANDANT      PIC S9(04) BINARY.
060515       10 PO-DRPA1063-PG-UST-BETRAG    PIC S9(09)V9(02)
060515                                                 PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************


