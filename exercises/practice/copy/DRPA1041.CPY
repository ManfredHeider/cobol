      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1041.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA1041.CPv  $
      *
      *   Rev 2.7   Sep 26 2016 11:17:42   t13ex67
      *APA02280: Dialoganpassungen PF11-PF14
      *
      *   Rev 2.6   Aug 27 2015 14:32:02   t13ae23
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: PF13
      *
      *   Rev 2.5   Aug 12 2015 09:57:30   t13ae23
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: PF13
      *
      *   Rev 2.4   Jul 21 2015 10:46:08   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.3   Jun 19 2012 09:26:46   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:51:38   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 11:39:36   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   May 11 1999 08:15:12   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 14:00:32   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1041                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1041                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                        *
      *    DATUM:    15.08.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !15.08.98! 01 ! NEU                       ! SCHRIEBER*
      * APA00622!30.07.07!2.01! NEUES AUSWAHLKRIT.KNR-VGES! STORCH   *
      * APA01101!05.03.12!2.02! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01932!16.07.15!2.03! PG-UST-BETRAG  NEU        ! WANG     *
      * APA01932!12.08.15!2.5 ! UST-BETRAG GUTS. BELAST.  ! RAHNER   *
      * APA02280!20.06.16!2.07! PI-BEREICH UM PRODUKT ERW.! STORCH  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1041
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1041 DES BUFFER-OZP ZYPA0002 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PF13, ANTRäGE NACH FEHLERCODE.
      *
      ****************************************************************
      *
       01  PI-DRPA1041.
           05  PI-DRPA1041-UFKT                PIC  9(02).
           05  PI-DRPA1041-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA1041-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA1041-DATEN.
               10 PI-DRPA1041-PGEB-GES         PIC S9(04).
               10 PI-DRPA1041-VSTAT-KLASSE     PIC  X(01).
               10 PI-DRPA1041-ABRECH-DATUM-VON PIC  X(10).
               10 PI-DRPA1041-ABRECH-DATUM-BIS PIC  X(10).
               10 PI-DRPA1041-KNR-MANDANT      PIC  9(02).
               10 PI-DRPA1041-S-KNR-MANDANT    PIC  X(01).
200616         10 PI-DRPA1041-PRODUKT          PIC  9(03).
200616         10 PI-DRPA1041-S-PRODUKT        PIC  X(01).
      *
       01  PO-DRPA1041.
           05  PO-DRPA1041-RC                  PIC  X(03).
               88 PO-DRPA1041-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1041-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1041-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1041-ANZAHL-ERG-ZEILEN   PIC  9(05).
      *
           05  PO-DRPA1041-DATEN.
               10 PO-DRPA1041-VSTAT               PIC  X(04).
               10 PO-DRPA1041-ANZ-BEWEGUNGEN      PIC  9(09) USAGE COMP.
120815*        10 PO-DRPA1041-SUMME-GUTSCH        PIC  S9(09)V9(02)
120815*                                                PACKED-DECIMAL.
120815*        10 PO-DRPA1041-SUMME-BELAST        PIC  S9(09)V9(02)
120815*                                                PACKED-DECIMAL.
120815         10 PO-DRPA1041-SUMME-GUTS-BRUTTO   PIC  S9(09)V9(02)
120815                                                 PACKED-DECIMAL.
120815         10 PO-DRPA1041-SUMME-GUTS-NETTO    PIC  S9(09)V9(02)
120815                                                 PACKED-DECIMAL.
120815         10 PO-DRPA1041-SUMME-BEL-BRUTTO    PIC  S9(09)V9(02)
120815                                                 PACKED-DECIMAL.
120815         10 PO-DRPA1041-SUMME-BEL-NETTO     PIC  S9(09)V9(02)
120815                                                 PACKED-DECIMAL.
               10 PO-DRPA1041-DIFFERENZ           PIC  S9(09)V9(02)
                                                       PACKED-DECIMAL.
               10 PO-DRPA1041-ABRECH-DATUM        PIC  X(10).
      *
160715         10 PO-DRPA1041-PG-UST-BETRAG       PIC  S9(09)V9(02)
160715                                                 PACKED-DECIMAL.
120815         10 PO-DRPA1041-PG-UST-GUTSCH       PIC  S9(09)V9(02)
120815                                                 PACKED-DECIMAL.
120815         10 PO-DRPA1041-PG-UST-BELAST       PIC  S9(09)V9(02)
                                                       PACKED-DECIMAL.
      ******* ENDE COPY-MEMBER **************************************

