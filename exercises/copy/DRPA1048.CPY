      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1048.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1048.CPv  $
      *
      *   Rev 2.5   Sep 26 2016 11:18:08   t13ex67
      *APA02280: Dialoganpassungen PF11-PF14
      *
      *   Rev 2.4   Jul 16 2015 15:09:12   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.3   Jun 19 2012 09:27:10   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:52:00   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 11:53:16   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   May 11 1999 09:42:08   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 14:00:54   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1048                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1048                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Wilhelm                                        *
      *    DATUM:    20.08.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !20.08.98! 01 ! NEU                       ! D.WILHELM*
      * APA00622!27.07.07!2.01! NEUES EINGABEFELD KNR-VGES! STORCH   *
      * APA01101!15.03.12!2.02! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01932!06.05.15!2.6 ! NEUES AUSGABEFELD         ! WANG     *
      * ????    !20.06.16!2.7 ! NEUES SCHLÜSSELFELD       ! STORCH   *
      *         !        !    ! PRODUKT                   !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1048
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1048 DES BUFFER-OZP ZYPA0021 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PA15, ANTRäGE NACH FEHLERCODE.
      *
      ****************************************************************
      *
       01  PI-DRPA1048.
           05  PI-DRPA1048-UFKT                PIC  9(02).
           05  PI-DRPA1048-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA1048-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA1048-AUSWAHL-KRITERIEN.
               10 PI-DRPA1048-PGEB-GES         PIC  9(02).
               10 PI-DRPA1048-S-PGEB-GES       PIC  X(01).
               10 PI-DRPA1048-ABR-DAT-VON      PIC  X(10).
               10 PI-DRPA1048-S-ABR-DAT-VON    PIC  X(01).
               10 PI-DRPA1048-ABR-DAT-BIS      PIC  X(10).
               10 PI-DRPA1048-S-ABR-DAT-BIS    PIC  X(01).
               10 PI-DRPA1048-KNR-MANDANT      PIC  9(02).
               10 PI-DRPA1048-S-KNR-MANDANT    PIC  X(01).
200616         10 PI-DRPA1048-PRODUKT          PIC  9(04).
200616         10 PI-DRPA1048-S-PRODUKT        PIC  X(01).
      *
       01  PO-DRPA1048.
           05  PO-DRPA1048-RC                  PIC  X(03).
               88 PO-DRPA1048-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1048-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1048-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1048-ANZAHL-ERG-ZEILEN   PIC  9(05).
      *
           05  PO-DRPA1048-ERGEBNISZEILE.
               10 PO-DRPA1048-PGEB-GES         PIC 99.
               10 PO-DRPA1048-PROV-SUMME       PIC S9(09)V99 COMP-3.
               10 PO-DRPA1048-ANZ-FEHL         PIC S9(07)    COMP-3.
               10 PO-DRPA1048-ANZ-ZUKUNFT      PIC S9(07)    COMP-3.
               10 PO-DRPA1048-ABR-DAT-VON      PIC  X(010).
               10 PO-DRPA1048-ABR-DAT-BIS      PIC  X(010).
060515         10 PO-DRPA1048-PG-UST-BETRAG    PIC S9(09)V99 COMP-3.
060515*        10 FILLER                       PIC  X(064) VALUE SPACE.
060515         10 FILLER                       PIC  X(058) VALUE SPACE.
      *
      ******* ENDE COPY-MEMBER **************************************


