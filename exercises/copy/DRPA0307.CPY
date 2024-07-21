      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0307.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA0307.CPv  $
      *
      *   Rev 2.6   05 Mar 2018 11:36:44   t13ex67
      *APA02481: Anzeige des Buchungsdatums im PG11 - MSS
      *
      *   Rev 2.5   Sep 26 2016 11:39:36   t13ex67
      *APA02280: Dialoganpassungen PG11
      *
      *   Rev 2.4   Jul 16 2015 15:07:10   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.3   Jun 19 2012 09:23:02   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:47:58   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 09:02:32   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Feb 27 1999 14:28:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 16:18:54   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   May 26 1997 15:22:38   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0307                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0307                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                        *
      *    DATUM:    03.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !03.06.97! 01 ! NEU                       ! SCHRIEBER*
      * APA00622!23.05.07!2.1 ! NEUES PO-FELD KNR-VGES    ! STORCH   *
      * APA01101!06.03.12!2.2 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01932!24.04.15!2.4 ! NEUES PO-FELD UST-BETRAG  ! WANG     *
      * APA02280!05.07.16!2.5 ! NEU: PI-SORTIERUNG        ! STORCH   *
      * APA02481!10.07.17!2.6 ! NEUES FELD PI/PO-BUCHDAT  ! STORCH   *
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
      *    FUNKTION DRPA0307 DES BUFFER-OZP ZYPA0002 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PA15, ANTRäGE NACH FEHLERCODE.
      *
      ****************************************************************
      *
       01  PI-DRPA0307.
           05  PI-DRPA0307-UFKT                PIC  9(02).
           05  PI-DRPA0307-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA0307-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA0307-AUSWAHL-KRITERIEN.
               10 PI-DRPA0307-PGEB-GES         PIC  9(02).
               10 PI-DRPA0307-S-PGEB-GES       PIC  X(01).
               10 PI-DRPA0307-DTA-ART          PIC  9(03).
               10 PI-DRPA0307-S-DTA-ART        PIC  X(01).
               10 PI-DRPA0307-VSTAT            PIC  X(04).
               10 PI-DRPA0307-S-VSTAT          PIC  X(01).
               10 PI-DRPA0307-UEBDAT-VON       PIC  X(10).
               10 PI-DRPA0307-UEBDAT-BIS       PIC  X(10).
               10 PI-DRPA0307-KNR-MANDANT      PIC  9(03).
               10 PI-DRPA0307-S-MANDANT        PIC  X(01).
050716         10 PI-DRPA0307-SORTIERUNG       PIC  9(01).
100717         10 PI-DRPA0307-BUCHDAT-VON      PIC  X(10).
100717         10 PI-DRPA0307-BUCHDAT-BIS      PIC  X(10).
100717         10 PI-DRPA0307-S-BUCHDAT        PIC  X(01).
      *
       01  PO-DRPA0307.
           05  PO-DRPA0307-RC                  PIC  X(03).
               88 PO-DRPA0307-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0307-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0307-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0307-ANZAHL-ERG-ZEILEN   PIC  9(05).

           05  PO-DRPA0307-ERGEBNISZEILE.
               10 PO-DRPA0307-DTA-ID           PIC  9(15).
               10 PO-DRPA0307-PGEB-GES         PIC  9(05).
               10 PO-DRPA0307-DTA-ART          PIC  9(03).
               10 PO-DRPA0307-VSTAT            PIC  X(04).
               10 PO-DRPA0307-ERSTELL-DAT      PIC  X(10).
               10 PO-DRPA0307-TRANSF-DAT       PIC  X(10).
               10 PO-DRPA0307-KONTR-WERT       PIC S9(09)V99
                                                     PACKED-DECIMAL.
               10 PO-DRPA0307-ANZ-BEWEGUNG     PIC S9(09)
                                                     USAGE IS BINARY.
               10 PO-DRPA0307-ANZ-FEHLER       PIC S9(09)
                                                     USAGE IS BINARY.
               10 PO-DRPA0307-DTA-ANZ-SAETZE   PIC S9(09)
                                                     USAGE IS BINARY.
               10 PO-DRPA0307-LAST-UPDATE      PIC  X(26).
               10 PO-DRPA0307-KNR-MANDANT      PIC  9(03).
240415         10 PO-DRPA0307-UST-BETRAG       PIC S9(09)V99
                                                     PACKED-DECIMAL.
100717         10 PO-DRPA0307-BUCH-DAT         PIC  X(10).
      *
      ******* ENDE COPY-MEMBER **************************************



