      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1305.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1305.CPv  $
      *
      *   Rev 2.4   Sep 26 2016 13:57:36   t13ae39
      *APA02280: Dialoganpassungen SF43
      *
      *   Rev 2.3   07 Nov 2006 15:20:24   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Mar 27 2003 11:01:10   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.1   Jan 04 2000 13:11:46   T13AE18
      *APA00068
      *
      *
      *   Rev 2.0   Sep 28 1999 14:06:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1305                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1305                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    18.19.1999                                      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA000xx!12.05.98! 01 ! NEU                       ! BOHLINGER*
      * APA00225!21.03.03!2.01! ERWEITERUNG PG-TARIF      ! STORCH   *
      * APA00452!07.11.06!2.03! ERW. WG. 3-STELL. PRODUKTE! WANG     *
      * APA02280!04.07.16!2.21! Key-Auswahl SF43 ergänzt: ! Strauss  *
      *         !        !    ! gültig-ab und gültig-bis  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1305
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1305 DES BUFFER-OZP ZYPA0032 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR DIALOG SF42 'PRODUKT-TARIFZUORDNUNG'
      *
      ****************************************************************
      *
       01  PI-DRPA1305.
           05  PI-DRPA1305-UFKT                    PIC  9(002).
           05  PI-DRPA1305-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA1305-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRPA1305-AUSWAHL-KRITERIEN.
               10 PI-DRPA1305-PGEB-GES             PIC 9(002).
071106*        10 PI-DRPA1305-PROD-SCHL            PIC 9(002).
071106         10 PI-DRPA1305-PROD-PROD-SCHL       PIC S9(004) BINARY.
               10 PI-DRPA1305-S-PROD-SCHL          PIC X(001).
210303*        10 PI-DRPA1305-PGTA-TARIF           PIC X(012).
210303         10 PI-DRPA1305-PGTA-TARIF           PIC X(015).
               10 PI-DRPA1305-S-PGTA-TARIF         PIC X(001).
               10 PI-DRPA1305-S-SORT-PROD-SCHL     PIC X(001).
040716         10 PI-DRPA1305-GUELTIG-AB           PIC X(010).
040716         10 PI-DRPA1305-S-GUELTIG-AB         PIC X(010).
040716         10 PI-DRPA1305-GUELTIG-BIS          PIC X(010).
040716         10 PI-DRPA1305-S-GUELTIG-BIS        PIC X(010).
      *
       01  PO-DRPA1305.
           05  PO-DRPA1305-RC                      PIC  X(03).
               88 PO-DRPA1305-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1305-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1305-RC-LEER                      VALUE 'LER'.

           05  PO-DRPA1305-ANZAHL-ERG-ZEILEN       PIC  9(05).

           05  PO-DRPA1305-ERGEBNISZEILE.
               COPY DRPA0159.
      *
      ******* ENDE COPY-MEMBER **************************************

