      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1011.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1011.CPv  $
      *
      *   Rev 2.4   23 Aug 2007 09:53:30   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.3   03 Nov 2006 09:57:18   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Oct 12 1999 10:04:54   T13EX21
      *APA00052 - Erweiterung der Suchkriterien
      *
      *   Rev 2.1   May 27 1999 10:34:10   T01JM2E
      *NT-Umstellung
      *
      *   Rev 1.0   14 Jul 1998 13:58:44   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1011                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1011                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    10.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!10.07.98! 01 ! NEU                       ! D.WILHELM*
      * APA00052!24.08.99! 02 ! ERWEITERUNG PI-BEREICH    ! STORCH   *
      *         !        !    ! (NEUE SCHLÜSSELFELDER)    !          *
      * APA00452!02.11.06!2.3 ! ERW. WG. 3-STELL. PRODUKTE! WANG     *
      * APA00639!22.06.07!2.4 ! ERW. DER PIN AUF 9 STELLEN! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1011
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1011 DES BUFFER-OZP ZYPA0018 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PO16, AUSKUNFT VERTRÄGE ÜBER PIN
      *
      ****************************************************************
      *
       01  PI-DRPA1011.
           05  PI-DRPA1011-UFKT                    PIC  9(02).
           05  PI-DRPA1011-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA1011-LOG-SATZ-NR             PIC  9(005).

           05 PI-DRPA1011-AUSWAHL-KRITERIEN.
               10 PI-DRPA1011-PGEB-GES             PIC 9(002).
220607*        10 PI-DRPA1011-PIN                  PIC 9(007).
220607         10 PI-DRPA1011-PIN                  PIC 9(009).
240899         10 PI-DRPA1011-S-PIN                PIC X(001).
240899         10 PI-DRPA1011-AVERM                PIC 9(007).
240899         10 PI-DRPA1011-S-AVERM              PIC X(001).
240899         10 PI-DRPA1011-NAME                 PIC X(025).
240899         10 PI-DRPA1011-S-NAME               PIC X(001).
240899         10 PI-DRPA1011-VORNAME              PIC X(020).
240899         10 PI-DRPA1011-S-VORNAME            PIC X(001).
021106*        10 PI-DRPA1011-PRODUKT              PIC 9(002).
021106         10 PI-DRPA1011-PROD-PROD-SCHL       PIC S9(004) BINARY.
240899         10 PI-DRPA1011-S-PRODUKT            PIC X(001).
240899         10 PI-DRPA1011-ZEITRAUM-VON         PIC X(010).
240899         10 PI-DRPA1011-ZEITRAUM-BIS         PIC X(010).
240899         10 PI-DRPA1011-S-ZEITRAUM           PIC X(001).
      *
       01  PO-DRPA1011.
           05  PO-DRPA1011-RC                      PIC  X(03).
               88 PO-DRPA1011-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1011-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1011-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1011-ANZAHL-ERG-ZEILEN       PIC  9(05).
           05  PO-DRPA1011-LOG-SATZ-NR-1           PIC  9(05).
      *
           05  PO-DRPA1011-ERGEBNISZEILE.
               10 PO-DRPA1011-VNR                  PIC X(15).
220607*        10 PO-DRPA1011-PIN                  PIC 9(07).
220607         10 PO-DRPA1011-PIN                  PIC 9(09).
               10 PO-DRPA1011-NAME                 PIC X(25).
               10 PO-DRPA1011-VNAME                PIC X(20).
               10 PO-DRPA1011-AVERM                PIC 9(07) COMP.
               10 PO-DRPA1011-PRODSL OCCURS 3.
021106*           15 PO-DRPA1011-PRODSCHL    PIC S9(04) COMP.
021106            15 PO-DRPA1011-PROD-PROD-SCHL   PIC S9(04) BINARY.
               10 PO-DRPA1011-VERTR-BEG      PIC X(10).
               10 PO-DRPA1011-PRODSL-KZ      PIC X.
               10 PO-DRPA1011-PRINTE         PIC S9(015)V USAGE COMP-3.
220607*        10 FILLER                     PIC X(24).
220607         10 FILLER                     PIC X(22).
      *
      ******* ENDE COPY-MEMBER **************************************
