      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1286.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1286.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:29:24   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:54:08   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Feb 20 2008 12:20:06   t13ex21
      *APA00622: Allfinanz Stufe 1 im abgerechneten Geschäft
      *
      *   Rev 2.1   Jul 27 1999 10:21:50   T13AE18
      *APA00012
      *
      *   Rev 2.0   Mar 30 1999 08:44:24   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1286                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1286                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    16.07.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!16.07.99! 01 ! NEU                       ! BOHLINGER*
      * APA00622!12.02.08!2.2 ! ERWEITERUNG UM VERTRIEBSG.! STORCH   *
      * APA01101!15.03.12!2.3 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1286
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1286 DES BUFFER-OZP ZYPA0031 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PK14, ANZEIGE UMBUCHUNGEN
      *
      ****************************************************************
      *
       01  PI-DRPA1286.
           05  PI-DRPA1286-UFKT                    PIC  9(002).
           05  PI-DRPA1286-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA1286-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRPA1286-AUSWAHL-KRITERIEN.
               10 PI-DRPA1286-UMBUCH-DAT-VON       PIC X(010).
               10 PI-DRPA1286-UMBUCH-DAT-BIS       PIC X(010).
               10 PI-DRPA1286-PGEB-GES             PIC 9(002).
               10 PI-DRPA1286-S-PGEB-GES           PIC X(001).
               10 PI-DRPA1286-VNR                  PIC X(015).
               10 PI-DRPA1286-S-VNR                PIC X(001).
               10 PI-DRPA1286-BUCH-GRUND-GRP       PIC 9(002).
               10 PI-DRPA1286-S-BUCH-GRUND-GRP     PIC X(001).
               10 PI-DRPA1286-UMBUCH-KENNUNG       PIC X(008).
               10 PI-DRPA1286-S-UMBUCH-KENNUNG     PIC X(001).
               10 PI-DRPA1286-MANDANT              PIC 9(002).
               10 PI-DRPA1286-S-MANDANT            PIC X(001).
      *
       01  PO-DRPA1286.
           05  PO-DRPA1286-RC                      PIC X(03).
               88 PO-DRPA1286-RC-OK                    VALUE 'OK '.
               88 PO-DRPA1286-RC-EOF                   VALUE 'EOF'.
               88 PO-DRPA1286-RC-LEER                  VALUE 'LER'.
           05  PO-DRPA1286-ANZAHL-ERG-ZEILEN       PIC 9(05).
           05  PO-DRPA1286-LOG-SATZ-NR-1           PIC 9(05).
      ***   KOMPRIMIERUNG = 'J', WENN ES SICH UM EINEN HAUPTEINTRAG
      ***   HANDELT, DESSEN UNTEREINTRäGE KOMPRIMIERT SIND.
           05  PO-DRPA1286-KZ-KOMPRIMIERUNG        PIC X(01).

           05  PO-DRPA1286-ERGEBNISZEILE.
               COPY DRPA1289.
      *
      ******* ENDE COPY-MEMBER **************************************
