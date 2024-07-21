      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1637.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1637.CPv  $
      *
      *   Rev 2.1   Nov 03 2004 11:54:48   t13ex25
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.0   Oct 19 2004 17:33:20   t13ex25
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1637                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1637 DES BUFFER-OZP'S  *
      *    ZYPA0014                                                  *
      *    ( ERGEBNISMENGE X BEREITSTELLEN ,                         *
      *      BISHER BENUTZT VON PGPA0031 IM DIALOG PO12              *
      *      PROVISIONSBESTAND - AUSKUNFT VERTRAGSPRODUKTE )         *
      *    --------------------------------------------------------  *
      *    ERSTELLT: KUEKENHOEHNER                                   *
      *    DATUM:    12.06.1998                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!12.06.98!1.1 ! NEU                       ! KUEKEN   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1637
      *    AUSSER: DEM FUNKTIONSNAMEN ( IN DRPA0966 ),
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1637 DES BUFFER-OZP ZYPA0014 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PO12, AUSKUNFT VERTRAGSPRODUKTE
      *
      ****************************************************************

       01  PI-DRPA1637.
           05  PI-DRPA1637-UFKT                    PIC  9(002).
           05  PI-DRPA1637-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA1637-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRPA1637-PRINTE                  PIC S9(015)
                                                   PACKED-DECIMAL.
           05  PI-DRPA1637-AUSWAHL-KRITERIEN.
               10 PI-DRPA1637-PGEB-GES             PIC 9(002).
               10 PI-DRPA1637-VNR                  PIC X(015).

       01  PO-DRPA1637.
           05  PO-DRPA1637-RC                      PIC  X(03).
               88 PO-DRPA1637-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1637-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1637-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1637-ANZAHL-ERG-ZEILEN       PIC  9(05).
           05  PO-DRPA1637-LOG-SATZ-NR-1           PIC  9(05).

           05  PO-DRPA1637-ERGEBNISZEILE.
               10 PVKB-GUELTIG-AB          PIC  X(026).
               10 PVKB-KD-BETREUER         PIC S9(009) COMP.
               10 PVKB-F-GUELTIG-AB        PIC  X(010).
               10 PVKB-DTA-KZ              PIC  X.
               10 PVKB-KENNUNG             PIC  X(008).
               10 PVKB-LAST-UPDATE         PIC  X(026).
      *
      ******* ENDE COPY-MEMBER **************************************

