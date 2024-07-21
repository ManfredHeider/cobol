      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0967.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0967.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:25:06   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:19:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0967                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0967 DES BUFFER-OZP'S  *
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
      *    FÜR DIE FUNKTION DRPA0967
      *    AUSSER: DEM FUNKTIONSNAMEN ( IN DRPA0966 ),
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0967 DES BUFFER-OZP ZYPA0014 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PO12, AUSKUNFT VERTRAGSPRODUKTE
      *
      ****************************************************************

       01  PI-DRPA0967.
           05  PI-DRPA0967-UFKT                    PIC  9(002).
           05  PI-DRPA0967-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA0967-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRPA0967-PRINTE                  PIC S9(015)
                                                   PACKED-DECIMAL.
           05  PI-DRPA0967-AUSWAHL-KRITERIEN.
               10 PI-DRPA0967-PGEB-GES             PIC 9(002).
               10 PI-DRPA0967-VNR                  PIC X(015).

       01  PO-DRPA0967.
           05  PO-DRPA0967-RC                      PIC  X(03).
               88 PO-DRPA0967-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0967-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0967-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0967-ANZAHL-ERG-ZEILEN       PIC  9(05).
           05  PO-DRPA0967-LOG-SATZ-NR-1           PIC  9(05).

           05  PO-DRPA0967-ERGEBNISZEILE.
               COPY DRPA0971.
      *
      ******* ENDE COPY-MEMBER **************************************
