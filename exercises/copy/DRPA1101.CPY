      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1101.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa1101.cpv  $
      *
      *   Rev 2.0   May 27 1999 14:18:14   T01JM2S
      *APA00042
      *
      *   Rev 1.0   21 Oct 1998 13:56:16   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1101                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<BUFFER PK12/PGPA0040>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    06.10.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!06.10.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1101
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1101 DES BUFFER-OZP ZYPA0025 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PK12
      *
      ****************************************************************
      *
       01  PI-DRPA1101.
           05  PI-DRPA1101-UFKT                     PIC  9(002).
           05  PI-DRPA1101-NAME-ZW-SPEICHER         PIC  X(008).
           05  PI-DRPA1101-LOG-SATZ-NR              PIC  9(005).
           05  PI-DRPA1101-AUSWAHL-KRITERIEN.
               10 PI-DRPA1101-PDTA-ID               PIC  9(015).
               10 PI-DRPA1101-WAEHRUNG              PIC  9(002).
               10 PI-DRPA1101-STICHTAG              PIC  X(010).
      *
       01  PO-DRPA1101.
           05  PO-DRPA1101-RC                       PIC  X(003).
               88 PO-DRPA1101-RC-OK                       VALUE 'OK '.
               88 PO-DRPA1101-RC-EOF                      VALUE 'EOF'.
               88 PO-DRPA1101-RC-LEER                     VALUE 'LER'.
           05  PO-DRPA1101-ANZAHL-ERG-ZEILEN        PIC  9(005).

           05  PO-DRPA1101-ERGEBNISZEILE.
               10 PO-DRPA1101-KOF-VNEU.
                  COPY DRPA0824.
               10 PO-DRPA1101-KOF-NNEU.
                  COPY DRPA0824.
      *
      ******* ENDE COPY-MEMBER **************************************
