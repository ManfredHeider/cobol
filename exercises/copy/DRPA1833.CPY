      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1833.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1833.CPv  $
      *
      *   Rev 2.1   Oct 13 2015 14:18:08   T13EX67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      * PA04
      *
      *   Rev 2.0   Jul 14 2015 12:56:02   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1833                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1833 DES BUFFER-OZP'S  *
      *    ZYPA0041                                                  *
      *    ( ERGEBNISMENGE X BEREITSTELLEN)                          *
      *    --------------------------------------------------------  *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    15.07.2015                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!15.07.15!2.1 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1833
      *    AUSSER: DEM FUNKTIONSNAMEN ( IN DRPA1832 ),
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1833 DES BUFFER-OZP ZYPA0041 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PA04, KORREKTUR STEUERLICHER STATUS
      *
      ****************************************************************
      *
       01  PI-DRPA1833.
           05  PI-DRPA1833-UFKT                PIC  9(02).
           05  PI-DRPA1833-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA1833-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA1833-AUSWAHL-KRITERIEN.
               10 PI-DRPA1833-SEL-FUNKTION     PIC  9(02).
               10 PI-DRPA1833-VBNR             PIC  9(07).
               10 PI-DRPA1833-S-VBNR           PIC  X(01).
               10 PI-DRPA1833-ZIELSTATUS       PIC  9(01).
               10 PI-DRPA1833-S-ZIELSTATUS     PIC  X(01).
               10 PI-DRPA1833-KORR-JAHR        PIC  9(04).
               10 PI-DRPA1833-S-KORR-JAHR      PIC  X(01).
               10 PI-DRPA1833-AENDAT-VON       PIC  X(10).
               10 PI-DRPA1833-S-AENDAT-VON     PIC  X(01).
               10 PI-DRPA1833-AENDAT-BIS       PIC  X(10).
               10 PI-DRPA1833-S-AENDAT-BIS     PIC  X(01).
               10 PI-DRPA1833-INFO-VB-MM       PIC  9(02).
               10 PI-DRPA1833-INFO-VB-JJJJ     PIC  9(04).
               10 PI-DRPA1833-S-INFO-VB        PIC  X(01).
               10 PI-DRPA1833-KNR-PASU-ZUSTAND PIC  9(02).
               10 PI-DRPA1833-S-PASU-ZUSTAND   PIC  X(01).
      *
       01  PO-DRPA1833.
           05  PO-DRPA1833-RC                  PIC  X(03).
               88 PO-DRPA1833-RC-OK                 VALUE 'OK '.
               88 PO-DRPA1833-RC-EOF                VALUE 'EOF'.
               88 PO-DRPA1833-RC-LEER               VALUE 'LER'.
           05  PO-DRPA1833-ANZAHL-ERG-ZEILEN   PIC  9(05).

           05  PO-DRPA1833-ERGEBNISZEILE.
               10 PO-DRPA1833-VBNR             PIC  9(07).
               10 PO-DRPA1833-ZIELSTATUS       PIC  9(01).
               10 PO-DRPA1833-LFDNR            PIC  9(04).
               10 PO-DRPA1833-KORR-JAHR        PIC  9(04).
               10 PO-DRPA1833-AEN-DAT          PIC  X(10).
               10 PO-DRPA1833-AEN-DAT-TS       PIC  X(26).
               10 PO-DRPA1833-VERARB-ZUSTAND   PIC  9(01).
               10 PO-DRPA1833-LAST-UPDATE      PIC  X(26).
               10 PO-DRPA1833-KENNUNG          PIC  X(08).
      *
      *
      ******* ENDE COPY-MEMBER **************************************

