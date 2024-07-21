      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1831.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1831.CPv  $
      *
      *   Rev 2.1   Oct 13 2015 14:17:50   T13EX67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      * PA04
      *
      *   Rev 2.0   Jul 14 2015 12:19:20   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1831                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1831                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    14.07.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!14.07.15!2.01! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1831
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1831 DES BUFFER-OZP ZYPA0041 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PA04, KORREKTUR STEUERLICHER STATUS.
      *
      ****************************************************************
      *
       01  PI-DRPA1831.
           05  PI-DRPA1831-UFKT                PIC  9(02).
           05  PI-DRPA1831-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA1831-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA1831-AUSWAHL-KRITERIEN.
               10 PI-DRPA1831-SEL-FUNKTION     PIC  9(02).
               10 PI-DRPA1831-VBNR             PIC  9(07).
               10 PI-DRPA1831-S-VBNR           PIC  X(01).
               10 PI-DRPA1831-ZIELSTATUS       PIC  9(01).
               10 PI-DRPA1831-S-ZIELSTATUS     PIC  X(01).
               10 PI-DRPA1831-KORR-JAHR        PIC  X(04).
               10 PI-DRPA1831-S-KORR-JAHR      PIC  X(01).
               10 PI-DRPA1831-AENDAT-VON       PIC  X(10).
               10 PI-DRPA1831-S-AENDAT-VON     PIC  X(01).
               10 PI-DRPA1831-AENDAT-BIS       PIC  X(10).
               10 PI-DRPA1831-S-AENDAT-BIS     PIC  X(01).
               10 PI-DRPA1831-INFO-VB-MM       PIC  9(02).
               10 PI-DRPA1831-INFO-VB-JJJJ     PIC  9(04).
               10 PI-DRPA1831-S-INFO-VB        PIC  X(01).
               10 PI-DRPA1831-KNR-PASU-ZUSTAND PIC  9(02).
               10 PI-DRPA1831-S-PASU-ZUSTAND   PIC  X(01).
      *
       01  PO-DRPA1831.
           05  PO-DRPA1831-RC                  PIC  X(03).
               88 PO-DRPA1831-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1831-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1831-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1831-ANZAHL-ERG-ZEILEN   PIC  9(05).

           05  PO-DRPA1831-ERGEBNISZEILE.
               10 PO-DRPA1831-VBNR             PIC  9(07).
               10 PO-DRPA1831-ZIELSTATUS       PIC  9(01).
               10 PO-DRPA1831-LFDNR            PIC  9(04).
               10 PO-DRPA1831-KORR-JAHR        PIC  9(04).
               10 PO-DRPA1831-AEN-DAT          PIC  X(10).
               10 PO-DRPA1831-AEN-DAT-TS       PIC  X(26).
               10 PO-DRPA1831-VERARB-ZUSTAND   PIC  9(01).
               10 PO-DRPA1831-LAST-UPDATE      PIC  X(26).
               10 PO-DRPA1831-KENNUNG          PIC  X(08).
      *
      ******* ENDE COPY-MEMBER **************************************

