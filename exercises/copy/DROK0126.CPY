      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.05.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0126.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0126.CPv  $
      *
      *   Rev 2.1   Jul 20 2012 10:26:14   t13ex14
      *AKI01135: Kunden- u. Vertragsdaten physisch löschen
      *
      *   Rev 2.0   May 14 2012 08:15:12   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01135!14.05.12!    ! Neu                       ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DROK0126
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01 PI-DROK0126.
           05  PI-DROK0126-PROGRAMM            PIC  X(008).
           05  PI-DROK0126-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DROK0126-DATEN.
             10 PI-DROK0126-HKUKUNDENID        PIC X(13).

      *
       01  PO-DROK0126.
           05  PO-DROK0126-RC                 PIC  X(003).
             88 PO-DROK0126-RC-OK                        VALUE 'OK '.
             88 PO-DROK0126-RC-LEER                      VALUE 'LER'.
             88 PO-DROK0126-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DROK0126-DATEN.
             10  PO-DROK0126-HKUID                 PIC  X(013).
             10  PO-DROK0126-HKUTYP                PIC S9(004) BINARY.
             10  PO-DROK0126-HKUERSTELLTVON        PIC  X(008).
             10  PO-DROK0126-HKUSTEUERART          PIC  X(001).
             10  PO-DROK0126-HKUVERTRAGID          PIC  X(013).
             10  PO-DROK0126-HKUVBNRALT            PIC S9(009) BINARY.
             10  PO-DROK0126-HKUVBNRNEU            PIC S9(009) BINARY.
      *
      **** ENDE COPY DROK0126 ****************************************
