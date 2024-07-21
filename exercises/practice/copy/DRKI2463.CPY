      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.06.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2463.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2463.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 16:16:00   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-Adressen
      *
      *   Rev 2.0   Jun 10 2011 10:43:26   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2463                                     *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2463                   *
      *    ----------------------------------------------------------*
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2463
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI2463.
           05  PI-DRKI2463-DATEN.
             10 PI-DRKI2463-KNDPERSONENID           PIC  X(013).
             10 PI-DRKI2463-VERVERTRAGSID           PIC  X(013).
      *      aus Aktualisierungsdatei
      *      Vertragsrolle
      *      space, N, Z
             10 PI-DRKI2463-VERTRAG                 PIC  X(001).
      *      Tarifrolle
      *      space, P
             10 PI-DRKI2463-TARIF                   PIC  X(001).
      *      Zusätzliches Kennzeichen für versicherte Personen
      *      space, M, 2
             10 PI-DRKI2463-ARTKZ                   PIC  X(001).
      *      Laufende Nr. der Person in CKV-Verträgen
             10 PI-DRKI2463-LFDNR                   PIC  9(002).
             10 PI-DRKI2463-TAGESDATUM              PIC  X(010).

             10 PI-DRKI2463-GES                     PIC  9(002).
             10 PI-DRKI2463-VNR                     PIC  9(015).
             10 PI-DRKI2463-STR                     PIC  X(040).
             10 PI-DRKI2463-AEND-STR-KZ             PIC  X(001).
             10 PI-DRKI2463-PLZ                     PIC  X(009).
             10 PI-DRKI2463-AEND-PLZ-KZ             PIC  X(001).
             10 PI-DRKI2463-ORT                     PIC  X(040).
             10 PI-DRKI2463-AEND-ORT-KZ             PIC  X(001).
             10 PI-DRKI2463-AUSL-KZ                 PIC  X(003).
             10 PI-DRKI2463-AEND-AUSL-KZ            PIC  X(001).
             10 PI-DRKI2463-AKTUAL-KZ               PIC  X(001).

       01  PO-DRKI2463.
           05  PO-DRKI2463-RC                     PIC  X(003).
               88 PO-DRKI2463-RC-OK               VALUE 'OK '.
               88 PO-DRKI2463-RC-NOK              VALUE 'NOK'.
      *
           05  PO-DRKI2463-DATEN.
             10 PO-DRKI2463-ADRESSEID               PIC  X(013).
             10 PO-DRKI2463-ADRESSEID-INSERT        PIC S9(0004) COMP.
             10 PO-DRKI2463-ADRESSE-GESUCHT         PIC S9(0004) COMP.
             10 PO-DRKI2463-ADRESSE-GEFUNDEN        PIC S9(0004) COMP.
             10 PO-DRKI2463-ADRESSE-ANGENOMMEN      PIC S9(0004) COMP.
             10 PO-DRKI2463-HISTADRESSAENDERG       PIC S9(0004) COMP.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2463-FEHLERSATZ.
               10 PO-DRKI2463-FUNKTION           PIC  X(10).
               10 PO-DRKI2463-TABELLE            PIC  X(20).
               10 PO-DRKI2463-SQL-FEHLER         PIC  X(04).
               10 PO-DRKI2463-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2463-KEY.
                  15 PO-DRKI2463-GES             PIC  9(002).
                  15 PO-DRKI2463-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
