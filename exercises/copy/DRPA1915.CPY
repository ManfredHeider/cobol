      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  26.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1915.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1915.CPv  $
      *
      *   Rev 2.1   Jun 05 2019 11:41:44   u130076
      *APA02796: Neuer Dialog für Korrekturen PRO-RATA Faktoren
      *
      *   Rev 2.0   Mar 26 2019 11:13:06   u130076
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1915 FUNKTION SET RPRO-RATA-ZÄHLER PO27  *
      *                          FÜR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KÖPRÜCÜ                                         *
      *    DATUM:    26.03.2019                                      *
      ****************************************************************
      *    VERSION   2.00                                            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA02796!26.03.19!2.00 !NEU                       ! KÖPRÜCÜ  *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1915                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1915.
             10 PI-DRPA1915-PRINTE             PIC S9(15)
                                               PACKED-DECIMAL.
             10 PI-DRPA1915-PDPT-ID            PIC S9(15)
                                               PACKED-DECIMAL.
             10 PI-DRPA1915-PRO-RATA-FZ        PIC  9(04).
 
          05 PI-DRPA1915-AUSWAHL-KRITERIEN.
             10 PI-DRPA1915-PGEB-GES           PIC  9(02).
             10 PI-DRPA1915-VNR                PIC  X(15).

       01 PO-DRPA1915.
          05 PO-DRPA1915-RC                    PIC  X(03).
             88 PO-DRPA1915-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1915-RC-UNZULAESSIG              VALUE 'UNZ'.

          05 PO-DRPA1915-MELDUNG.
             10 PO-DRPA1915-MELDUNG-TYP        PIC  X(01).
             10 PO-DRPA1915-MELDUNG-NR         PIC S9(04) BINARY.
      *
          05 PO-DRPA1915-MAP-STEUERUNG.
             10 PO-DRPA1915-MAP-ST-PRO-RATA-FZ.
                20 FELD-INTENSIV               PIC  X(01).
                20 FELD-OFFEN                  PIC  X(01).
                20 FELD-CURSOR-KZ              PIC  X(01).
      ******** ENDE COPY-MEMBER **************************************
