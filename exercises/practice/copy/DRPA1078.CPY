      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1078.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1078.CPv  $
      *
      *   Rev 2.0   May 25 1999 13:58:50   T13AE18
      *apa00012
      *
      *   Rev 1.0   20 Jul 1998 10:20:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1078 FUNKTION LIST PROVISIONSDETAIL      *
      *                                   PROV.-BESTAND  BILD1 + 2   *
      *                          FüR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    30.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!30.09.98! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1078                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1078.
          05 PI-DRPA1078-UFKT                  PIC  9(02).
          05 PI-DRPA1078-PRINTE                PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA1078-PDPT-ID               PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA1078-AUSWAHL-KRITERIEN.
             10 PI-DRPA1078-PGEB-GES           PIC 9(002).
             10 PI-DRPA1078-VNR                PIC X(015).

      *
       01 PO-DRPA1078.
          05 PO-DRPA1078-RC                    PIC  X(003).
             88 PO-DRPA1078-RC-OK                   VALUE 'OK '.
             88 PO-DRPA1078-RC-EOF                  VALUE 'EOF'.
             88 PO-DRPA1078-RC-LEER                 VALUE 'LER'.

          05 PO-DRPA1078-ERGEBNISZEILE.
             COPY DRPA0903.
      *
      ******** Ende Copy-Member **************************************
