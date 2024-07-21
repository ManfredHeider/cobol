      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1136.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1136.CPv  $
      *
      *   Rev 2.0   Mar 25 1999 14:10:06   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 13:59:36   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1136 FUNKTION LIST VERTRAGSPRODUKTTEILE  *
      *                          für EVS-Vertrag                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    23.11.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!23.11.98! 01 !NEU                        ! Bohlinger*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1136                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1136.
          05 PI-DRPA1136-UFKT                  PIC  9(02).
          05 PI-DRPA1136-PRINTE                PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA1136-AUSWAHL-KRITERIEN.
             10 PI-DRPA1136-PGEB-GES           PIC 9(002).
             10 PI-DRPA1136-VNR                PIC X(015).
      *
       01 PO-DRPA1136.
          05 PO-DRPA1136-RC                    PIC  X(003).
             88 PO-DRPA1136-RC-OK                   VALUE 'OK '.
             88 PO-DRPA1136-RC-EOF                  VALUE 'EOF'.
             88 PO-DRPA1136-RC-LEER                 VALUE 'LER'.

          05 PO-DRPA1136-ERGEBNISZEILE.
             COPY DRPA1140.
