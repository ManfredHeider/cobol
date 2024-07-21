      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0439.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0439.CPv  $
      *
      *   Rev 2.1   16 May 2007 06:43:00   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.0   May 27 1999 13:50:02   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:37:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0439                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PEST/PEBD ERSTELLEN >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    24.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!24.10.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * apa00012!28.09.98! 1.1! Struktur-Datum hinzu      ! Parent   *
      * APA00604!12.04.07! 2.1! ABSCHALTUNG PEW           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0439                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0439.
          05 PI-DRPA0439-DATEN.
120407       10 FILLER                 PIC X.
120407*      10 PI-DRPA0439-PEW-STRUK-DAT           PIC X(010).
120407*      10 PI-DRPA0439-PEW-STRUKTUR.
120407*         COPY DRPA0435.
      *
       01 PO-DRPA0439.
          05 PO-DRPA0439-RC                         PIC  X(003).
             88 PO-DRPA0439-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0439-DATEN.
             10 PO-DRPA0439-PEST-PEBD.
                COPY DRPA0433.
      *
      ******* ENDE COPY-MEMBER **************************************
