      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1129.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1129.CPv  $
      *
      *   Rev 2.1   13 Feb 2001 09:02:48   T13EX49
      *AKI00144 - Rückstufungsdatum
      *
      *   Rev 2.0   07 Jun 2000 17:08:20   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    ERSTELLT: Peter Pohl                                      *
      *    DATUM:    25.09.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00144!25.09.00! 01 ! NEUERSTELLUNG             ! P. Pohl  *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1129                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE     <<<         AUSSENDIENSTMITARBEITER       *
      *    Rückstufungsdatum           (AUSSEMDIENSTMA) "ADMA"       *
      ****************************************************************
      *
       01 PI-DRKI1129.
          05 PI-DRKI1129-ADMAKEY.
             10 PI-DRKI1129-ADMAPERSONENID        PIC X(013).
             10 PI-DRKI1129-ADMAPERSONENROLLE     PIC X(001).
          05 PI-DRKI1129-ADMADATEN.
             10 PI-DRKI1129-NI-RUECKSTUFDATUM     PIC S9(4)  COMP.
             10 PI-DRKI1129-ADMARUECKSTUFDATUM    PIC X(10).
      *
       01 PO-DRKI1129.
          05 PO-DRKI1129-RC                       PIC  X(003).
             88 PO-DRKI1129-RC-OK                         VALUE 'OK '.
             88 PO-DRKI1129-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRKI1129-RC-NOT-FOUND                  VALUE 'NFD'.

