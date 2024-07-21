      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1110.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1110.CPv  $
      *
      *   Rev 2.0   May 25 1999 14:00:44   T13AE18
      *apa00012
      *
      *   Rev 1.0   21 Oct 1998 13:57:12   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1110 FUNKTION VM IN PE-URSPRUNGS-STRUKTUR*
      *                          EINFüGEN                            *
      *                          FüR DEN VERTRAG                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    28.10.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!28.10.98! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1110                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1110.
             10 PI-DRPA1110-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1110-VM                 PIC S9(009) BINARY.


       01 PO-DRPA1110.
          05 PO-DRPA1110-RC                    PIC  X(003).
             88 PO-DRPA1110-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1110-RC-UMB-UNZULAESSIG          VALUE 'UUZ'.

          05 PO-DRPA1110-MELDUNG.
             10 PO-DRPA1110-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1110-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA1110-DATEN.
             10 PO-DRPA1110-PEST-ID            PIC S9(015)
                                               PACKED-DECIMAL.

      *
      ******** Ende Copy-Member **************************************
