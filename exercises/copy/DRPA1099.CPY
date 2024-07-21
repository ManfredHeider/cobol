      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1099.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1099.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:48:44   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   20 Jul 1998 10:21:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1099 FUNKTION SET URSPRUNGS-PE-STRUKTUR  *
      *                          FüR VERTRAGSDATENMODUL   UGPA0001   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    13.10.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!13.10.98! 01 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1099                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1099.
             10 PI-DRPA1099-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1099-PEST-ID            PIC S9(015)
                                               PACKED-DECIMAL.

       01 PO-DRPA1099.
          05 PO-DRPA1099-RC                    PIC  X(003).
             88 PO-DRPA1099-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1099-RC-UNZULAESSIG              VALUE 'UNZ'.

          05 PO-DRPA1099-MELDUNG.
             10 PO-DRPA1099-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1099-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA1099-VSTATUS               PIC  X(0004).
      *
