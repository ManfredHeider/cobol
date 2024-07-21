      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1145.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1145.CPv  $
      *
      *   Rev 2.0   Mar 25 1999 14:10:12   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:00:28   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1145 FUNKTION 'PE-STRUKTUR BEREINIGEN    *
      *                          FüR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    01.03.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!01.03.98! 01  !NEU                       !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1145                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1145.
             10 PI-DRPA1145-PEST-ID            PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1145-PDPT-ID            PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1145-EE-ENTSCHEIDUNG    PIC  X(001).
             10 PI-DRPA1145-S-EE-ENTSCHEIDUNG  PIC  X(001).

       01 PO-DRPA1145.
          05 PO-DRPA1145-RC                    PIC  X(003).
             88 PO-DRPA1145-RC-OK-KEINE-AEND            VALUE 'OKK'.
             88 PO-DRPA1145-RC-OK-PE-S-BEREIN           VALUE 'OKB'.
      *
          05 PO-DRPA1145-DATEN.
             10 PO-DRPA1145-PEST-ID            PIC S9(015)
                                               PACKED-DECIMAL.
      ******** Ende Copy-Member **************************************
