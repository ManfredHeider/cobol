      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1092.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1092.cpv  $
      *
      *   Rev 2.0   Apr 07 1999 09:51:28   T13AE18
      *APA00012 Erweiterungen
      *
      *   Rev 1.0   20 Jul 1998 10:21:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1092 FUNKTION GET URSPRUNGS-PE-STRUKTUR *
      *                          FüR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    07.10.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!07.10.98! 01  !NEU                       !BOHLINGER *
      *         !01.04.99! 01  !PDPT-ID IM PO-BEREICH     !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1092                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1092.
             10 PI-DRPA1092-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
       01 PO-DRPA1092.
          05 PO-DRPA1092-RC                    PIC  X(003).
             88 PO-DRPA1092-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1092-RC-NOT-FOUND                VALUE 'NFD'.
             88 PO-DRPA1092-RC-PE-S-NOT-EIND            VALUE 'PND'.
             88 PO-DRPA1092-RC-PE-S-NOT-ERST            VALUE 'PNE'.

          05 PO-DRPA1092-MELDUNG.
             10 PO-DRPA1092-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1092-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA1092-DATEN.
             10 PEST-ID                        PIC S9(015)
                                               PACKED-DECIMAL.
             10 PDPT-ID                        PIC S9(015)
                                               PACKED-DECIMAL.
             10 S-PDPT-ID                      PIC  X(001).
      *
      ******** Ende Copy-Member **************************************
