      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1137.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1137.CPv  $
      *
      *   Rev 2.0   Mar 25 1999 14:10:08   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 13:59:40   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1137 FUNKTION GET URSPRUNGS-PE-STRUKTUR  *
      *                          FüR PRODUKTTEIL                     *
      *                          FüR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    30.11.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!30.11.98! 01  !NEU                       !BOHLINGER *
      *         !24.02.99!1.02 !PO-BEREICH ERWEITERT      ! BOHLINGER*
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1137                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1137.
             10 PI-DRPA1137-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1137-PVPT-ID            PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1137-PVPT-VERSION       PIC S9(004)
                                               BINARY.
       01 PO-DRPA1137.
          05 PO-DRPA1137-RC                    PIC  X(003).
             88 PO-DRPA1137-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1137-RC-NOT-FOUND                VALUE 'NFD'.
             88 PO-DRPA1137-RC-PE-S-NOT-EIND            VALUE 'PND'.
             88 PO-DRPA1137-RC-PE-S-NOT-ERST            VALUE 'PNE'.

          05 PO-DRPA1137-MELDUNG.
             10 PO-DRPA1137-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1137-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA1137-DATEN.
             10 PEST-ID                        PIC S9(015)
                                               PACKED-DECIMAL.
             10 PDPT-ID                        PIC S9(015)
                                               PACKED-DECIMAL.
      *
      ******** Ende Copy-Member **************************************
