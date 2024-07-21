      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1144.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1144.CPv  $
      *
      *   Rev 2.0   Mar 25 1999 14:10:10   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:00:20   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1144 FUNKTION SET URSPRUNGS-PE-STRUKTUR  *
      *                          FüR PRODUKTTEIL                     *
      *                          FüR PROVISIONSDATENMODUL UGPA0001   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    07.12.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!07.12.98! 01  !NEU                       !BOHLINGER *
      *         !26.02.99!1.02 !PO-BEREICH ERWEITERT      !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1144                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1144.
             10 PI-DRPA1144-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1144-PVPT-ID            PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1144-PVPT-VERSION       PIC S9(004)
                                               BINARY.
             10 PI-DRPA1144-PEST-ID            PIC S9(015)
                                               PACKED-DECIMAL.
260299       10 PI-DRPA1144-KNR-BUCHUNGS-GRUND PIC S9(004) BINARY.

       01 PO-DRPA1144.
          05 PO-DRPA1144-RC                    PIC  X(003).
             88 PO-DRPA1144-RC-OK                       VALUE 'OK '.

          05 PO-DRPA1144-DATEN.
             10 PO-DRPA1144-PE-STRUKTUR-KZ     PIC  X(001).
      *
      ******** Ende Copy-Member **************************************
