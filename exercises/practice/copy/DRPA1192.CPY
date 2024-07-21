      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1192.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1192.CPv  $
      *
      *   Rev 2.0   Mar 25 1999 14:10:44   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:04:58   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1192 FUNKTION SET URSPRUNGS-PE-STRUKTUR  *
      *                          FüR PRODUKTTEIL                     *
      *                          FüR PROVISIONSDATENMODUL UGPA0003   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    10.03.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!10.03.99! 01  !NEU                       !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1192                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1192.
             10 PI-DRPA1192-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1192-PVPT-ID            PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1192-PVPT-VERSION       PIC S9(004)
                                               BINARY.
             10 PI-DRPA1192-PEST-ID            PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1192-KNR-BUCHUNGS-GRUND PIC S9(004) BINARY.

       01 PO-DRPA1192.
          05 PO-DRPA1192-RC                    PIC  X(003).
             88 PO-DRPA1192-RC-OK                       VALUE 'OK '.

          05 PO-DRPA1192-DATEN.
             10 PO-DRPA1192-PE-STRUKTUR-KZ     PIC  X(001).
      *
      ******** Ende Copy-Member **************************************
