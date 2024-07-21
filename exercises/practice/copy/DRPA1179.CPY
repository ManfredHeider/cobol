      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1179.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1179.CPv  $
      *
      *   Rev 2.0   Mar 25 1999 14:10:40   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.1   Mar 25 1999 12:16:14   T13AE18
      * 
      *
      *   Rev 1.0   21 Oct 1998 14:03:48   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1179 FACHFUNKTION FKPAW0069              *
      *                 URSPRUNGS PE-STRUKTUREN DER VERTRAGSSTRUKTUR *
      *                 AKTUALISIEREN FüR UPPA0051                   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    02.03.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!22.02.99! 01  !NEU                       !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1179                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1179.
          05 PI-DRPA1179-EVS-VERTRAGSSTRUK.
             10 PI-DRPA1179-MAX-USED-PVPT-EVS      PIC S9(004) BINARY.
             10 PI-DRPA1179-PVPT-EVS               OCCURS 40.
                15 PVPT-ID                         PIC S9(015)
                                                   PACKED-DECIMAL.
                15 KNR-PVPT-ART                    PIC S9(004)  BINARY.
                15 PGEB-GES                        PIC S9(004)  BINARY.
                15 PROD-PROD-SCHL                  PIC S9(004)  BINARY.
                15 PVPT-EE-ENTSCHEID               PIC  X(001).
                15 PVPT-ID-VVNR                    PIC S9(015)
                                                   PACKED-DECIMAL.
                15 PVPT-PIN                        PIC S9(009)  BINARY.

          05 PI-DRPA1179-VVNR-VERTRAGSSTRUK.
             10 PI-DRPA1179-MAX-USED-PVPT-VVNR     PIC S9(004) BINARY.
             10 PI-DRPA1179-PVPT-VVNR              OCCURS 400.
                15 PVPT-ID                         PIC S9(015)
                                                   PACKED-DECIMAL.
                15 PGEB-GES                        PIC S9(004)  BINARY.
                15 PROD-PROD-SCHL                  PIC S9(004)  BINARY.
                15 PVPT-PIN                        PIC S9(009)  BINARY.
                15 PEST-ID                         PIC S9(015)
                                                   PACKED-DECIMAL.

          05 PI-DRPA1179-EVS-AUSGANGSDATEN.
             10 PI-DRPA1179-AUSGANGS-PEST-ID       PIC S9(015)
                                                   PACKED-DECIMAL.
             10 PI-DRPA1179-S-AUSGANGS-PEST-ID     PIC  X(001).
             10 PI-DRPA1179-PIN                    PIC S9(009)  BINARY.

             10 PI-DRPA1179-MAX-USED-EE-ENT        PIC S9(004) BINARY.
             10 PI-DRPA1179-EE-ENT-TAB             OCCURS 9.
                15 EE-ENTSCHEID                    PIC  X(001).
                15 PGEB-GES                        PIC S9(004)  BINARY.
                15 PROD-PROD-SCHL                  PIC S9(004)  BINARY.


       01 PO-DRPA1179.
          05 PO-DRPA1179-RC                        PIC  X(003).
             88 PO-DRPA1179-RC-OK                  VALUE 'OK '.

          05 PO-DRPA1179-DATEN.
             10 PO-DRPA1179-MAX-USED-PVPT-EVS      PIC S9(004) BINARY.
             10 PO-DRPA1179-PVPT-EVS               OCCURS 40.
                15 PVPT-ID                         PIC S9(015)
                                                   PACKED-DECIMAL.
                15 PVPT-EE-ENTSCHEID               PIC  X(001).
                15 PVPT-PIN                        PIC S9(009)  BINARY.
                15 URSPRUNGS-PEST-ID               PIC S9(015)
                                                   PACKED-DECIMAL.
                15 S-URSPRUNGS-PEST-ID             PIC  X(001).
      ******** Ende Copy-Member **************************************
