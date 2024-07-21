      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0873.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0873.CPv  $
      *
      *   Rev 2.1   Jun 09 2020 10:04:38   t13ex67
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.0   Mar 25 1999 14:09:52   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   14 Jan 1998 12:47:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0873 FUNKTION LIST VERTRAGSPRODUKTTEILE  *
      *                          für EVS-Vertrag für ugpa0001        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    23.11.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!23.11.98! 01 !NEU                        ! Bohlinger*
      * APA02908!28.05.20!2.1 !ERWEITERUNG UM VVNR-GES    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0873                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0873.
          05 PI-DRPA0873-UFKT                  PIC  9(02).
          05 PI-DRPA0873-PRINTE                PIC S9(015)
                                               PACKED-DECIMAL.
      *
       01 PO-DRPA0873.
          05 PO-DRPA0873-RC                    PIC  X(003).
             88 PO-DRPA0873-RC-OK                   VALUE 'OK '.
             88 PO-DRPA0873-RC-EOF                  VALUE 'EOF'.
             88 PO-DRPA0873-RC-LEER                 VALUE 'LER'.

          05 PO-DRPA0873-MELDUNG.
             10 PO-DRPA0873-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA0873-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA0873-DATEN.

             35 PVPT-ID                          PIC S9(015)
                                                 PACKED-DECIMAL.
             35 PVPT-VERSION                     PIC S9(004) BINARY.
             35 PROD-PROD-SCHL                   PIC S9(004) BINARY.
             35 PVPT-VT-PRIO                     PIC S9(004) BINARY.
             35 PVPT-LAUFZEIT                    PIC S9(004) BINARY.
             35 MIND-LAUFZEIT                    PIC S9(004) BINARY.
             35 KNR-PVPT-ART                     PIC S9(004) BINARY.
             35 KNR-PVPT-ART-BK                  PIC  X(004) .
             35 PVPT-PIN                         PIC S9(009) BINARY.
             35 PVPT-PPFL-BEGINN                 PIC  X(010).
             35 PVPT-BEWERT-BETRAG               PIC S9(009)V9(2)
                                                 PACKED-DECIMAL.
             35 PVPT-KNR-WAEHRUNG-BK             PIC  X(004).
             35 KNR-EVS-GRUND                    PIC S9(004) BINARY.
             35 PVPT-EE-ENTSCHEID                PIC  X(001).
             35 PE-STRUKTUR-KZ                   PIC  X(001).
             35 PEST-ID                          PIC S9(015)
                                                 PACKED-DECIMAL.
             35 PDPT-ID                          PIC S9(015)
                                                 PACKED-DECIMAL.
             35 PVPT-ID-VVNR                     PIC S9(015)
                                                 PACKED-DECIMAL.
280520       35 PVPT-ID-VVNR-GES                 PIC S9(004) BINARY.

