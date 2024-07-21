      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0744.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0744.CPv  $
      *
      *   Rev 2.3   Jun 09 2020 11:27:40   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.2   15 Nov 1999 16:20:12   T01JM2S
      *APA00065 - EINFÜGEN PVPT-KOMPLETT IN DRPA0744
      *
      *   Rev 2.1   05 Nov 1999 13:58:16   T01JM2S
      *APA00065 - VERTRAGSSTRUKTUR VON EVS-PRODUKTEN VERDICHTEN
      *
      *   Rev 2.0   Feb 27 1999 14:29:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:44:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0744                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PVER MIT VERD. PVPT >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    02.11.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00065!02.11.99!2.1 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00065!15.11.99!2.2 ! AUSGABEBEREICH            ! WOLFF    *
      *         !        !    ! PVPT-KOMPLETT AUFGENOMMEN !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0744                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0744.
          05 PI-DRPA0744-DATEN.
             10 PI-DRPA0744-PIN                     PIC S9(009) BINARY.
             10 PI-DRPA0744-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0744-VNR                     PIC  X(015).
             10 PI-DRPA0744-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA0744-FAELLIG-VON             PIC  X(010).
             10 PI-DRPA0744-BASIS-GES               PIC S9(004) BINARY.
      *
       01 PO-DRPA0744.
          05 PO-DRPA0744-RC                         PIC  X(003).
             88 PO-DRPA0744-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0744-RC-NOT-FOUND                  VALUE 'NFD'.
             88 PO-DRPA0744-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA0744-RC-SEL-KRIT-NOK               VALUE 'SKN'.
             88 PO-DRPA0744-RC-VSTAT-MIT-FEHL             VALUE 'VMF'.
             88 PO-DRPA0744-RC-VSTAT-OHNE-FEHL            VALUE 'VOF'.
          05 PO-DRPA0744-DATEN.
             10 PO-DRPA0744-PVER.
                COPY DRPA0408.
             10 PO-DRPA0744-PVPT.
                COPY DRPA0420.
             10 PO-DRPA0744-PVPT-KOMPLETT.
                COPY DRPA0420.
      *
      ******* ENDE COPY-MEMBER **************************************

