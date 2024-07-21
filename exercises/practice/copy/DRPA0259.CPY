      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0259.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0259.CPv  $
      *
      *   Rev 2.2   Jun 09 2020 11:25:02   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.1   Jun 10 2008 10:29:58   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.0   Feb 27 1999 14:28:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:32:08   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:24:52   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0259                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PM PRÜFEN/NORM.     >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    05.06.97                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!05.06.97! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!18.05.98! 01 ! MFE EINGEBAUT             ! PARENT   *
      * AFPA0012!        !    ! CRPA0018                  !          *
      *         !18.09.98!    !   - MELDUNGSFEHLER RAUS-  ! WOLFF    *
      *         !        !    !     GENOMMEN              !          *
      * APA00712!08.04.08!2.01! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0259                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0259.
          05 PI-DRPA0259-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA0259-DATEN.
             10 PI-DRPA0259-AMV.
                COPY DRPA0295.
          05 PI-DRPA0259-KNR-MANDANT                PIC S9(004) BINARY.
      *
       01 PO-DRPA0259.
          05 PO-DRPA0259-RC                         PIC  X(003).
             88 PO-DRPA0259-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0259-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
      *
      ******* ENDE COPY-MEMBER **************************************

