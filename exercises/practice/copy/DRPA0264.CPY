      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0264.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0264.CPv  $
      *
      *   Rev 2.2   Mar 09 2000 15:57:00   T13EX22
      *APA00085: Verarbeitung von Fälligkeiten älter 2 Jahre
      *
      *   Rev 2.1   May 26 1999 12:27:08   T01JM35
      *APA00039 - CRPA0061
      *
      *   Rev 2.0   Feb 27 1999 14:28:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:34:22   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:27:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0264                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<FÄLLIGKEIT PRÜFEN   >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    02.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.06.97!    ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * afpa0012!18.05.98!    ! rc 'mfe' hinzu            ! parent   *
      *         !        !    !                           !          *
      * AFPA0012!        !    ! CRPA0018                  !          *
      *         !18.09.98!    !   - MELDUNGSFEHLER RAUS-  ! WOLFF    *
      *         !        !    !     GENOMMEN              !          *
      *         !        !    !                           !          *
      * APA00039!18.05.99!2.1 ! CRPA0061                  !PROEMPELER*
      *         !        !    !   - PRODUKT ergänzt       !          *
      * APA00088!01.03.00! 2.2! PGEB-GES ERGÄNZT          ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0264                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0264.
          05 PI-DRPA0264-PZVP-VERSION        PIC S9(004)  BINARY.
          05 PI-DRPA0264-DATEN.
             10 PI-DRPA0264-PMPP-PGV         PIC S9(004)  BINARY.
             10 PI-DRPA0264-PMPE-FAELLIG-VON PIC  X(010).
             10 PI-DRPA0264-PMPE-FAELLIG-BIS PIC  X(010).
             10 PI-DRPA0264-STICHTAG-VON     PIC  X(010).
             10 PI-DRPA0264-STICHTAG-BIS     PIC  X(010).
180599       10 PI-DRPA0264-PROD-PROD-SCHL   PIC S9(004)  BINARY.
010300       10 PI-DRPA0264-PGEB-GES         PIC S9(004)  BINARY.

      *
       01 PO-DRPA0264.
          05 PO-DRPA0264-RC                  PIC  X(003).
             88 PO-DRPA0264-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0264-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
      *
      ******* ENDE COPY-MEMBER **************************************
