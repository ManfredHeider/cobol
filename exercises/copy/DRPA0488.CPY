      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0488.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRPA0488.CPv  $
      *
      *   Rev 2.9   Jun 22 2018 09:10:38   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.8   Dec 22 2015 10:35:08   t13ex44
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.7   Jan 10 2014 12:39:56   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.6   Jun 19 2012 09:23:58   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.5   May 21 2012 08:49:02   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.4   Dec 10 2007 09:36:48   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.3   Nov 02 2006 14:50:48   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Aug 12 2003 10:38:10   t13ex21
      *APA00245 - Erweiterung PI-Bereich
      *
      *   Rev 2.1   Jun 18 1999 15:10:32   T13EX22
      *PIN und VERTRIEBS-GES entfernt
      *
      *   Rev 2.0   May 26 1999 17:00:22   T13EX25
      *APA00042, Erfassung V2.0
      *
      *   Rev 1.0   23 Jul 1997 12:43:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0488  FUNKTION STRUKTUR ZUSAMMENSTELLEN  *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    29.08.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !29.08.97! 01 ! NEUERSTELLUNG             ! JÄGER    *
      *         !        !    !                           !          *
      * APA00012!28.08.98! 1.1! VM HINZU                  ! PARENT   *
      * APA00012!17.06.99! 1.2! ÜBERFLÜSSIGE FELDER       ! MUND     *
      *         !        !    ! ENTFERNT                  !          *
      * APA00245!23.07.03! 2.2! PI-BEREICH ERWEITERT      ! STORCH   *
      * APA00452!01.11.06! 2.3! DREIST. PRODUKTSCHLUESSEL ! SCHEIDEL *
      * APA00622!21.06.07! 2.4! NEU PI-KNR-VGES           ! STORCH   *
      * APA01101!01.03.12! 2.5! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    ! *-ZEILEN ENTFERNT         !          *
      * APA01526!24.10.13! 2.6! BEGR.SÄTZE KOLLEKTIV      ! BÖTTCHER *
      * APA01932!16.12.15! 2.8! DURCHREICHEN STEUSTAT     ! SCHEIDEL *
      * APA02579!21.06.18! 2.9! HERKUNFT HINZU            ! SCHEIDEL *
      * DSY03279!25.03.21! 3.0! NEUE PI-PARAMETER         ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0488                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0488.
          05 PI-DRPA0488-DATEN.
             10 PI-DRPA0488-STICHTAG         PIC  X(010).
             10 PI-DRPA0488-DAISY-GEVO       PIC S9(004) BINARY.
             10 PI-DRPA0488-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0488-VNR              PIC  X(015).
             10 PI-DRPA0488-ALT-VNR          PIC  X(015).
             10 PI-DRPA0488-VBNR-PGEB        PIC S9(009) BINARY.
             10 PI-DRPA0488-PROD-PROD-SCHL   PIC S9(004) BINARY.
             10 PI-DRPA0488-PRODART          PIC S9(004) BINARY.
             10 PI-DRPA0488-KDB-REDUZ        PIC  X(001).
             10 PI-DRPA0488-KNR-MANDANT      PIC S9(004) BINARY.
241013       10 PI-DRPA0488-SAM-NR           PIC S9(009) BINARY.
241013       10 PI-DRPA0488-URSPR-POL-DAT    PIC  X(010).
210618       10 PI-DRPA0488-KNR-BEW-HRKFT    PIC S9(004) BINARY.
250321       10 PI-DRPA0488-KZ-STRUK-KDBETR  PIC  X(001).
161215    05 PI-DRPA0488-STEUSTAT.
161215    COPY DRPA1843.
      *
       01 PO-DRPA0488.
          05 PO-DRPA0488-RC                  PIC  X(003).
             88 PO-DRPA0488-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0488-RC-NOK                VALUE 'NOK'.
             88 PO-DRPA0488-RC-VSTAT-GEFUELLT     VALUE 'VSG'.
          05 PO-DRPA0488-DATEN.
             10 PO-DRPA0488-KONDITION-KDB    PIC S9(004) BINARY.
             10 PO-DRPA0488-KD-BETR          PIC S9(009) BINARY.
             10 PO-DRPA0488-AVERM            PIC S9(009) BINARY.
             10 PO-DRPA0488-VM               PIC S9(009) BINARY.
             10 PO-DRPA0488-PE-AENDERUNGS-KZ PIC  X(001).
161215    05 PO-DRPA0488-STEUSTAT.
161215    COPY DRPA1843.
      *
      ******** ENDE COPY-MEMBER **************************************


