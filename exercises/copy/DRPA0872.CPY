      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0872.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0872.CPv  $
      *
      *   Rev 2.5   Jun 09 2020 10:03:34   t13ex67
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.4   Dec 18 2013 14:34:30   t13ae46
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO-Dialoge (PO11-PO13)
      *
      *   Rev 2.3   Jun 19 2012 09:26:00   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:50:52   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 11:01:44   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   May 26 1999 13:21:40   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:47:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0872  FUNKTION                           *
      *                           GET VERTRAGSDATEN      (UGPA0001)  *
      *    --------------------------------------------------------- *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    02.03.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!02.03.98! 01  !NEUERSTELLUNG             ! STORCH   *
      *         !        !01.02!ANPASSUNGEN               ! BOHLINGER*
      *         !        !01.03!PO-BEREICH ERWEITERT      ! BOHLINGER*
      *         !        !01.04!PO-BEREICH KDBETR ERW.    ! BOHLINGER*
      * APA00622!29.05.07!02.01!PO-BEREICH KNR-VGES ERW.  ! STORCH   *
      * APA01101!14.03.12!02.02!VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01526!05.03.13!2.4  !PO-BEREICH ERWEITERT      ! WANG     *
      * APA02908!08.06.20!2.5  !PO-BEREICH ERWEITERT      ! STORCH   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0872                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0872.
          05 PI-DRPA0872-DATEN.
             10 PI-DRPA0872-PRINTE           PIC S9(015)V USAGE COMP-3.
             10 PI-DRPA0872-PGEB-GES         PIC X(002).
             10 PI-DRPA0872-VNR              PIC X(015).
             10 PI-DRPA0872-HISTORIE-NR      PIC S9(004)  BINARY.
             10 PI-DRPA0872-S-HISTORIE-NR    PIC X(001).
      *
       01 PO-DRPA0872.
          05 PO-DRPA0872-RC                  PIC  X(003).
             88 PO-DRPA0872-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0872-RC-NOT-FOUND          VALUE 'NFD'.

          05 PO-DRPA0872-MELDUNG.
             10 PO-DRPA0872-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA0872-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA0872-SCHALTER.
             10 PO-DRPA0872-S-AKT-PVER-SATZ  PIC X(001).
             10 PO-DRPA0872-S-HISTORIE       PIC X(001).
             10 PO-DRPA0872-S-EVS            PIC X(001).
             10 PO-DRPA0872-S-VORVERTRAG     PIC X(001).
             10 PO-DRPA0872-PRINTE-EVS       PIC S9(015) PACKED-DECIMAL.
             10 PO-DRPA0872-PVER-VERSION-EVS PIC S9(004) BINARY.
             10 PO-DRPA0872-VNR-EVS          PIC  X(015).
080620       10 PO-DRPA0872-VNR-EVS-GES      PIC S9(004) BINARY.

          05 PO-DRPA0872-DATEN.
             10 PO-DRPA0872-PVER-KUNDE-NAME       PIC X(025).
             10 PO-DRPA0872-PVER-KUNDE-VNAME      PIC X(020).
             10 PO-DRPA0872-PVER-VERTR-BEGINN     PIC X(010).
             10 PO-DRPA0872-PVER-VERTR-ABLAUF     PIC X(010).
             10 PO-DRPA0872-EINHEITEN             PIC S9(06)V9(2)
                                                  USAGE COMP-3.
             10 PO-DRPA0872-KNR-PVER-STAND        PIC S9(004)  BINARY.
             10 PO-DRPA0872-KNR-PVER-STAND-BL     PIC X(040).
             10 PO-DRPA0872-KNR-PVER-ART          PIC S9(004)  BINARY.
             10 PO-DRPA0872-KNR-PVER-ART-BM       PIC X(010).
             10 PO-DRPA0872-KNR-ZAHLWEISE         PIC S9(004)  BINARY.
             10 PO-DRPA0872-KNR-ZAHLWEISE-BM      PIC X(010).
             10 PO-DRPA0872-PVER-AVERM            PIC S9(009)  BINARY.
             10 PO-DRPA0872-KDBETR                PIC S9(009)  BINARY.
             10 PO-DRPA0872-PVER-VM               PIC S9(009)  BINARY.
             10 PO-DRPA0872-LAST-UPD-DAT          PIC X(010).
             10 PO-DRPA0872-PVER-KENNUNG          PIC X(008).
             10 PO-DRPA0872-KNR-MANDANT           PIC S9(004)  BINARY.
050313       10 PO-DRPA0872-PVER-BZ-NAME          PIC X(025).
050313       10 PO-DRPA0872-PVER-VP-NAME          PIC X(025).
050313       10 PO-DRPA0872-KNR-FEIV-KZ           PIC S9(004)  BINARY.
050313       10 PO-DRPA0872-KNR-FEIV-KZ-BK        PIC X(004).
050313       10 PO-DRPA0872-KNR-FEIV-KZ-BM        PIC X(010).
050313       10 PO-DRPA0872-KNR-VERM-GES          PIC S9(004)  BINARY.
050313       10 PO-DRPA0872-KNR-VERM-GES-BM       PIC X(010).
050313       10 PO-DRPA0872-PVER-UMW-NR           PIC S9(015).
050313       10 PO-DRPA0872-SAMMEL-NR             PIC S9(009)  COMP.
050313       10 PO-DRPA0872-KNR-RABSTUFE-BK       PIC X(003).
050313       10 PO-DRPA0872-PVER-VN-ADRESSE       PIC X(057).
050313       10 PO-DRPA0872-UNTERNUMMER           PIC 9(003).
050313       10 PO-DRPA0872-SCHICHTNUMMER         PIC 9(003).
      *
      ******** ENDE COPY-MEMBER **************************************

