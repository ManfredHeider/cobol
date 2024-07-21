      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0370.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA0370.CPv  $
      *
      *   Rev 2.7   Jan 03 2014 10:19:22   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): Sonstige Anpassungen (MSS)
      *
      *   Rev 2.6   Jun 19 2012 09:23:16   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.5   May 21 2012 08:48:14   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.4   19 Dec 2007 11:43:52   t13ex25
      *FPA00968: Anpassung UWPA0031 (Wechsel VGES)
      *
      *   Rev 2.3   Nov 10 2006 11:46:28   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Nov 08 2006 16:22:44   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   May 27 1999 09:50:28   T13EX26
      *NT-Umstellung 2
      *
      *   Rev 2.0   May 26 1999 16:57:32   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   09 Jul 1997 16:04:50   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        ! 01 ! NEU                       ! PARENT   *
      * APA00452!08.11.06!2.01! UMSTELLUNG 3-STELL. PROD. ! JÄGER    *
      * FPA00968!19.12.07!2.02! NEUES ATTRIBUT            ! JÄGER    *
      * APA01101!29.02.12!2.03! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01526!16.12.13!2.04! PI-BEREICH ERWEITERT      ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRPA0370.
           05  PI-DRPA0370-PGEB-GES           PIC S9(4) COMP.
           05  PI-DRPA0370-VNR                PIC X(15).
           05  PI-DRPA0370-ALT-VNR            PIC X(15).
           05  PI-DRPA0370-VBNR               PIC S9(9) COMP.
           05  PI-DRPA0370-PROV-TERM          PIC X(10).
           05  PI-DRPA0370-PROD-PROD-SCHL     PIC S9(4) COMP.
               88  GENEHM-PFLICHTIG               VALUE ZERO.
           05  PI-DRPA0370-KNR-MANDANT        PIC S9(4) COMP.
161213     05  PI-DRPA0370-URSPR-POL-DAT      PIC X(10).
161213     05  PI-DRPA0370-PMPE-SAM-NR        PIC S9(9) COMP.
      *
       01  PO-DRPA0370.
           05  PO-DRPA0370-RC                    PIC  X(03).
               88  PO-DRPA0370-RC-OK             VALUE 'OK '.
               88  PO-DRPA0370-RC-VSTAT-GEFUELLT VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER (DRPA0370) *******************
