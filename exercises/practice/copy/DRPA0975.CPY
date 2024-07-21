      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0975.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0975.CPv  $
      *
      *   Rev 2.5   Jul 16 2015 15:11:44   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e MSS
      *
      *   Rev 2.4   Jan 13 2014 09:47:00   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.3   Jun 19 2012 09:26:24   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:51:14   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 11:20:52   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Jul 28 1999 12:07:32   T13EX21
      *APA00046 - DAISY-Produktionsübergabe
      *
      *   Rev 1.1   Apr 21 1999 09:19:56   T13AE18
      *APA00012 Erweiterungen
      *
      *   Rev 1.0   01 Apr 1998 14:19:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER DRPA0975: DATENFELDER FÜR DIALOG PO21         *
      *    ( PROVISIONSBESTAND - AUSKUNFT PROVISIONSMELDUNGEN )      *
      *    BENUTZT ALS PO-DRPA0974-ERGEBNISZEILE                     *
      *                PO-DRPA0904-ERGEBNISZEILE                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KUEKENHOEHNER                                   *
      *    DATUM:    20.06.1998                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!20.06.98! 1.1!NEU                        !KUEKEN    *
      *         !        ! 1.2!NEU: HERKUNFTS-TAB         !BOHLINGER *
      *         !        ! 1.3!ERGÄNZT UM KNR-PMBE-ART    !BOHLINGER *
      *         !06.07.99! 2.0! PMPG-VERSION KORRIGIERT   ! STORCH   *
      * APA00622!23.07.07! 2.1! NEUES FELD PMBE-VGES      ! STORCH   *
      * APA01101!05.03.12! 2.2! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    ! *-ZEILEN ENTFERNT         !          *
      * APA01526!16.10.13! 2.4! ERWEITERUNG FÜR KALYPSO   ! STORCH   *
      * APA01932!14.04.15! 2.5! NEUES FELD HINZU (DVM)    ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FÜR DIALOG PO21       *
      *    ( PROVISIONSBESTAND - AUSKUNFT PROVISIONSMELDUNGEN )      *
      *    BENUTZT ALS PO-DRPA0974-ERGEBNISZEILE                     *
      *                PO-DRPA0904-ERGEBNISZEILE                     *
      *                                                              *
      ****************************************************************

      ***   DATENFELDER FÜR LISTDIALOG PO21

             35 PGFAELLIG-VON               PIC  X(010).
             35 PGFAELLIG-BIS               PIC  X(010).
             35 RFOLGE-NR                   PIC S9(004)    COMP.
             35 PGV                         PIC S9(004)    COMP.
             35 PG-GSMT-PROV-BETRAG         PIC S9(9)V9(2) COMP-3.
             35 KNR-WAEHRUNG-BK             PIC  X(004).
             35 VSTAT                       PIC  X(004).
             35 S-HERKUNFT-TAB-PEG          PIC  X(001).
             35 PMBE-ID                     PIC S9(015)  PACKED-DECIMAL.
             35 PMBE-VERSION                PIC S9(004)  BINARY.
             35 KNR-PMBE-ART                PIC S9(004)  BINARY.
             35 PDTA-ID                     PIC S9(015)  COMP-3.
             35 PMPG-ID                     PIC S9(015)  COMP-3.
             35 PMPG-VERSION                PIC  9(004).
             35 KNR-PMBE-MANDANT            PIC S9(004)  BINARY.
161013       35 PMBE-UNR                    PIC S9(004)  BINARY.
161013       35 PMBE-SCHICHT-NR             PIC S9(004)  BINARY.
140415       35 PG-UST-BETRAG               PIC S9(9)V9(2) COMP-3.
      *

