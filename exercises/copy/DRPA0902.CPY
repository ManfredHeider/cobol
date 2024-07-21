      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0902.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA0902.CPv  $
      *
      *   Rev 2.6   Nov 22 2016 14:56:52   t13ex67
      *APA02280: Dialoganpassungen PO23 - Benutzerprofil Rhönklinken
      *
      *   Rev 2.5   Jul 16 2015 15:11:24   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e MSS
      *
      *   Rev 2.4   Jan 13 2014 09:46:26   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.3   Jun 19 2012 09:26:16   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:51:08   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 11:11:30   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   May 26 1999 15:29:06   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0902 DATENFELDER FüR DIALOG PO23         *
      *                          'LIST PM JE PRODUKTTEIL '           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    25.05.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!25.05.98! 01 !NEU                        !BOHLINGER *
      *         !10.11.98!1.02!BUCHUNGS-GRUND EINGEFÜGT   !BOHLINGER *
      * APA00622!13.06.07!2.01!NEU: KNR-VGEB-GES          !STORCH    *
      * APA01101!05.03.12!2.8 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01526!25.04.1302.9 !ANPASSUNGEN FÜR KALYPSO:   ! STORCH   *
      *         !        !    !NEUE FELDER FÜR DIE 2.SEITE!          *
      * APA01932!14.04.15!2.10!NEUE FELDER FÜR DIE 3.SEITE! STORCH   *
      * APA02280!07.11.16!2.11!NEUES FELD ZUR FESTSTELLUNG! STORCH   *
      *         !        !    !DER RHÖNKLINKEN            !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FÜR DIE LISTDATEN     *
      *    DES DIALOGMODULS FÜR DEN DIALOG PO23                      *
      *                                                              *
      ****************************************************************

      ***   DATENFELDER FÜR LISTDIALOG PO23
            35 PMPT-ID                   PIC S9(015) PACKED-DECIMAL.
            35 PDPT-ID                   PIC S9(015) PACKED-DECIMAL.
            35 PEST-ID                   PIC S9(015) PACKED-DECIMAL.
            35 PMBE-RFOLGE-NR            PIC S9(004) BINARY.
            35 PROD-PROD-SCHL            PIC S9(004) BINARY.
            35 PVPT-VT-PRIO              PIC S9(004) BINARY.
            35 PMPT-FAELLIG-VON          PIC  X(010).
            35 PMPT-FAELLIG-BIS          PIC  X(010).
            35 PDPT-BUCH-RFOLGE          PIC S9(004) BINARY.
            35 PMPT-VSTAT                PIC  X(004).
            35 PDPT-MA-ABR-DAT           PIC  X(010).
            35 PDPT-DGV                  PIC S9(004) BINARY.
            35 PMPT-GSMT-PROV            PIC S9(009)V99 PACKED-DECIMAL.
            35 PMPT-EINH                 PIC S9(006)V99 PACKED-DECIMAL.
            35 KNR-BUCHUNGS-GRUND        PIC S9(004) BINARY.
            35 KNR-MANDANT               PIC S9(004) BINARY.
250413      35 PMPR-NEUST-KZ             PIC  X(001).
250413      35 PMBE-PROV-SCHL            PIC  X(005).
250413      35 PMPR-BEW-SCHL             PIC S9(009) BINARY.
250413      35 KNR-BEW-HRKFT             PIC S9(004) BINARY.
250413      35 KNR-TARIF-KZ              PIC S9(004) BINARY.
250413      35 PMBE-UNTER-NR             PIC S9(004) BINARY.
250413      35 PMBE-SCHICHT-NR           PIC S9(004) BINARY.
250413      35 KNR-DYN-KZ-2              PIC S9(004) BINARY.
140415      35 PG-UST-BETRAG             PIC S9(009)V99 PACKED-DECIMAL.
140415      35 PG-UST-SATZ               PIC S9(003)V999 PACKED-DECIMAL.
071116      35 PMBE-SAM-NR               PIC S9(009) BINARY.


