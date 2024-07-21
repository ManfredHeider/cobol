      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1289.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1289.CPv  $
      *
      *   Rev 2.5   Oct 23 2017 08:24:30   t13ex67
      *APA02492: 3. Statistik-Kategorie Folge Neu - Dialog PK14 MSS
      *
      *   Rev 2.4   Jun 19 2012 09:29:32   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:54:16   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Feb 20 2008 12:20:18   t13ex21
      *APA00622: Allfinanz Stufe 1 im abgerechneten Geschäft
      *
      *   Rev 2.1   Jul 27 1999 10:21:56   T13AE18
      *APA00012
      *
      *   Rev 2.0   Mar 30 1999 08:44:26   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1289 DATENFELDER FÜR DIALOG PK14         *
      *                          'LIST UMBUCHUNGEN '                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    16.07.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!16.07.99!2.01!NEU                        !BOHLINGER *
      * APA00622!12.02.08!2.02! KNR-VGES HINZU            ! STORCH   *
      * APA01101!27.03.12!2.03! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA02492!22.09.17!2.04! DGV DURCH PRODART ERSETZT ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FÜR DIE LISTDATEN     *
      *    FUER DEN DIALOG PK14                                      *
      *                                                              *
      ****************************************************************

      ***   DATENFELDER FÜR LISTDIALOG PK14
            35 PRUB-ID              PIC S9(015) PACKED-DECIMAL.
            35 PDPT-ID              PIC S9(015) PACKED-DECIMAL.
            35 PMPT-ID              PIC S9(015) PACKED-DECIMAL.
            35 PRINTE               PIC S9(015) PACKED-DECIMAL.
            35 PVER-VERSION         PIC S9(004) BINARY.
            35 PRUB-UMBUCH-DAT      PIC  X(010).
            35 PRUB-KENNUNG         PIC  X(008).
            35 PGEB-GES             PIC S9(004) BINARY.
            35 PVER-VNR             PIC  X(015).
            35 PVER-AKT-PIN         PIC S9(009) BINARY.
            35 PROD-PROD-SCHL       PIC S9(004) BINARY.
            35 PVPT-VT-PRIO         PIC S9(004) BINARY.
            35 PMPT-FAELLIG-VON     PIC X(010).
            35 PMPT-FAELLIG-BIS     PIC X(010).
            35 PDPT-BUCH-RFOLGE     PIC S9(004) BINARY.
220917*     35 PDPT-DGV             PIC S9(004) BINARY.
220917      35 PDPT-PRODART         PIC S9(004) BINARY.
            35 PMPT-GSMT-PROV       PIC S9(009)V99 PACKED-DECIMAL.
            35 KNR-BUCHUNGSART      PIC S9(004) BINARY.
            35 KNR-BUCHUNGS-GRUND   PIC S9(004) BINARY.
            35 KNR-WAEHRUNG-BK      PIC  X(004).
            35 GSMT-PROV-DIFF-KZ    PIC  X(001).
            35 KNR-MANDANT          PIC S9(004) BINARY.
      *---------------------------------------------------------------*

