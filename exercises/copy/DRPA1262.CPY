      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1262.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1262.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:28:52   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:53:40   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Feb 20 2008 12:19:30   t13ex21
      *APA00622: Allfinanz Stufe 1 im abgerechneten Geschäft
      *
      *   Rev 2.1   Jun 22 1999 23:23:24   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 30 1999 08:44:14   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:46   P13SP20
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRPA1262  PRUB - DATEN                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    08.06.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.06.99! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      * APA00622!12.02.08!2.02! KNR-VGES HINZU            ! STORCH   *
      * APA01101!23.02.12!2.03! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *  DIESE COPY ENTHÄLT DIE DATEN FÜR DIE TABELLE PRUB TBPA0025  *
      *                                                              *
      *          >> PRUB -DATEN <<                                   *
      *                                                              *
      ****************************************************************
             30 PRUB-ID              PIC S9(015)V PACKED-DECIMAL.
             30 PDPT-ID              PIC S9(015)V PACKED-DECIMAL.
             30 PGEB-GES             PIC S9(004)  BINARY.
             30 PRINTE               PIC S9(015)V PACKED-DECIMAL.
             30 PVER-VERSION         PIC S9(004)  BINARY.
             30 PMPT-ID              PIC S9(015)V PACKED-DECIMAL.
             30 PMPT-VERSION         PIC S9(004)  BINARY.
             30 KNR-BUCH-GRUND-GRP   PIC S9(004)  BINARY.
             30 PRUB-UMBUCH-DAT      PIC X(10).
             30 PRUB-KENNUNG         PIC X(10).
             30 KNR-MANDANT          PIC S9(004)  BINARY.
      *---------------------------------------------------------------*
