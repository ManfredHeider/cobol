      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0432.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0432.CPv  $
      *
      *   Rev 2.1   05 Oct 2007 14:03:12   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung -> 
      *PMRW0017
      *
      *   Rev 2.0   04 Sep 2007 15:32:40   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0432  KONSTANTEN FÜR KTY 95              *
      *                           SWNP-Ermittlungsart                *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J.Hering                                        *
      *    DATUM:    27.08.2007                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00041!27.08.07!2.1 ! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-NIE-ANGEMELDET        PIC S9(004) BINARY VALUE  0.
           05 K-KNR-SWNP-AKZEPTIERT       PIC S9(004) BINARY VALUE  1.
           05 K-KNR-SWNP-ABGELEHNT        PIC S9(004) BINARY VALUE  2.

