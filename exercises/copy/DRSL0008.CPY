      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0008.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRSL0008.CPv  $
      *
      *   Rev 2.2   Sep 07 2016 09:42:38   t13ae35
      *AVB01400: Projekt Lucy2 - BOB-Allfinanz CTV-Anschreiben (JHE)
      *
      *   Rev 2.1   Jan 29 2003 11:57:20   t13ae35
      *ASL00153 - neu
      *
      *   Rev 2.0   Feb 11 2000 15:19:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00153!20.01.03!2.1 ! Neu                       ! Hering   *
020916* AVB01400!02.09.16!2.2 ! Lucy2-Anpassung           ! Hering   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *            Inhalt    :  BOZ-Berechnungsbogen-CTV-Datei       *
      *            Member    :  DRSL0008                             *
      *            Organisat.:  VSAM                                 *
      *            Satzlänge :  140 Byte   -> 230 Byte               *
      *            Key       :  VBNR (1-7)                           *
      *                                                              *
      *--------------------------------------------------------------*
       01  DRSL0008-SATZ.
           05 DRSL0008-VBNR                     PIC 9(007).
           05 DRSL0008-PZ                       PIC X(001).
           05 DRSL0008-QUARTAL                  PIC 9(001).
           05 DRSL0008-JAHR                     PIC 9(004).
           05 DRSL0008-BOZ-BETRAG-FREI          PIC 9(010)V99.
           05 DRSL0008-GESELLSCHAFT             PIC 9(004).
           05 DRSL0008-NAME-VNAME               PIC X(040).
           05 DRSL0008-STUFE-BEZ                PIC X(004).
           05 DRSL0008-DIR-AKT                  PIC 9(004).
           05 DRSL0008-BOZ-STICHTAG-ISO         PIC X(010).
           05 DRSL0008-UMS-12MON-FREI           PIC 9(010)V9.
           05 DRSL0008-EQF-12MON                PIC 9(004)V99.
           05 DRSL0008-UMS-03MON-FREI           PIC 9(010)V9.
           05 DRSL0008-EQF-ABZUG-FREI           PIC 9(010)V9.
           05 DRSL0008-BOZ-FAKTOR               PIC 9(004)V99.
           05 DRSL0008-ANZ-MONATE               PIC 9(004).
           05 DRSL0008-DIR-ZPKT                 PIC 9(004).
020916*----neu ab Lucy 2 ---------------------------------------
           05 DRSL0008-BOZ-BETRAG-PFL           PIC 9(010)V99.
           05 DRSL0008-UMS-12MON-PFL            PIC 9(010)V9.
           05 DRSL0008-UMS-03MON-PFL            PIC 9(010)V9.
           05 DRSL0008-EQF-ABZUG-PFL            PIC 9(010)V9.
           05 DRSL0008-EH-ABZUG-NEG             PIC 9(010)V9.
           05 DRSL0008-USM-03M-KORR-FREI        PIC 9(010)V9.
           05 DRSL0008-USM-03M-KORR-PFL         PIC 9(010)V9.
           05 DRSL0008-EH-ANTEIL-FREI           PIC 9(004)V99.
           05 DRSL0008-EH-ANTEIL-PFL            PIC 9(004)V99.

