      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.04.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2246.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2246.CPv  $
      *
      *   Rev 2.3   Apr 14 2014 12:07:42   t13ex27
      *AKI01291: Fondsdaten leer bereitstellen
      *
      *   Rev 2.2   19 Mar 2009 14:12:30   t13ex27
      *AKI00798: AML - Erweiterung  auf 10 Fonds - Vertragsteil
      *
      *   Rev 2.1   04 Jun 2008 16:04:10   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben Datei-Copys
      *
      *   Rev 2.0   Apr 16 2008 14:56:38   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *              #####################
      *     ########    D R K I 2 2 4 6    ########
      *              #####################
      *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMM AML VERTRAGSDATEN
      *       LEBENFOND (Fondsbezeichnungen)
      *
      *    SATZLÄNGE   52
      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00711!10.04.08!2.1 ! NEU                       !I. Frenzel*
      * AKI00798!20.02.09!2.2 ! Erweiterung auf 10 Fonds  !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      *01  DRKI2246.

      *    02  'FO'-LEBENFOND.
           05  'F'-GES                    PIC  9(002).
           05  'F'-VNR                    PIC  9(015).
           05  'F'-LFOVERSTARLFDNR        PIC S9(004) COMP.
           05  'F'-LFOLFDNR               PIC S9(004) COMP.

200209*    05  FO-LFOFONDFONDKUERZEL OCCURS 4 TIMES PIC  X(002).
200209     05  'F'-LFOFONDFONDKUERZEL OCCURS 10 TIMES PIC  X(002).
200209*    05  FO-LFOFONDFONDKUERZEL-KZ OCCURS 4 TIMES PIC X(001).
200209     05  'F'-LFOFONDFONDKUERZEL-KZ OCCURS 10 TIMES PIC X(001).

      *        J WENN LFOFONDFONDKUERZEL GEAENDERT
           05  'F'-AKTUAL-KZ              PIC  X(01).
      *********************** Ende Copy DRKI2246 **********************
