      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0342.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0342.CPv  $
      *
      *   Rev 2.2   Feb 09 2015 15:20:28   t13ex27
      *FKI01438: AML-Ermittlung-Vertrag
      *
      *   Rev 2.1   02 May 2001 08:58:00   t13ex49
      *AKI00161 - erstmalige Prod.-Übergabe
      *
      *   Rev 2.0   07 Dec 2000 09:36:12   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *
      *   Dieses Copy beschreibt den Satzaufbau für die Übergabe von
      *   Daten an die Funktion 'Drucken Fehlerliste' aus dem
      *   Batch-Fehlerverfahren
      *
      *
      *--------------------------------------------------------------*
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00161!03.01.01!    ! Neuerstellung             !Peter Pohl*
      * FKI01438!30.10.14!2.2 ! Ausgabe der Fehlerreaktion! IFR      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *--------------------------------------------------------------*
       01  FEHLDAT-SATZ.
           05 FEHLDAT-LAUFDATUM                 PIC  X(010).
           05 FEHLDAT-PROGRAMM                  PIC  X(008).
           05 FEHLDAT-DATEN.
              10 FEHLDAT-SYSTEMCODE             PIC  X(002).
              10 FEHLDAT-RUF-PROGRAMM           PIC  X(008).
              10 FEHLDAT-JOBNAME                PIC  X(008).
              10 FEHLDAT-FEHLERNUMMER           PIC S9(004).
              10 FEHLDAT-FELDNAME               PIC  X(025).
              10 FEHLDAT-VERTRAG                PIC  9(015).
              10 FEHLDAT-GES                    PIC  9(002).
              10 FEHLDAT-FEHLERTEXT             PIC  X(100).
              10 FEHLDAT-FEHLERRELEVANZ         PIC  X(001).
TESTAB        10 FEHLDAT-FEHLERHINWEIS          PIC  X(058).
301014        10 FEHLDAT-FEHLERREAKTION         PIC  X(001).
301014*       10 FEHLDAT-FILLER                 PIC  X(059).
      *       10 FEHLDAT-FEHLERHINWEIS          PIC  X(025).
      *       10 FEHLDAT-FILLER                 PIC  X(092).
301014        10 FEHLDAT-FILLER                 PIC  X(058).
      *
      ******* ENDE COPY-MEMBER **************************************

