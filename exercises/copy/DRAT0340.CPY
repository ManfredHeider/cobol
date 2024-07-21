      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0340.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0340.CPv  $
      *
      *   Rev 2.3   May 15 2017 11:07:28   t13ac20
      *AAT00055: Anpassungen Fehlerverfahren
      *
      *   Rev 2.2   08 Jun 2001 07:57:58   t13ex13
      *AKI00480 - falsche bzw. unzureichende Returncodeübergabe
      *Beschreibung:
      *
      *   Rev 2.1   02 May 2001 08:57:30   t13ex49
      *AKI00161 - erstmalige Prod.-Übergabe
      *
      *   Rev 2.0   07 Dec 2000 09:36:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *
      *   Dieses Copy beschreibt den Aufbau für die Kommunikation
      *   zwischen PMKI's und der Funktion 'Prüfen Fehlermeldung'
      *   aus dem Batch-Fehlerverfahren
      *
      *--------------------------------------------------------------*
      *
      * mögliche Returncode
      *
      * OK  = Fehler konnte mit allen Parameter ausgewertete werden
      * NFD = es wurde keine gültige Fehlermeldung zur Fehlernummer
      *       gefunden
      * PAR = die Anzahl der übergebenen Parameter und der im Text ver-
      *       wendeten stimmen nicht überein
      * LEN = der zusammengesetzte Text war länger als 100 Stellen und
      *       wurde gekürzt
      *
      *        A   B   C
      * 001 = OK      NFD  --
      * 002 = PAR     OK    !  Kombination aus:
      * 003 = PAR     NFD   !  RC der Fehlertextermittlung            A
      * 004 = LEN     OK    !         und Variablenprüfung
      * 005 = LEN     NFD   !     des zusammengesetzten Fehlertextes  B
      * 006 = PAR LEN OK    !  RC der Fehlerreaktionsermittlung       C
      * 007 = PAR LEN NFD  --
      *
      *--------------------------------------------------------------*
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00161!03.01.01!    ! Neuerstellung             !Peter Pohl*
      * FKI00480!29.05.01!2.1 ! RC um numerische Werte er-!A.Blaess  *
      *         !        !    ! gänzt.                    !          *
120517* AAT00055!12.05.17!2.3 ! RC 004 un 005 entfernt    ! SWO
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *--------------------------------------------------------------*
       01  PI-DRAT0340.
           05 PI-DRAT0340-PROGRAMM                  PIC  X(008).
           05 PI-DRAT0340-DATEN.
              10 PI-DRAT0340-SYSTEMCODE             PIC  X(002).
              10 PI-DRAT0340-RUF-PROGRAMM           PIC  X(008).
              10 PI-DRAT0340-FEHLERNUMMER           PIC S9(004).
              10 PI-DRAT0340-FELDNAME               PIC  X(025).
              10 PI-DRAT0340-VERTRAG                PIC  9(015).
              10 PI-DRAT0340-GES                    PIC  9(002).
              10 PI-DRAT0340-VAR-1                  PIC  X(015).
              10 PI-DRAT0340-VAR-2                  PIC  X(015).
              10 PI-DRAT0340-VAR-3                  PIC  X(015).
              10 PI-DRAT0340-VAR-4                  PIC  X(015).
              10 PI-DRAT0340-VAR-5                  PIC  X(015).
      *
       01  PO-DRAT0340.
           05 PO-DRAT0340-RC                        PIC  X(003).
              88 PO-DRAT0340-RC-OK                       VALUE 'OK '.
              88 PO-DRAT0340-RC-NFD                      VALUE 'NFD'.
              88 PO-DRAT0340-RC-PAR                      VALUE 'PAR'.
              88 PO-DRAT0340-RC-LEN                      VALUE 'LEN'.
010529        88 PO-DRAT0340-RC-001                      VALUE '001'.
010529        88 PO-DRAT0340-RC-002                      VALUE '002'.
010529        88 PO-DRAT0340-RC-003                      VALUE '003'.
120597*       88 PO-DRAT0340-RC-004                      VALUE '004'.
120597*       88 PO-DRAT0340-RC-005                      VALUE '005'.
010529        88 PO-DRAT0340-RC-006                      VALUE '006'.
010529        88 PO-DRAT0340-RC-007                      VALUE '007'.
           05 PO-DRAT0340-RC-ORT                    PIC  X(003).
           05 PO-DRAT0340-DATEN.
              10 PO-DRAT0340-JOBNAME                PIC  X(008).
              10 PO-DRAT0340-FEHLERRELEVANZ         PIC  X(001).
              10 PO-DRAT0340-FEHLERREAKTION         PIC  X(001).
              10 PO-DRAT0340-FEHLERHINWEIS          PIC  X(025).
      *
      ******* ENDE COPY-MEMBER **************************************

