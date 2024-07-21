      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1005.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRKI1005.CPv  $
      *
      *   Rev 2.3   Mar 26 2003 17:17:50   t13ae44
      *AKI00303 - KoKü
      *
      *   Rev 2.2   10 Nov 2000 10:05:58   T13AE21
      *AKI00146
      *
      *   Rev 2.1   Mar 03 2000 14:50:06   T13AE21
      *FKI00337
      *
      *   Rev 2.0   Feb 09 2000 14:16:32   P13SP20
      *Archiverstellung
      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00337!03.02.00!    ! NEUERSTELLUNG             ! PHILIPP  *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00146!03.11.00!    ! Abfrage Infosperre bei    ! PHILIPP  *
      *         !        !    ! Banken in Fehlerausgabe   !          *
      *         !        !    ! Hier zusätzliches Feld    !          *
      *         !        !    ! Vertragszustandid         !          *
      * -------------------------------------------------------------*
      * AKI00303!20.11.02!    ! KoKü: VRBKOMMBETRKZ neu   ! R.Biehl  *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      *                                                              *
      * -------------------------------------------------------------*
       01  DRKI1005.
           05 VERVERTRAGSID                PIC X(013).
           05 VERPARTGESELLSCHID           PIC S9(004) COMP.
           05 VERGESAMTBEITRAG             PIC S9(9)V9(2) COMP-3.
           05 VERZAHLUNGSWEISEID           PIC S9(004) COMP.
           05 VERGESBEITRWKZ               PIC X(001).
           05 VEREIGENVERMKZ               PIC X(001).
           05 VERFREMDVERTRAGKZ            PIC X(001).
           05 VRBPERSONENID                PIC X(013).
           05 MITARBEITERNR                PIC 9(007) comp-3.
           05 MITARBEITER-NAME             PIC X(020).
           05 BETREUERNR                   PIC 9(007) comp-3.
           05 BETREUER-NAME                PIC X(020).
           05 STUFE                        PIC X(004).
           05 ADMAAUSTRITTSDATUM           PIC X(010).
           05 ADMADIREKTION                PIC S9(004) COMP.
           05 ADMASTRUKTURSTUFE            PIC S9(9) COMP.
           05 ADMAPRAXISPROVSATZ           PIC S9(3)V9(2) COMP-3.
031100     05 VERVERTRZUSTANDID            PIC S9(4) COMP.
           05 VERVERTRAGSNUMMER            PIC X(015).
201102     05 VRBKOMMBETRKZ                PIC X.

      ******* ENDE COPY-MEMBER ***************************************

      *---------------------------------------------------------------*
