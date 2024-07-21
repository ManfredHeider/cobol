      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0188.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0188.CPv  $
      *
      *   Rev 2.3   Jan 15 2011 14:05:44   t13ex15
      *AKI01005: PGM-Änderung AS-Club-Bedingungen (Investment)
      *
      *   Rev 2.2   Nov 30 2010 11:45:32   t13ex15
      *AKI01005: Änderung AS-Club-Bedingungen (Investment)
      *
      *   Rev 2.1   Oct 01 1999 09:02:50   T13AE21
      *AKI00029
      *
      *   Rev 2.0   Feb 27 1999 12:53:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:56:02   P13SP99
      *Initial revision.
      *
      *****************************************************************
      *
      *
      *    COPY-MEMBER ZUM PROGRAMM PMKI0282
      *    Auswertung AS-CLUB
      *
      *
      *    SATZLÄNGE    48
      *
      *
      *----------------------------------------------------------------
      *    LOGBUCH
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.
      *AUFTRAG !                                       !         !
      *--------!---------------------------------------!---------!-----
      *15.09.98!NEUERSTELLUNG                          !PHILIPP  ! 01
      *AKI00029!                                       !         !
      *--------!---------------------------------------!---------!-----
      *18.11.10!Zwei neue Felder (Investment)          !S.Warz   ! 2.1
      *AKI00029!                                       !         !
      *--------!---------------------------------------!---------!-----
      *15.01.11!Neues Feld GHPGUTHABPRODLFDNR          !S.Warz   ! 2.1
      *AKI00029!                                       !         !
      *****************************************************************
      *
       01  EFILE-SATZ.
           05  VERVERTRAGSID                 PIC X(13).
           05  VERVERTRAGSNUMMER             PIC X(15).
           05  NIL-VERVERTRAGSNUMMER         PIC X(1).
           05  VERPARTGESELLSCHID            PIC S9(4) COMP.
           05  VERGESAMTBEITRAG              PIC S9(9)V99 COMP-3.
           05  NIL-VERGESAMTBEITRAG          PIC X(01).
           05  VERZAHLUNGSWEISEID            PIC S9(4) COMP.
           05  NIL-VERZAHLUNGSWEISEID        PIC X(01).
           05  VERVERTRZUSTANDID             PIC S9(4) COMP.
           05  NIL-VERVERTRZUSTANDID         PIC X(01).
           05  VERGESBEITRWKZ                PIC X(01).
           05  VERGEBUENDELTKZ               PIC X(01).
           05  VEREIGENVERMKZ                PIC X(01).
           05  VERFREMDVERTRAGKZ             PIC X(01).
      *    bei Anz.>0 sind Guthabenanteile vorhanden
181110     05  ANZ-INVESTMENT                PIC S9(8) COMP.
      *    bei Anz.>0 ist TOP-ZINS-Paket Verrechnungskonto mit 1,5% Zins
181110     05  SALDO-TOPZINS                 PIC S9(13)V99 COMP-3.
150111     05  GHPGUTHABPRODLFDNR            PIC S9(4) COMP.

