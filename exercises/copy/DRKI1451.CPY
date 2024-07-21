      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1451.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1451.CPv  $
      *
      *   Rev 2.2   Jan 22 2016 14:32:34   t13ex48
      *AKI01382: CoC neue KÜ: KoKUe-Vorschlagslermittlung
      *
      *   Rev 2.1   Jan 20 2003 09:45:18   t13ex27
      *AKI00303 - Kommissarische Kundenübertragung
      *
      *   Rev 2.0   Sep 12 2002 08:29:44   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1451  Dateibeschr. Ausgabe Vorschlag     *
      *                           Stufe 1                            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00303!27.09.02!2.1 ! neu                       ! HSE      *
      * --------!--------!----!---------------------------!----------*
      * AKI01372!01.12.15!2.3 ! Neues Feld:               ! HSE      *
      *         !        !    ! DRKI1450-CoC-Widerspruch  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1451-SATZ.
           05  DRKI1451-KUNDENDATEN.
             10  DRKI1451-CoC-Widerspruch     PIC  X(001).
      * Flag für Widerspruch (Eintrag in KUEBLACKLIST). Werte:
      * "A" Kunde hat einer Uebertragung widersprochen:
      * "Z" kein Widerspruch
      * Feld wird fuer Sortierung der UNLOAD-Datei benötigt
      *
      ******** ENDE COPY-MEMBER (DRKI1450) *******************
             10  DRKI1451-KVBKIPERSONENID     PIC  X(013).
             10  DRKI1451-KVBLOESCHKZ         PIC  X(001).
             10  DRKI1451-KIKUNDENNUMMER      PIC  S9(009) COMP.
             10  DRKI1451-KIKUNDENSTATUSID    PIC  X(001).
             10  DRKI1451-KIANZAHLBETREUER    PIC  S9(004) COMP.
             10  DRKI1451-ADRESSEORT          PIC  X(030).
             10  DRKI1451-ADRESSEPLZ          PIC  X(015).
             10  DRKI1451-ADRESSESTRHAUSNUM   PIC  X(045).
             10  DRKI1451-KKVKDNAME           PIC  X(061).
             10  DRKI1451-PERSONGEBGRUENDDAT  PIC  X(010).
             10  DRKI1451-PERSONANREDEID      PIC  S9(004) COMP.
           05  DRKI1451-VERTRAGSDATEN.
             10  DRKI1451-VERVERTRAGSID       PIC  X(013).
             10  DRKI1451-VERVERTRAGSNUMMER   PIC  X(015).
             10  DRKI1451-VERVERTRZUSTANDID   PIC  S9(004) COMP.
      *        Vetragsbetreuer aktiv oder ausgeschieden möglich
           05  DRKI1451-VERTRAGSBETR.
             10  DRKI1451-VRBPERSONENID       PIC  X(013).
             10  DRKI1451-ADMADIREKTION       PIC  S9(004) COMP.
             10  DRKI1451-ADMASTRUKTURSTUFE   PIC  S9(009) COMP.
             10  DRKI1451-ADMAAUSTRITTSDATUM  PIC  X(010).
             10  DRKI1451-ADMARUECKSTUFDATUM  PIC  X(010).
             10  DRKI1451-MITARBEITERNR       PIC  S9(009) COMP.
             10  DRKI1451-MITARBVORGESPERSID  PIC  X(013).
             10  DRKI1451-VBALTNAME           PIC  X(061).
           05  DRKI1451-KOMMBETR.
             10  DRKI1451-KKB-DIR             PIC  9(003).
 
      *
      ******** ENDE COPY-MEMBER (DRKI1451) *******************

