      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0127.CPY  $
      *    $Revision:   1.1  $
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0127                                      *
      *    --------------------------------------------------------   *
      *    STANDARD-KONSTANTEN SPEZIELL F?R PROJEKT 'DAISY'           *
      *                                                               *
      *    IM DRAT0009 WERDEN ALLGEMEINE KONSTANTEN DEFINIERT, DIE    *
      *    UNABH„NGIG VOM PROJEKT G?LTIG SIND. HIER DAGEGEN WERDEN    *
      *    NUR SOLCHE KONSTANTEN DEFINIERT, DIE INNERHALB 'DAISY'     *
      *    G?LTIG SIND.                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    31.01.1997                                       *
      *****************************************************************
      *    VERSION   1    08.06.95 16:19:49
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!31.01.97! 01 ! NEU                       ! AUERBACH *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      ***  NUMERISCHE KONSTANTEN
       01  FILLER.

      ***  MAX. ANZAHL DER PRODUKTE IM PEW-WF (DRPA0105)
           05  K-PROD-ZEILEN-MAX         PIC S9(004) BINARY VALUE 15.

      ***  MAX. ANZAHL DER ZEILEN IN DER PE-STRUKTUR (DRPA0105)
           05  K-PE-S-ZEILEN-MAX         PIC S9(004) BINARY VALUE 20.

      ***  MAX. SUMME DES DIFFERENZ-PROVISIONS-SATZ JE PE-STRUKTUR
           05  K-MAX-DIFF-PROV-SATZ      PIC S9(004) BINARY VALUE +148.



      *** ALPHANUMERISCHE KONSTANTEN
       01  FILLER.


      ***  DIESER TEXT WIRD AUF DER PR„SENTATIONSSCHICHT (DIALOG, LISTE)
      ***  AUSGEGEBEN, WENN EIN VSTATUS NICHT IN DER TABELLE DEFINIERT
      ***  IST UND DEMZUFOLGE KEIN TEXT ERMITTELT WERDEN KANN.
           05  K-VSTAT-TEXT-NOT-FOUND    PIC  X(050) VALUE
               '--> VSTATUS IST NICHT IN DER DATENBANK DEFINIERT !'.


      ***  KONSTANTEN DER FUNKTIONEN F?R DIE ZVP-ZUORDNUNG
           05  K-AVERM-VM-ERMITTELN        PIC  X(020)
                                       VALUE 'AVERM/VM ERMITTELN  '.
           05  K-EMPF-TYPEN-SETZEN         PIC  X(020)
                                       VALUE 'EMPF-TYPEN SETZEN   '.
           05  K-PROV-STRUK-PRUEFEN        PIC  X(020)
                                       VALUE 'PROV-STRUK PRšFEN   '.
           05  K-PEW-PRUEF-UND-BERECH      PIC  X(020)
                                       VALUE 'PEW PRšF UND BERECH '.
           05  K-PEW-INHALTL-PRUEFEN       PIC  X(020)
                                       VALUE 'PEW INHALTL. PRšFEN '.
           05  K-GES-PROV-BERECHNEN        PIC  X(020)
                                       VALUE 'GESAMTPROV. BERECHN.'.
           05  K-GRUND-PROV-BERECHNEN      PIC  X(020)
                                       VALUE 'GRUNDPROV. BERECHNEN'.
           05  K-MA-PROV-BERECHNEN         PIC  X(020)
                                       VALUE 'MA-PROV. BERECHNEN  '.
           05  K-EINHEITEN-BERECHNEN       PIC  X(020)
                                       VALUE 'EINHEITEN BERECHNEN '.

      ***  GROBKLASSIFIZIERUNG VSTATUS (1. STELLE)
           05  K-VSTAT-KLASSE-SCHWER       PIC X(001) VALUE 'S'.
           05  K-VSTAT-KLASSE-FORMAL       PIC X(001) VALUE 'F'.
           05  K-VSTAT-KLASSE-INHALT       PIC X(001) VALUE 'I'.
           05  K-VSTAT-KLASSE-KOMPETENZ    PIC X(001) VALUE 'K'.
           05  K-VSTAT-KLASSE-HINWEIS      PIC X(001) VALUE 'H'.
           05  K-VSTAT-KLASSE-ERLEDIGT     PIC X(001) VALUE 'E'.
           05  K-VSTAT-KLASSE-OFFEN        PIC X(001) VALUE 'O'.

      ***  SPEZIELLE VSTATI
           05  K-VSTAT-OK                  PIC X(004) VALUE 'E000'.
           05  K-VSTAT-OK-MANUELL          PIC X(004) VALUE 'E001'.
           05  K-VSTAT-STORNO              PIC X(004) VALUE 'E900'.
           05  K-VSTAT-GEMELDET-AN-PG-E    PIC X(004) VALUE 'O000'.
           05  K-VSTAT-GEMELDET-AN-PG-H    PIC X(004) VALUE 'O001'.
           05  K-VSTAT-NICHT-GEMELDET-E    PIC X(004) VALUE 'O100'.
           05  K-VSTAT-NICHT-GEMELDET-H    PIC X(004) VALUE 'O101'.

      ******** ENDE COPY-MEMBER STANDARD-KONSTANTEN (DRAT0127) ********
