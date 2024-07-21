      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2781.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2781.CPv  $
      *
      *   Rev 2.7   Mar 17 2015 14:36:40   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Mar 11 2015 12:26:12   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.5   Mar 09 2015 11:15:06   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 02 2015 10:51:02   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Feb 25 2015 11:41:20   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Jan 13 2015 13:04:52   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jul 24 2014 15:15:44   t13ex77
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Sep 04 2013 16:19:30   T13Ex77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - KSDS-Datei Vertragsdaten Commerzbank                    *
      *                                                               *
      *    SATZLAENGE   337                                           *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      *****************************************************************
      *  AENDERUNGEN:                                                 *
      *  AUFTRAG !  DATUM   ! VER ! AENDERUNG              ! NAME     *
      * ------------------------------------------------------------- *
      * AKI01251 ! 04.09.13 !  01 ! Neuerstellung          ! Borgmann *
      *          !          !     !                        !          *
      *          !          !     !                        !          *
      *          !          !     !                        !          *
      *          !          !     !                        !          *
      *          !          !     !                        !          *
      *  ENDE-LOGBUCH                                                 *
      *****************************************************************
      *01  DRKI2781.
           05  'XX'-STATUS                     PIC 9(001).
      *        Status des Vertrags [ 0 = aktiv, 9 = nicht aktiv ]
           05  'XX'-TIMESTAMP                  PIC X(026).
      *        Timestamp, der den Zeitpunkt der letzten Aenderung
      *        des Datensatzes enthaelt
           05  'XX'-MANDANT                    PIC 9(009).
      *        Mandant
           05  'XX'-VERTR-SATZART              PIC X(001).
      *        Inhalt C
           05  'XX'-VERTRAG-KEY.
               10 'XX'-VERTR-GESELLSCHAFT      PIC 9(002).
      *        2-stellige DVAG interne Nummer für die Gesellschaft
               10 'XX'-VERTR-VERTRAGSNUMMER    PIC 9(015).
      *        Vertragsnummer rechtsbündig mit führenden Nullen
           05  'XX'-VERTR-VB-NUMMER            PIC 9(007).
      *        VB-Nummer von dem Antragsformular, ohne Pruefziffer
           05  'XX'-VERTR-BEGINN-DATUM         PIC 9(008).
      *        JJJJMMTT
      *        Datum vom Beginn des Vertrags oder der Kundenverbindung
           05  'XX'-VERTR-ABLAUF-DATUM         PIC 9(008).
      *        JJJJMMTT
      *        Datum vom Ende des Vertrags oder der Kundenverbindung
           05  'XX'-VERTR-DS-SPERRE            PIC X(001).
      *        'S' = Kunde hat der Datenweitergabe an DVAG
      *        widersprochen, Leerstelle = Kein Widerspruch
           05  'XX'-VERTR-DS-SPERRE-DATUM      PIC 9(008).
      *        JJJJMMTT  Datum letzte Änderung für Datenschutzsperre
           05  'XX'-FREIER-BEREICH             PIC X(250).
      *        Freier Reserve-Bereich
 
 
 
 
 

