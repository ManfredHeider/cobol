      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2782.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2782.CPv  $
      *
      *   Rev 2.7   Mar 17 2015 14:36:44   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Mar 11 2015 12:26:16   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.5   Mar 09 2015 11:15:12   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 02 2015 10:51:08   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Feb 25 2015 11:41:26   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Jan 13 2015 13:05:02   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jul 24 2014 15:15:54   t13ex77
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Sep 04 2013 16:19:36   T13Ex77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Aenderungs-Datei Vertragsdaten Commerzbank              *
      *                                                               *
      *    SATZLAENGE   300                                           *
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
           05 'XX'-VERTR-VERARB-KZ            PIC X(001).
      *    Verarbeitungskennzeichen  I = Insert U = Update
           05 'XX'-VERTR-GESELLSCHAFT         PIC 9(002).
      *    2-stellige DVAG interne Nummer für die Gesellschaft
           05 'XX'-VERTR-VERTRAGSNUMMER       PIC 9(015).
      *    Vertragsnummer rechtsbündig mit führenden Nullen
           05 'XX'-VERTR-VB-NUMMER            PIC 9(007).
      *    VB-Nummer von dem Antragsformular, ohne Pruefziffer
           05 'XX'-VERTR-VB-NUMMER-KZ         PIC X(001).
      *    Kennzeichen ob sich VB-Nummer geaendert hat
           05 'XX'-VERTR-BEGINN-DATUM         PIC 9(008).
      *    JJJJMMTT
      *    Datum vom Beginn des Vertrags oder der Kundenverbindung
           05 'XX'-VERTR-BEGINN-DATUM-KZ      PIC X(001).
      *    Kennzeichen ob sich das Beginndatum geaendert hat
           05 'XX'-VERTR-ABLAUF-DATUM         PIC 9(008).
      *    JJJJMMTT
      *    Datum vom Ende des Vertrags oder der Kundenverbindung
           05 'XX'-VERTR-ABLAUF-DATUM-KZ      PIC X(001).
      *    Kennzeichen ob sich das Endedatum geaendert hat
           05 'XX'-VERTR-DS-SPERRE            PIC X(001).
      *    'S' = Kunde hat der Datenweitergabe an DVAG
      *    widersprochen, Leerstelle = Kein Widerspruch
           05 'XX'-VERTR-DS-SPERRE-KZ         PIC X(001).
      *    Kennzeichen ob sich die Sperre geaendert hat
           05 'XX'-VERTR-SPERRE-DATUM         PIC 9(008).
      *    JJJJMMTT  Datum letzte Änderung für Datenschutzsperre
           05 'XX'-VERTR-SPERRE-DATUM-KZ      PIC X(001).
      *    Kennzeichen ob sich das Datum der Sperre geaendert hat
           05 FILLER                          PIC X(245).
 
 
 
 
 

