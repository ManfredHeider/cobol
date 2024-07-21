      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2779.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2779.CPv  $
      *
      *   Rev 2.6   Mar 27 2017 09:30:26   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.5   Mar 17 2015 14:39:10   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.4   Jan 13 2015 13:15:48   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank MDTA
      *
      *   Rev 2.3   Sep 26 2013 15:34:44   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Sep 18 2013 08:10:04   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Sep 13 2013 09:19:40   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Sep 06 2013 10:36:52   t13ex78
      *Archiverstellung
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 7 7 9    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER BANK-DTA Vertragsdaten                       *
      *                                                                *
      *    SATZLÄNGE   300 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *06.09.13!NEUERSTELLUNG                          !T.Flierl ! 01  *
      *--------!---------------------------------------!---------!-----*
      *24.03.17! FVD entfernt                          !HHU      !     *
      ******************************************************************
      *
      *01  DRKI2779.
 
           05  'XX'-VORSATZ.
               10  'XX'-V-SA            PIC  X(001).
      *
      *  'A' für Vorsatz (A-Satz)
 
               10  'XX'-V-ABSENDER      PIC  X(008).
      *
      *  Text, welcher den Absender des DTA identifiziert
 
               10  'XX'-V-DATUM         PIC  9(008).
      *
      *  Datum der Erstellung des DTA im Format JHJJMMTT
 
               10  'XX'-V-EMPFAENGER    PIC  X(004).
      *
      *  je nach Mandant 'DVAG', 'ALLF'
 
               10  'XX'-V-TEXT          PIC  X(008).
      *  Text, welcher den Inhalt des DTA identifiziert
      *  'KUNDEN  ', 'VERTRAG ' oder 'BAUFI   '
 
               10  'XX'-LIEFERUNG       PIC  X(001).
 
      *  'K' für Komplettbestand; 'D' dür Deltalieferung
 
 
               10  FILLER               PIC  X(270).
 
 
 
           05  'XX'-SATZ REDEFINES 'XX'-VORSATZ.
               10  'XX'-SA              PIC  X(001).
      *
      *  C-SATZ
 
               10  'XX'-GES             PIC  9(002).
      *
      *  Gesellschaftsnummer DVAG-intern
 
               10  'XX'-VERNR           PIC  9(015).
      *
      *  Vertragsnummer
               10  'XX'-VB              PIC  9(007).
      *        10  'XX'-VB11.
      *             15 'XX'-VBDIR       PIC  9(003).
      *             15 'XX'-VB          PIC  9(007).
      *             15 'XX'-VBPRUEF     PIC  X(001).
 
               10  'XX'-DAT-VERBEG      PIC  9(008).
      *
      *  Datum des Vetragsbeginnes im Format JHJJMMTT
 
               10  'XX'-DAT-VEREND      PIC  9(008).
      *
      *  Datum des Vertragsendes im Format JHJJMMTT
 
               10  'XX'-DS-SPERRE       PIC  X(001).
      *
      *  Kennzeichen Datenschutzsperre
      *  'S' -> Kunde hat der Datenweitergabe widersprochen
      *  BLANK -> kein Widerspruch seitens des Kunden
 
               10  'XX'-DAT-DS-SPERRE   PIC  9(008).
      *
      *  Datum der Datenschutzsperre im Format JHJJMMTT
 
               10  FILLER               PIC  X(250).
 
 
 
           05  'XX'-NACHSATZ REDEFINES  'XX'-VORSATZ.
               10  'XX'-N-SA            PIC  X(001).
      *
      *  'E' für Nachsatz (E-Satz)
 
               10  'XX'-N-ANZ           PIC  9(008).
      *
      *   Anzahl der gelieferten C-Sätze
 
               10  FILLER               PIC  X(291).
 
      *---------------------------------------------------------------*
 
 

