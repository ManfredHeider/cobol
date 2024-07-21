      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2778.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2778.CPv  $
      *
      *   Rev 2.7   Mar 27 2017 09:30:20   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.6   Mar 17 2015 14:39:06   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.5   Jan 13 2015 13:15:42   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank MDTA
      *
      *   Rev 2.4   Sep 30 2013 16:05:40   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Sep 26 2013 15:34:38   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Sep 18 2013 08:09:56   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Sep 13 2013 09:19:32   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Sep 04 2013 16:06:26   t13ex78
      *Archiverstellung
      *
 
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 7 7 8    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER BANK-DTA Kundendaten                         *
      *
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
 
      *01  DRKI2778.
      *
           05  'XX'-VORSATZ.
               10  'XX'-V-SA           PIC  X(001).
      *
      *  'A' für Vorsatz (A-Satz)
 
               10  'XX'-V-ABSENDER     PIC  X(008).
      *
      *  Text, welcher den Absender des DTA identifiziert
 
               10  'XX'-V-DATUM        PIC  9(008).
      *
      *  Datum der Erstellung des DTA im Format JHJJMMTT
 
               10  'XX'-V-EMPFAENGER   PIC  X(004).
      *
      *  je nach Mandant 'DVAG', 'ALLF'
 
 
               10  'XX'-V-TEXT         PIC  X(008).
      *
      *  Text, der den Inhalt des DTA identifiziert
      *  'KUNDEN  ', 'VERTRAG ' oder 'BAUFI   '
 
               10  'XX'-LIEFERUNG      PIC  X(001).
 
      *  'K' für Komplettbestand; 'D' dür Deltalieferung
 
               10  FILLER              PIC  X(270).
 
 
 
 
           05  'XX'-SATZ REDEFINES 'XX'-VORSATZ.
               10  'XX'-SA             PIC  X(001).
      *
      *  'C' für C-Satz
               10  'XX'-GES            PIC  9(002).
      *
      *  Gesellschaftsnummer DVAG-intern
 
               10  'XX'-VERNR          PIC  9(015).
               10  'XX'-LFDNR          PIC  9(002).
               10  'XX'-NACHNAME       PIC  X(030).
               10  'XX'-VORNAME        PIC  X(030).
      *  Geburtsdatum im Format JHJJMMTT
               10  'XX'-GEBDAT         PIC  9(008).
      *
      *  Geburtsdatum des Kunden im Format JHJJMMTT
 
               10  'XX'-ANREDE         PIC  9(001).
      *
      *  Anredeschlüssel
      *    1 = Herr
      *    2 = Frau
      *    3 = Firma
      *    9 = keine Angabe
 
               10  'XX'-GESCHLECHT     PIC  9(001).
      *
      *  Verschlüsselung des Geschlechts
      *    1 = männlich
      *    2 = weiblich
      *    3 = unbekannt
 
               10  'XX'-STRHNR         PIC  X(040).
      *
      *  Straße und Hausnummer
 
               10  'XX'-PLZ            PIC  X(009).
               10  'XX'-ORT            PIC  X(030).
               10  'XX'-LAND           PIC  X(003).
      *
      *  ISO-Ländercode
 
               10  'XX'-LOESCHKZ       PIC  X(001).
      *
      *  Löschkennzeichen; 'D' wenn ein Kontoinhaber gelöscht werden
      *  soll
 
               10  FILLER              PIC  X(127).
 
 
 
           05  'XX'-NACHSATZ REDEFINES  'XX'-VORSATZ.
               10  'XX'-N-SA           PIC  X(001).
      *
      *  'E' für Nachsatz (E-Satz)
 
               10  'XX'-N-ANZ          PIC  9(008).
      *
      *  Anzahl der gelieferten C-Sätze
 
               10  FILLER              PIC  X(291).
 
      ******************************************************************
 
      *---------------------------------------------------------------*
 
 

