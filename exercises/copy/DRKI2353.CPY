      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2353.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2353.CPv  $
      *
      *   Rev 2.7   Jan 21 2021 17:05:20   u130685
      *AKI01821: Guthabenzahlung COPY
      *
      *   Rev 2.6   Jan 20 2021 14:08:54   u130685
      *AKI01821: Guthabenzahlung COPY
      *
      *   Rev 2.5   Nov 18 2019 12:46:52   t13ex39
      *AKI01733: FoDB Verarbeitung "urspr. Sparrate des Sparplan"
      * OZP
      *
      *   Rev 2.4   Jul 02 2012 10:36:20   t13ex39
      *AKI01149: PIN bei DWS und DeuBa
      *
      *   Rev 2.3   Jun 29 2011 07:55:36   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g Dauerzulagenantrag
      *
      *   Rev 2.1   Aug 20 2009 15:24:52   t13ex50
      *AKI00844: DWS-Investmentsparpläne
      *
      *   Rev 2.0   Aug 13 2009 08:34:48   t13ex50
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2353                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2353                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.01.2021 / 16:59:11 / U130685
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2353
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2353.
           05  PI-DRKI2353-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2353-FOZFONDID   PIC S9(4) COMP.
             10 PI-DRKI2353-FOZGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2353-FOZLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2353-FOZVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2353-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2353-NI-FOZABRKTO-KNR    PIC S9(4) COMP.
             15 PI-DRKI2353-NI-FOZABRKTO-KTY    PIC S9(4) COMP.
             15 PI-DRKI2353-NI-FOZANZKINDERZUL  PIC S9(4) COMP.
             15 PI-DRKI2353-N-FZBTRHCHSTST      PIC S9(4) COMP.
             15 PI-DRKI2353-NI-FOZBONIFIKATION  PIC S9(4) COMP.
             15 PI-DRKI2353-N-FZGRNTSMM         PIC S9(4) COMP.
             15 PI-DRKI2353-NI-FOZPIN           PIC S9(4) COMP.
             15 PI-DRKI2353-NI-FOZURSPRVNR      PIC S9(4) COMP.
END   *

           05  PI-DRKI2353-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2353-FOZAAUFSCHLAKTUELL   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2353-FOZAAUFSCHLURSPR   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2353-FOZABRKTO-KNR   PIC S9(4) COMP.
             10 PI-DRKI2353-FOZABRKTO-KTY   PIC S9(4) COMP.
             10 PI-DRKI2353-FOZANZKINDERZUL   PIC S9(4) COMP.
             10 PI-DRKI2353-FOZBANKBLZ   PIC  X(0011).
             10 PI-DRKI2353-FOZBANKKONTONR   PIC  X(0018).
             10 PI-DRKI2353-FOZBANKNAME   PIC  X(0020).
             10 PI-DRKI2353-FOZBEGDATDYNAM   PIC  X(010).
             10 PI-DRKI2353-FOZBETRHOECHSTST   PIC S9(13)V9(2) COMP-3.
             10 PI-DRKI2353-FOZBONIFIKATION   PIC S9(3)V9(4) COMP-3.
             10 PI-DRKI2353-FOZBUCHUNGSTEXTID   PIC S9(4) COMP.
             10 PI-DRKI2353-FOZDATBEGABLSTAB   PIC  X(010).
             10 PI-DRKI2353-FOZDATBEGHOECHSI   PIC  X(010).
             10 PI-DRKI2353-FOZDATBEGINNAUS   PIC  X(010).
             10 PI-DRKI2353-FOZDATENDEAUS   PIC  X(010).
             10 PI-DRKI2353-FOZDATGELDBEWEG   PIC  X(010).
             10 PI-DRKI2353-FOZDATKAPUEBERTR   PIC  X(010).
             10 PI-DRKI2353-FOZDATKUENDIGUNG   PIC  X(010).
             10 PI-DRKI2353-FOZDATLETHOECHST   PIC  X(010).
             10 PI-DRKI2353-FOZDATVERTRAUFLOE   PIC  X(010).
             10 PI-DRKI2353-FOZDAUERZULAGEKZ   PIC  X(0001).
             10 PI-DRKI2353-FOZDYNAMIK   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI2353-FOZEINAUSDATUM   PIC S9(4) COMP.
             10 PI-DRKI2353-FOZEINAUSTYP   PIC  X(0001).
             10 PI-DRKI2353-FOZEINZAHFRISTEND   PIC  X(010).
             10 PI-DRKI2353-FOZGARANTIESUMME   PIC S9(13)V9(2) COMP-3.
             10 PI-DRKI2353-FOZKONTOINHABER   PIC  X(0027).
             10 PI-DRKI2353-FOZKOSTENPROZ   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI2353-FOZKUENDMERKMAL   PIC  X(0001).
             10 PI-DRKI2353-FOZMAHNSTATUS   PIC  X(0002).
             10 PI-DRKI2353-FOZPIN   PIC S9(9) COMP.
             10 PI-DRKI2353-FOZPROZSATZDYNAMIS   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2353-FOZSPARPLANEROEF   PIC  X(010).
             10 PI-DRKI2353-FOZSPARPLANSCHLUSS   PIC  X(010).
             10 PI-DRKI2353-FOZUEBERTRAGART   PIC  X(0001).
             10 PI-DRKI2353-FOZURSPARRATE   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2353-FOZURSPARRATEWKZ   PIC  X(0001).
             10 PI-DRKI2353-FOZURSPRSPARLST   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI2353-FOZURSPRSPARLSTWKZ   PIC  X(0001).
             10 PI-DRKI2353-FOZURSPRVNR   PIC S9(12)V COMP-3.
             10 PI-DRKI2353-FOZVERTRAGSTYP   PIC  X(0002).
             10 PI-DRKI2353-FOZVERTRLAUFZEIT   PIC S9(4) COMP.
             10 PI-DRKI2353-FOZWEGKAPUEBERTR   PIC  X(0001).
             10 PI-DRKI2353-FOZZAHLUNG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2353-FOZZAHLUNGSWEISEID   PIC S9(4) COMP.
             10 PI-DRKI2353-FOZZAHLUNGWKZ   PIC  X(0001).
             10 PI-DRKI2353-FOZZLASTSCHRRUECK   PIC S9(4) COMP.
             10 PI-DRKI2353-FOZZULBERECHTKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2353-NI1.
           05 PI-DRKI2353-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 8.
       01  PI-DRKI2353-NI7.
           05 PI-DRKI2353-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 8.
      *
       01  PO-DRKI2353.
           05  PO-DRKI2353-RC                     PIC  X(003).
               88 PO-DRKI2353-RC-OK               VALUE 'OK '.
               88 PO-DRKI2353-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
