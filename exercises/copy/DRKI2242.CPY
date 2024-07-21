      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.04.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2242.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2242.CPv  $
      *
      *   Rev 2.4   Jul 02 2012 10:35:14   t13ex39
      *AKI01149: PIN bei DWS und DeuBa
      *
      *   Rev 2.3   Apr 22 2008 09:33:10   t13ex51
      *AKI00708: OnlineKI - Deutsche Bank Depot - Neue Insert und U
      *pdate Funktionen
      *
      *   Rev 2.2   Apr 14 2008 16:42:52   t13ex51
      *AKI00708: OnlineKI - Deutsche Bank Depot - Neue Insert und U
      *pdate Funktionen
      *
      *   Rev 2.1   Apr 09 2008 14:30:34   t13ex51
      *AKI00708: OnlineKI - Deutsche Bank Depot - Neue Insert und U
      *pdate Funktionen
      *
      *   Rev 2.0   Apr 09 2008 10:25:40   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2242                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2242                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         15.06.2012 / 14:56:12 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI2242
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2242.
           05  PI-DRKI2242-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2242-FOZFONDID   PIC S9(4) COMP.
             10 PI-DRKI2242-FOZGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2242-FOZLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2242-FOZVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2242-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2242-FOZAAUFSCHLAKTUELL   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2242-FOZAAUFSCHLURSPR   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2242-FOZBANKBLZ   PIC  X(0011).
             10 PI-DRKI2242-FOZBANKKONTONR   PIC  X(0018).
             10 PI-DRKI2242-FOZBANKNAME   PIC  X(0020).
             10 PI-DRKI2242-FOZBEGDATDYNAM   PIC  X(010).
             10 PI-DRKI2242-FOZBONIFIKATION   PIC S9(3)V9(4) COMP-3.
             10 PI-DRKI2242-FOZBUCHUNGSTEXTID   PIC S9(4) COMP.
             10 PI-DRKI2242-FOZDATBEGINNAUS   PIC  X(010).
             10 PI-DRKI2242-FOZDATENDEAUS   PIC  X(010).
             10 PI-DRKI2242-FOZDATVERTRAUFLOE   PIC  X(010).
             10 PI-DRKI2242-FOZDYNAMIK   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI2242-FOZEINAUSDATUM   PIC S9(4) COMP.
             10 PI-DRKI2242-FOZEINAUSTYP   PIC  X(0001).
             10 PI-DRKI2242-FOZEINZAHFRISTEND   PIC  X(010).
             10 PI-DRKI2242-FOZKONTOINHABER   PIC  X(0027).
             10 PI-DRKI2242-FOZKOSTENPROZ   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI2242-FOZKUENDMERKMAL   PIC  X(0001).
             10 PI-DRKI2242-FOZPIN   PIC S9(9) COMP.
             10 PI-DRKI2242-FOZPROZSATZDYNAMIS   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2242-FOZSPARPLANEROEF   PIC  X(010).
             10 PI-DRKI2242-FOZSPARPLANSCHLUSS   PIC  X(010).
             10 PI-DRKI2242-FOZURSPARRATE   PIC S9(4)V9(2) COMP-3.
             10 PI-DRKI2242-FOZURSPARRATEWKZ   PIC  X(0001).
             10 PI-DRKI2242-FOZURSPRSPARLST   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI2242-FOZURSPRSPARLSTWKZ   PIC  X(0001).
             10 PI-DRKI2242-FOZURSPRVNR   PIC S9(12)V COMP-3.
             10 PI-DRKI2242-FOZVERTRAGSTYP   PIC  X(0002).
             10 PI-DRKI2242-FOZVERTRLAUFZEIT   PIC S9(4) COMP.
             10 PI-DRKI2242-FOZZAHLUNG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2242-FOZZAHLUNGSWEISEID   PIC S9(4) COMP.
             10 PI-DRKI2242-FOZZAHLUNGWKZ   PIC  X(0001).
             10 PI-DRKI2242-FOZZLASTSCHRRUECK   PIC S9(4) COMP.
END   *


      *
       01  PO-DRKI2242.
           05  PO-DRKI2242-RC                     PIC  X(003).
               88 PO-DRKI2242-RC-OK               VALUE 'OK '.
               88 PO-DRKI2242-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
