      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.04.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2241.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2241.CPv  $
      *
      *   Rev 2.3   Jul 02 2012 10:35:04   t13ex39
      *AKI01149: PIN bei DWS und DeuBa
      *
      *   Rev 2.2   Apr 14 2008 16:42:46   t13ex51
      *AKI00708: OnlineKI - Deutsche Bank Depot - Neue Insert und U
      *pdate Funktionen
      *
      *   Rev 2.1   Apr 09 2008 14:30:26   t13ex51
      *AKI00708: OnlineKI - Deutsche Bank Depot - Neue Insert und U
      *pdate Funktionen
      *
      *   Rev 2.0   Apr 09 2008 10:25:34   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2241                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2241                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         15.06.2012 / 14:56:08 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI2241
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2241.
           05  PI-DRKI2241-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2241-FOZFONDID   PIC S9(4) COMP.
             10 PI-DRKI2241-FOZGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2241-FOZLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2241-FOZVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2241-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2241-FOZAAUFSCHLAKTUELL   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2241-FOZAAUFSCHLURSPR   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2241-FOZBANKBLZ   PIC  X(0011).
             10 PI-DRKI2241-FOZBANKKONTONR   PIC  X(0018).
             10 PI-DRKI2241-FOZBANKNAME   PIC  X(0020).
             10 PI-DRKI2241-FOZBEGDATDYNAM   PIC  X(010).
             10 PI-DRKI2241-FOZBONIFIKATION   PIC S9(3)V9(4) COMP-3.
             10 PI-DRKI2241-FOZBUCHUNGSTEXTID   PIC S9(4) COMP.
             10 PI-DRKI2241-FOZDATBEGINNAUS   PIC  X(010).
             10 PI-DRKI2241-FOZDATENDEAUS   PIC  X(010).
             10 PI-DRKI2241-FOZDATVERTRAUFLOE   PIC  X(010).
             10 PI-DRKI2241-FOZDYNAMIK   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI2241-FOZEINAUSDATUM   PIC S9(4) COMP.
             10 PI-DRKI2241-FOZEINAUSTYP   PIC  X(0001).
             10 PI-DRKI2241-FOZEINZAHFRISTEND   PIC  X(010).
             10 PI-DRKI2241-FOZKONTOINHABER   PIC  X(0027).
             10 PI-DRKI2241-FOZKOSTENPROZ   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI2241-FOZKUENDMERKMAL   PIC  X(0001).
             10 PI-DRKI2241-FOZPIN   PIC S9(9) COMP.
             10 PI-DRKI2241-FOZPROZSATZDYNAMIS   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2241-FOZSPARPLANEROEF   PIC  X(010).
             10 PI-DRKI2241-FOZSPARPLANSCHLUSS   PIC  X(010).
             10 PI-DRKI2241-FOZURSPARRATE   PIC S9(4)V9(2) COMP-3.
             10 PI-DRKI2241-FOZURSPARRATEWKZ   PIC  X(0001).
             10 PI-DRKI2241-FOZURSPRSPARLST   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI2241-FOZURSPRSPARLSTWKZ   PIC  X(0001).
             10 PI-DRKI2241-FOZURSPRVNR   PIC S9(12)V COMP-3.
             10 PI-DRKI2241-FOZVERTRAGSTYP   PIC  X(0002).
             10 PI-DRKI2241-FOZVERTRLAUFZEIT   PIC S9(4) COMP.
             10 PI-DRKI2241-FOZZAHLUNG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2241-FOZZAHLUNGSWEISEID   PIC S9(4) COMP.
             10 PI-DRKI2241-FOZZAHLUNGWKZ   PIC  X(0001).
             10 PI-DRKI2241-FOZZLASTSCHRRUECK   PIC S9(4) COMP.
END   *

      *
       01  PO-DRKI2241.
           05  PO-DRKI2241-RC                     PIC  X(003).
               88 PO-DRKI2241-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2241-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
