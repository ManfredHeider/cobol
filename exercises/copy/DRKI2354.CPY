      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2354.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2354.CPv  $
      *
      *   Rev 2.5   Jan 21 2021 17:05:36   u130685
      *AKI01821: Guthabenzahlung COPY
      *
      *   Rev 2.4   Jan 20 2021 14:09:12   u130685
      *AKI01821: Guthabenzahlung COPY
      *
      *   Rev 2.1   Aug 20 2009 15:24:58   t13ex50
      *AKI00844: DWS-Investmentsparpläne
      *
      *   Rev 2.0   Aug 13 2009 08:34:56   t13ex50
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2354                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2354                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.01.2021 / 16:59:13 / U130685
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
      *    FÜR DIE FUNKTION DRKI2354
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2354.
           05  PI-DRKI2354-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2354-FOZFONDID   PIC S9(4) COMP.
             10 PI-DRKI2354-FOZGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2354-FOZLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2354-FOZVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2354.
           05  PO-DRKI2354-RC                     PIC  X(003).
               88 PO-DRKI2354-RC-OK               VALUE 'OK '.
               88 PO-DRKI2354-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2354-NI.
               10 NI-DRKI2354-TAB.
                  15 DRKI2354-ELE
                     PIC S9(04) COMP OCCURS 2 TIMES.
               10 NI-DRKI2354 REDEFINES NI-DRKI2354-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI2354-NI-FOZBONIFIKATION   PIC S9(4) COMP.
             15 PO-DRKI2354-NI-FOZURSPRVNR   PIC S9(4) COMP.
END   *

           05  PO-DRKI2354-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2354-FOZAAUFSCHLAKTUELL   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI2354-FOZAAUFSCHLURSPR   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI2354-FOZABRKTO-KNR   PIC S9(4) COMP.
             10 PO-DRKI2354-FOZABRKTO-KTY   PIC S9(4) COMP.
             10 PO-DRKI2354-FOZANZKINDERZUL   PIC S9(4) COMP.
             10 PO-DRKI2354-FOZBANKBLZ   PIC  X(0011).
             10 PO-DRKI2354-FOZBANKKONTONR   PIC  X(0018).
             10 PO-DRKI2354-FOZBANKNAME   PIC  X(0020).
             10 PO-DRKI2354-FOZBEGDATDYNAM   PIC  X(010).
             10 PO-DRKI2354-FOZBETRHOECHSTST   PIC S9(13)V9(2) COMP-3.
             10 PO-DRKI2354-FOZBONIFIKATION   PIC S9(3)V9(4) COMP-3.
             10 PO-DRKI2354-FOZBUCHUNGSTEXTID   PIC S9(4) COMP.
             10 PO-DRKI2354-FOZDATBEGABLSTAB   PIC  X(010).
             10 PO-DRKI2354-FOZDATBEGHOECHSI   PIC  X(010).
             10 PO-DRKI2354-FOZDATBEGINNAUS   PIC  X(010).
             10 PO-DRKI2354-FOZDATENDEAUS   PIC  X(010).
             10 PO-DRKI2354-FOZDATGELDBEWEG   PIC  X(010).
             10 PO-DRKI2354-FOZDATKAPUEBERTR   PIC  X(010).
             10 PO-DRKI2354-FOZDATKUENDIGUNG   PIC  X(010).
             10 PO-DRKI2354-FOZDATLETHOECHST   PIC  X(010).
             10 PO-DRKI2354-FOZDATVERTRAUFLOE   PIC  X(010).
             10 PO-DRKI2354-FOZDAUERZULAGEKZ   PIC  X(0001).
             10 PO-DRKI2354-FOZDYNAMIK   PIC S9(1)V9(2) COMP-3.
             10 PO-DRKI2354-FOZEINAUSDATUM   PIC S9(4) COMP.
             10 PO-DRKI2354-FOZEINAUSTYP   PIC  X(0001).
             10 PO-DRKI2354-FOZEINZAHFRISTEND   PIC  X(010).
             10 PO-DRKI2354-FOZGARANTIESUMME   PIC S9(13)V9(2) COMP-3.
             10 PO-DRKI2354-FOZKONTOINHABER   PIC  X(0027).
             10 PO-DRKI2354-FOZKOSTENPROZ   PIC S9(1)V9(2) COMP-3.
             10 PO-DRKI2354-FOZKUENDMERKMAL   PIC  X(0001).
             10 PO-DRKI2354-FOZMAHNSTATUS   PIC  X(0002).
             10 PO-DRKI2354-FOZPIN   PIC S9(9) COMP.
             10 PO-DRKI2354-FOZPROZSATZDYNAMIS   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI2354-FOZSPARPLANEROEF   PIC  X(010).
             10 PO-DRKI2354-FOZSPARPLANSCHLUSS   PIC  X(010).
             10 PO-DRKI2354-FOZUEBERTRAGART   PIC  X(0001).
             10 PO-DRKI2354-FOZURSPARRATE   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI2354-FOZURSPARRATEWKZ   PIC  X(0001).
             10 PO-DRKI2354-FOZURSPRSPARLST   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI2354-FOZURSPRSPARLSTWKZ   PIC  X(0001).
             10 PO-DRKI2354-FOZURSPRVNR   PIC S9(12)V COMP-3.
             10 PO-DRKI2354-FOZVERTRAGSTYP   PIC  X(0002).
             10 PO-DRKI2354-FOZVERTRLAUFZEIT   PIC S9(4) COMP.
             10 PO-DRKI2354-FOZWEGKAPUEBERTR   PIC  X(0001).
             10 PO-DRKI2354-FOZZAHLUNG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2354-FOZZAHLUNGSWEISEID   PIC S9(4) COMP.
             10 PO-DRKI2354-FOZZAHLUNGWKZ   PIC  X(0001).
             10 PO-DRKI2354-FOZZLASTSCHRRUECK   PIC S9(4) COMP.
             10 PO-DRKI2354-FOZZULBERECHTKZ   PIC  X(0001).
             10 PO-DRKI2354-LETZTEAENDERUNG   PIC  X(0026).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
