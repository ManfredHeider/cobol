      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2802.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2802.CPv  $
      *
      *   Rev 2.7   Mar 17 2015 14:37:52   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Mar 11 2015 12:27:22   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.5   Mar 09 2015 11:16:16   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 02 2015 10:52:12   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Feb 25 2015 11:42:50   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Jan 13 2015 13:06:28   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jul 24 2014 15:17:42   t13ex77
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Nov 27 2013 08:31:29 T13EX77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2802                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2802                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         09.10.2014 / 12:56:30 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2802
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2802.
           05  PI-DRKI2802-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2802-G68BLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2802-G68BVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI2802.
           05  PO-DRKI2802-RC                     PIC  X(003).
               88 PO-DRKI2802-RC-OK               VALUE 'OK '.
               88 PO-DRKI2802-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2802-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2802-G68BABWEICHBZ   PIC  X(0001).
             10 PO-DRKI2802-G68BANNUITAET   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2802-G68BANREDE   PIC  X(0001).
             10 PO-DRKI2802-G68BAUSZAHLDATUM   PIC  X(010).
             10 PO-DRKI2802-G68BBETRAGANFANG   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2802-G68BBETRGAKTUELL   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2802-G68BBIC   PIC  X(0011).
             10 PO-DRKI2802-G68BDARLEHENSART   PIC  X(0001).
             10 PO-DRKI2802-G68BFESTZINSENDE   PIC  X(010).
             10 PO-DRKI2802-G68BGEBURTSDATUM   PIC  X(010).
             10 PO-DRKI2802-G68BIBAN   PIC  X(0034).
             10 PO-DRKI2802-G68BLAND   PIC  X(0003).
             10 PO-DRKI2802-G68BMANDATREF   PIC  X(0035).
             10 PO-DRKI2802-G68BNACHNAME   PIC  X(0030).
             10 PO-DRKI2802-G68BNOMINALZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2802-G68BORT   PIC  X(0030).
             10 PO-DRKI2802-G68BPOSTLEITZAHL   PIC  X(0009).
             10 PO-DRKI2802-G68BPRODBEGINN   PIC  X(010).
             10 PO-DRKI2802-G68BPRODUKTENDE   PIC  X(010).
             10 PO-DRKI2802-G68BSTRASSE   PIC  X(0040).
             10 PO-DRKI2802-G68BTILGBEGINN   PIC  X(010).
             10 PO-DRKI2802-G68BTILGUNGPROZ   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2802-G68BTILGUNGSART   PIC  X(0001).
             10 PO-DRKI2802-G68BVORNAME   PIC  X(0030).
             10 PO-DRKI2802-G68BZAHLUNGSART   PIC  X(0001).
             10 PO-DRKI2802-G68BZUSAGEDATUM   PIC  X(010).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 
 
 
 

