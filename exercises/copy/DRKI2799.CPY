      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2799.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2799.CPv  $
      *
      *   Rev 2.7   Mar 17 2015 14:37:40   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Mar 11 2015 12:27:08   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.5   Mar 09 2015 11:16:02   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 02 2015 10:51:58   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Feb 25 2015 11:42:30   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Jan 13 2015 13:06:12   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jul 24 2014 15:17:22   t13ex77
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Nov 27 2013 08:25:23 T13EX77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2799                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2799                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         09.10.2014 / 12:53:39 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2799
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2799.
           05  PI-DRKI2799-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2799-G68KLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2799-G68KVERTRAGSNR   PIC  X(0015).
END   *
 
 
           05  PI-DRKI2799-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2799-G68KANREDE   PIC  X(0001).
             10 PI-DRKI2799-G68KGEBURTSDATUM   PIC  X(010).
             10 PI-DRKI2799-G68KGESCHLECHT   PIC  X(0001).
             10 PI-DRKI2799-G68KLAND   PIC  X(0003).
             10 PI-DRKI2799-G68KLOESCHKZ   PIC  X(0001).
             10 PI-DRKI2799-G68KNACHNAME   PIC  X(0030).
             10 PI-DRKI2799-G68KORT   PIC  X(0030).
             10 PI-DRKI2799-G68KPOSTLEITZAHL   PIC  X(0009).
             10 PI-DRKI2799-G68KSTRASSE   PIC  X(0040).
             10 PI-DRKI2799-G68KVORNAME   PIC  X(0030).
END   *
 
      *
       01  PO-DRKI2799.
           05  PO-DRKI2799-RC                     PIC  X(003).
               88 PO-DRKI2799-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2799-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 
 
 
 

