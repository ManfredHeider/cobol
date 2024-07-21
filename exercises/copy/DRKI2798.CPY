      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2798.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2798.CPv  $
      *
      *   Rev 2.7   Mar 17 2015 14:37:34   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Mar 11 2015 12:27:04   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.5   Mar 09 2015 11:15:58   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 02 2015 10:51:54   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Feb 25 2015 11:42:26   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Jan 13 2015 13:06:06   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jul 24 2014 15:17:14   t13ex77
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Nov 27 2013 08:25:21 T13EX77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2798                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2798                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         09.10.2014 / 12:53:37 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2798
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2798.
           05  PI-DRKI2798-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2798-G68KLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2798-G68KVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI2798.
           05  PO-DRKI2798-RC                     PIC  X(003).
               88 PO-DRKI2798-RC-OK               VALUE 'OK '.
               88 PO-DRKI2798-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2798-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2798-G68KANREDE   PIC  X(0001).
             10 PO-DRKI2798-G68KGEBURTSDATUM   PIC  X(010).
             10 PO-DRKI2798-G68KGESCHLECHT   PIC  X(0001).
             10 PO-DRKI2798-G68KLAND   PIC  X(0003).
             10 PO-DRKI2798-G68KLOESCHKZ   PIC  X(0001).
             10 PO-DRKI2798-G68KNACHNAME   PIC  X(0030).
             10 PO-DRKI2798-G68KORT   PIC  X(0030).
             10 PO-DRKI2798-G68KPOSTLEITZAHL   PIC  X(0009).
             10 PO-DRKI2798-G68KSTRASSE   PIC  X(0040).
             10 PO-DRKI2798-G68KVORNAME   PIC  X(0030).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 
 
 
 

