      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2808.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2808.CPv  $
      *
      *   Rev 2.7   Mar 17 2015 14:38:18   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Mar 11 2015 12:27:48   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.5   Mar 09 2015 11:16:42   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 02 2015 10:52:38   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Feb 25 2015 11:43:18   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Jan 13 2015 13:07:06   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jul 24 2014 15:18:30   t13ex77
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Nov 27 2013 08:19:33 T13EX77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2808                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2808                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.11.2013 / 08:19:33 / T13EX77
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
      *    FÜR DIE FUNKTION DRKI2808
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2808.
           05 PI-DRKI2808-PROGRAMM                 PIC  X(008).
           05 PI-DRKI2808-UFKT-CODE                PIC  9(002).
              88 PO-DRKI2808-UFKT-FIRST                 VALUE 1.
              88 PO-DRKI2808-UFKT-NEXT                  VALUE 2.
              88 PO-DRKI2808-UFKT-QUIT                  VALUE 3.
              88 PO-DRKI2808-UFKT-UPDATE                VALUE 4.
 
      *
       01  PO-DRKI2808.
           05  PO-DRKI2808-RC                      PIC  X(003).
               88 PO-DRKI2808-RC-OK                     VALUE 'OK '.
               88 PO-DRKI2808-RC-EOF                    VALUE 'EOF'.
               88 PO-DRKI2808-RC-LEER                   VALUE 'LER'.
      *
           05  PO-DRKI2808-DATEN.
            10 'XX'-G68KVERTRAGSNR                PIC  X(0015).
            10 'XX'-G68KLFDNR                     PIC S9(4) COMP.
            10 'XX'-G68KNACHNAME                  PIC  X(0030).
            10 'XX'-NIL-G68KNACHNAME              PIC S9(4) COMP.
            10 'XX'-G68KVORNAME                   PIC  X(0030).
            10 'XX'-NIL-G68KVORNAME               PIC S9(4) COMP.
            10 'XX'-G68KGEBURTSDATUM              PIC  X(010).
            10 'XX'-NIL-G68KGEBURTSDATUM          PIC S9(4) COMP.
            10 'XX'-G68KANREDE                    PIC  X(0001).
            10 'XX'-NIL-G68KANREDE                PIC S9(4) COMP.
            10 'XX'-G68KGESCHLECHT                PIC  X(0001).
            10 'XX'-NIL-G68KGESCHLECHT            PIC S9(4) COMP.
            10 'XX'-G68KSTRASSE                   PIC  X(0040).
            10 'XX'-NIL-G68KSTRASSE               PIC S9(4) COMP.
            10 'XX'-G68KPOSTLEITZAHL              PIC  X(0009).
            10 'XX'-NIL-G68KPOSTLEITZAHL          PIC S9(4) COMP.
            10 'XX'-G68KORT                       PIC  X(0030).
            10 'XX'-NIL-G68KORT                   PIC S9(4) COMP.
            10 'XX'-G68KLAND                      PIC  X(0003).
            10 'XX'-NIL-G68KLAND                  PIC S9(4) COMP.
            10 'XX'-G68KLOESCHKZ                  PIC  X(0001).
 
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 
 
 
 

