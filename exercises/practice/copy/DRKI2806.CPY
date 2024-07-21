      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2806.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2806.CPv  $
      *
      *   Rev 2.7   Mar 17 2015 14:38:10   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Mar 11 2015 12:27:40   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.5   Mar 09 2015 11:16:32   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 02 2015 10:52:30   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Feb 25 2015 11:43:08   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Jan 13 2015 13:06:54   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jul 24 2014 15:18:16   t13ex77
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Nov 27 2013 08:19:33 T13EX77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2806                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2806                    *
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
      *    FÜR DIE FUNKTION DRKI2806
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2806.
           05 PI-DRKI2806-PROGRAMM                 PIC  X(008).
           05 PI-DRKI2806-UFKT-CODE                PIC  9(002).
              88 PI-DRKI2806-UFKT-FIRST                 VALUE 1.
              88 PI-DRKI2806-UFKT-NEXT                  VALUE 2.
              88 PI-DRKI2806-UFKT-QUIT                  VALUE 3.
              88 PI-DRKI2806-UFKT-UPDATE                VALUE 4.
 
      *
       01  PO-DRKI2806.
           05  PO-DRKI2806-RC                      PIC  X(003).
               88 PO-DRKI2806-RC-OK                     VALUE 'OK '.
               88 PO-DRKI2806-RC-EOF                    VALUE 'EOF'.
               88 PO-DRKI2806-RC-LEER                   VALUE 'LER'.
      *
           05  PO-DRKI2806-DATEN.
            10 'XX'-G68VVERTRAGSNR                PIC  X(0015).
            10 'XX'-G68VSTATUS                    PIC  X(0001).
            10 'XX'-G68VMANDANT                   PIC S9(9) COMP.
            10 'XX'-G68VABSCHLVBNR                PIC S9(9) COMP.
            10 'XX'-G68VVRTRGBEGINN               PIC  X(010).
            10 'XX'-G68VVRTRGENDE                 PIC  X(010).
            10 'XX'-NIL-G68VVRTRGENDE             PIC S9(4) COMP.
            10 'XX'-G68VDSSPERRE                  PIC  X(0001).
            10 'XX'-G68VDSSPERRDATUM              PIC  X(010).
            10 'XX'-NIL-G68VDSSPERRDATUM          PIC S9(4) COMP.
 
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 
 
 
 

